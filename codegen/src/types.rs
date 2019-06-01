use std::io::Write;
use std::collections::HashSet;

use climeta::{ResolveToTypeDef, AssemblyInfo};
use climeta::schema::{TypeDef, TypeRef, Type, TypeTag, PrimitiveType, FieldInit, PrimitiveValue};

use crate::Result;
use crate::generator::prevent_keywords;

trait TypeDefExt<'db> {
    /// Type name without the marker for generic types
    fn definition_name(&self) -> Result<&'db str>;
}

impl<'db> TypeDefExt<'db> for TypeDef<'db> {
    fn definition_name(&self) -> Result<&'db str> {
        let name = self.type_name()?;
        Ok(match name.find('`') {
            None => name,
            Some(i) => &name[..i]
        })
    }
}

#[derive(Default, Debug)]
pub struct Dependencies {
    assemblies: HashSet<String>
}

impl Dependencies {
    fn add_type(&mut self, typ: &climeta::schema::TypeDefOrRef) {
        use climeta::schema::TypeDefOrRef::*;
        let name = match typ {
            TypeDef(d) => d.assembly_name().expect("TypeDef without assembly name"),
            TypeRef(r) => match r.resolution_scope().expect("can't access resolution scope") {
                Some(climeta::schema::ResolutionScope::AssemblyRef(asm)) => asm.name().expect("can't access assembly name"),
                Some(_) => r.assembly_name().expect("TypeRef without assembly name"),
                None => unimplemented!()
            },
            _ => unimplemented!()
        };
        
        if !self.assemblies.contains(name) {
            self.assemblies.insert(name.into());
        }
    }
}

#[derive(Debug)]
pub enum TyDef<'db> {
    Enum(TypeDef<'db>),
    Struct(TypeDef<'db>, String),
    Dummy
}

impl<'db> TyDef<'db> {
    pub fn prepare_enum(ty: TypeDef<'db>) -> Result<TyDef<'db>> {
        Ok(TyDef::Enum(ty))
    }

    pub fn prepare_struct(ty: TypeDef<'db>) -> Result<TyDef<'db>> {
        Ok(TyDef::Struct(ty, String::new()))
    }

    pub fn can_be_skipped(&self) -> bool {
        match self {
            TyDef::Enum(_) => false,
            TyDef::Struct(td, _) => td.field_list().unwrap().next().is_none(),
            TyDef::Dummy => true
        }
    }

    pub fn collect_dependencies(&mut self) -> Result<Dependencies> {
        let mut deps = Dependencies::default();
        match self {
            TyDef::Enum(_) => { /* Nothing to do */ },
            TyDef::Struct(td, ref mut prepared) => {
                let result = itertools::join(td.field_list()?.map(|f| {
                    format!("{}: {}",
                        prevent_keywords(f.name().expect("can't decode struct field name")),
                        get_type_name(f.signature().expect("can't decode struct field signature").type_(), TypeUsage::Raw, &mut deps).expect("can't get struct field type name"))
                }), ", ");
                //result.push_str(&format!(" [[deps: {:?}]]", deps));
                std::mem::replace(prepared, result);
            },
            TyDef::Dummy => {}
        }

        Ok(deps)
    }

    pub fn emit(&self, file: &mut std::fs::File) -> Result<()> {
        match self {
            TyDef::Enum(ref td) => {
                let underlying = td.enum_get_underlying_type()?;
                let underlying_name = get_type_name_primitive(underlying, TypeUsage::Raw)?;
                //writeln!(file, "  // {} : {}", td.type_name()?, underlying_name)?;
                writeln!(file, "\nRT_ENUM! {{ enum {0}: {1} {{", td.definition_name()?, underlying_name)?;
                write!(file, "    ")?;
                let mut first = true;
                for field in td.field_list()? {
                    let flags = field.flags()?;
                    if flags.literal() || flags.static_() {
                        if !first {
                            write!(file, ", ")?;
                        }
                        let name = field.name()?;
                        let constant = field.constant()?.expect("expecting constant value for enum constant");
                        let value = constant.value()?;
                        write!(file, "{} = ", prevent_keywords(name))?;
                        match value {
                            FieldInit::Primitive(PrimitiveValue::Int32(num)) => write!(file, "{}", num)?,
                            FieldInit::Primitive(PrimitiveValue::UInt32(num)) => write!(file, "{}", num)?,
                            other => panic!("Unexpected type of constant value: {:?}", other),
                        }                        
                        first = false;
                    }
                }
                writeln!(file, "\n}}}}")?;
            },
            TyDef::Struct(td, prepared) => {
                // TODO: derive(Eq) whenever possible?
                writeln!(file, "\nRT_STRUCT! {{ struct {0} {{", td.definition_name()?)?;
                writeln!(file, "    {}", prepared)?;
                writeln!(file, "}}}}")?;
            },
            TyDef::Dummy => {}
        }
        Ok(())
    }

    pub fn dummy() -> Result<TyDef<'db>> {
        Ok(TyDef::Dummy)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeUsage {
    In,
    Out,
    OutNonNull,
    Raw,
    Alias,
    GenericArg,
}

pub fn get_type_name_primitive(ty: PrimitiveType, usage: TypeUsage) -> Result<String> {
    Ok(match ty {
        PrimitiveType::Boolean => "bool".into(),
        PrimitiveType::Char => "Char".into(),
        PrimitiveType::I1 => "i8".into(),
        PrimitiveType::U1 => "u8".into(),
        PrimitiveType::I2 => "i16".into(),
        PrimitiveType::U2 => "u16".into(),
        PrimitiveType::I4 => "i32".into(),
        PrimitiveType::U4 => "u32".into(),
        PrimitiveType::I8 => "i64".into(),
        PrimitiveType::U8 => "u64".into(),
        PrimitiveType::R4 => "f32".into(),
        PrimitiveType::R8 => "f64".into(),
        PrimitiveType::I => unimplemented!(),
        PrimitiveType::U => unimplemented!(),
    })
}

pub fn get_type_name(ty: &Type, usage: TypeUsage, deps: &mut Dependencies) -> Result<String> {
    Ok(match ty {
        Type::Primitive(prim) => get_type_name_primitive(*prim, usage)?,
        Type::Array(array) => unimplemented!(),
        Type::Ref(tag, def_or_ref, generic_args) => {
            deps.add_type(def_or_ref);
            let fullname = def_or_ref.namespace_name_pair();
            if fullname == ("System", "Guid") {
                assert!(*tag == TypeTag::ValueType);
                "Guid".into()
            } else {
                let mut path = format!("crate::{}::", fullname.0);
                path = path.replace(".", "::");
                path.make_ascii_lowercase();
                if let Some(i) = fullname.1.find('`') {
                    path.push_str(&fullname.1[..i]);
                    assert!(generic_args.is_some());
                    let generic_args = generic_args.as_ref().unwrap().as_ref();
                    path.push_str("<");
                    let mut first = true;
                    for arg in generic_args {
                        if !first {
                            path.push_str(", ");
                        }
                        path.push_str(&get_type_name(arg, usage, deps)?);
                        first = false;
                    }
                    path.push_str(">");
                    if (!ty.contains_generic_var()) {
                        // TODO: add generic pinterface instance to output
                    }
                } else {
                    path.push_str(fullname.1);
                    assert!(generic_args.is_none());
                }

                match (tag, usage) {
                    (TypeTag::ValueType, _) => path,
                    (TypeTag::Class, TypeUsage::Raw) => format!("*mut {}", path),
                    (TypeTag::Class, TypeUsage::In) => format!("&{}", path),
                    (TypeTag::Class, TypeUsage::Out) => format!("Option<ComPtr<{}>>", path),
                    (TypeTag::Class, TypeUsage::OutNonNull) => format!("ComPtr<{}>", path),
                    (TypeTag::Class, TypeUsage::GenericArg) => path,
                    _ => unimplemented!()
                }
            }
        },
        Type::GenericVar(scope, idx) => unimplemented!(),
        Type::Object => {
            match usage {
                TypeUsage::Raw => "*mut IInspectable".into(),
                TypeUsage::In => "&IInspectable".into(),
                TypeUsage::Out => "Option<ComPtr<IInspectable>>".into(),
                TypeUsage::GenericArg => "IInspectable".into(),
                _ => unimplemented!()
            }
        },
        Type::String => {
            match usage {
                TypeUsage::Raw => "HSTRING".into(),
                TypeUsage::In => "&HStringArg".into(),
                TypeUsage::Out => "HString".into(),
                TypeUsage::GenericArg => "HString".into(),
                _ => unimplemented!()
            }
        }
    })
}
