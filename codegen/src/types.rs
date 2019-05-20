use std::io::Write;

use climeta::ResolveToTypeDef;
use climeta::schema::{TypeDef, Type, TypeTag, PrimitiveType, FieldInit, PrimitiveValue};

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

pub struct Dependencies {} // TODO

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
        match self {
            TyDef::Enum(_) => { /* Nothing to do */ },
            TyDef::Struct(td, ref mut prepared) => {
                let result = itertools::join(td.field_list()?.map(|f| {
                    format!("{}: {}",
                        prevent_keywords(f.name().expect("can't decode struct field name")),
                        get_type_name(f.signature().expect("can't decode struct field signature").type_(), TypeUsage::Raw).expect("can't get struct field type name"))
                }), ", ");

                std::mem::replace(prepared, result);
            },
            TyDef::Dummy => {}
        }

        Ok(Dependencies {})
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
                writeln!(file, "\n}}}}")?;
            },
            TyDef::Dummy => {}
        }
        Ok(())
    }

    pub fn dummy() -> Result<TyDef<'db>> {
        Ok(TyDef::Dummy)
    }
}

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

pub fn get_type_name(ty: &Type, usage: TypeUsage) -> Result<String> {
    Ok(match ty {
        Type::Primitive(prim) => get_type_name_primitive(*prim, usage)?,
        Type::Array(array) => unimplemented!(),
        Type::Ref(tag, def_or_ref, generic_args) => if *tag == TypeTag::ValueType {
            let fullname = def_or_ref.namespace_name_pair();
            if fullname == ("System", "Guid") {
                "Guid".into()
            } else {
                let mut path = format!("crate::{}::", fullname.0);
                let mut path = path.replace(".", "::");
                path.make_ascii_lowercase();
                path.push_str(fullname.1);
                path
            }
        } else {
            let fullname = def_or_ref.namespace_name_pair();
            //panic!("Unimplemented handling for {:?}", fullname);
            format!("[WIP]{:?}", fullname)
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
