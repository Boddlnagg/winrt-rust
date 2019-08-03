use std::io::Write;
use std::collections::HashSet;

use climeta::{ResolveToTypeDef, AssemblyAccess};
use climeta::schema::{self, TypeDef, TypeRef, Type, TypeTag, PrimitiveType, FieldInit, PrimitiveValue};

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
    Interface(TypeDef<'db>, Option<InterfaceKind>),
    Dummy
}

#[derive(Debug)]
pub enum InterfaceKind {
    Factory,
    Statics,
    Instance
}

fn get_interface_kind<'db, 'c: 'db>(cache: &'c climeta::Cache<'c>, td: &TypeDef<'db>, exclusive_to_type: Option<TypeDef<'db>>) -> Result<InterfaceKind> {
    let trimmed_name = td.type_name()?.trim_end_matches(&['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'][..]);
    let guessed_from_name = trimmed_name.ends_with("Factory") || trimmed_name.ends_with("Statics");
    let mut candidates: [Option<TypeDef>; 3] = [None, None, None];
    if let Some(t) = exclusive_to_type {
        candidates[0].replace(t);
    }
    if guessed_from_name {
        let target_type_name = &trimmed_name[0..trimmed_name.len() - 7]; // "Factory" and "Statics" both have length 7
        if let Some(found) = (td.type_namespace()?, target_type_name).resolve(cache) {
            candidates[1].replace(found);
        }
        if target_type_name.starts_with('I') {
            if let Some(found) = (td.type_namespace()?, &target_type_name[1..]).resolve(cache) {
                candidates[2].replace(found);
            }
        }
    }
    let target_name = td.namespace_name_pair();
    if candidates.iter().any(|candidate| {
        if let Some(c) = candidate {
            let mut types = get_factory_types(&c, cache).expect("error retrieving factory types");
            types.any(|c2| c2 == target_name)
        } else {
            false
        }
    }) {
        Ok(InterfaceKind::Factory)
    } else if candidates.iter().any(|candidate| {
        if let Some(c) = candidate {
            let mut types = get_static_types(&c, cache).expect("error retrieving static types");
            types.any(|c2| c2 == target_name)
        } else {
            false
        }
    }) {
        Ok(InterfaceKind::Statics)
    } else {
        Ok(InterfaceKind::Instance)
    }
}

fn get_factory_types<'db, 'c: 'db, 'r>(td: &'r TypeDef<'db>, cache: &'c climeta::Cache<'c>) -> Result<impl Iterator<Item=(&'r str, &'r str)>> where 'db: 'r, 'c: 'r {
    let cache: &'r climeta::Cache<'r> = unsafe { std::mem::transmute(cache) }; // TODO: find a better way to shrink the lifetime here ...
    Ok(td.custom_attributes()?.filter_map(move |attr| {
        let pair = attr.namespace_name_pair();
        if attr.namespace_name_pair() != ("Windows.Foundation.Metadata", "ActivatableAttribute") {
            return None;
        }
        let sig: climeta::schema::CustomAttributeSig<'r> = attr.value(cache).expect("Error reading value of ActivatableAttribute");
        match &sig.fixed_args()[0] {
            schema::FixedArg::Elem(schema::Elem::SystemType(typename)) => Some(typename.namespace_name_pair()),
            _ => None
        }
    }))
}

fn get_static_types<'db, 'c: 'db, 'r>(td: &'r TypeDef<'db>, cache: &'c climeta::Cache<'c>) -> Result<impl Iterator<Item=(&'r str, &'r str)>> where 'db: 'r, 'c: 'r {
    let cache: &'r climeta::Cache<'r> = unsafe { std::mem::transmute(cache) }; // TODO: find a better way to shrink the lifetime here ...
    Ok(td.custom_attributes()?.filter_map(move |attr| {
        let pair = attr.namespace_name_pair();
        if attr.namespace_name_pair() != ("Windows.Foundation.Metadata", "StaticAttribute") {
            return None;
        }
        let sig: climeta::schema::CustomAttributeSig<'r> = attr.value(cache).expect("Error reading value of StaticAttribute");
        match &sig.fixed_args()[0] {
            schema::FixedArg::Elem(schema::Elem::SystemType(typename)) => Some(typename.namespace_name_pair()),
            _ => unreachable!()
        }
    }))
}

impl<'db> TyDef<'db> {
    pub fn prepare_enum(ty: TypeDef<'db>) -> Result<TyDef<'db>> {
        Ok(TyDef::Enum(ty))
    }

    pub fn prepare_struct(ty: TypeDef<'db>) -> Result<TyDef<'db>> {
        Ok(TyDef::Struct(ty, String::new()))
    }

    pub fn prepare_interface(ty: TypeDef<'db>) -> Result<TyDef<'db>> {
        Ok(TyDef::Interface(ty, None))
    }

    pub fn can_be_skipped(&self) -> bool {
        match self {
            TyDef::Enum(_) => false,
            TyDef::Struct(td, _) => td.field_list().unwrap().next().is_none(),
            TyDef::Interface(..) => false,
            TyDef::Dummy => true
        }
    }

    pub fn collect_dependencies<'c: 'db>(&mut self, cache: &'c climeta::Cache<'c>) -> Result<Dependencies> {
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
            TyDef::Interface(td, ref mut kind) => {
                let exclusive_to = td.get_attribute("Windows.Foundation.Metadata", "ExclusiveToAttribute")?;
                let exclusive_to_type = if let Some(ex) = exclusive_to {
                    let sig = ex.value(cache)?;
                    match sig.fixed_args() {
                        &[schema::FixedArg::Elem(schema::Elem::SystemType(typename))] => Some(typename.resolve(cache).expect("ExclusiveTo type not found")),
                        _ => return Err("invalid signature for ExclusiveToAttribute".into())
                    }
                } else {
                    None
                };
                kind.replace(get_interface_kind(cache, td, exclusive_to_type)?);
                //println!("Interface {} is of kind {:?}", td.definition_name()?, kind);
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
            TyDef::Interface(td, kind) => {
                writeln!(file, "\nRT_INTERFACE! {{ {prepend_static}interface {name}{generic}({name}Vtbl): IInspectable [IID_{name}] {{",
                    prepend_static = match kind.as_ref().unwrap() {
                        InterfaceKind::Factory | InterfaceKind::Statics => "static ",
                        InterfaceKind::Instance => ""
                    },
                    name = td.definition_name()?,
                    generic = ""
                )?;
                // TODO: raw methods
                writeln!(file, "}}}}")?;
            }
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
                    if !ty.contains_generic_var() {
                        // TODO: add generic pinterface instance to output
                    }
                } else {
                    path.push_str(fullname.1);
                    assert!(generic_args.is_none());
                }

                match (tag, usage) {
                    (TypeTag::ValueType, _) => path,
                    (TypeTag::Class, TypeUsage::Raw) => format!("<{} as RtType>::Abi", path),
                    (TypeTag::Class, TypeUsage::In) => format!("&{}", path),
                    (TypeTag::Class, TypeUsage::Out) => format!("Option<{}>", path),
                    (TypeTag::Class, TypeUsage::OutNonNull) => path,
                    (TypeTag::Class, TypeUsage::GenericArg) => path,
                    _ => unimplemented!()
                }
            }
        },
        Type::GenericVar(scope, idx) => unimplemented!(),
        Type::Object => {
            match usage {
                TypeUsage::Raw => "<IInspectable as RtType>::Abi".into(),
                TypeUsage::In => "&IInspectable".into(),
                TypeUsage::Out => "Option<IInspectable>".into(),
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
