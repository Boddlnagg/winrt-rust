use std::io::Write;

use climeta::schema::{TypeDef, Type, PrimitiveType, FieldInit, PrimitiveValue};

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

#[derive(Debug)]
pub enum TyDef<'db> {
    Enum(TypeDef<'db>),
    Dummy
}

impl<'db> TyDef<'db> {
    pub fn prepare_enum(ty: TypeDef<'db>) -> Result<TyDef<'db>> {
        //println!("Type: {:?}", ty);
        Ok(TyDef::Enum(ty))
    }

    pub fn collect_dependencies(&mut self) -> Result<()> {
        match self {
            TyDef::Enum(_) => { /* Nothing to do */ },
            TyDef::Dummy => {}
        }

        Ok(())
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
            TyDef::Dummy => {}
        }
        Ok(())
    }

    pub fn dummy() -> Result<TyDef<'db>> {
        Ok(TyDef::Dummy)
    }
    
    pub fn can_be_skipped(&self) -> bool {
        false
    }
}

pub trait TypeRequestSource {
    fn get_module();
    fn add_dependency(t: &mut TyDef);
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

pub fn get_type_name<T: TypeRequestSource>(source: &T, ty: Type, usage: TypeUsage) -> Result<String> {
    Ok(match ty {
        Type::Primitive(prim) => get_type_name_primitive(prim, usage)?,
        Type::Array(array) => unimplemented!(),
        Type::Ref(tag, def_or_ref, generic_args) => unimplemented!(),
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
