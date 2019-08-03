use std::io::Write;

use climeta::schema::{self, MethodDef, TypeDef, TypeRef, Type, TypeTag, PrimitiveType, FieldInit, PrimitiveValue};

use crate::Result;
use crate::generator::prevent_keywords;
use crate::types::{self, Dependencies, TypeUsage};

pub struct Method<'db> {
    md: MethodDef<'db>,
    dependencies: Dependencies,
    raw_name: String,
    raw_params: String,
    wrapped_name: String,
}

impl<'db> Method<'db> {
    pub fn new<'c: 'db>(md: MethodDef<'db>, td: &TypeDef<'db>, cache: &'c climeta::Cache<'c>) -> Result<Method<'db>> {
        let raw_name = if let Some(overload) = md.get_attribute("Windows.Foundation.Metadata", "OverloadAttribute")? {
            match overload.value(cache)?.fixed_args() {
                &[schema::FixedArg::Elem(schema::Elem::String(Some(name)))] => name.to_string(),
                _ => return Err("invalid signature for OverloadAttribute".into())
            }
        } else {
            md.name()?.to_string()
        };

        let mut dependencies = Dependencies::default();

        let raw_params = Self::prepare_raw_params(&md, td, cache, &mut dependencies)?;

        Ok(Method {
            md: md,
            dependencies: dependencies,
            raw_name: raw_name,
            raw_params: raw_params,
            wrapped_name: String::new(),
        })
    }

    pub fn dependencies(&self) -> &Dependencies {
        &self.dependencies
    }

    fn prepare_raw_params<'c: 'db>(md: &MethodDef<'db>, td: &TypeDef<'db>, cache: &'c climeta::Cache<'c>, dependencies: &mut Dependencies) -> Result<String> {
        let mut result = String::new();

        let sig = md.signature()?;
        for (mpar, mpar_t) in md
            .param_list()?
            .skip_while(|p| {
                if let Ok(0) = p.sequence() {
                    true
                } else {
                    false
                }
            })
            .zip(sig.params())
        {
            let length_is = if let Some(length_is) = md.get_attribute("Windows.Foundation.Metadata", "LengthIsAttribute")? {
                match length_is.value(cache)?.fixed_args() {
                    &[schema::FixedArg::Elem(schema::Elem::Primitive(length))] => Some(length),
                    _ => return Err("invalid signature for LengthIsAttribute".into())
                }
            } else {
                None
            };

            result.push_str(", ");
            let param_name = {
                let tmp = mpar.name()?;
                let first = tmp.chars().nth(0).expect("at least one character").to_ascii_lowercase();
                Some(first).into_iter().chain(tmp.chars().skip(1)).collect::<String>()
            };
            
            if let schema::ParamKind::Type(schema::Type::Array(_)) = mpar_t.kind() {
                // need additional input size parameter (even if parameter is marked as [Out])
                result.push_str(&param_name);
                result.push_str("Size: u32, ");
            } else if let schema::ParamKind::TypeByRef(schema::Type::Array(_)) = mpar_t.kind() {
                assert!(length_is.is_none());
                // need additional output size parameter
                result.push_str(&param_name);
                result.push_str("Size: *mut u32, ");
            }
            result.push_str(&prevent_keywords(&param_name));
            result.push_str(": ");
            match mpar_t.kind() {
                schema::ParamKind::Type(ty) => result.push_str(&types::get_type_name(ty, TypeUsage::Raw, dependencies, Some(td))?),
                schema::ParamKind::TypeByRef(ty) => result.push_str(&types::get_type_name_by_ref(ty, TypeUsage::Raw, dependencies, Some(td), false)?),
                _ => unimplemented!()
            }
        }

        match sig.return_type().kind() {
            schema::RetTypeKind::Void => (), // add nothing
            schema::RetTypeKind::Type(ty) => {
                if let schema::Type::Array(_) = ty {
                    result.push_str(", outSize: *mut u32");
                }
                result.push_str(", out: *mut ");
                result.push_str(&types::get_type_name(ty, TypeUsage::Raw, dependencies, Some(td))?);
            },
            _ => unimplemented!()
        }
        Ok(result)
    }

    pub fn emit_raw_declaration<W: Write>(&self, indentation: &str, index: usize, file: &mut W) -> Result<()> {
        let condition = self.dependencies.make_feature_condition(&self.md);
        write!(file, "{}", indentation)?;
        if condition.is_empty() {
            write!(file, "fn {name}(&self{raw_params}) -> HRESULT", name = self.raw_name, raw_params = self.raw_params)?;
        } else {
            condition.emit_inverted_attribute(file)?;
            writeln!(file, " fn __Dummy{}(&self) -> ()", index)?;
            write!(file, "{}", indentation)?;
            condition.emit_attribute(file)?;
            write!(file, " fn {name}(&self{raw_params}) -> HRESULT", name = self.raw_name, raw_params = self.raw_params)?;
        }
        Ok(())
    }
}

impl<'db> std::fmt::Debug for Method<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Method '{}'", self.raw_name)
    }
}
