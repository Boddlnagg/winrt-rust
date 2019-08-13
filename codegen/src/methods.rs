use std::borrow::Cow;
use std::io::Write;

use climeta::ResolveToTypeDef;
use climeta::schema::{self, MethodDef, TypeDef, TypeRef, Type, TypeTag, PrimitiveType, FieldInit, PrimitiveValue};

use regex::bytes::Regex;

use crate::Result;
use crate::generator::{prevent_keywords, prevent_keywords_buf};
use crate::types::{self, Dependencies, TypeUsage};

static MUTATING_METHODS: &'static [(&str, &str, &str)] = &[
    // MUST BE LEXICOGRAPHICALLY SORTED!
    ("Windows.Foundation.Collections", "IMap`2", "Clear"),
    ("Windows.Foundation.Collections", "IMap`2", "Insert"),
    ("Windows.Foundation.Collections", "IMap`2", "Remove"),
    ("Windows.Foundation.Collections", "IVector`1", "Append"),
    ("Windows.Foundation.Collections", "IVector`1", "Clear"),
    ("Windows.Foundation.Collections", "IVector`1", "InsertAt"),
    ("Windows.Foundation.Collections", "IVector`1", "RemoveAt"),
    ("Windows.Foundation.Collections", "IVector`1", "RemoveAtEnd"),
    ("Windows.Foundation.Collections", "IVector`1", "ReplaceAll"),
    ("Windows.Foundation.Collections", "IVector`1", "SetAt"),
];

fn camel_to_snake_case(input: &str, result: &mut String) {
    lazy_static! {
        static ref CASE_CONVERSION_EXCEPTIONS_REGEX: Regex = Regex::new(
            "(Base64|Float32|Float64|DBm|UInt|SInt|SFloat|Direct3D11|Direct3D|\
            VCard|OAuth|CData|ESim|MPeg|WMAudio|URLs|EFSpn|EFOns|EFOpl|EFPnn|\
            ETag|IndexedDB|JavaScript|HResult|UIElement|IMediaSource|\
            IAnimationObject|IBuffer)([A-Z0-9_]|$)").unwrap();
    }

    assert!(input.is_ascii());

    result.clear();

    let input_bytes = input.as_bytes();

    let mut no_underscore = true;
    let mut previous_upper = false;
    let mut in_numeric = false;
    let mut dist_prev_upper = 10; // something larger than 4

    let matches = CASE_CONVERSION_EXCEPTIONS_REGEX.captures_iter(input_bytes).map(|cap| cap.get(1).unwrap()).collect::<Vec<_>>();

    let mut ci = 0;

    for (ci, c) in input_bytes.iter().enumerate() {
        let is_lower = |offset: usize| { ci + offset < input_bytes.len() && (input_bytes[ci + offset] as char).is_lowercase() };
        let is_upper = |offset: usize| { ci + offset < input_bytes.len() && (input_bytes[ci + offset] as char).is_uppercase() };
        let is_digit = |offset: usize| { ci + offset < input_bytes.len() && (input_bytes[ci + offset] as char).is_digit(10) };
        let within_match = || { matches.iter().any(|m| ci > m.start() && ci < m.end()) };

        let c = input_bytes[ci];
        if is_upper(0) {
            dist_prev_upper = 0;
            if !no_underscore && (!previous_upper || is_lower(1)) && !within_match() {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase() as char);
            previous_upper = true;
            in_numeric = false;
        } else if is_digit(0) && (is_digit(1) || (is_upper(1) && !is_lower(2))) {
            dist_prev_upper += 1;
            let seen_4letter_exception = if dist_prev_upper == 4 {
                let last4 = &input_bytes[ci - 4 .. ci];
                last4 == b"Char" || last4 == b"Argb" || last4 == b"Wtls" || last4 == b"Ieee"
            } else {
                false
            };
            let seen_2letter_exception = if ci >= 2 {
                let last2 = &input_bytes[ci - 2 .. ci];
                last2 == b"Is" || last2 == b"3D"
            } else {
                false
            };

            if !no_underscore && !in_numeric && ((dist_prev_upper > 3 && !seen_4letter_exception) || seen_2letter_exception) && !within_match() {
                result.push('_');
            }
            result.push(c as char);
            previous_upper = true;
            in_numeric = true;
        } else {
            dist_prev_upper += 1;
            result.push(c as char);
            previous_upper = false;
            in_numeric = in_numeric && (is_digit(0) || is_digit(1)); // stay in numeric mode if there's just one letter in between
        }

        no_underscore = c == b'_';
    }
}

pub struct Method<'db> {
    md: MethodDef<'db>,
    dependencies: Dependencies,
    raw_name: String,
    raw_params: String,
    wrapped_name: String, // TODO: call get_wrapper_name here and "fixup" conflicting names in a single pass after that
    is_mutating: bool,
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

        let (td_namespace, td_name) = td.namespace_name_pair();
        let is_mutating = MUTATING_METHODS.binary_search(&(td_namespace, td_name, &raw_name)).is_ok();

        Ok(Method {
            md: md,
            dependencies: dependencies,
            raw_name: raw_name,
            raw_params: raw_params,
            wrapped_name: String::new(),
            is_mutating: is_mutating,
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
                schema::ParamKind::TypeByRef(ty) => {
                    let mut has_const_modifier = false;
                    for cmod in mpar_t.custom_mod() {
                        if cmod.type_().namespace_name_pair() == ("System.Runtime.CompilerServices", "IsConst") {
                            has_const_modifier = true;
                            continue;
                        }

                        if cmod.tag() == schema::CustomModTag::Required {
                            unimplemented!()
                        }
                    }
                    result.push_str(&types::get_type_name_by_ref(ty, TypeUsage::Raw, dependencies, Some(td), has_const_modifier)?)
                },
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

    pub fn get_wrapper_name(&self, methods: &[Method], result: &mut String) {
        let name_without_put: Cow<str> = if self.raw_name.starts_with("put_") {
            Cow::from(self.raw_name.replacen("put_", "set_", 1))
        } else {
            Cow::from(&self.raw_name)
        };

        camel_to_snake_case(&name_without_put, result);

        prevent_keywords_buf(result);

        // name already contains '_' -> might result in a name clash after renaming, e.g. caused by
        // original names `get_Text` (property getter) and `GetText` (method)
        if self.raw_name.contains('_') {
            let mut result2 = String::new();
            let conflict = methods
                            .iter()
                            .filter(|elem| !elem.raw_name.contains('_'))
                            .any(|elem| {
                                elem.get_wrapper_name(methods, &mut result2);
                                &result2 == result
                            });
            if conflict {
                result.push('_');
            }
        }
    }

    pub fn emit_wrapper<W: Write>(&self, indentation: &str, wrapper_name: &str, file: &mut W) -> Result<()> {
        write!(file, "{}", indentation)?;

        let self_arg = if self.is_mutating { "&mut self" } else { "&self" };
        // TODO: remaining parameters and return type
        let condition = self.dependencies.make_feature_condition(&self.md);
        if !condition.is_empty() {
            condition.emit_attribute(file)?;
            write!(file, " ")?;
        }
        writeln!(file, "#[inline] pub fn {wrapper_name}({self_arg} /*TODO: ARGS*/) -> Result</*TODO: OUT*/> {{ unsafe {{",
            wrapper_name = wrapper_name,
            self_arg = self_arg
        )?;
        // TODO: wrapper body
        writeln!(file, "{}}}}}", indentation)?;
        Ok(())
    }
}

impl<'db> std::fmt::Debug for Method<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Method '{}'", self.raw_name)
    }
}
