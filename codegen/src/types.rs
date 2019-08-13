use std::io::Write;
use std::collections::HashSet;

use climeta::{ResolveToTypeDef, AssemblyAccess};
use climeta::schema::{self, TypeDef, TypeRef, Type, TypeTag, PrimitiveType, FieldInit, PrimitiveValue};

use crate::Result;
use crate::methods;
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

    fn extend(&mut self, other: &Dependencies) {
        for name in other.assemblies.iter() {
            if !self.assemblies.contains(name) {
                self.assemblies.insert(name.into());
            }
        }
    }

    pub fn make_feature_condition<T: climeta::AssemblyAccess>(&self, defining_assembly: &T) -> FeatureCondition {
        let mut result = self.assemblies.iter().filter_map(|asm| {
            if asm != "mscorlib" && asm != "Windows.Foundation" {
                if let Some(def_asm) = defining_assembly.assembly_name() {
                    if def_asm == asm {
                        return None;
                    }
                }
                // TODO: skip defining assembly
                let mut feature_name = asm.replace('.', "-");
                feature_name.make_ascii_lowercase();
                Some(format!("feature=\"{}\"", feature_name))
            } else {
                None
            }
        }).collect::<Vec<String>>();
        result.sort();
        FeatureCondition { conditions: result }
    }
}

pub struct FeatureCondition {
    conditions: Vec<String>
}

impl FeatureCondition {
    pub fn is_empty(&self) -> bool {
        self.conditions.is_empty()
    }

    pub fn emit_attribute<W: Write>(&self, file: &mut W) -> Result<()> {
        match self.conditions.len() {
            0 => (),
            1 => write!(file, "#[cfg({})]", self.conditions[0])?,
            _ => {
                write!(file, "#[cfg(all(")?;
                let mut first = true;
                for cond in &self.conditions {
                    let comma = if first {
                        first = false;
                        ""
                    } else {
                        ","
                    };
                    write!(file, "{}{}", comma, cond)?;
                }
                write!(file, "))]")?;
            }
        }
        Ok(())
    }

    pub fn emit_inverted_attribute<W: Write>(&self, file: &mut W) -> Result<()> {
        match self.conditions.len() {
            0 => (),
            1 => write!(file, "#[cfg(not({}))]", self.conditions[0])?,
            _ => {
                write!(file, "#[cfg(not(all(")?;
                let mut first = true;
                for cond in &self.conditions {
                    let comma = if first {
                        first = false;
                        ""
                    } else {
                        ","
                    };
                    write!(file, "{}{}", comma, cond)?;
                }
                write!(file, ")))]")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum TyDef<'db> {
    Enum(TypeDef<'db>),
    Struct(TypeDef<'db>, String),
    Interface(TypeDef<'db>, Option<InterfaceKind>, String, Vec<methods::Method<'db>>),
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
        Ok(TyDef::Interface(ty, None, String::new(), Vec::new()))
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
                        get_type_name(f.signature().expect("can't decode struct field signature").type_(), TypeUsage::Raw, &mut deps, Some(&td)).expect("can't get struct field type name"))
                }), ", ");
                //result.push_str(&format!(" [[deps: {:?}]]", deps));
                std::mem::replace(prepared, result);
            },
            TyDef::Interface(td, ref mut kind, ref mut guid, ref mut methods) => {
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

                // read GUID
                let guid_attr = td.get_attribute("Windows.Foundation.Metadata", "GuidAttribute")?.expect("Interface without GuidAttribute");
                let sig = guid_attr.value(cache)?;
                let mut first = true;
                for arg in sig.fixed_args() {
                    use std::fmt::Write;
                    match arg {
                        schema::FixedArg::Elem(schema::Elem::Primitive(prim)) => {
                            if first {
                                write!(guid, "{}", prim)?;
                                first = false;
                            } else {
                                write!(guid, ", {}", prim)?;
                            }
                        },
                        _ => unreachable!()
                    }
                };

                // read methods
                let deps_ref = &mut deps;
                std::mem::replace(methods, td.method_list()?.filter_map(move |md| {
                    if md.name().expect("method without name") == ".ctor" {
                        None
                    } else {
                        let m = methods::Method::new(md, &td, cache);
                        let m = m.and_then(|meth| {
                            deps_ref.extend(meth.dependencies());
                            Ok(meth)
                        });
                        Some(m)
                    }
                 }).collect::<Result<Vec<_>>>()?);

            },
            TyDef::Dummy => {}
        }

        Ok(deps)
    }

    pub fn emit<W: Write>(&self, file: &mut W) -> Result<()> {
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
            TyDef::Interface(td, kind, guid, methods) => {
                let definition_name = td.definition_name()?;
                let generic_params = td.generic_params()?;
                let (generic, generic_with_bounds) = if !generic_params.is_empty() {
                    let mut generic = "<".to_string();
                    let mut generic_with_bounds = "<".to_string();
                    let mut first = true;
                    for gp in generic_params {
                        if !first {
                            generic.push_str(", ");
                            generic_with_bounds.push_str(", ");
                        } else {
                            first = false;
                        }
                        generic.push_str(gp.name()?);
                        generic_with_bounds.push_str(gp.name()?);
                        generic_with_bounds.push_str(": RtType");
                    }
                    generic.push('>');
                    generic_with_bounds.push('>');
                    (generic, generic_with_bounds)
                } else {
                    (String::new(), String::new())
                };

                writeln!(file, "\nDEFINE_IID!(IID_{name}, {guid});", name = definition_name, guid = guid)?;
                writeln!(file, "RT_INTERFACE! {{ {prepend_static}interface {name}{generic}({name}Vtbl): IInspectable [IID_{name}] {{",
                    prepend_static = match kind.as_ref().unwrap() {
                        InterfaceKind::Factory | InterfaceKind::Statics => "static ",
                        InterfaceKind::Instance => ""
                    },
                    name = definition_name,
                    generic = generic
                )?;
                for (i, meth) in methods.iter().enumerate() {
                    meth.emit_raw_declaration("    ", i, file)?;
                    let comma = if i == methods.len() - 1 { "" } else { "," };
                    writeln!(file, "{}", comma)?;
                }
                writeln!(file, "}}}}")?;

                if !methods.is_empty() {
                    writeln!(file, "impl{generic_with_bounds} {name}{generic} {{",
                        generic_with_bounds = generic_with_bounds,
                        name = definition_name,
                        generic = generic
                    )?;
                    let mut wrapper_name = String::new();
                    for (i, meth) in methods.iter().enumerate() {
                        meth.get_wrapper_name(&methods[..], &mut wrapper_name);
                        meth.emit_wrapper("    ", &wrapper_name, file)?;
                    }
                    writeln!(file, "}}")?
                }
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

pub fn get_type_name_by_ref(ty: &Type, usage: TypeUsage, deps: &mut Dependencies, generic_context: Option<&TypeDef>, has_const_modifier: bool) -> Result<String> {
    Ok(if has_const_modifier {
        match usage {
            TypeUsage::Raw => format!("*const {}", get_type_name(ty, usage, deps, generic_context)?),
            TypeUsage::In => format!("&{}", get_type_name(ty, usage, deps, generic_context)?),
            _ => unimplemented!()
        }
    } else {
        match usage {
            TypeUsage::Raw => format!("*mut {}", get_type_name(ty, usage, deps, generic_context)?),
            TypeUsage::In => format!("&mut {}", get_type_name(ty, usage, deps, generic_context)?),
            _ => unimplemented!()
        }
    })
}

pub fn get_type_name(ty: &Type, usage: TypeUsage, deps: &mut Dependencies, generic_context: Option<&TypeDef>) -> Result<String> {
    Ok(match ty {
        Type::Primitive(prim) => get_type_name_primitive(*prim, usage)?,
        Type::Array(array) => {
            match usage {
                TypeUsage::Out => format!("ComArray<{}>", get_type_name(array.elem_type(), TypeUsage::GenericArg, deps, generic_context)?),
                _ => format!("*mut {}", get_type_name(array.elem_type(), usage, deps, generic_context)?)
            }
        }
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
                        path.push_str(&get_type_name(arg, TypeUsage::GenericArg, deps, generic_context)?);
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
        Type::GenericVar(scope, idx) => {
            let var_name = match scope {
                schema::GenericVarScope::Method => unimplemented!(),
                schema::GenericVarScope::Type => generic_context
                                                    .expect("no generic context provided")
                                                    .generic_params()?
                                                    .nth(*idx as usize)
                                                    .expect("generic param out of bounds")
                                                    .name()?.to_string()
            };

            match usage {
                TypeUsage::Raw => format!("{}::Abi", var_name),
                TypeUsage::In => format!("&{}::In", var_name),
                TypeUsage::Out => format!("{}::Out", var_name),
                TypeUsage::OutNonNull => var_name.to_string(),
                TypeUsage::GenericArg => var_name.to_string(),
                _ => unimplemented!()
            }
        }
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
