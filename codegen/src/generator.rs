use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::fs::{File, DirBuilder};
use std::io::{Write, BufWriter};
use std::collections::HashMap;

use climeta::{Cache, ResolveToTypeDef, AssemblyAccess};
use climeta::schema::{TypeDef, TypeCategory};

use crate::Result;
use crate::types::*;

type FullName<'db> = (&'db str, &'db str);

#[derive(Clone, Copy, Debug)]
pub struct Module<'db> {
    namespace: &'db str,
    assembly_idx: Option<u32>,
    depth: u32,
}

pub struct Generator<'db> {
    cache: &'db Cache<'db>,
    all_definitions: HashMap<FullName<'db>, TyDef<'db>>,
    modules: Vec<Module<'db>>,
    allow_add_deps: bool
}

type ModuleMap<'db> = HashMap<&'db str, Option<u32>>;

fn next_from_namespace<'db>(definitions: &mut Vec<(FullName<'db>, TyDef<'db>)>, namespace: &str) -> Option<(FullName<'db>, TyDef<'db>)> {
    if let Some(last) = definitions.last() {
        if (last.0).0 == namespace {
            definitions.pop()
        } else {
            None
        }
    } else {
        None
    }
}

fn get_sort_key_ignoring_interface_prefix<'a>(input: (&'a str, &'a str)) -> (&'a str, &'a str, u8) {
    let (input_ns, input_type) = input;
    let mut chars = input_type.chars();
    if (chars.next() == Some('I') && chars.next().map(|c| c.is_uppercase()) == Some(true)) {
        let mut chars2 = input_type.chars();
        chars2.next();
        (input_ns, chars2.as_str(), 0)
    } else {
        (input_ns, input_type, 1)
    }
}

impl<'db> Generator<'db> {
    pub fn new(cache: &'db Cache<'db>) -> Result<Generator<'db>> {
        let mut all_definitions = HashMap::new();
        let mut modules = ModuleMap::new();
        modules.insert("Windows", None);
        let mut prev_ns = None;
        for (i, db) in cache.iter().enumerate() {
            for t in db.table::<TypeDef>() {
                if t.type_name()? == "<Module>" { continue; }

                let fullname = t.namespace_name_pair();
                let ns = fullname.0;
                if let Some(prev) = prev_ns {
                    if prev != ns {
                        Self::insert_module(&mut modules, ns, i as u32, true);
                        prev_ns = Some(ns);
                    }
                } else {
                    prev_ns = Some(ns);
                }

                let typedef = match t.type_category()? {
                    TypeCategory::Enum => TyDef::prepare_enum(t),
                    TypeCategory::Struct => TyDef::prepare_struct(t),
                    TypeCategory::Interface => TyDef::prepare_interface(t),
                    TypeCategory::Delegate => TyDef::prepare_delegate(t),
                    TypeCategory::Class => TyDef::prepare_class(t)
                }?;

                if !typedef.can_be_skipped()? {
                    all_definitions.insert(fullname, typedef);
                }
            }
        }

        //pinterfaceManager.AddBaseIReferenceInstances(this);

        println!("Number of modules: {}", modules.len());

        let mut modules_vec: Vec<_> = modules.into_iter().map(|(k, v)| Module { namespace: k, assembly_idx: v, depth: k.bytes().filter(|b| *b == b'.').count() as u32 }).collect();
        modules_vec.sort_by_key(|m| m.namespace);

        Ok(Generator {
            cache,
            all_definitions,
            modules: modules_vec,
            allow_add_deps: true
        })
    }

    fn insert_module(modules: &mut ModuleMap<'db>, namespace: &'db str, assembly_idx: u32, check_consistency: bool) {
        use std::collections::hash_map::Entry::*;
        let remaining;
        match modules.entry(namespace) {
            Occupied(e) => {
                let existing: Option<u32> = *e.into_mut();
                if check_consistency {
                    assert_eq!(existing, Some(assembly_idx));
                }
                remaining = None;
            },
            Vacant(e) => {
                //println!("New Module {} ({})", namespace, assembly_idx);
                remaining = namespace.rfind('.');
                e.insert(Some(assembly_idx));
            },
        };
        if let Some(i) = remaining {
            Self::insert_module(modules, &namespace[..i], assembly_idx, false);
        }
    }

    pub fn collect_dependencies(&mut self, cache: &'db climeta::Cache<'db>) -> Result<()> {
        for typedef in self.all_definitions.values_mut() {
            typedef.collect_dependencies(cache)?;
        }

        self.allow_add_deps = false; // this prevents logic bugs where new dependencies are added after this phase
        Ok(())
    }

    pub fn write_module_tree_multifile_root(&mut self, directory: &Path) -> Result<()> {
        let all_definitions = std::mem::replace(&mut self.all_definitions, HashMap::new());
        let mut definitions_vec: Vec<_> = all_definitions.into_iter().map(|(k, v)| (k, v)).collect();
        definitions_vec.sort_by(|a, b| get_sort_key_ignoring_interface_prefix(b.0).cmp(&get_sort_key_ignoring_interface_prefix(a.0)));

        let mut pathbuf: PathBuf = directory.into();
        pathbuf.push("mod.rs");
        let mut file = BufWriter::new(File::create(pathbuf)?);
        writeln!(file, "// DO NOT MODIFY THIS MODULE NOR ANY OF ITS CHILDREN - THEY ARE AUTOMATICALLY GENERATED!")?;
        writeln!(file, "#![allow(non_camel_case_types, unused_imports)]")?;

        let mut idx = 0;
        self.write_module_tree_multifile(&mut idx, &mut definitions_vec, directory, &mut file, None)?;
        assert!(definitions_vec.is_empty()); // all definitions have been consumed, i.e. emitted
        Ok(())
    }

    fn write_module_tree_multifile(&self, idx: &mut usize, definitions: &mut Vec<(FullName<'db>, TyDef<'db>)>, directory: &Path, parent_file: &mut BufWriter<File>, parent_module: Option<&Module<'db>>) -> Result<()> {
        let initial_depth = self.modules[*idx].depth;
        let mut current_directory = Cow::from(directory);
        let mut owned_file = None;
        let mut current_file = parent_file;
        let mut unclosed_module: Option<(usize, bool)> = None;

        loop {
            if *idx >= self.modules.len() {
                break;
            }
            let m = self.modules[*idx];

            if m.depth > initial_depth {
                // first child
                assert!(m.depth == initial_depth + 1);
                let parent = &self.modules[*idx - 1];
                self.write_module_tree_multifile(idx, definitions, &current_directory, current_file, Some(parent))?;
                // `idx` now points to the next sibling or parent; `definitions` contains the remaining definitions
                continue;
            }

            if m.depth < initial_depth || unclosed_module.is_some() {
                break;
            }

            let module_name = if let Some(i) = m.namespace.rfind('.') {
                &m.namespace[i+1..]
            } else {
                &m.namespace
            };

            let module_name = module_name.to_ascii_lowercase(); // TODO: correct PascalCase -> snake_case conversion?

            let new_dir = match parent_module {
                Some(pm) if pm.assembly_idx == m.assembly_idx => None, // same assembly as parent
                _ => Some(&module_name)
            };

            if let Some(new_dir) = new_dir {
                DirBuilder::new().recursive(true).create(directory)?;
                let mut new_path = directory.join(new_dir.to_ascii_lowercase());
                new_path.set_extension("rs");

                writeln!(current_file, "pub mod {}; // {}", module_name, m.namespace)?;

                owned_file = Some(BufWriter::new(File::create(&new_path)?));
                current_file = owned_file.as_mut().unwrap();
                new_path.set_extension("");
                current_directory = Cow::from(new_path);
                unclosed_module = Some((*idx, false));
            } else {
                writeln!(current_file, "pub mod {} {{ // {}", module_name, m.namespace)?;
                unclosed_module = Some((*idx, true));
            }

            // write module contents
            writeln!(current_file, "use crate::prelude::*;")?;

            while let Some((fullname, td)) = next_from_namespace(definitions, m.namespace) {
                td.emit(current_file)?;
            }

            *idx += 1;
        }

        if let Some((unclosed, true)) = unclosed_module {
            writeln!(current_file, "}} // {}", self.modules[unclosed].namespace)?;
        }

        Ok(())
    }
}

pub fn is_keyword(name: &str) -> bool {
    // TODO: add more keywords
    match name {
        "type"  |
        "Self"  |
        "box"   |
        "move"  |
        "async" |
        "await" |
        "const" => true,
        _ => false
    }
}

pub fn prevent_keywords(name: &str) -> Cow<str> {
    if is_keyword(name) {
        let mut s = String::from(name);
        s.push('_');
        s.into()
    } else {
        name.into()
    }
}

pub fn prevent_keywords_buf(name: &mut String) {
    if is_keyword(&name) {
        name.push('_');
    }
}
