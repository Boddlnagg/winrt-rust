use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::fs::{File, DirBuilder};
use std::io::Write;
use std::collections::HashMap;

use climeta::{Cache, ResolveToTypeDef};
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
                    TypeCategory::Interface => TyDef::dummy(),
                    TypeCategory::Struct => TyDef::dummy(),
                    TypeCategory::Delegate => TyDef::dummy(),
                    TypeCategory::Class => TyDef::dummy()
                }?;

                if typedef.can_be_skipped() {
                    all_definitions.insert(fullname, typedef);
                }
            }
        }

        //pinterfaceManager.AddBaseIReferenceInstances(this);

        println!("Number of modules: {}", modules.len());

        let mut modules_vec: Vec<_> = modules.into_iter().map(|(k, v)| Module { namespace: k, assembly_idx: v, depth: k.bytes().filter(|b| *b == b'.').count() as u32 }).collect();
        modules_vec.sort_by_key(|m| m.namespace);

        // let mut files_vec = Vec::new();
        // let mut prev_idx: Option<Option<u32>> = None;
        // for m in modules_vec {
        //     if prev_idx != Some(m.assembly_idx) {
        //         println!("Module file {} ({:?})", m.namespace, m.assembly_idx);
        //         files_vec.push(m);
        //     }
        //     prev_idx = Some(m.assembly_idx);
        // }

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

    pub fn write_module_tree_multifile_root(&self, directory: &Path) -> Result<()> {
        let mut pathbuf: PathBuf = directory.into();
        pathbuf.push("mod.rs");
        let mut file = File::create(pathbuf)?;
        writeln!(file, "// DO NOT MODIFY THIS MODULE NOR ANY OF ITS CHILDREN - THEY ARE AUTOMATICALLY GENERATED!")?;
        writeln!(file, "#![allow(non_camel_case_types, unused_imports)]")?;

        let mut idx = 0;
        self.write_module_tree_multifile(&mut idx, directory, &mut file, None)?;

        Ok(())
    }

    fn write_module_tree_multifile(&self, idx: &mut usize, directory: &Path, parent_file: &mut File, parent_module: Option<&Module<'db>>) -> Result<()> {
        let mut initial_depth = self.modules[*idx].depth;
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
                self.write_module_tree_multifile(idx, &current_directory, current_file, Some(parent))?;
                // idx now points to the next sibling or parent
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

            let module_name = module_name.to_lowercase(); // TODO: correct PascalCase -> snake_case conversion?

            let new_dir = match parent_module {
                Some(pm) if pm.assembly_idx == m.assembly_idx => None, // same assembly as parent
                _ => Some(&module_name)
            };

            if let Some(new_dir) = new_dir {
                DirBuilder::new().recursive(true).create(directory)?;
                let mut new_path = directory.join(new_dir.to_lowercase());
                new_path.set_extension("rs1");

                writeln!(current_file, "pub mod {}; // {}", module_name, m.namespace)?;

                owned_file = Some(File::create(&new_path)?);
                current_file = owned_file.as_mut().unwrap();
                new_path.set_extension("");
                current_directory = Cow::from(new_path);
                unclosed_module = Some((*idx, false));
            } else {
                writeln!(current_file, "pub mod {} {{ // {}", module_name, m.namespace)?;
                unclosed_module = Some((*idx, true));
            }

            writeln!(current_file, "  // TODO: Contents of Namespace {}", m.namespace)?;

            *idx += 1;
        }

        if let Some((unclosed, true)) = unclosed_module {
            writeln!(current_file, "}} // {}", self.modules[unclosed].namespace)?;
        }

        Ok(())
    }
}