use std::path::{Path, PathBuf};
use std::fs::{File, DirBuilder};
use std::io::Write;

use climeta::Cache;
use climeta::schema::{TypeDef, TypeCategory};

use crate::Result;

pub struct Generator<'db> {
    cache: &'db Cache<'db>,
    allow_add_deps: bool
}

impl<'db> Generator<'db> {
    pub fn new(cache: &'db Cache<'db>) -> Result<Generator<'db>> {

        for db in cache {
            for t in db.table::<TypeDef>() {
                if t.type_name()? == "<Module>" { continue; }

                println!("Type: {:?}", t);

                let typedef = match t.type_category()? {
                    TypeCategory::Enum => unimplemented!(),
                    TypeCategory::Interface => unimplemented!(),
                    TypeCategory::Struct => unimplemented!(),
                    TypeCategory::Delegate => unimplemented!(),
                    TypeCategory::Class => unimplemented!(),
                };

                // if (!typedef.CanBeSkipped)
                // {
                //     definitionsList.Add(t.FullName, typedef);
                // }
            }
        }

        //pinterfaceManager.AddBaseIReferenceInstances(this);

        Ok(Generator {
            cache,
            allow_add_deps: true
        })
    }

    pub fn write_module_tree_multifile_root(&self, directory: &Path) -> Result<()> {
        let mut pathbuf: PathBuf = directory.into();
        pathbuf.push("mod.rs");
        let mut file = File::create(pathbuf)?;
        writeln!(file, "// DO NOT MODIFY THIS MODULE NOR ANY OF ITS CHILDREN - THEY ARE AUTOMATICALLY GENERATED!")?;
        writeln!(file, "#![allow(non_camel_case_types, unused_imports)]")?;

        /*foreach (var child in rootModule.Children.Values) {
            WriteModuleTreeMultiFile(child, directory, file);
        }*/

        Ok(())
    }

    /*fn write_module_tree_multifile(mod: &Module, DirectoryInfo directory, StreamWriter file, string path = null, AssemblyDefinition asm = null)
    {
        if (mod.IsEmpty) return;

        const string IMPORTS = "use crate::prelude::*;";

        string name = mod.Name.ToLower();
        string newPath = path == null ? mod.Name : (path + "." + mod.Name);

        bool isNewAssembly = (asm != mod.Assembly);

        if (isNewAssembly && mod.Assembly.Name.Name != "Windows.Foundation")
        {
            var featureCond = new FeatureConditions(new string[] { mod.Assembly.Name.Name }).GetAttribute().TrimEnd();
            file.Write(featureCond);
            file.Write(" ");
        }

        if (isNewAssembly || path == null || mod.ContainsMoreThanOneAssembly)
        {
            // write module to separate file
            file.WriteLine($"pub mod { name }; // { newPath }");
            DirectoryInfo childDir;
            StreamWriter childFile;
            if (mod.ContainsMoreThanOneAssembly)
            {
                childDir = directory.CreateSubdirectory(name);
                childFile = new StreamWriter(Path.Combine(childDir.FullName, "mod.rs"));
            }
            else
            {
                childDir = directory;
                childFile = new StreamWriter(Path.Combine(childDir.FullName, name + ".rs"));
            }

            var text = mod.Text.ToString();
            if (!string.IsNullOrWhiteSpace(text))
            {
                childFile.Write(IMPORTS);
                childFile.WriteLine(text);
            }
            foreach (var child in mod.Children.Values)
            {
                WriteModuleTreeMultiFile(child, childDir, childFile, newPath, mod.Assembly);
            }

            childFile.Close();
        }
        else
        {
            // write module inline
            file.WriteLine($"pub mod { name } {{ // { newPath }");
            var text = mod.Text.ToString();
            if (!string.IsNullOrWhiteSpace(text))
            {
                file.Write(IMPORTS);
                file.WriteLine(text);
            }
            foreach (var child in mod.Children.Values)
            {
                WriteModuleTreeMultiFile(child, directory, file, newPath, mod.Assembly);
            }
            file.WriteLine($"}} // { newPath }");
        }
    }*/
}