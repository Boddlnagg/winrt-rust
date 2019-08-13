use std::error::Error;
use std::path::Path;
use std::fs::{self, DirBuilder};
use std::io::Write;

use climeta::{Cache, Database};
#[macro_use] extern crate lazy_static;

pub(crate) mod generator;
pub(crate) mod types;
pub(crate) mod methods;

use generator::Generator;

type Result<T> = std::result::Result<T, Box<Error>>;

fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() < 2 {
        return Err("Please specify result directory path as first argument".into());
    }

    let cache = Cache::new();

    for entry in fs::read_dir("C:\\Windows\\System32\\WinMetadata")? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            cache.insert(Database::from_file(path)?);
        }
    }

    let mut generator = Generator::new(&cache)?;
    generator.collect_dependencies(&cache)?;
    //PrintStatistics(generator.AllTypes);
    //PrintDependencyStatistics(generator.AllTypes);
    //int pinterfaceCount = generator.EmitParametricInstances();
    //println!("Found and generated IIDs for {} distinct generic instances.", pinterfaceCount);

    let target_dir = Path::new(&args[1]);
    DirBuilder::new().recursive(true).create(target_dir)?;

    print!("Writing results to {} ...", target_dir.canonicalize()?.to_string_lossy());
    std::io::stdout().flush()?;
    generator.write_module_tree_multifile_root(&target_dir)?;
    println!(" done.");

    Ok(())
}



/*

        static void PrintStatistics(IEnumerable<TypeDef> types)
        {
            var groups = types.GroupBy(t => t.Kind);
            var enums = groups.Single(g => g.Key == TypeKind.Enum);
            var interfaces = groups.Single(g => g.Key == TypeKind.Interface);
            var structs = groups.Single(g => g.Key == TypeKind.Struct);
            var classes = groups.Single(g => g.Key == TypeKind.Class);
            var delegates = groups.Single(g => g.Key == TypeKind.Delegate);
            var methods = interfaces.Sum(i => i.Methods.Count()) + (delegates.Sum(d => d.Methods.Count()));
            Console.WriteLine("Generated {0} type definitions ({1} enums, {2} interfaces, {3} structs, {4} classes, {5} delegates) and {6} method definitions", types.Count(), enums.Count(), interfaces.Count(), structs.Count(), classes.Count(), delegates.Count(), methods);
        }

        static void PrintDependencyStatistics(IEnumerable<TypeDef> types)
        {
            Console.WriteLine("Listing methods with dependencies from different assemblies (excluding dependencies on Windows.Foundation):");
            var typesWithForeignAssemblyDeps = types.Where(t => t.ForeignAssemblyDependencies.Any());

            var methods = types.Where(t => t.Methods != null && t.Methods.Any()).SelectMany(t => t.Methods);
            var methodsWithForeignAssemblyDeps = methods.Where(m => m.ForeignAssemblyDependencies.Any());

            var methodsByAssembly = methods.GroupBy(m => m.DeclaringType.Module.Assembly);

            foreach (var group in methodsWithForeignAssemblyDeps.GroupBy(m => m.DeclaringType.Module.Assembly))
            {
                var totalMethodsInAssembly = methodsByAssembly.Single(g => g.Key == group.Key);
                Console.WriteLine("\tMethods in {0} ({1} of {2}):", group.Key.Name.Name, group.Count(), totalMethodsInAssembly.Count());
                foreach (var a in group.SelectMany(m => m.ForeignAssemblyDependencies).GroupBy(t => t.Module.Assembly))
                {
                    var asm = a.Key;
                    Console.WriteLine("\t- {0:D2} methods depending on {1}", group.Count(m => m.ForeignAssemblyDependencies.Any(dep => dep.Module.Assembly == asm)), asm.Name.Name);
                }

            }

            var structsWithForeignAssemblyDeps = typesWithForeignAssemblyDeps.Where(t => t.Kind == TypeKind.Struct);
            Assert(!structsWithForeignAssemblyDeps.Any());
        }
    }
}
*/
