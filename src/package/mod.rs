mod constraint;
mod parse;
mod raw;

use anyhow::{Context, Result};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

pub use constraint::{Constraint, ConstraintArgs};
use parse::Config;
pub use parse::{Build, Module, PanicMode};

use crate::{
    ds::{Dependencies, DependencyGraph},
    package::parse::Dependency,
};

#[derive(Default)]
pub struct Input {
    pub features: HashSet<String>,
    pub no_default_features: bool,
    pub args: ConstraintArgs,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Lib {
    Name(String),
    Path(PathBuf),
}

#[derive(Debug)]
pub struct Project {
    pub build: Build,
    pub libs: HashSet<Lib>,
    pub mods: Vec<Module>,
    pub args: ConstraintArgs,
}

impl Project {
    pub fn load(path: &Path, input: Input) -> Result<Self> {
        let mut cfgs = HashMap::new();
        let mut libs = HashSet::new();
        let mut graph = DependencyGraph::default();
        let path = if path.is_dir() {
            path.join("ctl.toml").canonicalize()?
        } else {
            path.canonicalize()?
        };
        let build = Self::load_and_enable_features(
            path,
            &input.args,
            input.features,
            input.no_default_features,
            &mut cfgs,
            &mut graph,
            &mut libs,
            true,
            None,
        )?;

        let mut mods = Vec::new();
        graph.visit_all(|path| {
            if let Some(item) = cfgs.remove(path) {
                mods.push(item.module);
            }
        });

        // No canonicalize on the build dir, it might not even exist yet
        Ok(Self { build: build.unwrap(), libs, mods, args: input.args })
    }

    #[allow(clippy::too_many_arguments)]
    fn load_and_enable_features(
        path: PathBuf,
        args: &ConstraintArgs,
        mut features: HashSet<String>,
        mut no_default: bool,
        loaded: &mut HashMap<PathBuf, Config>,
        graph: &mut DependencyGraph<PathBuf>,
        libs: &mut HashSet<Lib>,
        main: bool,
        parent: Option<&Path>,
    ) -> Result<Option<Build>> {
        let (config, build) = if main && path.file_name().is_some_and(|p| p != "ctl.toml") {
            let mut deps = vec![];
            if !args.no_std {
                deps.push(Dependency {
                    path: Config::default_std_location()?,
                    features: std::mem::take(&mut features),
                    no_default: std::mem::take(&mut no_default),
                    requires: HashSet::default(),
                });
            }

            let config = loaded.entry(path.clone()).or_insert(Config {
                module: Module {
                    name: Self::default_package_name(&path)?,
                    path: path.clone(),
                    lib: false,
                    features: HashMap::new(),
                },
                libs: vec![],
                deps,
            });

            let mut build =
                if args.release { Build::default_release() } else { Build::default_debug() };
            build.no_gc = args.no_gc.get();
            build.overflow_checks = !args.no_overflow_checks.get();
            build.panic_mode = args.panic_mode;

            (config, Some(build))
        } else {
            match loaded.get_mut(&path) {
                Some(config) => (config, None),
                None => {
                    let data = std::fs::read_to_string(&path)?;
                    let (config, build) = Config::parse(path.parent().unwrap(), &data, args)?;
                    let config = loaded.entry(path.clone()).or_insert(config);
                    config.module.path = config.module.path.canonicalize()?;
                    if no_default {
                        for feat in config.module.features.values_mut() {
                            feat.enabled = false;
                        }
                    }

                    if main {
                        args.no_gc.set(build.no_gc);
                        args.no_overflow_checks.set(!build.overflow_checks);
                    }

                    (config, Some(build))
                }
            }
        };

        let mut deps = match graph.insert(path.clone(), Dependencies::Resolving) {
            Some(Dependencies::Resolved(deps)) => deps,
            Some(_) => {
                anyhow::bail!(
                    "dependency cycle trying to load '{}' (dependency of '{}')",
                    path.display(),
                    parent.unwrap().display()
                );
            }
            None => Default::default(),
        };

        if !no_default {
            for feat in config.module.features.values_mut() {
                if feat.default {
                    feat.enabled = true;
                }
            }
        }

        for feature in &features {
            let Some(feat) = config.module.features.get_mut(feature) else {
                // TODO: display an error
                continue;
            };

            feat.enabled = true;
            for dep in feat.deps.clone() {
                config.module.features.get_mut(&dep).unwrap().enabled = true;
            }
        }

        for lib in config.libs.iter() {
            if lib.features.iter().all(|v| config.module.features[v].is_enabled()) {
                if let Some(path) = &lib.path {
                    libs.insert(Lib::Path(path.canonicalize()?));
                } else {
                    libs.insert(Lib::Name(lib.name.clone()));
                }
            }
        }

        let next: Vec<_> = config
            .deps
            .iter()
            .filter(|dep| dep.requires.iter().all(|v| config.module.features[v].is_enabled()))
            .cloned()
            .collect();
        for dep in next {
            let dep_path = dep.path.join("ctl.toml").canonicalize()?;
            deps.insert(dep_path.clone());

            Self::load_and_enable_features(
                dep_path,
                args,
                dep.features,
                dep.no_default,
                loaded,
                graph,
                libs,
                false,
                Some(&path),
            )?;
        }

        *graph.get_mut(&path).unwrap() = Dependencies::Resolved(deps);

        Ok(build)
    }

    pub fn default_package_name(path: &Path) -> Result<String> {
        let base = if path.is_file() { path.file_stem() } else { path.file_name() };
        Ok(base
            .with_context(|| format!("trying to derive module name for {}", path.display()))?
            .to_string_lossy()
            .into_owned())
    }
}
