use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use super::{constraint::ConstraintArgs, raw};

#[derive(Debug)]
pub struct LinkLib {
    pub name: String,
    pub path: Option<PathBuf>,
    pub features: HashSet<String>,
}

#[derive(Debug, Clone)]
pub struct Dependency {
    pub path: PathBuf,
    pub features: HashSet<String>,
    pub requires: HashSet<String>,
    pub no_default: bool,
}

#[derive(Debug, Clone)]
pub struct Build {
    pub overflow_checks: bool,
    pub dir: PathBuf,
    pub opt_level: usize,
    pub debug_info: bool,
    pub no_gc: bool,
    pub no_bit_int: bool,
    // Not exposed in config
    pub minify: bool,
}

impl Build {
    pub fn default_debug() -> Self {
        Build {
            overflow_checks: true,
            dir: PathBuf::from("./build"),
            opt_level: 0,
            debug_info: true,
            no_gc: false,
            no_bit_int: false,
            minify: false,
        }
    }

    pub fn default_release() -> Self {
        Build {
            overflow_checks: true,
            dir: PathBuf::from("./build"),
            opt_level: 3,
            debug_info: false,
            no_gc: false,
            no_bit_int: false,
            minify: false,
        }
    }
}

#[derive(Debug)]
pub struct Feature {
    pub enabled: bool,
    pub available: bool,
    pub default: bool,
    pub deps: HashSet<String>,
}

impl Feature {
    pub fn is_enabled(&self) -> bool {
        self.enabled && self.available
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub path: PathBuf,
    pub lib: bool,
    pub features: HashMap<String, Feature>,
}

#[derive(Debug)]
pub struct Config {
    pub module: Module,
    pub libs: Vec<LinkLib>,
    pub deps: Vec<Dependency>,
}

macro_rules! build_prop {
    ($cfg: expr, $v: ident, $default: expr, $property: ident) => {
        $v.$property.or($cfg.build.default.$property.clone()).unwrap_or($default.$property)
    };
}

impl Config {
    pub fn parse(
        dir: impl AsRef<Path>,
        data: &str,
        args: &ConstraintArgs,
    ) -> anyhow::Result<(Self, Build)> {
        let dir = dir.as_ref();
        let config = toml::from_str::<raw::Config>(data)?;
        let dir_relative = |path: PathBuf| {
            if path.is_absolute() { path.to_owned() } else { dir.join(path) }
        };

        let mut module = Module {
            name: config
                .package
                .name
                .unwrap_or_else(|| dir.file_name().unwrap().to_string_lossy().into_owned()),
            path: config.package.root.map(dir_relative).unwrap_or(dir.into()),
            lib: config.package.lib,
            features: HashMap::new(),
        };
        let mut libs = Vec::with_capacity(config.link_libs.len());
        let mut deps = Vec::with_capacity(config.dependencies.len());
        for (name, feat) in config.features {
            module.features.insert(
                name,
                Feature {
                    available: feat.constraint.is_none_or(|c| c.applies(args)),
                    enabled: feat.default,
                    default: feat.default,
                    deps: feat.requires,
                },
            );
        }

        Self::check_features(&module)?;

        for (name, lib) in config.link_libs {
            if lib.constraint.is_some_and(|c| !c.applies(args)) {
                continue;
            }

            libs.push(LinkLib {
                features: Self::validate_features(&module, lib.requires, || {
                    format!("link library '{name}'")
                })?,
                name,
                path: lib.path.map(dir_relative),
            });
        }

        let mut included_std = false;
        for (name, dep) in config.dependencies {
            match dep {
                raw::Dependency::Path(path) => deps.push(Dependency {
                    path: dir_relative(path),
                    features: HashSet::new(),
                    requires: HashSet::new(),
                    no_default: false,
                }),
                raw::Dependency::Full { constraint, path, features, no_default, requires } => {
                    if constraint.is_some_and(|c| !c.applies(args)) {
                        continue;
                    }

                    let path = if let Some(path) = path {
                        dir_relative(path)
                    } else if name == "std" {
                        included_std = true;
                        Self::default_std_location()?
                    } else {
                        anyhow::bail!("in declaration of dependency '{name}': a path is required");
                    };

                    let requires = Self::validate_features(&module, requires, || {
                        format!("declaration of dependency '{name}'")
                    })?;

                    match features {
                        raw::FeatureReq::List(features) => {
                            deps.push(Dependency { path, features, requires, no_default })
                        }
                        raw::FeatureReq::WithConstraints(features) => {
                            deps.push(Dependency {
                                path,
                                features: features
                                    .into_iter()
                                    .filter_map(|(feat, c)| c.applies(args).then_some(feat))
                                    .collect(),
                                requires,
                                no_default,
                            });
                        }
                    }
                }
            }
        }

        if !included_std && !config.package.no_std {
            deps.push(Dependency {
                path: Self::default_std_location()?,
                features: Default::default(),
                requires: Default::default(),
                no_default: false,
            });
        }

        let (base, fallback) = if args.release {
            (config.build.release, Build::default_release())
        } else {
            (config.build.debug, Build::default_debug())
        };
        let build = Build {
            overflow_checks: build_prop!(config, base, fallback, overflow_checks),
            dir: dir_relative(build_prop!(config, base, fallback, dir)),
            opt_level: build_prop!(config, base, fallback, opt_level),
            debug_info: build_prop!(config, base, fallback, debug_info),
            no_gc: build_prop!(config, base, fallback, no_gc),
            no_bit_int: build_prop!(config, base, fallback, no_bit_int),
            minify: false,
        };

        Ok((Self { module, libs, deps }, build))
    }

    pub fn check_features(module: &Module) -> anyhow::Result<()> {
        // TODO: check for recursive dependencies
        for (name, feat) in module.features.iter() {
            for dep in feat.deps.iter() {
                if !module.features.contains_key(dep) {
                    anyhow::bail!(
                        "in declaration of feature '{name}': requiring undeclared feature '{dep}'"
                    );
                }
            }
        }

        Ok(())
    }

    pub fn default_std_location() -> std::io::Result<PathBuf> {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("std").canonicalize()
    }

    fn validate_features(
        module: &Module,
        features: HashSet<String>,
        name: impl FnOnce() -> String,
    ) -> anyhow::Result<HashSet<String>> {
        for feat in &features {
            if !module.features.contains_key(feat) {
                anyhow::bail!("in {}: requiring undeclared feature '{feat}'", name());
            }
        }

        Ok(features)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_config() -> anyhow::Result<()> {
        let dir = Path::new("~/test/project");
        let _config = Config::parse(dir, SAMPLE, &ConstraintArgs::default())?;
        // println!("{config:#?}");

        Ok(())
    }

    const SAMPLE: &str = r#"
[package]
name = "test"
root = "."
no_std = true
lib = true
version = "0"

[features]
foo = { default = true, available = "os:linux+arch:x86-64" }
bar = { default = true }
baz = { default = true, requires = ["bar"] }

[link-libs]
c = { requires = ["bar"] }
dw = { requires = ["foo"], available = "os:linux+arch:x86-64" }
gc = { available = "!ctl:no-gc" }
raylib = { path = "..." }
curl = {}

[build]
overflow-checks = false
dir = "./build"
opt-level = 2
debug-info = true
no-gc = true

[build.debug]
opt-level = 1

[build.release]
dir = "./build-release"

[dependencies]
std = { features = ["alloc"], no-default = true }
whatever = "../../Path"
net = { features = { winsock = "os:windows", other = "" }, path = "../" }
    "#;
}
