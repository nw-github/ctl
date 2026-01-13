use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use serde::Deserialize;

use crate::package::Constraint;

#[derive(Deserialize, Default, Debug)]
#[serde(deny_unknown_fields)]
pub struct Package {
    pub name: Option<String>,
    pub root: Option<PathBuf>,
    #[serde(default)]
    pub no_std: bool,
    pub lib: bool,
    pub version: Option<String>, /* semver */
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
#[serde(deny_unknown_fields)]
pub enum FeatureReq {
    List(HashSet<String>),
    WithConstraints(HashMap<String, Constraint>),
}

impl Default for FeatureReq {
    fn default() -> Self {
        Self::List(Default::default())
    }
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
#[serde(deny_unknown_fields)]
pub enum Dependency {
    Path(PathBuf),
    #[serde(rename_all = "kebab-case")]
    Full {
        #[serde(rename = "available")]
        constraint: Option<Constraint>,
        path: Option<PathBuf>,
        #[serde(default)]
        features: FeatureReq,
        #[serde(default)]
        no_default: bool,
        #[serde(default)]
        requires: HashSet<String>,
    },
}

#[derive(Deserialize, Default, Debug)]
#[serde(deny_unknown_fields)]
pub struct FeatureDef {
    #[serde(default)]
    pub default: bool,
    #[serde(rename = "available")]
    pub constraint: Option<Constraint>,
    #[serde(default)]
    pub requires: HashSet<String>,
}

#[derive(Deserialize, Debug)]
pub struct LinkLib {
    #[serde(rename = "available")]
    pub constraint: Option<Constraint>,
    #[serde(default)]
    pub requires: HashSet<String>,
    pub path: Option<PathBuf>,
}

#[derive(Deserialize, Default, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(deny_unknown_fields)]
pub struct Build {
    pub overflow_checks: Option<bool>,
    pub dir: Option<PathBuf>,
    pub opt_level: Option<usize>,
    pub debug_info: Option<bool>,
    pub no_gc: Option<bool>,
    pub no_bit_int: Option<bool>,
}

#[derive(Deserialize, Default, Debug)]
#[serde(deny_unknown_fields)]
pub struct FullBuild {
    #[serde(flatten)]
    pub default: Build,
    pub debug: Build,
    pub release: Build,
}

#[derive(Deserialize, Default, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(deny_unknown_fields)]
pub struct Config {
    #[serde(default)]
    pub package: Package,
    #[serde(default)]
    pub features: HashMap<String, FeatureDef>,
    #[serde(default)]
    pub link_libs: HashMap<String, LinkLib>,
    #[serde(default)]
    pub build: FullBuild,
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}
