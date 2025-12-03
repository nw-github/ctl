use crate::hash::{HashMap, HashSet};
use std::hash::Hash;

pub enum Dependencies<T> {
    Resolving,
    Resolved(Vec<T>),
    Recursive,
}

#[derive(derive_more::Deref, derive_more::DerefMut)]
pub struct DependencyGraph<T> {
    graph: HashMap<T, Dependencies<T>>,
}

impl<T: Hash + Eq> DependencyGraph<T> {
    pub fn visit_all(&self, mut func: impl FnMut(&T)) {
        let mut visited = HashSet::new();
        for id in self.graph.keys() {
            if !visited.contains(id) {
                self.dfs_raw(id, &mut visited, &mut func);
            }
        }
    }

    pub fn dfs(&self, id: T, visited: &mut HashSet<T>, mut func: impl FnMut(T))
    where
        T: Clone,
    {
        self.dfs_p(id, visited, &mut func);
    }

    fn dfs_p(&self, id: T, visited: &mut HashSet<T>, func: &mut impl FnMut(T))
    where
        T: Clone,
    {
        visited.insert(id.clone());
        if let Some(Dependencies::Resolved(deps)) = self.graph.get(&id) {
            for dep in deps.iter() {
                if !visited.contains(dep) {
                    self.dfs_p(dep.clone(), visited, func);
                }
            }
        }

        func(id);
    }

    fn dfs_raw<'a>(&'a self, id: &'a T, visited: &mut HashSet<&'a T>, func: &mut impl FnMut(&T)) {
        visited.insert(id);
        if let Some(Dependencies::Resolved(deps)) = self.graph.get(id) {
            for dep in deps.iter() {
                if !visited.contains(dep) {
                    self.dfs_raw(dep, visited, func);
                }
            }
        }

        func(id);
    }
}

impl<T> Default for DependencyGraph<T> {
    fn default() -> Self {
        Self {
            graph: Default::default(),
        }
    }
}
