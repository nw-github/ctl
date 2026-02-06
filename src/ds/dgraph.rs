use super::hash::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Clone)]
pub enum Dependencies<Container> {
    Resolving,
    Resolved(Container),
    Recursive,
}

pub trait HasIter<'a, T: 'a> {
    type Iter: IntoIterator<Item = &'a T>;

    fn get_iter(self) -> Self::Iter;
}

impl<'a, T> HasIter<'a, T> for &'a HashSet<T> {
    type Iter = std::collections::hash_set::Iter<'a, T>;

    fn get_iter(self) -> Self::Iter {
        self.iter()
    }
}

impl<'a, T> HasIter<'a, T> for &'a Vec<T> {
    type Iter = std::slice::Iter<'a, T>;

    fn get_iter(self) -> Self::Iter {
        self.iter()
    }
}

pub type DependencyGraph<T> = RawDependencyGraph<T, HashSet<T>>;

#[derive(derive_more::Deref, derive_more::DerefMut, Clone)]
pub struct RawDependencyGraph<T, C> {
    graph: HashMap<T, Dependencies<C>>,
}

impl<T: Hash + Eq, C> RawDependencyGraph<T, C>
where
    for<'a> &'a C: HasIter<'a, T>,
{
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
            for dep in deps.get_iter() {
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
            for dep in deps.get_iter() {
                if !visited.contains(dep) {
                    self.dfs_raw(dep, visited, func);
                }
            }
        }

        func(id);
    }
}

impl<T, C> Default for RawDependencyGraph<T, C> {
    fn default() -> Self {
        Self { graph: Default::default() }
    }
}
