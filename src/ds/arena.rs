use std::{marker::PhantomData, ops::Index};

#[repr(transparent)]
#[derive(Default)]
pub struct Id<T>(usize, PhantomData<T>);

impl<T> Id<T> {
    pub const fn new(id: usize) -> Self {
        Self(id, PhantomData)
    }
}

impl<T> Copy for Id<T> {}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl<T> std::hash::Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Id").field(&self.0).finish()
    }
}

impl<T> Eq for Id<T> {}

pub struct Arena<T> {
    data: Vec<T>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self { data: Vec::with_capacity(cap) }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn reserve(&mut self, add: usize) {
        self.data.reserve(add);
    }

    pub fn alloc(&mut self, item: T) -> Id<T> {
        let key = Id::new(self.data.len());
        self.data.push(item);
        key
    }

    pub fn get(&self, item: Id<T>) -> &T {
        &self.data[item.0]
    }

    pub fn get_mut(&mut self, item: Id<T>) -> &mut T {
        &mut self.data[item.0]
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self { data: Default::default() }
    }
}

impl<T> Index<Id<T>> for Arena<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        self.get(index)
    }
}
