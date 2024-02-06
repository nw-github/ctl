#(lang(iter))
pub trait Iterator<T> {
    fn next(mut this): ?T;
}
