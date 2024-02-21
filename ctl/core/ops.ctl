#(lang(op_eq))
pub trait Eq<T> {
    fn eq(this, rhs: *T): bool;

    fn ne(this, rhs: *T): bool {
        !this.eq(rhs)
    }
}
