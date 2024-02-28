#(lang(ordering))
pub union Ordering: i8 {
    Less /* = -1 */,
    Greater /* = 0 */,
    Equal /* = 1 */,
}

#(lang(op_cmp))
pub trait Cmp<T> {
    fn cmp(this, rhs: *T): Ordering;
}

#(lang(op_eq))
pub trait Eq<T> {
    fn eq(this, rhs: *T): bool;

    fn ne(this, rhs: *T): bool {
        !this.eq(rhs)
    }
}

#(lang(op_add))
pub trait Add<T, R> {
    fn add(this, rhs: T): R;
}

#(lang(op_sub))
pub trait Sub<T, R> {
    fn sub(this, rhs: T): R;
}

#(lang(op_mul))
pub trait Mul<T, R> {
    fn mul(this, rhs: T): R;
}

#(lang(op_div))
pub trait Div<T, R> {
    fn div(this, rhs: T): R;
}

#(lang(op_rem))
pub trait Rem<T, R> {
    fn rem(this, rhs: T): R;
}
