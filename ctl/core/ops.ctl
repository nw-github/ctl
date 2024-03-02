#(lang(ordering))
pub union Ordering: i8 {
    Less /* = -1 */,
    Greater /* = 0 */,
    Equal /* = 1 */,
}

#(lang(op_cmp))
pub trait Cmp<T> {
    fn cmp(this, rhs: *T): Ordering;

    fn ge(this, rhs: *T): bool {
        match this.cmp(rhs) {
            Ordering::Greater => true,
            Ordering::Equal => true,
            _ => false,
        }
    }

    fn gt(this, rhs: *T): bool {
        this.cmp(rhs) is Ordering::Greater
    }

    fn le(this, rhs: *T): bool {
        match this.cmp(rhs) {
            Ordering::Less => true,
            Ordering::Equal => true,
            _ => false,
        }
    }

    fn lt(this, rhs: *T): bool {
        this.cmp(rhs) is Ordering::Less
    }
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

#(lang(op_and))
pub trait And<T, R> {
    fn and(this, rhs: T): R;
}

#(lang(op_and))
pub trait Or<T, R> {
    fn or(this, rhs: T): R;
}

#(lang(op_xor))
pub trait Xor<T, R> {
    fn xor(this, rhs: T): R;
}

#(lang(op_shl))
pub trait Shl<T, R> {
    fn shl(this, rhs: T): R;
}

#(lang(op_shr))
pub trait Shr<T, R> {
    fn shr(this, rhs: T): R;
}
