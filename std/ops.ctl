$[lang(ordering)]
pub union Ordering: i8 {
    Less = -1,
    Greater = 0,
    Equal = 1,

    pub fn ==(this, rhs: *Ordering): bool => *this as i8 == *rhs as i8;
    pub fn <=>(this, rhs: *Ordering): Ordering => *this as i8 <=> *rhs as i8;
}

$[lang(op_cmp)]
pub trait Cmp<T> {
    fn cmp(this, rhs: *T): Ordering;

    fn ge(this, rhs: *T): bool => this.cmp(rhs) is Ordering::Greater | Ordering::Equal;
    fn gt(this, rhs: *T): bool => this.cmp(rhs) is Ordering::Greater;
    fn le(this, rhs: *T): bool => this.cmp(rhs) is Ordering::Less | Ordering::Equal;
    fn lt(this, rhs: *T): bool => this.cmp(rhs) is Ordering::Less;
}

pub trait TotallyOrdered: Cmp<This> {
    fn max(my this, rhs: This): This => this > rhs then this else rhs;
    fn min(my this, rhs: This): This => this > rhs then rhs else this;

    fn clamp(my this, low: This, hi: This): This {
        if this < low {
            low
        } else if this > hi {
            hi
        } else {
            this
        }
    }
}

$[lang(op_eq)]
pub trait Eq<T> {
    fn eq(this, rhs: *T): bool;

    fn ne(this, rhs: *T): bool => !this.eq(rhs);
}

$[lang(op_add)]
pub trait Add<T, R> {
    fn add(this, rhs: T): R;
}

$[lang(op_sub)]
pub trait Sub<T, R> {
    fn sub(this, rhs: T): R;
}

$[lang(op_mul)]
pub trait Mul<T, R> {
    fn mul(this, rhs: T): R;
}

$[lang(op_div)]
pub trait Div<T, R> {
    fn div(this, rhs: T): R;
}

$[lang(op_rem)]
pub trait Rem<T, R> {
    fn rem(this, rhs: T): R;
}

$[lang(op_and)]
pub trait BitAnd<T, R> {
    fn bit_and(this, rhs: T): R;
}

$[lang(op_or)]
pub trait BitOr<T, R> {
    fn bit_or(this, rhs: T): R;
}

$[lang(op_xor)]
pub trait Xor<T, R> {
    fn xor(this, rhs: T): R;
}

$[lang(op_shl)]
pub trait Shl<T, R> {
    fn shl(this, rhs: T): R;
}

$[lang(op_shr)]
pub trait Shr<T, R> {
    fn shr(this, rhs: T): R;
}

$[lang(op_neg)]
pub trait Neg<R> {
    fn neg(this): R;
}

$[lang(op_not)]
pub trait Not<R> {
    fn not(this): R;
}

$[lang(op_unwrap)]
pub trait Unwrap<R> {
    fn unwrap(my this): R;
}

$[lang(op_inc)]
pub trait Inc {
    fn inc(mut this);
}

$[lang(op_dec)]
pub trait Dec {
    fn dec(mut this);
}

$[lang(op_add_assign)]
pub trait AddAssign<T> {
    fn add_assign(mut this, rhs: T);
}

$[lang(op_sub_assign)]
pub trait SubAssign<T> {
    fn sub_assign(mut this, rhs: T);
}

$[lang(op_mul_assign)]
pub trait MulAssign<T> {
    fn mul_assign(mut this, rhs: T);
}

$[lang(op_div_assign)]
pub trait DivAssign<T> {
    fn div_assign(mut this, rhs: T);
}

$[lang(op_rem_assign)]
pub trait RemAssign<T> {
    fn rem_assign(mut this, rhs: T);
}

$[lang(op_and_assign)]
pub trait BitAndAssign<T> {
    fn and_assign(mut this, rhs: T);
}

$[lang(op_or_assign)]
pub trait BitOrAssign<T> {
    fn or_assign(mut this, rhs: T);
}

$[lang(op_xor_assign)]
pub trait XorAssign<T> {
    fn xor_assign(mut this, rhs: T);
}

$[lang(op_shl_assign)]
pub trait ShlAssign<T> {
    fn shl_assign(mut this, rhs: T);
}

$[lang(op_shr_assign)]
pub trait ShrAssign<T> {
    fn shr_assign(mut this, rhs: T);
}

$[lang(op_fn)]
pub trait Fn<Args: std::reflect::Tuple, R> {
    fn invoke(this, args: Args): R;
}

use std::reflect::{Tuple, SafeFnPtr};

extension<Args: Tuple, R, F: SafeFnPtr<Args, R>> F {
    impl Fn<Args, R> {
        fn invoke(this, args: Args): R => std::intrin::invoke_with_tuple(this, args);
    }
}

extension<Args: Tuple, R> *dyn Fn<Args, R> {
    impl Fn<Args, R> {
        // TODO: This call is not recursive because the . operator will check dynamic calls
        // before checking extensions, but this syntax seems ambiguous
        fn invoke(this, args: Args): R => (*this).invoke(args);
    }
}

extension<Args: Tuple, R> *dyn mut Fn<Args, R> {
    impl Fn<Args, R> {
        fn invoke(this, args: Args): R => (*this).invoke(args);
    }
}
