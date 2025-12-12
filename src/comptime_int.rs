use std::{
    fmt::{Display, LowerHex},
    hash::Hash,
    ops::{
        Add, AddAssign, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Shl, Shr, ShrAssign, Sub,
        SubAssign,
    },
};

use num_bigint::{BigInt, Sign};
use num_traits::{Num, NumCast};

fn bigint_as_small(v: &BigInt) -> Option<i64> {
    v.try_into().ok()
}

#[derive(Debug, Clone, Eq)]
/// An infinite-precision integer that doesn't allocate for small numbers
pub enum ComptimeInt {
    Small(i64),
    Big(BigInt),
}

/* === Comparison operators === */

impl PartialEq<ComptimeInt> for ComptimeInt {
    fn eq(&self, rhs: &ComptimeInt) -> bool {
        match self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => lhs == rhs,
                ComptimeInt::Big(rhs) => Some(*lhs) == bigint_as_small(rhs),
            },
            ComptimeInt::Big(lhs) => match rhs {
                ComptimeInt::Small(rhs) => bigint_as_small(lhs) == Some(*rhs),
                ComptimeInt::Big(rhs) => lhs == rhs,
            },
        }
    }
}

impl Hash for ComptimeInt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ComptimeInt::Small(v) => v.hash(state),
            ComptimeInt::Big(v) => v.hash(state),
        }
    }
}

impl PartialOrd<ComptimeInt> for ComptimeInt {
    fn partial_cmp(&self, other: &ComptimeInt) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ComptimeInt {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        // TODO: maybe this can be done without promoting one side to a BigInt
        match self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => lhs.cmp(rhs),
                ComptimeInt::Big(rhs) => BigInt::from(*lhs).cmp(rhs),
            },
            ComptimeInt::Big(lhs) => match rhs {
                ComptimeInt::Small(rhs) => lhs.cmp(&BigInt::from(*rhs)),
                ComptimeInt::Big(rhs) => lhs.cmp(rhs),
            },
        }
    }
}

/* === Arithmetic & logical operators === */

impl BitAnd<&ComptimeInt> for ComptimeInt {
    type Output = ComptimeInt;

    fn bitand(mut self, rhs: &ComptimeInt) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => *lhs &= rhs,
                ComptimeInt::Big(rhs) => {
                    if let Some(v) = bigint_as_small(rhs) {
                        *lhs &= v;
                    } else {
                        return ComptimeInt::Big(BigInt::from(*lhs) & rhs);
                    }
                }
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => *lhs &= BigInt::from(rhs),
                ComptimeInt::Big(rhs) => *lhs &= rhs,
            },
        }

        self
    }
}

impl BitAnd<&ComptimeInt> for &ComptimeInt {
    type Output = ComptimeInt;

    fn bitand(self, rhs: &ComptimeInt) -> Self::Output {
        match self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => ComptimeInt::Small(lhs & rhs),
                ComptimeInt::Big(rhs) => bigint_as_small(rhs)
                    .map(|v| ComptimeInt::Small(lhs & v))
                    .unwrap_or_else(|| ComptimeInt::Big(BigInt::from(*lhs) & rhs)),
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => bigint_as_small(lhs)
                    .map(|v| ComptimeInt::Small(v & rhs))
                    .unwrap_or_else(|| ComptimeInt::Big(lhs & BigInt::from(rhs))),
                ComptimeInt::Big(rhs) => ComptimeInt::Big(lhs & rhs),
            },
        }
    }
}

impl BitXor<&ComptimeInt> for ComptimeInt {
    type Output = ComptimeInt;

    fn bitxor(mut self, rhs: &ComptimeInt) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => *lhs ^= rhs,
                ComptimeInt::Big(rhs) => {
                    if let Some(v) = bigint_as_small(rhs) {
                        *lhs ^= v;
                    } else {
                        return ComptimeInt::Big(BigInt::from(*lhs) ^ rhs);
                    }
                }
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => *lhs ^= BigInt::from(rhs),
                ComptimeInt::Big(rhs) => *lhs ^= rhs,
            },
        }

        self
    }
}

impl BitOr<&ComptimeInt> for ComptimeInt {
    type Output = ComptimeInt;

    fn bitor(mut self, rhs: &ComptimeInt) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => *lhs |= rhs,
                ComptimeInt::Big(rhs) => {
                    if let Some(v) = bigint_as_small(rhs) {
                        *lhs |= v;
                    } else {
                        return ComptimeInt::Big(BigInt::from(*lhs) | rhs);
                    }
                }
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => *lhs |= BigInt::from(rhs),
                ComptimeInt::Big(rhs) => *lhs |= rhs,
            },
        }

        self
    }
}

impl Neg for ComptimeInt {
    type Output = ComptimeInt;

    fn neg(self) -> Self::Output {
        match self {
            ComptimeInt::Small(v) => v
                .checked_neg()
                .map(ComptimeInt::Small)
                .unwrap_or_else(|| ComptimeInt::Big(-BigInt::from(v))),
            ComptimeInt::Big(v) => ComptimeInt::Big(-v),
        }
    }
}

impl Add<&ComptimeInt> for ComptimeInt {
    type Output = ComptimeInt;

    fn add(mut self, rhs: &ComptimeInt) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => *lhs += rhs,
                ComptimeInt::Big(rhs) => {
                    if let Some(v) = bigint_as_small(rhs) {
                        *lhs += v;
                    } else {
                        return ComptimeInt::Big(BigInt::from(*lhs) + rhs);
                    }
                }
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => *lhs += rhs,
                ComptimeInt::Big(rhs) => *lhs += rhs,
            },
        }

        self
    }
}

impl Sub<&ComptimeInt> for ComptimeInt {
    type Output = ComptimeInt;

    fn sub(mut self, rhs: &ComptimeInt) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => *lhs -= rhs,
                ComptimeInt::Big(rhs) => {
                    if let Some(v) = bigint_as_small(rhs) {
                        *lhs -= v;
                    } else {
                        return ComptimeInt::Big(BigInt::from(*lhs) - rhs);
                    }
                }
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => *lhs -= rhs,
                ComptimeInt::Big(rhs) => *lhs -= rhs,
            },
        }

        self
    }
}

impl Mul<&ComptimeInt> for ComptimeInt {
    type Output = ComptimeInt;

    fn mul(mut self, rhs: &ComptimeInt) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => *lhs *= rhs,
                ComptimeInt::Big(rhs) => {
                    if let Some(v) = bigint_as_small(rhs) {
                        *lhs *= v;
                    } else {
                        return ComptimeInt::Big(BigInt::from(*lhs) * rhs);
                    }
                }
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => *lhs *= rhs,
                ComptimeInt::Big(rhs) => *lhs *= rhs,
            },
        }

        self
    }
}

impl Div<&ComptimeInt> for ComptimeInt {
    type Output = ComptimeInt;

    fn div(mut self, rhs: &ComptimeInt) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => *lhs /= rhs,
                ComptimeInt::Big(rhs) => {
                    if let Some(v) = bigint_as_small(rhs) {
                        *lhs /= v;
                    } else {
                        return ComptimeInt::Big(BigInt::from(*lhs) / rhs);
                    }
                }
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => *lhs /= rhs,
                ComptimeInt::Big(rhs) => *lhs /= rhs,
            },
        }

        self
    }
}

impl Rem<&ComptimeInt> for ComptimeInt {
    type Output = ComptimeInt;

    fn rem(mut self, rhs: &ComptimeInt) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => match rhs {
                ComptimeInt::Small(rhs) => *lhs %= rhs,
                ComptimeInt::Big(rhs) => {
                    if let Some(v) = bigint_as_small(rhs) {
                        *lhs %= v;
                    } else {
                        return ComptimeInt::Big(BigInt::from(*lhs) % rhs);
                    }
                }
            },
            ComptimeInt::Big(lhs) => match rhs {
                &ComptimeInt::Small(rhs) => *lhs %= rhs,
                ComptimeInt::Big(rhs) => *lhs %= rhs,
            },
        }

        self
    }
}

/* === Primitive operators === */

impl Shl<u32> for ComptimeInt {
    type Output = ComptimeInt;

    fn shl(mut self, rhs: u32) -> Self::Output {
        match &mut self {
            ComptimeInt::Small(lhs) => {
                // shift is too large or shift goes into the sign bit
                if let Some(v) = lhs.checked_shl(rhs).filter(|&v| v >= 0 || *lhs < 0) {
                    *lhs = v;
                } else {
                    self = ComptimeInt::Big(BigInt::from(*lhs) << rhs)
                }
            }
            ComptimeInt::Big(lhs) => *lhs <<= rhs,
        }

        self
    }
}

impl Shr<u32> for ComptimeInt {
    type Output = ComptimeInt;

    fn shr(mut self, rhs: u32) -> Self::Output {
        self.shr_assign(rhs);
        self
    }
}

impl ShrAssign<u32> for ComptimeInt {
    fn shr_assign(&mut self, rhs: u32) {
        match self {
            ComptimeInt::Small(lhs) => {
                if let Some(v) = lhs.checked_shr(rhs) {
                    *lhs = v;
                } else {
                    *self = ComptimeInt::Big(BigInt::from(*lhs) >> rhs);
                }
            }
            ComptimeInt::Big(lhs) => *lhs >>= rhs,
        }
    }
}

impl Add<i64> for ComptimeInt {
    type Output = ComptimeInt;

    fn add(mut self, rhs: i64) -> Self::Output {
        self.add_assign(rhs);
        self
    }
}

impl Add<i64> for &ComptimeInt {
    type Output = ComptimeInt;

    fn add(self, rhs: i64) -> Self::Output {
        match self {
            ComptimeInt::Small(lhs) => lhs
                .checked_add(rhs)
                .map(ComptimeInt::Small)
                .unwrap_or_else(|| ComptimeInt::Big(BigInt::from(*lhs) + rhs)),
            ComptimeInt::Big(lhs) => ComptimeInt::Big(lhs + rhs),
        }
    }
}

impl AddAssign<i64> for ComptimeInt {
    fn add_assign(&mut self, rhs: i64) {
        match self {
            ComptimeInt::Small(lhs) => {
                if let Some(v) = lhs.checked_add(rhs) {
                    *lhs = v;
                } else {
                    *self = ComptimeInt::Big(BigInt::from(*lhs) + rhs)
                }
            }
            ComptimeInt::Big(lhs) => *lhs += rhs,
        }
    }
}

impl Sub<i64> for ComptimeInt {
    type Output = ComptimeInt;

    fn sub(mut self, rhs: i64) -> Self::Output {
        self.sub_assign(rhs);
        self
    }
}

impl SubAssign<i64> for ComptimeInt {
    fn sub_assign(&mut self, rhs: i64) {
        match self {
            ComptimeInt::Small(lhs) => {
                if let Some(v) = lhs.checked_sub(rhs) {
                    *lhs = v;
                } else {
                    *self = ComptimeInt::Big(BigInt::from(*lhs) - rhs)
                }
            }
            ComptimeInt::Big(lhs) => *lhs -= rhs,
        }
    }
}

impl TryInto<usize> for ComptimeInt {
    type Error = ();

    fn try_into(self) -> Result<usize, Self::Error> {
        match self {
            ComptimeInt::Small(v) => v.try_into().map_err(|_| ()),
            ComptimeInt::Big(v) => v.try_into().map_err(|_| ()),
        }
    }
}

impl TryInto<u32> for ComptimeInt {
    type Error = ();

    fn try_into(self) -> Result<u32, Self::Error> {
        match self {
            ComptimeInt::Small(v) => v.try_into().map_err(|_| ()),
            ComptimeInt::Big(v) => v.try_into().map_err(|_| ()),
        }
    }
}

/* === Convenience traits === */

impl<T: Copy> From<T> for ComptimeInt
where
    i64: TryFrom<T>,
    BigInt: From<T>,
{
    fn from(value: T) -> Self {
        if let Ok(v) = i64::try_from(value) {
            Self::Small(v)
        } else {
            Self::Big(BigInt::from(value))
        }
    }
}

impl Default for ComptimeInt {
    fn default() -> Self {
        ComptimeInt::new(0)
    }
}

impl Display for ComptimeInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComptimeInt::Small(v) => write!(f, "{v}"),
            ComptimeInt::Big(v) => write!(f, "{v}"),
        }
    }
}

impl LowerHex for ComptimeInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComptimeInt::Small(v) => write!(f, "{v:#x}"),
            ComptimeInt::Big(v) => write!(f, "{v:#x}"),
        }
    }
}

impl ComptimeInt {
    pub const fn new(val: i64) -> ComptimeInt {
        ComptimeInt::Small(val)
    }

    pub fn from_str_radix(src: &str, radix: u32) -> Option<ComptimeInt> {
        match i64::from_str_radix(src, radix) {
            Ok(v) => Some(ComptimeInt::Small(v)),
            Err(v) => match v.kind() {
                std::num::IntErrorKind::PosOverflow | std::num::IntErrorKind::NegOverflow => {
                    Some(ComptimeInt::Big(BigInt::from_str_radix(src, radix).ok()?))
                }
                _ => None,
            },
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            &ComptimeInt::Small(v) => v < 0,
            ComptimeInt::Big(big_int) => big_int.sign() == Sign::Minus,
        }
    }

    pub fn bits(&self) -> u32 {
        const SMALL_BITS: u32 = core::mem::size_of::<i64>() as u32 * 8;
        match self {
            ComptimeInt::Small(v) => {
                if self.is_negative() {
                    SMALL_BITS - v.leading_ones() + 1
                } else {
                    SMALL_BITS - v.leading_zeros()
                }
            }
            ComptimeInt::Big(v) => v.bits() as u32,
        }
    }

    pub fn abs(self) -> ComptimeInt {
        match self {
            ComptimeInt::Small(v) => v
                .checked_abs()
                .map(ComptimeInt::Small)
                .unwrap_or_else(|| ComptimeInt::Big(-BigInt::from(v))),
            ComptimeInt::Big(v) => ComptimeInt::Big(if v.sign() == Sign::Minus { -v } else { v }),
        }
    }

    pub fn into_word<T: NumCast>(self) -> Option<T> {
        match self {
            ComptimeInt::Small(v) => T::from(v),
            ComptimeInt::Big(v) => T::from(v),
        }
    }

    pub fn fits_into(&self, bits: u32, signed: bool) -> bool {
        self.bits() <= bits && (!signed || !self.is_negative())
    }
}

#[cfg(test)]
mod tests {
    use crate::typeid::Integer;

    // use super::*;

    #[test]
    pub fn bigint_limits() {
        for bits in 0..256 {
            let mut int = Integer { bits, signed: false, char: false };
            let umin = int.min();
            let umax = int.max();
            int.signed = true;
            let imin = int.min();
            let imax = int.max();

            assert_eq!(umin.bits(), 0, "UMIN = ({umin})");
            assert_eq!(umax.bits(), bits, "(UMAX = {umax})");
            assert_eq!(imin.bits(), bits, "(IMIN = {imin})");
            assert_eq!(imax.bits(), (bits).saturating_sub(1), "(IMAX = {imax})");
        }
    }
}
