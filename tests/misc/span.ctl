use super::assert;
use core::range::RangeFull;

fn span_eq<T>(a: [T..], b: [T..]): bool {
    if a.len() != b.len() {
        false
    } else {
        unsafe core::mem::compare(a.as_raw().as_ptr(), b.as_raw().as_ptr(), a.len())
    }
}

pub fn ranges() {
    mut x = @[1, 2, 3, 4].as_span();
    assert(span_eq(x.subspan(1usize..2), @[2].as_span()), "x[1..2] != [2, 3]");
    assert(span_eq(x.subspan(1usize..=2), @[2, 3].as_span()), "x[1..=2] != [2, 3]");
    assert(span_eq(x.subspan(..2usize), @[1, 2].as_span()), "x[..2] != [1, 2]");
    assert(span_eq(x.subspan(..=2usize), @[1, 2, 3].as_span()), "x[..=2] != [1, 2, 3]");
    assert(span_eq(x.subspan(RangeFull::<usize>()), x), "x[..] != [1, 2, 3, 4]");
}
