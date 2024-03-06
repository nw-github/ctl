use super::assert;
use std::range::RangeFull;
use std::span;

struct Foo {
    a: int,
    b: int,
}

pub fn ranges() {
    mut x = @[1, 2, 3, 4].as_span();
    assert(span::compare(x.subspan(1u..2), @[2].as_span()), "x[1..2] != [2, 3]");
    assert(span::compare(x.subspan(1u..=2), @[2, 3].as_span()), "x[1..=2] != [2, 3]");
    assert(span::compare(x.subspan(..2u), @[1, 2].as_span()), "x[..2] != [1, 2]");
    assert(span::compare(x.subspan(..=2u), @[1, 2, 3].as_span()), "x[..=2] != [1, 2, 3]");
    // assert(span::compare(x.subspan(RangeFull()), x), "x[..] != [1, 2, 3, 4]");
}

pub fn pattern() {
    mut vec = @[10, 10, 10, 10, 10];
    match vec.as_span_mut() {
        [a, b, ...mid, c, d] => {
            *a = 1;
            *b = 2;
            *mid.get_mut(0)! = 3;
            *c = 4;
            *d = 5;

            assert(*vec.get(0)! == 1, "vec[0] != 1");
            assert(*vec.get(1)! == 2, "vec[1] != 2");
            assert(*vec.get(2)! == 3, "vec[2] != 3");
            assert(*vec.get(3)! == 4, "vec[3] != 4");
            assert(*vec.get(4)! == 5, "vec[4] != 5");
        }
        _ => {
            assert(false, "span pattern didnt match");
        }
    }
}

pub fn pattern_destructure() {
    mut vec = @[Foo(a: 10, b: 10), Foo(a: 10, b: 10)];
    match vec.as_span_mut() {
        [{a, b}, {a: a2, b: b2}] => {
            *a = 1;
            *b = 2;
            *a2 = 3;
            *b2 = 4;

            assert(vec.get(0)!.a == 1, "vec[0] != 1");
            assert(vec.get(0)!.b == 2, "vec[0] != 2");
            assert(vec.get(1)!.a == 3, "vec[1] != 3");
            assert(vec.get(1)!.b == 4, "vec[1] != 4");
        }
        _ => {
            assert(false, "span pattern didnt match");
        }
    }
}

pub fn pattern_destructure_2() {
    mut fooa = Foo(a: 10, b: 10);
    mut foob = Foo(a: 10, b: 10);
    match @[&mut fooa, &mut foob].as_span_mut() {
        [{a, b}, {a: a2, b: b2}] => {
            *a = 1;
            *b = 2;
            *a2 = 3;
            *b2 = 4;

            assert(fooa.a == 1, "fooa != 1");
            assert(fooa.b == 2, "fooa != 2");
            assert(foob.a == 3, "foob != 3");
            assert(foob.b == 4, "foob != 4");
        }
        _ => {
            assert(false, "span pattern didnt match");
        }
    }
}
