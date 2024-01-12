use super::assert;

struct Foo {
    a: i32,
    b: i32,
}

pub fn struct_pattern() {
    let x = Foo(a: 10, b: 15);

    let {a, b} = x;
    assert(a == 10, "a != 10");
    assert(b == 15, "b != 15");

    match x {
        {mut a, b} => {
            a += 5;
            assert(a == 15, "a != 15");
            assert(b == 15, "b != 15");
        },
        _ => {
            assert(false, "catch all after destructure should never run");
        }
    }
}

union Bar {
    A,
    B(i32),
}

pub fn union_pattern() {
    let i = Bar::B(20);
    match i {
        Bar::A => assert(false, "i was Bar::A"),
        Bar::B(val) => assert(val == 20, "i was not Bar::B(20)"),
    }
}

union Quux {
    A,
    B(Foo),
}

pub fn union_struct_pattern() {
    let i = Quux::B(Foo(a: 10, b: 20));
    match i {
        Quux::A => assert(false, "i was Quux::A"),
        Quux::B({a, b}) => {
            assert(a == 10, "a was not 10");
            assert(b == 20, "b was not 20");
        },
    }
}

pub fn option_struct_pattern() {
    let i: ?Foo = Foo(a: 10, b: 20);
    match i {
        null => assert(false, "i was null"),
        ?{mut a, b} => {
            a += 5;
            assert(a == 15, "a was not 10");
            assert(b == 20, "b was not 20");
        },
    }
}

pub fn integer_pattern() {
    let x = 5;

    match x {
        0 => assert(false, "0 matched x = 5"),
        1 => assert(false, "1 matched x = 5"),
        5 => {},
        _ => assert(false, "5 didnt match x = 5"),
    }

    match -x {
        0 => assert(false, "0 matched x = -5"),
        1 => assert(false, "1 matched x = -5"),
        -5 => {},
        _ => assert(false, "-5 didnt match x = -5"),
    }

    match x {
        0..5 => assert(false, "0..5 matches x = 5"),
        _ => {}
    }

    match x {
        0..=5 => {},
        _ => assert(false, "0..=5 didn't match x = 5"),
    }

    match -x {
        -10..=5 => {},
        _ => assert(false, "-10..=5 didn't match x = -5"),
    }
}

pub fn string_pattern() {
    match "hello" {
        "goodbye" => assert(false, "hello matched 'goodbye'"),
        "oi" => assert(false, "hello matched 'oi'"),
        "bonjour" => assert(false, "hello matched 'bonjour'"),
        "hello" => {},
        _ => assert(false, "hello didn't match 'hello'"),
    }
}

pub fn span_pattern() {
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

pub fn span_pattern_destructure() {
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

pub fn span_pattern_destructure_2() {
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
