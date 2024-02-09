use super::assert;

struct Foo {
    a: int,
    b: int,
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
    B(int),
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
