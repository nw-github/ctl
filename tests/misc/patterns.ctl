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
