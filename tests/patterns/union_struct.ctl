// Output: 10 20

struct Foo {
    a: int,
    b: int,
}

union Quux {
    A,
    B(Foo),
}

fn main() {
    let i = Quux::B(Foo(a: 10, b: 20));
    match i {
        Quux::A => println("fail!"),
        Quux::B({a, b}) => {
            println("{a} {b}");
        },
    }
}