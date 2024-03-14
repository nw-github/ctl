// Output: 15 20

struct Foo {
    a: int,
    b: int,
}

fn main() {
    let i: ?Foo = Foo(a: 10, b: 20);
    match i {
        null => println("fail!"),
        ?{mut a, b} => {
            a += 5;
            println("{a} {b}");
        },
    }
}

