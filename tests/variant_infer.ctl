// Output: pass

union Foo {
    A,
    B(i32),
    C(x: i32)
}

fn main() {
    match Foo::A {
        :A => println("pass"),
        :B(_x) => {}
        :C{x: _} => {}
    }
}
