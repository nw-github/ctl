// Output: 1ffffffffffffffff
// Output: 20000000000000013

packed struct Foo {
    a: u2,
    x: u68,
}

fn main() {
    mut foo = Foo(a: 0, x: 0x1ffffffffffffffff);
    println("{foo.x:x}");
    foo.x += 20;
    println("{foo.x:x}");
}
