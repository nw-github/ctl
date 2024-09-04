// Output: 1FFFFFFFFFFFFFFFF
// Output: 20000000000000013

packed struct Foo {
    a: u2,
    x: u68,
}

fn main() {
    mut foo = Foo(a: 0, x: 0xffffffffffffffff | (1 << 64));
    println("{foo.x.to_str_radix(16)}");
    foo.x += 20;
    println("{foo.x.to_str_radix(16)}");
}
