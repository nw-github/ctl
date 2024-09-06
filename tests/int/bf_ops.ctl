// Output: 7 0 3 1 1
// Output: 16 8

packed struct Foo {
    a: u64,
    b: u4,
    c: u2,
    d: u2,
    e: u8,
}

fn main() {
    mut foo = Foo(a: 5, b: 4, c: 3, d: 2, e: 1);
    foo.a += 2;
    foo.b -= 4;
    foo.d = 1;

    println("{foo.a} {foo.b} {foo.c} {foo.d} {foo.e}");
    println("{std::mem::size_of_val(&foo)} {std::mem::align_of_val(&foo)}");
}
