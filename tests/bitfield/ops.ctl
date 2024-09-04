// Output: true true true true true
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

    // FIXME: format is currently broken for numbers too small to store the radix
    println("{foo.a == 7} {foo.b == 0} {foo.c == 3} {foo.d == 1} {foo.e == 1}");
    println("{std::mem::size_of_val(&foo)} {std::mem::align_of_val(&foo)}");
}
