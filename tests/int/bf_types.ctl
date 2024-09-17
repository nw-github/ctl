// Output: 4
// Output: 0 A -1 2 false
// Output: 1 B -2 4 true
// Output: 2 C -3 6 false

union Foo { A, B, C }

packed struct Packed {
    a: Foo,
    b: char,
    c: i4,
    d: u4,
    e: bool,
}

fn main() {
    println("{std::mem::size_of::<Packed>()}");

    mut x = Packed(a: :A, b: 'A', c: -1, d: 2, e: false);
    println("{x.a as u2} {x.b} {x.c} {x.d} {x.e}");

    x.a = :B;
    x.b = 'B';
    x.c -= 1;
    x.d += 2;
    x.e = !x.e;

    println("{x.a as u2} {x.b} {x.c} {x.d} {x.e}");

    x.a = :C;
    x.b = 'C';
    x.c -= 1;
    x.d += 2;
    x.e = !x.e;

    println("{x.a as u2} {x.b} {x.c} {x.d} {x.e}");
}
