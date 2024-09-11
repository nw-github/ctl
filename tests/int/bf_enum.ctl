// Output: 0 10
// Output: 1 15
// Output: 2 20

union Foo { A, B, C }

packed struct Packed {
    a: Foo,
    b: u14,
}

fn main() {
    mut x = Packed(a: :A, b: 10);
    println("{x.a as u2} {x.b}");

    x.a = :B;
    x.b += 5;

    println("{x.a as u2} {x.b}");

    x.a = :C;
    x.b += 5;

    println("{x.a as u2} {x.b}");
}
