// Output: 1 2 3 4

struct Foo {
    a: int,
    b: int,
}

fn main() {
    mut fooa = Foo(a: 10, b: 10);
    mut foob = Foo(a: 10, b: 10);
    match [&mut fooa, &mut foob][..] {
        [{a, b}, {a: a2, b: b2}] => {
            *a = 1;
            *b = 2;
            *a2 = 3;
            *b2 = 4;
            print("{fooa.a} {fooa.b} {foob.a} {foob.b}");
        }
        _ => {
            println("span pattern didnt match");
        }
    }
}
