// Output: true
// Output: true
// Output: true
// Output: true

struct Foo {
    a: int,
    b: int,
}

fn main() {
    mut fooa = Foo(a: 10, b: 10);
    mut foob = Foo(a: 10, b: 10);
    match @[&mut fooa, &mut foob].as_span_mut() {
        [{a, b}, {a: a2, b: b2}] => {
            *a = 1;
            *b = 2;
            *a2 = 3;
            *b2 = 4;

            println("{fooa.a == 1}");
            println("{fooa.b == 2}");
            println("{foob.a == 3}");
            println("{foob.b == 4}");
        }
        _ => {
            println("span pattern didnt match");
        }
    }
}
