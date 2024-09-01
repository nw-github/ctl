// Output: true
// Output: true
// Output: true
// Output: true

struct Foo {
    a: int,
    b: int,
}

fn main() {
    let vec = [Foo(a: 10, b: 10), Foo(a: 10, b: 10)][..];
    match vec {
        [{a, b}, {a: a2, b: b2}] => {
            *a = 1;
            *b = 2;
            *a2 = 3;
            *b2 = 4;

            println("{vec.get(0)!.a == 1}");
            println("{vec.get(0)!.b == 2}");
            println("{vec.get(1)!.a == 3}");
            println("{vec.get(1)!.b == 4}");
        }
        _ => {
            println("span pattern didnt match");
        }
    }
}