// Output: 10 15
// Output: 15 15

struct Foo {
    a: int,
    b: int,
}

fn main() {
    let x = Foo(a: 10, b: 15);

    let {a, b} = x;
    println("{a} {b}");
    match x {
        {mut a, b} => {
            a += 5;
            println("{a} {b}");
        },
        _ => {
            println("fail!");
        }
    }
}
