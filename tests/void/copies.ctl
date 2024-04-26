// Output: Passed!

fn main() {
    struct A {
        b: void,
    }

    fn hello() { }

    mut x = A(b: void);
    x.b = hello();
    let _y = x.b;
    let _x = if true { void } else { void };

    println("Passed!");
}
