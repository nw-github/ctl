// Output: 8 5

fn double(n: *mut int): int {
    let old = *n;
    *n *= 2;
    old
}

fn add(a: int, b: int): int {
    a + b
}

fn main() {
    struct A {
        x: int,
        y: int,
    }

    mut x = 1;
    let y = add(
        A(x: x++,           // x = 2, ret = 1
          y: double(&mut x) // x = 4, ret = 2
        ).x,                // 1
        double(&mut x)      // x = 8, ret = 4
    );

    println("{x} {y}");
}
