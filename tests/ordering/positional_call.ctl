// Output: 8 7
// Output: 8 7
// Output: 8 7

fn main() {
    for _ in 0..=2 {
        mut x = 1;
        mut y = double(&mut x)  // x = 2, ret = 1
            + add(
                double(&mut x), // x = 4, ret = 2
                double(&mut x), // x = 8, ret = 4
            );                       // x = 8, ret = 6
        println("{x} {y}");
    }
}

fn double(n: *mut int): int {
    let old = *n;
    *n *= 2;
    old
}

fn add(a: int, b: int): int {
    a + b
}
