// Output: 2 2 3
// Output: 2 2 3
// Output: 2 2 3
// Output: 1

fn main() {
    fn test(a: int, b: int, c: int) {
        println("{a} {b} {c}");
    }

    mut x = 0;
    for _ in 0..=2 {
        test(
            c: x = 3,
            b: --x,
            a: x--,
        );
    }
    println("{x}");
}

