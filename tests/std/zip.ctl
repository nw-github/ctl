// Output: 1 3 2 2 3 1

fn main() {
    let a = [1, 2, 3][..];
    let b = [3, 2, 1][..];
    for (a, b) in a.iter().zip(b.iter()) {
        print("{a} {b} ");
    }
}
