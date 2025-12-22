// Output: 1
// Output: 2

fn main() {
    coolb(&[1, 2]);
}

fn coolb([a, b]: *[u8; 2]) {
    println(*a);
    println(*b);
}
