// Output: 10 11 12
// Output: 10 11 12
// Output: 10 11 12

fn main() {
    hello(y: 11, 10, 12);
    hello(10, y: 11, 12);
    hello(10, 12, y: 11);
}

fn hello(x: int, kw y: int, z: int) {
    println("{x} {y} {z}");
}
