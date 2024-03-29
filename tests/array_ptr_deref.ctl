// Output: 1 2 1 2
// Output: true
// Output: 97 98

fn main() {
    let x = &[1, 2];
    let y = *x;

    println("{x[0]} {x[1]} {y[0]} {y[1]}");
    println("{!std::ptr::eq(x, &y)}");

    let x = *b"ab";
    println("{x[0]} {x[1]}");
}
