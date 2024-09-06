// Output: null -2 (-2, true)

fn main() {
    let [a, b] = [i2::min_value(), -1];
    let x = a.checked_div(b);
    let y = a.wrapping_div(b);
    let z = a.overflowing_div(b);
    println("{x} {y} ({z.0}, {z.1})");
}
