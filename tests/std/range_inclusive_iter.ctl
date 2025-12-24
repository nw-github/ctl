// Output: 0
// Output: 4294967295
// Output: 2147483647
// Output: -2147483648

fn main() {
    for x in u32::min_value()..=u32::min_value() {
        println(x);
    }

    for x in u32::max_value()..=u32::max_value() {
        println(x);
    }

    for x in i32::max_value()..=i32::max_value() {
        println(x);
    }

    for x in i32::min_value()..=i32::min_value() {
        println(x);
    }
}