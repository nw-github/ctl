// Output: Pass!

fn main() {
    let x: ?i32 = -10;
    guard x is ?item and item.abs() == 10 else {
        return println("Broken!");
    }

    println("Pass!");
}
