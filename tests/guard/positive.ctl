// Output: Pass!

fn main() {
    guard 5 == 5 else {
        return println("Broken!");
    }

    println("Pass!");
}
