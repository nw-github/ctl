// Output: Pass!

fn main() {
    guard 5 == 10 else {
        return println("Pass!");
    }

    println("Broken!");
}
