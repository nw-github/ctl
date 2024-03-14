// Output: 0 1 2 3 4
// Output: 0 1 2 3 4 5

fn main() {
    for i in 0..5 {
        print("{i} ");
    }
    println();

    for i in 0..=5 {
        print("{i} ");
    }
    println();
}
