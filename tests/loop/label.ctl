// Output: 0 1 2 0 1 2 0 1 2
// Output: Done: 3

fn main() {
    mut outer = 0;
    @outer: while outer < 3 {
        defer outer++;

        while true {
            break;
            outer++;
        }

        for i in 0..5 {
            print("{i} ");
            if i == 2 {
                continue @outer;
            }
        }

        panic("Reached the outer loop!");
    }

    println("\nDone: {outer}");
}
