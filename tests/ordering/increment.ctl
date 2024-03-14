// Output: 3 4
// Output: 3 4
// Output: 3 4

fn main() {
    for _ in 0..=2 {
        mut x = 1;
        let y = x++  // x = 2, ret = 1
                + 
                ++x; // x = 3, ret = 3
        println("{x} {y}");
    }
}
