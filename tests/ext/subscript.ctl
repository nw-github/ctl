// Output: 4
// Output: 3
// Output: 2
// Output: 1

extension Test for int {
    pub fn [](this, i: int): int {
        mut base = 1;
        for _ in 0..i {
            base *= 10;
        }
        (this / base) % 10
    }
}

fn main() {
    println(1234[0]);
    println(1234[1]);
    println(1234[2]);
    println(1234[3]);
}
