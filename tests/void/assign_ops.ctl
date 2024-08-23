// Output: Passed!

fn main() {
    mut a = 10;
    mut c = {};
    c = a += 5;
    c = a -= 5;
    c = a *= 5;
    c = a /= 5;
    c = a %= 5;
    c = a &= 5;
    c = a |= 5;
    c = a ^= 5;
    c = a <<= 5;
    c = a >>= 5;
    c = a = 5;
    println("Passed!");
}
