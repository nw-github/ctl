// Output: pass!

fn main() {
    static mut FOO: i32 = 0;
    let _ = &raw mut FOO;

    println("pass!");
}
