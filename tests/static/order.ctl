// Output: 15 10 10 15

fn main() {
    static FOO: int = BAR + 5;
    static BAR: int = 10;

    static BAZ: int = 10;
    static QUUX: int = BAZ + 5;

    println("{FOO} {BAR} {BAZ} {QUUX}");
}
