// Error: static initializer depends directly or indirectly on itself

fn main() {
    static FOO: int = BAR;
    static BAR: int = FOO;
}
