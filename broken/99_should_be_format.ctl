fn main() {
    let v = ?10;
    println("{v}");

    // ?int is format, so ??int should be too
    let v = ??10;
    println("{v}");
}
