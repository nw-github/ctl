// Output: Pass

fn main() {
    assert_eq(str::from_utf8(b"\xc0\xaf\xc1\x81"[..]), null); // Overlong encoding of '/A'
    println("Pass");
}
