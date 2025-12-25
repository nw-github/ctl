// Output: Pass!

extern fn sprintf(dst: ^mut u8, fmt: ^u8, ...): c_int;

struct NoFmt {}

fn main() {
    let auto = 10i32;
    let auto = &auto;
    let nofmt = &NoFmt();

    mut buf = [0u8; 1024];
    let len = unsafe sprintf(
        dst: buf.as_raw_mut(),
        fmt: "Cool: %p %d %p!\0".as_raw().cast(),
        auto,
        *auto,
        nofmt,
    ) as! uint;

    let ctl = "Cool: {auto:p} {auto} {nofmt:p}!".to_str();
    assert_eq(ctl.len(), len);
    assert_eq(ctl.as_bytes(), buf[..len]);

    println("Pass!");
}
