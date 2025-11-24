
fn deref_never(): i32 {
    let x = &raw {} as *raw never;
    let y = unsafe *x;
    // should not get to this print
    println("0x{(&y as uint).to_str_radix(16)}");
}
