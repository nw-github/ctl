fn main() {
    let x = &raw {} as ^never;
    let y = unsafe *x;
    // should not get to this print in debug mode, although this whole thing is UB
    println("0x{(&y as uint).to_str_radix(16)}");
}
