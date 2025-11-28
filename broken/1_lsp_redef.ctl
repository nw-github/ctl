fn hi(a: i32) { let _ = a; }

fn main() {
    // this gets the wrong semantic token
    let hi = 10;
    let _  = hi;
}
