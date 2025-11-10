fn main() {
    let a = 0;
    mut b = 1;

    mut c = &raw a;
    c = &a;
    c = &mut b;
    c = &raw mut b;
}
