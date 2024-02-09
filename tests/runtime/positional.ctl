use super::assert;

fn hello(x: int, kw y: int, z: int) {
    assert(x == 10, "x != 10");
    assert(y == 11, "y != 11");
    assert(z == 12, "z != 12");
}

pub fn start() {
    hello(y: 11, 10, 12);
}

pub fn middle() {
    hello(10, y: 11, 12);
}

pub fn end() {
    hello(10, 12, y: 11);
}
