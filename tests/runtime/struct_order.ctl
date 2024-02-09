use super::assert;

struct A {
    pub x: B,
    pub y: B,
    pub z: B,

    fn get(this): int {
        this.x.get() + this.y.get() + this.z.get()
    }
}

struct B {
    pub d: D,

    fn get(this): int {
        this.d.get()
    }
}

struct C {
    pub a: A,

    fn get(this): int {
        this.a.get()
    }
}

struct D {
    pub e: E,

    fn get(this): int {
        this.e.get()
    }
}

struct E {
    pub x: int,

    fn get(this): int {
        this.x
    }
}

pub fn test() {
    let x = A(
        x: B(d: D(e: E(x: 1))),
        y: B(d: D(e: E(x: 2))),
        z: B(d: D(e: E(x: 3))),
    );

    assert(x.get() == 6, "x != 6");
}
