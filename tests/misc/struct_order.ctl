use super::assert;

struct A {
    pub x: B,
    pub y: B,
    pub z: B,

    fn get(this): i32 {
        this.x.get() + this.y.get() + this.z.get()
    }
}

struct B {
    pub d: D,

    fn get(this): i32 {
        this.d.get()
    }
}

struct C {
    pub a: A,

    fn get(this): i32 {
        this.a.get()
    }
}

struct D {
    pub e: E,

    fn get(this): i32 {
        this.e.get()
    }
}

struct E {
    pub x: i32,

    fn get(this): i32 {
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
