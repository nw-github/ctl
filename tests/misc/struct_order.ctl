use super::assert;

struct A {
    pub x: B,
    pub y: B,
    pub z: B,

    fn get(this): c_int {
        return this.x.get() + this.y.get() + this.z.get();
    }
}

struct B {
    pub d: D,

    fn get(this): c_int {
        return this.d.get();
    }
}

struct C {
    pub a: A,

    fn get(this): c_int {
        return this.a.get();
    }
}

struct D {
    pub e: E,

    fn get(this): c_int {
        return this.e.get();
    }
}

struct E {
    pub x: c_int,

    fn get(this): c_int {
        return this.x;
    }
}

pub fn test() {
    let x = A(
        x: B(d: D(e: E(x: 1 as! c_int))),
        y: B(d: D(e: E(x: 2 as! c_int))),
        z: B(d: D(e: E(x: 3 as! c_int))),
    );

    assert(x.get() == 6, "x != 6");
}
