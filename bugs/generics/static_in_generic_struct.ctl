struct A<T> {
    t: T,

    pub fn something(t: T): A<T> {
        A(t:)
    }

    pub fn self_type(t: T): This {
        A::something(t)
    }
}

fn main() {
    let a = A::something;     // should say: needs type annotations
    let b = A::something(20); // type should be A<i32>, not A<T>
}
