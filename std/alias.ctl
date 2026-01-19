// Output: Pass!

pub type Foo = i32;
pub type Bar<T> = [T];

fn main() {
    let foo: Foo = 10;
    let bar: Bar<int> = @[1, 2, 3];

    assert_eq(foo, 10);
    assert_eq(bar[..], [1, 2, 3][..]);
    assert_eq(Foo::max_value(), i32::max_value());

    println("Pass!");
}

