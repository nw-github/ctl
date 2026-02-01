// Output: PASS!

fn foo<F: Fn(int) => int>(f: F): int => f(10);

fn identity(x: int): int => x;

fn minus_5(x: int): int => x - 5;

fn main() {
    let v = 5;
    assert_eq(foo(|=v, x| => x + v), 15);
    assert_eq(foo(&identity), 10);
    assert_eq(foo(minus_5), 5);

    println("PASS!");
}
