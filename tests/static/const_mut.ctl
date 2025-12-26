// Output: Pass!

fn main() {
    const FOO: i32 = 3;
    let x = &mut FOO;
    let y = &mut FOO;
    *x = 1;
    *y = 2;

    assert_eq(*x, 1);
    assert_eq(*y, 2);
    assert_eq(FOO, 3);

    println("Pass!");
}
