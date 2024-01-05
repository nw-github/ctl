struct Hello<T, U> {}

// should say: expected 2 type params
fn foo(x: Hello) {}
