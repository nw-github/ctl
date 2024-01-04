// doesn't complain A is not Hash + Eq<A>

struct A {}

fn foo(a: [A: u32]) {}

fn bar() {
    let x = [
        A(): 10,
        A(): 5,
    ];
}
