struct Hello<T, U> {
    fn something(): This {
        This() // should work
    }
}

fn quux<T>() {
    // should error out
    fn bar(t: T) {}
}
