fn bad_inference() {
    // cant just make T = unknown, because that would break the type check
    // cant just make all templates ignorable, because that would break code like this
    /*
        extern fn hello<T>(t: T): T;

        fn test<T>(t: T) {
            let x: T = hello(10); // target = T, if all templates are malleable this would pass typechecking
        }
        */
    fn test<T, U>(t: ?T, u: ?*mut U) {}

    test(5, &mut 6);
    test(Some(5), Some(&mut 6));
}
