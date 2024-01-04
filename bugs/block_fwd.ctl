fn main() {
    {
        // blocks are not processed in the forward-declaration step, so this fails
        test();
        fn test() {}
    }
}
