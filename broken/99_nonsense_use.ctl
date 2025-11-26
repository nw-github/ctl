fn main() {
    // This happens due to src/typechecker.rs:4843 (check_block). Because we declare the statements
    // inside a block right before typechecking and not in the forward declaration pass, the use
    // initially fails to resolve, gets put in use_stmts, but is never checked because the enter_id_and_resolve()
    // call has already happened
    use nonsense;
}
