// should complain that a value of ?i32 is not returned from all paths
fn foo(): ?i32 {
    if false {
        return null;
    }

    println();
}
