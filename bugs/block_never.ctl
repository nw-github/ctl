union Infallible {}

fn main() {
    // arguably not a bug, but different from what rust does. x gets type never here, meaning the
    // 10 tail expression is an error. rust just forces the block to be type i32 in that case
    // I think its sensible for this to be an error though, as code like this never makes any sense
    let x = {
        let x = unreachable();
        let x: Infallible = x;
        10
    };
}
