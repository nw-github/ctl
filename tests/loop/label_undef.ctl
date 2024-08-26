// Error: cannot find loop with label inner
// Error: cannot find loop with label outer
// Error: break outside of loop

fn main() {
    @outer: loop {
        break @inner;

        defer {
            break @outer;
        }
    }

    break;
}
