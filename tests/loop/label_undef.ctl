// Error: undefined label 'inner'
// Error: undefined label 'outer'
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
