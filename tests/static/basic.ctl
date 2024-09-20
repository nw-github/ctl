// Output: 30

static OUTER: int = 10;

fn func(): int { OUTER }

fn main() {
    static INNER: int = 20;
    println("{func() + INNER}");
}
