use std::fmt::Write;

pub fn nearest_pow_of_two(bits: u32) -> usize {
    2usize.pow((bits as f64).log2().ceil() as u32).max(8)
}

pub fn join<T: Write, I: IntoIterator>(
    w: &mut T,
    sep: &str,
    iter: I,
    mut cb: impl FnMut(&mut T, I::Item) -> std::fmt::Result,
) -> std::fmt::Result {
    for (i, item) in iter.into_iter().enumerate() {
        if i != 0 {
            w.write_str(sep)?;
        }
        cb(w, item)?;
    }
    Ok(())
}
