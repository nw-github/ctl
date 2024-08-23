// Error: no symbol 'item' found in this module

fn main() {
    let x: ?i32 = -10;
    guard x is ?item and item.abs() == 10 else {
        let _ = item;
        return;
    }
}
