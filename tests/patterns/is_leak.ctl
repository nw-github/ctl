// Error: no symbol 'item' found in this module
// Error: no symbol 'item2' found in this module
// Error: no symbol 'item3' found in this module
// Error: no symbol 'item4' found in this module
// Error: no symbol 'item4' found in this module
// Error: no symbol 'item5' found in this module
// Error: no symbol 'item5' found in this module

fn main() {
    let a = Some(10);
    if a is ?item {
        let _ = item;
    } else {
        let _ = item;
    }

    if a is ?item2 and item2 > 5 {
        let _ = item2;
    }
    let _ = item2;

    while a is ?item3 and item3 > 5 {
        let _ = item3;
    }
    let _ = item3;

    loop {
        let _ = item4;
    } while a is ?item4;
    let _ = item4;

    if a is ?item5 or item5 > 4 {
        let _ = item5;
    }
}
