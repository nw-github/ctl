// Output: arr2[0] (ref): [5, 6]
// Output: arr2[0] (val): [5, 6]
// Output: arr2: [[5, 6], [3, 4]]
// Output: arr3: [[[5, 6], [3, 4]]]
// Output: arr3: [[[7, 8], [9, 0]]]
// Output: arr2: [[5, 6], [3, 4]]

fn main() {
    mut arr2 = [[1, 2], [3, 4]];
    arr2[0] = [5, 6];

    let ref = &arr2[0];
    let copy = arr2[0];
    println("arr2[0] (ref): {ref:?}");
    println("arr2[0] (val): {copy:?}");
    println("arr2: {arr2:?}");

    mut arr3 = [arr2];
    println("arr3: {arr3:?}");

    arr3[0] = [[7, 8], [9, 0]];
    println("arr3: {arr3:?}");

    println("arr2: {arr2:?}");
}
