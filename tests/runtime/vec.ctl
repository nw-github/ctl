use super::assert;

pub fn init1() {
    mut x = @[1, 2, 3];
    assert(x.len() == 3, "length was not 3 with initializer [1, 2, 3]");

    assert(x.pop()! == 3, "element 2 was not 3");
    assert(x.pop()! == 2, "element 1 was not 2");
    assert(x.pop()! == 1, "element 0 was not 1");
}

pub fn init2() {
    mut x = @[0xff; 5];
    assert(x.len() == 5, "length was not 5 with initializer [0xff; 5]");

    assert(x.pop()! == 0xff, "element 4 was not 0xff");
    assert(x.pop()! == 0xff, "element 3 was not 0xff");
    assert(x.pop()! == 0xff, "element 2 was not 0xff");
    assert(x.pop()! == 0xff, "element 1 was not 0xff");
    assert(x.pop()! == 0xff, "element 0 was not 0xff");
}

pub fn push_pop() {
    mut x: [int] = @[];
    x.push(1);
    x.push(2);
    x.push(3);

    assert(x.len() == 3, "length was not 3 after 3 push()s");
    assert(x.pop()! == 3, "element 2 was not 3");
    assert(x.pop()! == 2, "element 1 was not 2");
    assert(x.pop()! == 1, "element 0 was not 1");
}

pub fn insert() {
    mut x = @[1, 2, 3];

    x.insert(idx: 1, 4);
    assert(x.len() == 4, "length was not 4 after 3 push()s + insert");

    assert(x.pop()! == 3, "element 3 was not 3");
    assert(x.pop()! == 2, "element 2 was not 2");
    assert(x.pop()! == 4, "element 1 was not 4");
    assert(x.pop()! == 1, "element 0 was not 1");
}

pub fn remove() {
    mut vec = @[1, 2, 3];

    assert(vec.remove(1) == 2, "index 1 was not 2");
    assert(vec.len() == 2, "length was not 2 after remove");

    assert(vec.pop()! == 3, "element 1 was not 3");
    assert(vec.pop()! == 1, "element 0 was not 1");
}

pub fn swap_remove() {
    mut vec = @[1, 2, 3, 4];

    assert(vec.swap_remove(1) == 2, "index 1 was not 2");
    assert(vec.len() == 3, "length was not 3 after swap_remove");
    assert(vec.pop()! == 3, "element 2 was not 3");
    assert(vec.pop()! == 4, "element 1 was not 4");
    assert(vec.pop()! == 1, "element 0 was not 1");
}
