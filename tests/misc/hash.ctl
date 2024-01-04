use super::assert;

pub fn map() {
    mut map: [str: str] = [:];
    assert(map.len() == 0, "map len was not 0 at start");

    map = [
        "j": "jersey",
        "i": "intermediate",
        "h": "history",
        "g": "gemstone",
        "f": "farce",
    ];
    assert(map.len() == 5, "map len was not 5");

    map.clear();
    assert(map.len() == 0, "map len was not 0 after clear");

    map.insert("e", "eternal");
    map.insert("d", "dragonfruit");
    map.insert("c", "cat");
    map.insert("b", "banana");
    map.insert("a", "apple");
    assert(map.len() == 5, "map len was not 5");

    assert(map.remove(&"c")!.eq(&"cat"), "removing 'c' did not return 'cat'");
    assert(map.len() == 4, "map len was not 4 after remove");

    assert(map.insert("a", "asthma")!.eq(&"apple"), "inserting 'asthma' did not return 'apple'");
    assert(map.len() == 4, "inserting identical key changed the map len");
}

pub fn set() {
    mut set: {str} = #[];
    assert(set.len() == 0, "set len was not 0 at start");

    set.insert("jersey");
    set.insert("intermediate");
    set.insert("history");
    set.insert("germany");
    set.insert("farce");
    assert(set.len() == 5, "set len was not 5");

    set.clear();
    assert(set.len() == 0, "set len was not 0 after clear");

    set.insert("eternal");
    set.insert("dragonfruit");
    set.insert("cat");
    set.insert("banana");
    set.insert("apple");
    assert(set.len() == 5, "set len was not 5");

    assert(set.remove(&"cat"), "removing 'cat' returned false");
    assert(set.len() == 4, "set len was not 4 after remove");

    assert(set.insert("apple"), "inserting 'apple' did not return true");
    assert(set.len() == 4, "replacing entry changed the set len");
}
