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

    assert(map.insert("e", "eternal") is null, "insertion of eternal returned an item");
    assert(map.insert("d", "dragonfruit") is null, "insertion of dragonfruit returned an item");
    assert(map.insert("c", "cat") is null, "insertion of cat returned an item");
    assert(map.insert("b", "banana") is null, "insertion of banana returned an item");
    assert(map.insert("a", "apple") is null, "insertion of apple returned an item");
    assert(map.len() == 5, "map len was not 5");

    assert(map.remove(&"c")!.eq(&"cat"), "removing 'c' did not return 'cat'");
    assert(map.len() == 4, "map len was not 4 after remove");

    assert(map.insert("a", "asthma")!.eq(&"apple"), "inserting 'asthma' did not return 'apple'");
    assert(map.len() == 4, "inserting identical key changed the map len");
}

pub fn set() {
    mut set: {str} = #[];
    assert(set.len() == 0, "set len was not 0 at start");

    assert(set.insert("jersey"), "insertion of jersey failed");
    assert(set.insert("intermediate"), "insertion of intermediate failed");
    assert(set.insert("history"), "insertion of history failed");
    assert(set.insert("germany"), "insertion of germany failed");
    assert(set.insert("farce"), "insertion of farce failed");

    assert(set.len() == 5, "set len was not 5");

    set.clear();
    assert(set.len() == 0, "set len was not 0 after clear");

    assert(set.insert("eternal"), "insertion of eternal failed");
    assert(set.insert("dragonfruit"), "insertion of dragonfruit failed");
    assert(set.insert("cat"), "insertion of cat failed");
    assert(set.insert("banana"), "insertion of banana failed");
    assert(set.insert("apple"), "insertion of apple failed");
    assert(set.len() == 5, "set len was not 5 after inserts");

    assert(set.remove(&"cat"), "removing 'cat' returned false");
    assert(set.len() == 4, "set len was not 4 after remove");

    assert(!set.insert("apple"), "inserting 'apple' did not return false");
    assert(set.len() == 4, "replacing entry changed the set len");
}
