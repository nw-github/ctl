use super::assert;

pub fn booleans() {
    let x = 5 == 2;
    assert(x == false, "5 == 2 should be false");
    assert((!x) == true, "! should negate");
    assert((!!x) == false, "!! should be a no op");
}
