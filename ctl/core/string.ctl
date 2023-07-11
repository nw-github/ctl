pub struct str {
    data: *u8,
    len:   usize,

    pub fn len(this) usize {
        return this.len;
    }

    pub fn as_ptr(this) *u8 {
        return this.data;
    }
}
