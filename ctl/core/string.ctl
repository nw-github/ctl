pub struct str {
    data: *u8,
    len:   usize,

    pub fn len(this) usize {
        return this.len;
    }

    pub fn as_ptr(this) *u8 {
        return this.data;
    }

    pub fn slice(this, kw start: usize, kw end: usize) str {
        if end < start || start > this.len {
            return "";
        }

        return str(
            data: (this.data as usize + start) as *u8,
            len: end - start
        );
    }
}
