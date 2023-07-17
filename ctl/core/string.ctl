pub struct str {
    ptr: *u8,
    len:  usize,

    pub fn len(this) usize {
        return this.len;
    }

    pub fn as_ptr(this) *u8 {
        return this.ptr;
    }

    pub fn as_c_str(this) *c_char {
        return this.ptr as *c_char;
    }

    pub fn slice(this, kw start: usize, kw end: usize) str {
        if end < start || start > this.len {
            return "";
        }

        return str(
            ptr: (this.ptr as usize + start) as *u8,
            len: end - start
        );
    }
}
