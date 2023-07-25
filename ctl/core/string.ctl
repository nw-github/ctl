use core::span::Span;

pub struct str {
    slc: [u8..],

    pub fn from_c_str(ptr: *c_char) str {
        extern fn strlen(ptr: *c_char) usize;

        return str(slc: Span::new(ptr as *u8, strlen(ptr)));
    }

    pub fn len(this) usize {
        return this.slc.len();
    }

    pub fn is_empty(this) bool {
        return this.slc.is_empty();
    }

    pub fn as_ptr(this) *u8 {
        return this.slc.as_raw().as_ptr();
    }

    pub fn as_c_str(this) *c_char {
        return this.slc.as_raw().as_ptr() as *c_char;
    }

//     pub fn slice<R: RangeBounds<usize> >(this, range: R) str {
//         let start = match range.begin() {
//             Bound::Inclusive(start) => start,
//             Bound::Exclusive(start) => start + 1,
//             Bound::Unbounded => 0,
//         };
// 
//         let end = match range.end() {
//             Bound::Inclusive(end) => end + 1,
//             Bound::Exclusive(end) => end,
//             Bound::Unbounded => this.len(),
//         };
// 
//         if end < start || start > this.len || end > this.len {
//             core::panic("str::slice(): invalid range!");
//         }
// 
//         return str(
//             ptr: (this.ptr as usize + start) as *u8,
//             len: end - start
//         );
//     }
}
