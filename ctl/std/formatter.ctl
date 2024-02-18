use core::fmt::*;

#(lang(string_formatter))
struct StringFormatter {
    buffer: [u8] = @[],

    pub fn new(): This {
        StringFormatter()
    }

    impl Write {
        fn write(mut this, data: [u8..]): ?uint {
            // TODO: utf8 validation
            if data.is_empty() {
                return null;
            }

            let len = data.len();
            let new_len = this.buffer.len() + len;
            this.buffer.reserve(new_len);
            unsafe {
                core::mem::copy(
                    dst: this.buffer.as_span().subspan(this.buffer.len()..).as_raw(),
                    src: data.as_raw(),
                    num: len,
                );
                this.buffer.set_len(new_len);
                len
            }
        }
    }

    impl Formatter {
        fn written(this): str {
            unsafe str::from_utf8_unchecked(this.buffer.as_span())
        }
    }
}
