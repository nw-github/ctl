use core::fmt::*;

#(lang(string_formatter))
struct StringFormatter {
    buffer: [u8] = @[],

    pub fn new(): This {
        StringFormatter()
    }

    impl Write {
        fn write(mut this, data: [u8..]): ?uint {
            this.write_str(str::from_utf8(data)?)
        }

        fn write_str(mut this, data: str): ?uint {
            if data.is_empty() {
                return 0;
            }

            unsafe {
                this.write_unchecked(data.as_bytes())
            }
        }
    }

    impl Formatter {
        fn written(this): str {
            unsafe str::from_utf8_unchecked(this.buffer[..])
        }
    }

    unsafe fn write_unchecked(mut this, data: [u8..]): ?uint {
        let len = data.len();
        let new_len = this.buffer.len() + len;
        this.buffer.reserve(new_len);
        unsafe {
            core::mem::copy(
                dst: this.buffer.as_raw() + this.buffer.len(),
                src: data.as_raw(),
                num: len,
            );
            this.buffer.set_len(new_len);
            len
        }
    }
}
