pub trait Write {
    fn write(mut this, data: [u8..]): ?uint;
}

#(lang(formatter))
pub trait Formatter: Write {
    fn written(this): core::string::str;
}

#(lang(format))
pub trait Format {
    fn format<F: Formatter>(this, f: *mut F);
}
