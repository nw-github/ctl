pub trait Write {
    fn write(mut this, data: [u8..]): ?uint;

    fn write_str(mut this, data: str): ?uint {
        this.write(data.as_bytes())
    }
}

#(lang(formatter))
pub trait Formatter: Write {
    fn written(this): str;
}

#(lang(format))
pub trait Format {
    fn fmt<F: Formatter>(this, f: *mut F);
}
