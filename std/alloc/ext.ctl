use std::fmt::Format;
use std::fmt::Formatter;

pub extension VecFormat<T: Format> for [T] {
    impl Format {
        fn fmt(this, f: *mut Formatter) {
            this[..].fmt(f);
        }
    }
}

pub extension MapFormat<K: Format, V: Format> for [K: V] {
    impl Format {
        fn fmt(this, f: *mut Formatter) {
            "[".fmt(f);
            for (i, (key, value)) in this.iter().enumerate() {
                if i > 0 {
                    ", ".fmt(f);
                }
                "{key}: {value}".fmt(f);
            }
            "]".fmt(f);
        }
    }
}

pub extension SetFormat<T: Format> for #[T] {
    impl Format {
        fn fmt(this, f: *mut Formatter) {
            "\{".fmt(f);
            for (i, item) in this.iter().enumerate() {
                if i > 0 {
                    ", ".fmt(f);
                }
                item.fmt(f);
            }
            "\}".fmt(f);
        }
    }
}
