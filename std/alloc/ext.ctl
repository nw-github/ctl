use std::fmt::Format;
use std::fmt::Formatter;

@(feature(alloc))
pub extension VecFormat<T: Format> for [T] {
    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            this[..].fmt(f);
        }
    }
}

@(feature(alloc))
pub extension MapFormat<K: Format, V: Format> for [K: V] {
    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            "[".fmt(f);
            for (i, (key, value)) in this.iter().enumerate() {
                if i > 0 {
                    ", ".fmt(f);
                }

                key.fmt(f);
                ": ".fmt(f);
                value.fmt(f);
            }
            "]".fmt(f);
        }
    }
}

@(feature(alloc))
pub extension SetFormat<T: Format> for #[T] {
    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
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
