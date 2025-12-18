use std::fmt::Debug;
use std::fmt::Formatter;

pub extension VecDebug<T: Debug> for [T] {
    impl Debug {
        fn dbg(this, f: *mut Formatter) => this[..].dbg(f);
    }
}

pub extension MapFormat<K: Debug, V: Debug> for [K: V] {
    impl Debug {
        fn dbg(this, f: *mut Formatter) {
            f.write_str("[");
            for (i, (key, value)) in this.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ");
                }
                write(f, "{key:?}: {value:?}");
            }
            f.write_str("]");
        }
    }
}

pub extension SetDebug<T: Debug> for #[T] {
    impl Debug {
        fn dbg(this, f: *mut Formatter) {
            f.write_str("\{");
            for (i, item) in this.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ");
                }
                write(f, "{item:?}");
            }
            f.write_str("\}");
        }
    }
}
