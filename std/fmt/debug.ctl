use super::{Formatter, Format, Pad, Options};

extension Formatter {
    fn use_indentation(this): bool => this.options().alt;

    fn indent(mut this) {
        if this.use_indentation() {
            Pad(fill: ' ', width: this.indent * 4).fmt(this);
        }
    }

    pub fn dbg_list<R, F: Fn(*mut List) => R>(
        mut this,
        f: F,
        kw begin: str = "[",
        kw end: str = "]",
    ): R {
        mut list = List::begin(this, begin);
        defer list.end(end);
        f(&mut list)
    }
}

$[lang(debug_list)]
pub struct List {
    fmt: Formatter,
    items: bool = false,

    fn begin(fmt: *mut Formatter, begin: str): This {
        mut fmt = fmt.with_options(Options(alt: fmt.opts.alt));
        begin.fmt(&mut fmt);
        fmt.indent++;
        This(fmt:)
    }

    fn end(mut this, end: str) {
        if this.fmt.use_indentation() and this.items {
            this.fmt.write_str("\n");
        }

        this.fmt.indent--;
        this.fmt.indent();
        end.fmt(&mut this.fmt);
    }

    pub fn item<R, F: Fn() => R>(mut this, f: F): R {
        if this.fmt.use_indentation() {
            this.fmt.write_str("\n");
            this.fmt.indent();
        } else if this.items {
            this.fmt.write_str(", ");
        }

        defer {
            if this.fmt.use_indentation() {
                this.fmt.write_char(',');
            }
        }

        this.items = true;
        f()
    }

    // Used by the compiler for derived Debug impl
    pub fn named<V>(mut this, k: str, v: *V) {
        this.item(|=this, =k, =v| {
            this.fmt.write_str(k);
            this.fmt.write_str(": ");
            v.dbg(&mut this.fmt);
        })
    }

    pub fn keyed<K, V>(mut this, k: *K, v: *V) {
        this.item(|=this, =k, =v| {
            k.dbg(&mut this.fmt);
            this.fmt.write_str(": ");
            v.dbg(&mut this.fmt);
        })
    }

    pub fn value<V>(mut this, v: *V) {
        this.item(|=this, =v| v.dbg(&mut this.fmt))
    }
}
