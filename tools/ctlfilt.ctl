extern {
    fn read(fd: c_int, buf: ^mut void, count: uint): int;
    safe fn isatty(fd: c_int): c_int;
}

const STDIN: c_int = 0;
static DEMANGLE_TYPES: bool = isatty(STDIN) == 1;

fn read_stdin(buffer: [mut u8..]): ?[u8..] {
    let len = unsafe read(STDIN, buffer.as_raw_mut().cast(), buffer.len());
    if len > 0 {
        buffer[..uint::from(len)]
    }
}

fn write_stdout(buffer: [u8..]) {
    // TODO: this is UB but since it directly goes to write(), we avoid any of the UTF8 functions
    // that assume the contents of `str` are UTF8 encoded. Fix this when we add more I/O to the stdlib
    std::io::Stdout().write_str(unsafe str::from_utf8_unchecked(buffer))
}

fn demangle(data: [u8..]) {
    if std::bt::demangle_name(data) is ?demangled {
        print(demangled);
    } else if DEMANGLE_TYPES and std::bt::demangle_type_name(data) is ?demangled {
        print(demangled);
    } else {
        write_stdout(data);
    }
}

fn main() {
    mut buffer = [0u8; 4096];
    mut word: [u8] = @[];

    defer demangle(word[..]);
    while read_stdin(buffer[..]) is ?mut data_to_scan {
        @find_next: loop {
            if word.is_empty() {
                let begin = if DEMANGLE_TYPES {
                    data_to_scan.iter().position(|b| b.is_ascii_alphabetic())
                } else {
                    data_to_scan.find(b"C"[..])
                };
                if begin is ?begin {
                    write_stdout(data_to_scan[..begin]);
                    data_to_scan = data_to_scan[begin..];
                } else {
                    write_stdout(data_to_scan);
                    break;
                }
            }

            for (i, byte) in data_to_scan.iter().enumerate() {
                if !(byte.is_ascii_alphanumeric() or byte is b'_' | b'$') {
                    word.extend(data_to_scan[..i].iter());
                    demangle(word[..]);
                    word.clear();

                    data_to_scan = data_to_scan[i..];
                    continue @find_next;
                }
            }

            word.extend(data_to_scan.iter());
            break;
        }
    }
}
