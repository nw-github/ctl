// Output: [72, 105, 32, 116, 104, 101, 114, 101, 33, 32, 55296, 56320]
// Output: code point: 0x7f
// Output: utf8:       [127]
// Output: utf16:      (127, null)
// Output: code point: 0x7ff
// Output: utf8:       [223, 191]
// Output: utf16:      (2047, null)
// Output: code point: 0xffff
// Output: utf8:       [239, 191, 191]
// Output: utf16:      (65535, null)
// Output: code point: 0x10ffff
// Output: utf8:       [244, 143, 191, 191]
// Output: utf16:      (56319, Some(57343))

fn main() {
    let foo = "Hi there! 𐀀";
    let utf16: [u16] = foo.utf16().collect();
    println("{utf16:?}");

    for ch in "\u{7f}\u{7ff}\u{ffff}\u{10ffff}".chars() {
        print_stuff(ch);
    }
}

fn print_stuff(data: char) {
    println("code point: {data as u32:#x}");
    println("utf8:       {data.encode_utf8(&mut [0u8; 4]).as_bytes():?}");
    println("utf16:      {data.encode_utf16():?}");
}
