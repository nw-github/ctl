// Output: -2147483648 4294967295
// Output: -9223372036854775808 18446744073709551615
// Output: -18446744073709551616 36893488147419103231
// Output: -18446744073709551616 36893488147419103231

fn main() {
    println("{i32::min_value()} {u32::max_value()}");
    println("{i64::min_value()} {u64::max_value()}");
    println("{i65::min_value()} {u65::max_value()}");

    mut foo = -0x1_0000_0000_0000_0000i65;
    mut bar =  0x1_ffff_ffff_ffff_ffffu65;
    println("{foo} {bar}");
}
