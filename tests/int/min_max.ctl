// Output: -2147483648 2147483647 4294967295
// Output: -9223372036854775808 9223372036854775807 18446744073709551615
// Output: -18446744073709551616 18446744073709551615 36893488147419103231
// Output: -18446744073709551616 18446744073709551615 36893488147419103231

fn main() {
    println("{i32::min_value()} {i32::max_value()} {u32::max_value()}");
    println("{i64::min_value()} {i64::max_value()} {u64::max_value()}");
    println("{i65::min_value()} {i65::max_value()} {u65::max_value()}");

    mut foo = -0x1_0000_0000_0000_0000i65;
    mut bar =  0x0_ffff_ffff_ffff_ffffi65;
    mut baz =  0x1_ffff_ffff_ffff_ffffu65;
    println("{foo} {bar} {baz}");
}
