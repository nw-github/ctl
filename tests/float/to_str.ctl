// Output: inf -inf NaN NaN
// Output: inf -inf NaN NaN
// Output: true
// Output: true

fn main() {
    println("{f32::inf()} {f32::neg_inf()} {f32::nan()} {f32::nan().copysign(-1.0)}");
    println("{f64::inf()} {f64::neg_inf()} {f64::nan()} {f64::nan().copysign(-1.0)}");

    let nan = f32::from_bits(f32::nan().to_bits() & !(1 << 31));
    println("{nan.to_bits() != nan.copysign(-1.0).to_bits()}");

    let nan = f64::from_bits(f64::nan().to_bits() & !(1 << 63));
    println("{nan.to_bits() != nan.copysign(-1.0).to_bits()}");
}
