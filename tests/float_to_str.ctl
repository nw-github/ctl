// Output: inf -inf NaN NaN
// Output: inf -inf NaN NaN
// Output: true
// Output: true

fn main() {
    println("{f32::inf()} {f32::neg_inf()} {f32::nan()} {f32::nan().copysign(-1.0)}");
    println("{f64::inf()} {f64::neg_inf()} {f64::nan()} {f64::nan().copysign(-1.0)}");
    println("{f32::nan().to_bits() != f32::nan().copysign(-1.0).to_bits()}");
    println("{f64::nan().to_bits() != f64::nan().copysign(-1.0).to_bits()}");
}
