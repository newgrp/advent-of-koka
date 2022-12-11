// Computes the greatest common divisor of two integers. Always returns a
// non-negative integer.
pub fun gcd(a : int, b : int) : int
  match (a, b)
    (_, 0) -> abs(a)
    (0, _) -> abs(b)
    _ -> gcd(b, (a % b).unsafe-decreasing)

// Computes the least common multimple of two integers. Always returns a
// non-negative integer.
pub fun lcm(a : int, b : int) : int
  abs(a * b / gcd(a, b))
