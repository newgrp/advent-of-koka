// Tuple methods.

// Converts a function of two arguments to a function of one pair.
fun by-tuple(f : (a, b) -> e t) : (((a, b)) -> e t)
  fn((a, b)) f(a, b)

// Maps the first element of a pair.
fun map1((a, b) : (a, b), f : a -> e t) : e (t, b)
  (f(a), b)

// Maps the second element of a pair.
fun map2((a, b) : (a, b), g : b -> e u) : e (a, u)
  (a, g(b))

// Maps each element of a pair.
fun bimap((a, b) : (a, b), f : a -> e t, g : b -> e u) : e (t, u)
  (f(a), g(b))
