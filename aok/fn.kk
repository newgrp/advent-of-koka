// Function composition.
pub fun (*)(f : b -> e c, g : () -> e b) : (() -> e c)
  fn() f(g())

// Function composition.
pub fun (*)(f : b -> e c, g : a -> e b) : (a -> e c)
  fn(x) f(g(x))
