// More list methods.

fun windows-internal(xs : list<a>, size : int) : div list<list<a>>
  match compare(xs.length, size)
    Lt -> []
    Eq -> [xs]
    Gt -> Cons(xs.take(size), xs.drop(1).windows-internal(size))

// A sliding window of the given size on the original list. Slides by one
// element at a time.
pub fun windows(xs : list<a>, size : int) : list<list<a>>
  unsafe-no-div(fn() xs.windows-internal(size))
