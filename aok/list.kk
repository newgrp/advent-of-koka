// More list methods.

fun chunks-internal(xs : list<a>, size : int) : div list<list<a>>
  match xs
    Nil -> Nil
    _ -> Cons(xs.take(size), xs.drop(size).chunks-internal(size))

// Splits a list into chunks of the given size. The last chunk may contain fewer
// elements if the length of the list is not divisible by `size`.
pub fun chunks(xs : list<a>, size : int) : list<list<a>>
  unsafe-no-div(fn() xs.chunks-internal(size))

// Pairs each element of the list with its index.
pub fun enumerate(xs : list<a>) : list<(int, a)>
  xs.map-indexed(fn(i, a) (i, a))

fun find-split-internal(
  xs : list<a>,
  pred : a -> bool
) : div maybe<(list<a>, a, list<a>)>
  match xs
    Cons(x, xx) -> if pred(x)
      then Just(([], x, xx))
      else match find-split-internal(xx, pred)
        Just((l, a, r)) -> Just((Cons(x, l), a, r))
        Nothing -> Nothing
    Nil -> Nothing

// Finds the first element matching a predicate, splitting the list on it.
pub fun find-split(xs : list<a>, pred : a -> bool) : maybe<(list<a>, a, list<a>)>
  unsafe-no-div(fn() xs.find-split-internal(pred))

// As `foldl`, but accumulates the list of state values at each stage. The
// returned list includes the initial value.
pub fun foldl-accumulate(xs : list<a>, init : b, f : (b, a) -> e b) : e list<b>
  Cons(init, xs.foldl([], fn(acc, a)
    val b = match acc
      Nil -> init
      Cons(b, _) -> b
    Cons(f(b, a), acc)
  ).reverse)

// Steps through the original list `n` elements at a time, starting with the
// first element.
pub fun step-by(xs : list<a>, n : int) : list<a>
  match xs
    Nil -> Nil
    Cons(x, xx) -> Cons(x, xx.drop(n - 1).unsafe-decreasing.step-by(n))

fun windows-internal(xs : list<a>, size : int) : div list<list<a>>
  match compare(xs.length, size)
    Lt -> []
    Eq -> [xs]
    Gt -> Cons(xs.take(size), xs.drop(1).windows-internal(size))

// A sliding window of the given size on the original list. Slides by one
// element at a time.
pub fun windows(xs : list<a>, size : int) : list<list<a>>
  unsafe-no-div(fn() xs.windows-internal(size))
