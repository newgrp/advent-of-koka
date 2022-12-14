// More list methods.

// Splits a list into chunks of the given size. The last chunk may contain fewer
// elements if the length of the list is not divisible by `size`.
pub fun chunks(xs : list<a>, size : int) : list<list<a>>
  match xs
    Nil -> Nil
    _ -> Cons(xs.take(size), xs.drop(size).unsafe-decreasing.chunks(size))

// Splits a list into chunks of two as pairs. Omits the last element if the list
// has odd length.
pub fun chunk-pairs(xs : list<a>) : list<(a, a)>
  zip(xs, xs.drop(1)).step-by(2)

// Splits a list into chunks of three as triples. Omits the last element or two
// if the list's length is not divisible by three.
pub fun chunk-triples(xs : list<a>) : list<(a, a, a)>
  zip(xs, zip(xs.drop(1), xs.drop(2))).step-by(3).map(fn((a, (b, c))) (a, b, c))

// Splits a list into chunks of four as quadruples. Omits elements off the end
// if the list's length is not divisible by four.
pub fun chunk-quadruples(xs : list<a>) : list<(a, a, a, a)>
  zip(xs, zip(xs.drop(1), zip(xs.drop(2), xs.drop(3)))).step-by(4).map(
    fn((a, (b, (c, d)))) (a, b, c, d)
  )

// Splits a list into chunks of five as quintuples. Omits elements off the end
// if the list's length is not divisible by five.
pub fun chunk-quintruples(xs : list<a>) : list<(a, a, a, a, a)>
  zip(xs, zip(xs.drop(1), zip(xs.drop(2), zip(xs.drop(3), xs.drop(4)))))
    .step-by(5)
    .map(fn((a, (b, (c, (d, e))))) (a, b, c, d, e))

// Pairs each element of the list with its index.
pub fun enumerate(xs : list<a>) : list<(int, a)>
  xs.map-indexed(fn(i, a) (i, a))

// Finds the first element matching a predicate, splitting the list on it.
pub fun find-split(
  xs : list<a>,
  pred : a -> bool
) : div maybe<(list<a>, a, list<a>)>
  match xs
    Cons(x, xx) -> if pred(x)
      then Just(([], x, xx))
      else match find-split(xx.unsafe-decreasing, pred)
        Just((l, a, r)) -> Just((Cons(x, l), a, r))
        Nothing -> Nothing
    Nil -> Nothing

// As `foldl`, but accumulates the list of state values at each stage. The
// returned list includes the initial value.
pub fun scanl(xs : list<a>, init : b, f : (b, a) -> e b) : e list<b>
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

// A sliding window of the given size on the original list. Slides by one
// element at a time.
fun windows(xs : list<a>, size : int) : div list<list<a>>
  match compare(xs.length, size)
    Lt -> []
    Eq -> [xs]
    Gt -> Cons(xs.take(size), xs.drop(1).unsafe-decreasing.windows(size))
