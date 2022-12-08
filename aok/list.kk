// More list methods.

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
fun find-split(xs : list<a>, pred : a -> bool) : maybe<(list<a>, a, list<a>)>
  unsafe-no-div(fn() xs.find-split-internal(pred))

fun windows-internal(xs : list<a>, size : int) : div list<list<a>>
  match compare(xs.length, size)
    Lt -> []
    Eq -> [xs]
    Gt -> Cons(xs.take(size), xs.drop(1).windows-internal(size))

// A sliding window of the given size on the original list. Slides by one
// element at a time.
pub fun windows(xs : list<a>, size : int) : list<list<a>>
  unsafe-no-div(fn() xs.windows-internal(size))
