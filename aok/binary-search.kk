// Binary search.
//
// Functions in this file return `either<(int, a), int>`. If an exact match is
// found, they return `Left(i, x)` where `i` is the index of the match and `x`
// is the matched element. If no exact match is found, they return `Right(j)`
// where `j` is an index that the element could be inserted at while preserving
// the list order.
//
// In the case of multiple exact matches, an arbitrary one is returned.

import std/num/int32
import std/num/int64

// Searches a list for the given element.
//
// The input list must be sorted.
pub fun binary-search(xs : list<int>, x : int) : either<(int, int), int>
  xs.binary-search-for(x, compare)

// Searches a list for the given element.
//
// The input list must be sorted.
pub fun binary-search(xs : list<char>, x : char) : either<(int, char), int>
  xs.binary-search-for(x, compare)

// Searches a list for the given element.
//
// The input list must be sorted.
pub fun binary-search(xs : list<string>, x : string) : either<(int, string), int>
  xs.binary-search-for(x, compare)

// Searches a list for the given element.
//
// The input list must be sorted.
pub fun binary-search(xs : list<int32>, x : int32) : either<(int, int32), int>
  xs.binary-search-for(x, compare)

// Searches a list for the given element.
//
// The input list must be sorted.
pub fun binary-search(xs : list<int64>, x : int64) : either<(int, int64), int>
  xs.binary-search-for(x, compare)

// Searches a list for an element matching the given hint.
//
// The input list must be sorted according to the hint, i.e. in `xs.map(hint)`
// all `Lt`s must appear before all `Eq`s, which must appear before all `Gt`s.
pub fun binary-search-by(xs : list<a>, hint : a -> order) : either<(int, a), int>
  match xs
    [] -> Right(0)
    [x] -> match hint(x)
      Lt -> Right(0)
      Eq -> Left((0, x))
      Gt -> Right(1)
    _ ->
      val i = xs.length / 2
      val (left, right') = xs.split(i)
      match right'
        // This branch should be unreachable.
        Nil -> left.unsafe-decreasing.binary-search-by(hint)
        Cons(x, right) -> match hint(x)
          Lt -> left.unsafe-decreasing.binary-search-by(hint)
          Eq -> Left((i, x))
          Gt -> match right.unsafe-decreasing.binary-search-by(hint)
            Left((j, y)) -> Left((i + j + 1, y))
            Right(j) -> Right(i + j + 1)

// Searches a list for an element using the given comparator.
//
// The input list must be sorted according to the comparator.
pub fun binary-search-for(
  xs : list<a>,
  x : a,
  cmp : (a, a) -> order
) : either<(int, a), int>
  xs.binary-search-by(fn(y) cmp(x, y))
