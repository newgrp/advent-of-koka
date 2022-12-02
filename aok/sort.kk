fun merge(lhs : list<a>, rhs : list<a>, cmp : (a, a) -> order) : div list<a>
  match (lhs, rhs)
    (l, Nil) -> l
    (Nil, r) -> r
    (Cons(l, ls), Cons(r, rs)) ->
      if !cmp(l, r).is-gt
      then
        Cons(l, merge(ls, rhs, cmp))
      else
        Cons(r, merge(lhs, rs, cmp))

fun sort-internal(l : list<a>, cmp : (a, a) -> order) : div list<a>
  match l
    Nil -> Nil
    Cons(x, Nil) -> [x]
    _ ->
      val (lhs, rhs) = l.split(l.length / 2)
      merge(sort-internal(lhs, cmp), sort-internal(rhs, cmp), cmp)

// Merge-sorts a list by the given comparator.
pub fun sort-by(l : list<a>, cmp : (a, a) -> order) : list<a>
  unsafe-no-div(fn() sort-internal(l, cmp))

// Merge-sorts a list of characters.
pub fun sort(l : list<char>) : list<char>
  sort-by(l, compare)

// Merge-sorts a list of integers.
pub fun sort(l : list<int>) : list<int>
  sort-by(l, compare)

// Merge-sorts a list of strings.
pub fun sort(l : list<string>) : list<string>
  sort-by(l, compare)
