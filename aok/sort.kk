fun merge(
  lhs : list<a>,
  rhs : list<a>,
  cmp : (a, a) -> order,
  dedup : bool
) : div list<a>
  match (lhs, rhs)
    (l, Nil) -> l
    (Nil, r) -> r
    (Cons(l, ls), Cons(r, rs)) ->
      match cmp(l, r)
        Lt -> Cons(l, merge(ls, rhs, cmp, dedup))
        Eq -> if dedup
          then Cons(l, merge(ls, rs, cmp, dedup))
          else Cons(l, merge(ls, rhs, cmp, dedup))
        Gt -> Cons(r, merge(lhs, rs, cmp, dedup))

fun sort-internal(
  l : list<a>,
  cmp : (a, a) -> order,
  dedup : bool
) : div list<a>
  match l
    Nil -> Nil
    Cons(x, Nil) -> [x]
    _ ->
      val (lhs, rhs) = l.split(l.length / 2)
      merge(
        sort-internal(lhs, cmp, dedup),
        sort-internal(rhs, cmp, dedup),
        cmp,
        dedup
      )

// Merge-sorts a list by the given comparator.
pub fun sort-by(l : list<a>, cmp : (a, a) -> order) : list<a>
  unsafe-no-div(fn() sort-internal(l, cmp, False))

// Merge-sorts a list of characters.
pub fun sort(l : list<char>) : list<char>
  sort-by(l, compare)

// Merge-sorts a list of integers.
pub fun sort(l : list<int>) : list<int>
  sort-by(l, compare)

// Merge-sorts a list of strings.
pub fun sort(l : list<string>) : list<string>
  sort-by(l, compare)

// Merge-sorts and deduplicates a list using the given comparator.
//
// When equal elements are found, only the first element is kept.
pub fun sort-unique-by(l : list<a>, cmp : (a, a) -> order) : list<a>
  unsafe-no-div(fn() sort-internal(l, cmp, True))

// Merge-sorts and deduplicates a list of characters.
pub fun sort-unique(l : list<char>) : list<char>
  sort-unique-by(l, compare)

// Merge-sorts and deduplicates a list of integers.
pub fun sort-unique(l : list<int>) : list<int>
  sort-unique-by(l, compare)

// Merge-sorts and deduplicates a list of strings.
pub fun sort-unique(l : list<string>) : list<string>
  sort-unique-by(l, compare)
