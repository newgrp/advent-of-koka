// Additional parsers.

import std/text/parse

// Runs the given action `n` times, but recurses only to `O(log(n))` depth.
fun repeat-log2(n : int, f : () -> e ()) : e ()
  if n > 0 then
    f()
    val m = n / 2
    repeat-log2(m.unsafe-decreasing, f)
    repeat-log2((n - m - 1).unsafe-decreasing, f)

// Parses a literal string.
pub fun lit(s : string) : parse ()
  s.list.map(char).ignore

// As `many`, but less recursive. This helps prevent stack overflows on large
// inputs.
// pub fun many-safer(p : parser<e,a>) : <parse|e> list<a>
  

  // val cont = ref(True)
  // val lst = ref([])
  // repeat-log2(current-input().count) {
  //   cont.modify(fn(c) c && pick())
  //   if !cont then
  //     val a = p()
  //     unsafe-no-div { lst.modify(fn(l) Cons(a, l)).ignore }
  // }
  // (!lst).reverse

  // // Use `vector-init` because it is built on non-recursive C code.
  // vector-init(
  //   // Use `current-input()` to bound the operation.
  //   current-input().string.list.length,
  //   fn(_)
  //     cont := (cont && pick())
  //     if cont then Just(p()) else Nothing
  // ).list.concat-maybe

  // // Use `current-input()` to bound the operation.
  // current-input().string.list.take-while(fn(_) pick()).map(fn(_) p())

// Returns a parser for a list of `p` separated by `s`. The `s` parse results
// are dropped. Requires at least one successful `p` parse.
pub fun separated(p : parser<e, a>, s : parser<e, b>) : <parse|e> list<a>
  Cons(p(), many(fn()
    s()
    p()
  ))
