// Additional parsers.

import std/text/parse

// Parses a literal string.
fun lit(s : string) : parse ()
  s.list.map(char).ignore

// Returns a parser for a list of `p` separated by `s`. The `s` parse results
// are dropped. Requires at least one successful `p` parse.
fun separated(p : parser<e, a>, s : parser<e, b>) : <parse|e> list<a>
  Cons(p(), many(fn()
    s()
    p()
  ))
