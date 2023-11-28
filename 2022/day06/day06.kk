import aok/list
import aok/main
import aok/sort
import std/text/parse

// Parses an input signal.
fun pinput() : parse string
  val buf = many(alpha)
  whitespace0()
  buf.string

// Returns true if all elements of a list are distinct.
fun all-distinct(xs : list<a>, cmp : (a, a) -> order) : bool
  val sorted = xs.sort-by(cmp)
  zip(sorted, sorted.drop(1)).all(fn((a, b)) cmp(a, b) != Eq)

// Finds the first index preceded by `n` distinct characters.
fun find-first-n-distinct(buf : string, n : int) : maybe<int>
  val idx = buf.list.windows(n).index-of(fn(w) w.all-distinct(compare))
  if idx != -1
    then Just(idx + n)
    else Nothing

fun exmain(input : string)
  val buf = parse(input.slice, pinput).expect("invalid signal file")
  val sop = find-first-n-distinct(buf, 4).expect("no start of packet found")
  val som = find-first-n-distinct(buf, 14).expect("no start of message found")
  println(sop)
  println(som)

fun main()
  run-main(exmain)
