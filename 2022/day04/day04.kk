import aok/main
import std/text/parse

// An ID range.
struct range
  lo : int
  hi : int

// Creates a new ID range.
fun rng(lo : int, hi : int) : abort range
  if lo > hi then abort(
    "Left ID must be <= right ID, but got " ++ lo.show ++ "-" ++ hi.show
  )
  Range(lo, hi)

// Parses a list of ID range pairs from the input text.
fun parse-input(input : string) : abort list<(range, range)>
  val pline = fn()
    val a = pnat()
    char('-')
    val b = pnat()
    char(',')
    val c = pnat()
    char('-')
    val d = pnat()
    eof()
    (rng(a, b), rng(c, d))

  input.lines.filter(fn(line) !line.is-empty).map-indexed(fn(i, line)
    with ctx("On line " ++ i.show)
    parse(line.slice, pline).expect("Invalid line")
  )

// Counts the number of range pairs where one fully contains the other.
fun count-fully-contains(pairs : list<(range, range)>) : int
  pairs.filter(fn((left, right))
    (left.lo <= right.lo && right.hi <= left.hi) ||
    (right.lo <= left.lo && left.hi <= right.hi)
  ).length

// Counts the number of range pairs that overlap.
fun count-overlap(pairs : list<(range, range)>) : int
  pairs.filter(fn((left, right))
    !(left.hi < right.lo || right.hi < left.lo)
  ).length

fun exmain(input : string)
  val pairs = parse-input(input)
  val fcs = count-fully-contains(pairs)
  val over = count-overlap(pairs)
  println(fcs)
  println(over)

fun main()
  run-main(exmain)
