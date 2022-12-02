import aok/main
import aok/sort
import std/os/env
import std/os/file
import std/os/path
import std/text/parse

// Returns a parser for a list of `p` separated by `s`. The `s` parse results
// are dropped. Requires at least one successful `p` parse.
fun separated(p : parser<e, a>, s : parser<e, b>) : <parse|e> list<a>
  Cons(p(), many(fn()
    s()
    p()
  ))

// Parser for a single elf's food.
fun elf() : parse list<int>
  separated(pnat, fn() char('\n'))

// Parser for all elves.
fun elves() : parse list<list<int>>
  // Use optional to handle an empty file.
  optional([], fn()
    val es = separated(elf, fn()
      char('\n')
      char('\n')
    )
    // Allow extra newlines.
    many(fn() char('\n'))
    es
  )

// Computes the answer to part 1 by finding the sum of the list with the
// largest sum.
fun most-calories(es : list<list<int>>) : int
  es.map(sum).maximum

// Computes the answer to part 2 by finding the sum of the lists with the
// three largest sums.
fun top-three(es : list<list<int>>) : int
  es.map(sum).sort.reverse.take(3).sum

fun exmain(input : string)
  val es = match parse-eof(input.slice, elves)
    ParseOk(es, _) -> es
    ParseError(msg, _) -> abort("Error parsing input: " ++ msg)
  val part1 = most-calories(es)
  val part2 = top-three(es)
  println(part1)
  println(part2)

fun main()
  run-main(exmain)
