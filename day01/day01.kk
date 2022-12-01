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

// Computes the answer by finding the sum of the list with the largest sum.
fun most-calories(es : list<list<int>>) : int
  es.map(sum).maximum

// Aborts computation with an error message.
effect exit
  ctl exit(msg : string) : a

fun main()
  with ctl exit(msg)
    println(msg)
  with ctl throw-exn(x)
    exit(x.show)
  match get-args()
    Cons(filename, _) ->
      val text = read-text-file(path(filename))
      val es = match parse-eof(text.slice, elves)
        ParseOk(es, _) -> es
        ParseError(msg, _) -> exit("Error parsing input: " ++ msg)
      val answer = most-calories(es)
      println(answer)
    _ -> exit("Input file must be provided")