import aok/main
import std/os/env
import std/os/file
import std/os/path
import std/text/parse

// Rock Paper Scissors moves.
type move
  Rock
  Paper
  Scissors

// The inherent value of each move.
fun value(m : move) : int
  match m
    Rock -> 1
    Paper -> 2
    Scissors -> 3

// The score of a round of Rock Paper Scissors.
fun score(me : move, you : move) : int
  ((me.value - you.value + 1) % 3) * 3

// Parses an opponent's move.
fun theirs(s : string) : abort move
  match s
    "A" -> Rock
    "B" -> Paper
    "C" -> Scissors
    _ -> abort("Unrecognized move " ++ s)

// Parses my own move.
fun mine(s : string) : abort move
  match s
    "X" -> Rock
    "Y" -> Paper
    "Z" -> Scissors
    _ -> abort("Unrecognized move " ++ s)

// Parses a strategy guide.
fun parse-guide(guide : string) : abort list<(move, move)>
  guide.lines.filter-map(fn(line)
    match line.split(" ")
      [them, me] -> Just((theirs(them), mine(me)))
      [""] -> Nothing  // Ignore blank lines
      [] -> Nothing  // Ignore blank lines
      _ -> abort("Invalid line " ++ line)
  )

// Calculates the score of a strategy guide.
fun calculate-score(guide : list<(move, move)>) : int
  guide.map(fn((them, me)) me.value + score(me, them)).sum

fun exmain()
  match get-args()
    Cons(filename, _) ->
      val text = read-text-file(filename.path)
      val guide = parse-guide(text)
      val my-score = calculate-score(guide)
      println(my-score)
    _ -> abort("Input file must be provided")

fun main()
  run-main(exmain)
