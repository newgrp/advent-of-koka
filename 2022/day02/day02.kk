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

// An outcome of a round.
type outcome
  Lose
  Draw
  Win

// The inherent value of each move.
fun value(m : move) : int
  match m
    Rock -> 1
    Paper -> 2
    Scissors -> 3

// The outcome of a round of Rock Paper Scissors.
fun round(me : move, you : move) : outcome
  match me
    Rock -> match you
      Rock -> Draw
      Paper -> Lose
      Scissors -> Win
    Paper -> match you
      Rock -> Win
      Paper -> Draw
      Scissors -> Lose
    Scissors -> match you
      Rock -> Lose
      Paper -> Win
      Scissors -> Draw

// Computes the response needed to achieve the given outcome.
fun response(you : move, o : outcome) : move
  match o
    Lose -> match you
      Rock -> Scissors
      Paper -> Rock
      Scissors -> Paper
    Draw -> you
    Win -> match you
      Rock -> Paper
      Paper -> Scissors
      Scissors -> Rock

// The score of a round of Rock Paper Scissors.
fun score(o : outcome) : int
  match o
    Lose -> 0
    Draw -> 3
    Win -> 6

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

// Parses a goal.
fun goal(s : string) : abort outcome
  match s
    "X" -> Lose
    "Y" -> Draw
    "Z" -> Win
    _ -> abort("Unrecognized goal " ++ s)

// Parses a strategy guide for part 1.
fun parse-guide1(guide : string) : abort list<(move, move)>
  guide.lines.filter-map(fn(line)
    match line.split(" ")
      [them, me] -> Just((theirs(them), mine(me)))
      [""] -> Nothing  // Ignore blank lines
      [] -> Nothing  // Ignore blank lines
      _ -> abort("Invalid line " ++ line)
  )

// Parses a strategy guide for part 2.
fun parse-guide2(guide : string) : abort list<(move, outcome)>
  guide.lines.filter-map(fn(line)
    match line.split(" ")
      [them, g] -> Just((theirs(them), goal(g)))
      [""] -> Nothing  // Ignore blank lines
      [] -> Nothing  // Ignore blank lines
      _ -> abort("Invalid line " ++ line)
  )

// Calculates the score of a strategy guide for part 1.
fun calculate-score(guide : list<(move, move)>) : int
  guide.map(fn((them, me)) me.value + round(me, them).score).sum

// Calculates the score of a strategy guide for part 2.
fun calculate-score(guide : list<(move, outcome)>) : int
  guide.map(fn((them, o)) response(them, o).value + o.score).sum

fun exmain(input : string)
  val guide1 = parse-guide1(input)
  val guide2 = parse-guide2(input)
  val pt1-score = calculate-score(guide1)
  val pt2-score = calculate-score(guide2)
  println(pt1-score)
  println(pt2-score)

fun main()
  run-main(exmain)
