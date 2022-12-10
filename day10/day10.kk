import aok/list
import aok/main
import aok/parse
import std/text/parse

// An instruction.
type instr
  con Noop
  con Addx(n : int)

// Parses an instruction.
fun pinstr() : parse instr
  val noop = fn()
    lit("noop")
    Noop
  val addx = fn()
    lit("addx ")
    Addx(pint())
  noop || addx

// Parses a program.
fun pinput() : parse list<instr>
  val prog = separated(pinstr, fn() char('\n'))
  whitespace0()
  prog

// Expands a list of instructions into a list of `X` values.
fun run(prog : list<instr>, init : int) : list<int>
  [init, init] ++ prog.foldl([], fn(acc, instr)
    val x = match acc
      Nil -> init
      Cons(x, _) -> x
    match instr
      Noop -> Cons(x, acc)
      Addx(n) -> Cons(x + n, Cons(x, acc))
  ).reverse

// Determines the sum of the signal strengths at given sample points in an
// executed program.
fun signal-strengths-sum(xs : list<int>, samples : list<int>) : int
  xs.enumerate.filter-map(fn((i, x))
    if samples.find(fn(s) s == i).is-just then Just(i * x) else Nothing
  ).sum

// Renders an image using an executed program.
fun render(xs : list<int>, width : int) : string
  xs.enumerate.drop(1).map(fn((i, x))
    if abs(x - (i - 1) % width) <= 1 then '#' else '.'
  ).chunks(width).map(string).join("\n")

fun exmain(input : string)
  val prog = parse(input.slice, pinput).expect("Invalid input")
  val part1 = prog.run(1).signal-strengths-sum([20, 60, 100, 140, 180, 220])
  val part2 = prog.run(1).render(40)
  println(part1)
  println(part2)

fun main()
  run-main(exmain)
