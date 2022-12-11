import aok/list
import aok/main
import aok/math
import aok/parse
import aok/sort
import std/text/parse

// An arithmetic expression in terms of `old`.
type expr
  con Old
  con Nat(value : int)
  con Add(lhs : expr, rhs : expr)
  con Mul(lhs : expr, rhs : expr)

// Evaluates an exression on a value.
fun eval(expr : expr, old : int) : int
  match expr
    Old -> old
    Nat(value) -> value
    Add(lhs, rhs) -> lhs.eval(old) + rhs.eval(old)
    Mul(lhs, rhs) -> lhs.eval(old) * rhs.eval(old)

// Parses an expression, treating `*` as having higher precedence than `+` and
// both as left-associative.
fun pexpr() : <exn,parse> expr
  val plus = fn()
    whitespace0()
    char('+')
    whitespace0()
  val star = fn()
    whitespace0()
    char('*')
    whitespace0()

  val old = fn()
    lit("old")
    Old
  val nat = fn() Nat(pnat())
  val atom = fn() choose([old, nat])
  val mul = fn() separated(atom, star).foldl1(Mul)
  separated(mul, plus).foldl1(Add)

// A troublemaking monkey.
struct monkey
  operation : expr
  test : int
  if-true : int
  if-false : int

// Parses a monkey.
fun pmonkey() : <exn,parse> (int, list<int>, monkey)
  lit("Monkey ")
  val n = pnat()
  lit(":")
  whitespace0()

  lit("Starting items: ")
  val start = separated(pnat, fn() lit(", "))
  whitespace0()

  lit("Operation: new = ")
  val operation = pexpr()
  whitespace0()

  lit("Test: divisible by ")
  val test = pnat()
  whitespace0()

  lit("If true: throw to monkey ")
  val if-true = pnat()
  whitespace0()

  lit("If false: throw to monkey ")
  val if-false = pnat()
  whitespace0()

  (n, start, Monkey(operation, test, if-true, if-false))

// Parses the input text, resolving monkey IDs to list indices.
fun pinput() : <abort,exn,parse> (list<monkey>, list<list<int>>)
  many(pmonkey).enumerate.map(fn((i, (j, start, monkey)))
    if i != j then abort("Monkeys defined out of order")
    (monkey, start)
  ).unzip

// Determines a modulus to do computations in that won't affect the result.
fun find-modulus(monkeys : list<monkey>) : int
  monkeys.foldl(1, fn(mod, monkey) lcm(mod, monkey.test))

// An effect for monkeys tossing items around.
effect monkey-business
  // Pops all the items held by a monkey and returns them as a list.
  fun pop-all(i : int) : list<int>

  // Throws an item with the given worry level to a monkey.
  fun throw-item(i : int, worry : int) : ()

// A handler for monkey business.
fun monkey-business(
  start : list<list<int>>,
  action : forall<h> () -> <exn,monkey-business,st<h>|e> a
) : <exn|e> a
  val items = ref(start.vector)
  with handler
    fun pop-all(i)
      val ret = (!items)[i]
      modify(items, fn(is) is[i] := [])
      ret
    fun throw-item(i, worry)
      modify(items, fn(is)
        val tmp = is[i]
        is[i] := tmp ++ [worry]
      )
  action()

// Runs the actions of a single monkey using the given relief function. Returns
// the number of items the monkey inspected.
fun run-monkey(
  idx : int,
  monkey : monkey,
  relief : int -> int
) : monkey-business int
  pop-all(idx).map(fn(worry)
    val new-worry = relief(monkey.operation.eval(worry))
    if new-worry % monkey.test == 0
      then throw-item(monkey.if-true, new-worry)
      else throw-item(monkey.if-false, new-worry)
  ).length

// Runs the actions of all monkeys for a given number of rounds using the given
// relief funciton. Returns the number of times each monkey inspected items.
fun run-monkeys-get-inspections(
  monkeys : list<monkey>,
  relief : int -> int,
  start : list<list<int>>,
  time : int
) : <console,exn> list<int>
  with monkey-business(start)
  list(1, time).foldl(0.replicate(monkeys.length), fn(inspections, n)
    zip(inspections, monkeys.enumerate.map(fn((i, monkey))
      run-monkey(i, monkey, relief)
    )).map(fn((a, b)) a + b)
  )

fun exmain(input : string)
  val (monkeys, start) = parse(input.slice, pinput).expect("Invalid input")
  val modulus = monkeys.find-modulus

  val inspections-pt1 =
    run-monkeys-get-inspections(monkeys, fn(w) w / 3, start, 20)
  val inspections-pt2 =
    run-monkeys-get-inspections(monkeys, fn(w) w % modulus, start, 10000)

  val total-monkey-business-pt1 =
    inspections-pt1.sort.reverse.take(2).foldl1((*))
  val total-monkey-business-pt2 =
    inspections-pt2.sort.reverse.take(2).foldl1((*))

  println(total-monkey-business-pt1)
  println(total-monkey-business-pt2)

fun main()
  run-main(exmain)
