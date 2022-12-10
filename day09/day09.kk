import aok/list
import aok/main
import aok/order
import aok/parse
import aok/set
import std/text/parse

// A point in Z x Z.
struct point
  x : int
  y : int

// The point (0, 0).
val origin = Point(0, 0)

// Lexicographical comparison of points by `x` then `y`.
fun compare(lhs : point, rhs : point) : order
  compare(lhs.x, rhs.x).or(compare(lhs.y, rhs.y))

// Returns whether two points are adjacent or touching via at most a single step
// in each direction.
fun are-adjacent(lhs : point, rhs : point) : bool
  (abs(lhs.x - rhs.x) <= 1) && (abs(lhs.y - rhs.y) <= 1)

// Movement direction.
type dir
  con DirU
  con DirD
  con DirL
  con DirR

// Steps a point in the given direction.
fun step(p : point, dir : dir) : point
  val Point(x, y) = p
  match dir
    DirU -> Point(x, y + 1)
    DirD -> Point(x, y - 1)
    DirL -> Point(x - 1, y)
    DirR -> Point(x + 1, y)

// Parses a dir.
fun pdir() : parse dir
  val up = fn()
    char('U')
    DirU
  val down = fn()
    char('D')
    DirD
  val left = fn() : parse dir
    char('L')
    DirL
  val right = fn() : parse dir
    char('R')
    DirR
  choose([up, down, left, right])

// A head move.
struct move
  dir : dir
  amount : int

// Run-length decodes a list of moves into a list of directions.
fun dirs(moves : list<move>) : list<dir>
  moves.flatmap(fn(Move(dir, amount)) dir.replicate(amount))

// Parses a list of moves.
fun pinput() : parse list<move>
  val move = fn()
    val d = pdir()
    char(' ')
    Move(d, pnat())
  val ms = separated(move, fn() char('\n'))
  whitespace0()
  ms

// Returns `tail` one step closer to `head`.
fun step-towards(tail : int, head : int) : int
  match compare(tail, head)
    Lt -> tail + 1
    Eq -> tail
    Gt -> tail - 1

// Returns the tail position after the tail has moved to catch up with the head.
fun chase(tail : point, head : point) : point
  if are-adjacent(tail, head) then tail else Point(
    tail.x.step-towards(head.x),
    tail.y.step-towards(head.y)
  )

// The position of a rope at a given instant.
struct rope
  head : point
  tail : point

// Applies a head step in the given direction to the rope, along with a tail
// chase.
fun apply(r : rope, dir : dir) : rope
  val head = r.head.step(dir)
  Rope(head, r.tail.chase(head))

// Applies each head step to the rope, returning a list of all rope positions in
// order.
fun apply-all(r : rope, dirs : list<dir>) : list<rope>
  Cons(r, dirs.foldl-accumulate(r, apply))

// Finds the number of distinct tail positions in a list.
fun distinct-tail-positions(rs : list<rope>) : console int
  // This loop is a bit slow, so print something to let the user know we're
  // still making progress.
  rs.enumerate.foldl(
    newset(compare : (point, point) -> order),
    fn(s, (i, r))
      if i % 100 == 0 then println("Examining position " ++ i.show)
      s.insert(r.tail)
  ).size

fun exmain(input : string)
  val moves = parse(input.slice, pinput).expect("Invalid input")
  val ropes = Rope(origin, origin).apply-all(moves.dirs)
  val pt1 = distinct-tail-positions(ropes)
  println(pt1)

fun main()
  run-main(exmain)
