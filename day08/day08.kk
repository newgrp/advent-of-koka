import aok/list
import aok/main
import aok/parse
import aok/set
import aok/string
import std/num/int32
import std/text/parse

// A location in the grid.
struct loc
  row : int
  col : int

// Compares two locations lexicographically by row then column.
fun compare(lhs : loc, rhs : loc) : order
  match compare(lhs.row, rhs.row)
    Eq -> compare(lhs.col, rhs.col)
    o -> o

// A grid of trees.
struct grid<a>
  // All rows concatenated.
  linear : list<a>
  width : int

// Returns the common length of all slices in the list or aborts if they have
// different lengths.
fun equal-length(slices : list<list<a>>) : abort int
  val lengths = slices.map(length)
  zipwith-indexed(lengths, lengths.drop(1), fn(i, a, b)
    if a != b
      then abort(
        "Slices " ++ i.show ++ " and " ++ i.inc.show ++
        " have different lengths: " ++ a.show ++ " != " ++ b.show
      )
      else b
  ).last.default(0)

// Converts a list of slices seen from the north into a grid.
fun from-north(slices : list<list<a>>) : abort grid<a>
  Grid(
    slices.reverse.foldl([].replicate(slices.equal-length), fn(acc, s)
      zipwith(s, acc, Cons)
    ).concat,
    slices.length
  )

// Converts a list of slices seen from the west into a grid.
fun from-west(slices : list<list<a>>) : abort grid<a>
  Grid(slices.concat, slices.equal-length)

// Converts a list of slices seen from the south into a grid.
fun from-south(slices : list<list<a>>) : abort grid<a>
  slices.map(reverse).from-north

// Converts a list of slices seen from the east into a grid.
fun from-east(slices : list<list<a>>) : abort grid<a>
  slices.map(reverse).from-west

// The height of a grid.
fun height(g : grid<a>) : int
  g.linear.length / g.width

// The rows of the grid.
fun rows(g : grid<a>) : list<list<a>>
  g.linear.chunks(g.width)

// The columns of the grid.
fun columns(g : grid<a>) : list<list<a>>
  list(0, g.width - 1).map(fn(j)
    g.linear.drop(j).step-by(g.width)
  )

// Each slice of the grid as viewed from the north.
fun north(g : grid<a>) : list<list<a>>
  g.columns

// Each slice of the grid as viewed from the east.
fun east(g : grid<a>) : list<list<a>>
  g.rows.map(reverse)

// Each slice of the grid as viewed from the south.
fun south(g : grid<a>) : list<list<a>>
  g.columns.map(reverse)

// Each slice of the grid as viewed from the west.
fun west(g : grid<a>) : list<list<a>>
  g.rows

// Prints the grid using the given cell printer.
fun show(g : grid<a>, show : a -> string) : abort string
  g.north.map(fn(col)
    val unpadded = col.map(show)
    val max-length = unpadded.map(char-length).maximum
    unpadded.map(fn(s) " ".repeat(max-length - s.char-length) ++ s)
  ).from-north.west.map(fn(row) row.join(" ")).join("\n")

// Annotates each cell of the grid with its location.
fun located(g : grid<a>) : grid<(loc, a)>
  Grid(
    g.linear.map-indexed(fn(n, a)
      val (i, j) = divmod(n, g.width)
      (Loc(i, j), a)
    ),
    g.width
  )

fun zipwith(lhs : grid<a>, rhs : grid<b>, f : (a, b) -> <abort|e> c) : <abort|e> grid<c>
  zipwith(lhs.west, rhs.west, fn(l, r) zipwith(l, r, f)).from-west

// Returns the set of visible trees along the given line from the edge.
fun visibles(line : list<(loc, int32)>) : rbset<loc>
  line.foldl(
    (-1.int32, newset(compare : (loc, loc) -> order)),
    fn((m, vs), (loc, h))
      if h > m
        then (h, vs.insert(loc))
        else (m, vs)
  ).snd

// Returns the set of the locations of all visible trees in the grid.
fun visibles(g : grid<int32>) : rbset<loc>
  (
    g.located.north.map(visibles).union(compare) ||
    g.located.east.map(visibles).union(compare) ||
    g.located.south.map(visibles).union(compare) ||
    g.located.west.map(visibles).union(compare)
  )

// Returns the partial scenic score of each tree in a line along the line.
fun partial-scores(line : list<int32>) : list<int>
  line.map(int).enumerate.foldl(
    (0.replicate(10), []),
    fn((last-seens, acc), (i, h))
      (
        last-seens.take(h) ++ [i] ++ last-seens.drop(h + 1),
        Cons(max(i - last-seens.drop(h).maximum, 0), acc)
      )
  ).snd.reverse

// Returns a grid of scenic scores of each tree in a grid.
fun scenic-scores(g : grid<int32>) : <abort,console> grid<int>
  val n = g.north.map(partial-scores).from-north
  val e = g.east.map(partial-scores).from-east
  val s = g.south.map(partial-scores).from-south
  val w = g.west.map(partial-scores).from-west
  zipwith(zipwith(n, s, (*)), zipwith(e, w, (*)), (*))

// Parses a grid from the input text.
fun pinput() : <abort,parse> grid<int32>
  val row = fn() many1(digit).map(int32)
  val g = separated(row, fn() char('\n')).from-west
  whitespace0()
  g

fun exmain(input : string)
  val g = parse(input.slice, pinput).expect("Invalid input")
  val vis = g.visibles
  val score = g.scenic-scores.west.concat.maximum
  println(vis.size)
  println(score)

fun main()
  run-main(exmain)
