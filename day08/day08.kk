import aok/list
import aok/main
import aok/parse
import aok/set
import std/num/int32
import std/text/parse

// A location in the grid.
struct loc
  row : int
  col : int

// A grid of trees.
struct grid
  // All rows concatenated.
  linear : list<int32>
  width : int

// Converts a list of rows into a grid.
fun grid(rows : list<list<int32>>) : abort grid
  val lengths = rows.map(length)
  val width = zip(lengths, lengths.drop(1)).map-indexed(fn(i, (a, b))
    if a != b
      then abort(
        "Rows " ++ i.show ++ " and " ++ i.inc.show ++
        " have different lengths: " ++ a.show ++ " != " ++ b.show
      )
      else b
  ).last.default(0)
  Grid(rows.concat, width)

// The height of a grid.
fun height(g : grid) : int
  g.linear.length / g.width

// The rows of the grid.
fun rows(g : grid) : list<list<(loc, int32)>>
  g.linear.chunks(g.width).map-indexed(fn(i, row)
    row.map-indexed(fn(j, h)
      (Loc(i, j), h)
    )
  )

// The columns of the grid.
fun columns(g : grid) : list<list<(loc, int32)>>
  list(0, g.width - 1).map(fn(j)
    g.linear.drop(j).step-by(g.width).map-indexed(fn(i, h)
      (Loc(i, j), h)
    )
  )

// Compares two locations lexicographically by row then column.
fun compare(lhs : loc, rhs : loc) : order
  match compare(lhs.row, rhs.row)
    Eq -> compare(lhs.col, rhs.col)
    o -> o

// Returns the set of visible trees along the given line from the edge, using
// `loc` to construct their full locations in the grid.
fun visibles(line : list<(loc, int32)>) : rbset<loc>
  line.foldl(
    (-1.int32, newset(compare : (loc, loc) -> order)),
    fn((m, vs), (loc, h))
      if h > m
        then (h, vs.insert(loc))
        else (m, vs)
  ).snd

// Returns the set of the locations of all visible trees in the grid.
fun visibles(g : grid) : rbset<loc>
  (
    g.rows.map(visibles).union(compare) ||
    g.rows.map(reverse).map(visibles).union(compare) ||
    g.columns.map(visibles).union(compare) ||
    g.columns.map(reverse).map(visibles).union(compare)
  )

// Parses a grid from the input text.
fun pinput() : <abort,parse> grid
  val row = fn() many1(digit).map(int32)
  val g = separated(row, fn() char('\n')).grid
  whitespace0()
  g

fun exmain(input : string)
  val g = parse(input.slice, pinput).expect("Invalid input")
  val vis = g.visibles
  println(vis.size)

fun main()
  run-main(exmain)
