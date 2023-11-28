import aok/grid
import aok/list
import aok/main
import aok/maybe
import aok/parse
import aok/tuple
import std/text/parse

// The hole where sand starts falling from.
val hole = Loc(0, 500)

// Parses a coordinate pair.
fun pcoord() : parse loc
  val x = pnat()
  lit(",")
  val y = pnat()
  Loc(y, x)

// Parses a rock structure.
fun pstructure() : parse list<loc>
  separated(pcoord, { lit(" -> ") })

// Parses all rock structures from the input.
fun parse-input(input : string) : abort list<list<loc>>
  do-list {
    val (i, line) = input.lines.enumerate.try
    with ctx("Line " ++ i.show)
    if !line.is-empty
      then parse(line.slice, pstructure).expect("Invalid rock structure")
      else [].try
  }

// A range between a pair of integers. Sorts them first.
fun range(a : int, b : int) : console list<int>
  println("range(" ++ a.show ++ ", " ++ b.show ++ ")")
  list(min(a, b), max(a, b))

// Returns a list of the locations in a direct horizontal or vertical line.
fun make-line(start : loc, end : loc) : <abort,console> list<loc>
  println("make-line")
  if start.row == end.row
    then range(start.col, end.col).map(fn(c) Loc(start.row, c))
    else if start.col == end.col
      then range(start.row, end.row).map(fn(r) Loc(r, start.col))
      else abort(
        "Not a horizontal or vertical line: " ++ start.show ++ " -> " ++
        end.show
      )

// A cell in the grid.
type cell
  con Air
  con Rock
  con Sand

// Clamps an integer to a range.
fun clamp(x : int, lo : int, hi : int) : int
  match ()
    _ | x < lo -> lo
    _ | x > hi -> hi
    _ -> x

// Shifts the structures so that the topmost and leftmost rocks are at row and
// column 1, respectively.
//
// Returns the new structures, the height and width of a grid where the bottom
// and rightmost rocks are 1 away from the edges, and the new location of the
// sand hole.
fun shift-and-crop(structures : list<list<loc>>) : (list<list<loc>>, (int, int), loc)
  val min-row = (do-list { structures.try.map(row).minimum }).minimum
  val max-row = (do-list { structures.try.map(row).maximum }).maximum
  val min-col = (do-list { structures.try.map(col).minimum }).minimum
  val max-col = (do-list { structures.try.map(col).maximum }).maximum

  val new-structures = do-list {
    do-list {
      val Loc(r, c) = structures.try.try
      Loc(r - min-row + 1, c - min-col + 1)
    }
  }
  val (height, width) = (max-row - min-row + 3, max-col - min-col + 3)
  val hole = Loc(
    (hole.row - min-row + 1).clamp(0, height - 1),
    (hole.col - min-col + 1).clamp(0, width - 1)
  )

  (new-structures, (height, width), hole)

// Constructs a grid from the structures list.
fun render-structures(
  structures : list<list<loc>>,
  height : int,
  width : int
) : <abort,console,exn,st<h>> grid<h,cell>
  unsafe-no-div {
    println("render-structures")
    var grid := constant-grid(height, width, Air)
    println("constant-grid")
    structures.foreach(fn(structure)
      println(structure.map(show).join(" -> "))
      structure.window-pairs.foreach(fn((start, end))
        println(start.show ++ " -> " ++ end.show)
        println("lining")
        val line = (match ()
          _ | start.row == end.row ->
            range(start.col, end.col).map(fn(c) Loc(start.row, c))
          _ | start.col == end.col ->
            range(start.row, end.row).map(fn(r) Loc(r, start.col))
          _ -> abort(
            "Not a horizontal or vertical line: " ++ start.show ++ " -> " ++
            end.show
          )
        )
        println(line.length)
        line.foreach(fn(loc)
          println(loc.show)
          println((!grid.linear).length)
          println(grid.size)
          println(grid.is-valid(loc))
          println(grid.unchecked-index(loc))
          grid[loc] := Rock
          println("foo")
        )
      )
    )
    grid
  }

// Moves a piece of sand a single step. Returns `Nothing` if it cannot move.
fun step-sand(grid : grid<h,cell>, sand : loc) : read<h> maybe<loc>
  (do-list {
    val loc = Loc(sand.row - 1, sand.col + [0, -1, 1].try)
    do-maybe { if grid.at(loc).try.is-air then loc else Nothing.try }
  }).concat-maybe.head

// Determines pessimistically whether a piece of sand will fall forever.
fun is-bound-for-abyss(grid : grid<h,cell>, sand : loc) : read<h> bool
  sand.row == grid.height - 1 || sand.col == 0 || sand.col == grid.width - 1

// Drops a new piece of sand from the hole until it comes to rest or falls off
// the edge. Returns the sand's resting location.
fun drop-sand(grid : grid<h,cell>, hole : loc) : read<h> maybe<loc>
  unsafe-no-div {
    var sand := hole
    while {
      !grid.is-bound-for-abyss(sand) && (match grid.step-sand(sand)
        Just(loc) ->
          sand := loc
          True
        Nothing -> False
      )
    } {}
    if !grid.is-bound-for-abyss(sand) then Just(sand) else Nothing
  }

// Drops sand from the hole until one falls into the abyss. Returns the amount
// of settled sand.
fun dump-sand(start : grid<h,cell>, hole : loc) : <console,exn,read<h>,write<h>> int
  unsafe-no-div {
    var grid := start
    var n := 0
    while {
      match grid.drop-sand(hole)
        Just(loc) ->
          grid[loc] := Sand
          n := n + 1
          True
        Nothing -> False
    } {}
    n
  }

fun exmain(input : string)
  val structures = parse-input(input)
  println("parsed")
  val (structures', (height, width), hole) = shift-and-crop(structures)
  println("adjusted : " ++ height.show ++ " x " ++ width.show)
  val grid = render-structures(structures', height, width)
  println("rendered")
  val pt1 = grid.dump-sand(hole)
  println(pt1)

fun main()
  run-main(exmain)
