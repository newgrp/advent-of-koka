import aok/list
import aok/order

// A location in a grid.
pub struct loc
  row : int
  col : int

// Compares two locations lexicographically by row then column.
fun compare(lhs : loc, rhs : loc) : order
  compare(lhs.row, rhs.row).or(compare(lhs.col, rhs.col))

// Prints a `loc`.
pub fun show(loc : loc) : string
  "Loc(" ++ loc.row.show ++ ", " ++ loc.col.show ++ ")"

// A rectangular grid of elements. Don't put references to the grid inside the
// grid or your computer will explode.
pub type grid<h,a>
  Grid(
    // The rows of the grid concatenated.
    linear : ref<h,vector<a>>,

    // The width of each row.
    width : int
  )

// A grid of the given size filled with the given value.
pub fun constant-grid(height : int, width : int, value : a) : alloc<h> grid<h,a>
  Grid(ref(vector(height * width, value)), width)

// The height of the grid.
pub fun height(g : grid<h,a>) : read<h> int
  unsafe-no-div { !g.linear }.length / g.width

// The total size of the grid.
pub fun size(g : grid<h,a>) : read<h> int
  unsafe-no-div { !g.linear }.length

// The given row of the grid.
pub fun row(g : grid<h,a>, i : int) : read<h> list<a>
  unsafe-no-div { !g.linear }.list.drop(i * g.width).take(g.width)

// The given column of the grid.
pub fun column(g : grid<h,a>, j : int) : read<h> list<a>
  unsafe-no-div { !g.linear }.list.drop(j).step-by(g.width)

// The rows of the grid.
pub fun rows(g : grid<h,a>) : read<h> list<list<a>>
  unsafe-no-div { !g.linear }.list.chunks(g.width)

// The columns of the grid.
pub fun columns(g : grid<h,a>) : read<h> list<list<a>>
  list(0, g.width - 1).map(fn(j) g.column(j))

fun is-valid(g : grid<h,a>, loc : loc) : bool
  unsafe-total {
    0 <= loc.row && loc.row < g.height && 0 <= loc.col && loc.col < g.width
  }

fun unchecked-index(g : grid<h,a>, loc : loc) : int
  unsafe-total { loc.row * g.width + loc.col }

fun index(g : grid<h,a>, loc : loc) : maybe<int>
  if g.is-valid(loc) then Just(g.unchecked-index(loc)) else Nothing

fun oob(loc : loc) : exn a
  Exception(loc.show ++ " is out of bounds", ExnError).throw-exn

// Returns the given element of the grid.
pub fun [](g : grid<h,a>, loc : loc) : <exn,read<h>> a
  match g.at(loc)
    Nothing -> oob(loc)
    Just(a) -> a

// Returns the given element of the grid if it exists.
pub fun at(g : grid<h,a>, loc : loc) : read<h> maybe<a>
  if g.is-valid(loc)
    then Just(unsafe-no-exn {
      unsafe-no-div { !g.linear }[g.unchecked-index(loc)]
    })
    else Nothing

// Assigns to the given location in the grid, if it exists.
pub fun at(
  g : local-var<s,grid<h,a>>,
  loc : loc,
  value : a
) : <local<s>,read<h>,write<h>|e> ()
  unsafe-no-div {
    match g.index(loc)
      Nothing -> ()
      Just(k) ->
        g.linear.modify(fn(v) unsafe-no-exn { v[k] := value })
  }

// Maps a grid according to the given function.
pub fun map(
  g : grid<h,a>,
  f : a -> <alloc<h>,read<h>|e> b
) : <alloc<h>,read<h>|e> grid<h,b>
  Grid(ref(unsafe-no-div { !g.linear }.map(f)), g.width)

// Maps a grid according to the given function.
pub fun map-located(
  g : grid<h,a>,
  f : (loc, a) -> <alloc<h>,read<h>|e> b
) : <alloc<h>,read<h>|e> grid<h,b>
  Grid(
    ref(g.rows.map-indexed(fn(i, row)
      row.map-indexed(fn(j, value)
        f(Loc(i, j), value)
      )
    ).concat.vector),
    g.width
  )
