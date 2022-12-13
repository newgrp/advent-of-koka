import aok/grid
import aok/list
import aok/main
import aok/maybe
import aok/order

// A cell in the elevation map.
type cell
  con Start
  con End
  con Elev(h : int)

// The elevation of a cell.
fun elev(cell : cell) : int
  match cell
    Start -> 0
    End -> 25
    Elev(e) -> e

// Parses the input text into a grid of cells.
fun parse-input(input : string) : <abort,st<h>> grid<h, cell>
  val grid-lines = input.lines.filter(fn(line) !line.is-empty)
  val height = grid-lines.length
  val width = grid-lines.maybe.expect("Empty grid").list.length

  unsafe-no-div {
    var cells := constant-grid(height, width, Start)
    grid-lines.map-indexed(fn(i, line)
      line.list.map-indexed(fn(j, c)
        with ctx("At line " ++ i.show ++ ", column " ++ j.show)
        cells.at(Loc(i, j)) := (match c
          'S' -> Start
          'E' -> End
          _ | c.is-lower -> Elev(c.int - 'a'.int)
          _ -> abort("Unexpected character in input: " ++ c.show)
        )
      )
    )
    cells
  }

// Extracts the endpoint from the grid and flattens into a plain elevation map.
fun extract-endpoints(
  cells : grid<h, cell>
) : <abort,alloc<h>,read<h>> (grid<h, int>, loc, loc)
  var starts := []
  var ends := []
  val elevs = cells.map-located(fn(loc, cell)
    match cell
        Start -> starts := Cons(loc, starts)
        End -> ends := Cons(loc, ends)
        _ -> ()
    cell.elev
  )
  if starts.length > 1 then abort("Multiple start locations")
  if ends.length > 1 then abort("Multiple end locations")
  (
    elevs,
    starts.maybe.expect("No start location"),
    ends.maybe.expect("No end location")
  )

// Returns the neighbors of a cell in the elevation map.
fun neighbors(elevs : grid<h, int>, loc : loc) : read<h> list<loc>
  match elevs.at(loc)
    Nothing -> []
    Just(e) ->
      [
        Loc(loc.row - 1, loc.col),
        Loc(loc.row + 1, loc.col),
        Loc(loc.row, loc.col - 1),
        Loc(loc.row, loc.col + 1)
      ].filter(fn(loc')
        elevs.at(loc').map(fn(e') e + 1 >= e').default(False)
      )

// Determines the shortest path between two cells via breadth-first search.
fun bfs(elevs : grid<h, int>, start : loc, end : loc) : <abort,console,st<h>> maybe<int>
  unsafe-no-div {
    var dists := constant-grid(elevs.height, elevs.width, Nothing)
    var queue := [(start, 0)]
    while {
      match queue
        Nil -> False
        Cons((loc, n), queue') ->
          match dists.at(loc)
            Nothing -> abort("Found " ++ loc.show ++ " in queue")
            Just(Nothing) ->
              dists.at(loc) := Just(n)
              queue := queue' ++ elevs.neighbors(loc).map(
                fn(loc') (loc', n + 1)
              )
            Just(Just(_)) ->
              queue := queue'
          True
    } {}
    dists.at(end).flatten
  }

fun exmain(input : string)
  val cells = parse-input(input)
  val (elevs, start, end) = cells.extract-endpoints
  val distance = bfs(elevs, start, end).expect("No path from start to end")
  println(distance)

fun main()
  run-main(exmain)
