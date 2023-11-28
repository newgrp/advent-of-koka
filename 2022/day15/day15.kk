import aok/int
import aok/main
import aok/maybe
import aok/parse
import aok/quadtree
import std/text/parse

// Compares an integer to a closed-open range.
fun compare-range(lo : int, x : int, hi : int) : order
  match ()
    _ | x < lo -> Lt
    _ | x >= hi -> Gt
    _ -> Eq

// Computes the intersection of two closed-open ranges.
fun (&&)(lhs : (int, int), rhs : (int, int)) : maybe<(int, int)>
  val lo = max(lhs.fst, rhs.fst)
  val hi = min(lhs.snd, rhs.snd)
  if lo >= hi then Nothing else Just((lo, hi))

// Splits a closed-open range on a point.
fun split-range(
  lo : int,
  x : int,
  hi : int
) : (maybe<(int, int)>, maybe<(int, int)>)
  match ()
    _ | x < lo -> (Nothing, Just((lo, hi)))
    _ | x >= hi -> (Just((lo, hi)), Nothing)
    _ -> (Just((lo, x)), Just((x, hi)))

// A point in the plane with integer coordinates.
struct point
  x : int
  y : int

// Equality for points.
fun (==)(lhs : point, rhs : point) : bool
  lhs.x == rhs.x && lhs.y == rhs.y

// Transforms a point to a 45-degree rotated grid, with distances sqrt(2)
// larger.
fun to45(p : point) : point
  Point(p.x + p.y, p.y - p.x)

// Calculates Manhattan/taxicab distance between two points.
fun taxicab(p : point, q : point) : int
  abs(p.x - q.x) + abs(p.y - q.y)

fun ppoint() : parse point
  lit("x=")
  val x = pint()
  lit(", y=")
  val y = pint()
  Point(x, y)

// A box of points. Each interval is closed-open.
//
// Must satisfy `lo.x < hi.x` and `lo.y < hi.y`.
struct box
  lo : point
  hi : point

// Equality for boxes.
fun (==)(lhs : box, rhs : box) : bool
  lhs.lo == rhs.lo && lhs.hi == rhs.hi

// Creates a box with the given X and Y ranges.
fun range-box(x-range : (int, int), y-range : (int, int)) : box
  Box(Point(x-range.fst, y-range.fst), Point(x-range.snd, y-range.snd))

// Creates a box with a given center and side length.
fun square(center : point, side : int) : box
  Box(
    Point(center.x - side, center.y - side),
    Point(center.x + side + 1, center.y + side + 1)
  )

// Splits a square into equal quadrants, returning the center of each.
fun quadrants(center : point, side : int) : (point, point, point, point)
  val lo-x = center.x - side / 2
  val lo-y = center.y - side / 2
  val hi-x = center.x + side / 2
  val hi-y = center.y + side / 2
  (Point(lo-x, lo-y), Point(lo-x, hi-y), Point(hi-x, lo-y), Point(hi-x, hi-y))

// Computes the intersection of two boxes.
fun (&&)(lhs : box, rhs : box) : maybe<box>
  val lp = (lhs.lo, lhs.hi)
  val rp = (rhs.lo, rhs.hi)
  do-maybe {
    val xp = (lp.map(x) && rp.map(x)).try
    val yp = (lp.map(y) && rp.map(y)).try
    Box(Point(xp.fst, yp.fst), Point(xp.snd, yp.snd))
  }

// Splits a box into quadrants.
fun split(box : box, p : point, side : int) : quadnode<bool,maybe<box>>
  val sq = square(p, side)
  match box && sq
    Nothing -> QTLeaf(False)
    Just(box') | box' == sq -> QTLeaf(True)
    Just(box') ->
      val (lo-x, hi-x) = split-range(box'.lo.x, p.x, box'.hi.x)
      val (lo-y, hi-y) = split-range(box'.lo.y, p.y, box'.hi.y)
      QTSplit(
        do-maybe { range-box(lo-x.try, lo-y.try) },
        do-maybe { range-box(lo-x.try, hi-y.try) },
        do-maybe { range-box(hi-x.try, lo-y.try) },
        do-maybe { range-box(hi-x.try, hi-y.try) }
      )

// Recursively splits a box into quadrants.
fun split-rec(
  box : box,
  p : point,
  side : int
) : quadtree-search<(maybe<box>, point)> bool
  match box.split(p, side)
    QTLeaf(b) -> b
    QTSplit(lolo, lohi, hilo, hihi) ->
      val (ll, lh, hl, hh) = quadrants(p, side)
      val (maybe-box', p') = search(
        (lolo, ll),
        (lohi, lh),
        (hilo, hl),
        (hihi, hh)
      )
      match maybe-box'
        Nothing -> False
        Just(box') -> box'.unsafe-decreasing.split-rec(p', side / 2)

// The reading from a sensor.
struct sensor-reading
  sensor : point
  beacon : point

// Determines the box in the 45-degree rotated grid where no undiscovered beacon
// may exist.
fun to45box(sr : sensor-reading) : box
  val dist = taxicab(sr.sensor, sr.beacon)
  sr.sensor.to45.square(dist)

fun psensor-reading() : parse sensor-reading
  lit("Sensor at ")
  val sensor = ppoint()
  lit(": closest beacon is at ")
  val beacon = ppoint()
  Sensor-reading(sensor, beacon)

fun pinput() : parse list<sensor-reading>
  val readings = separated(psensor-reading, whitespace0)
  whitespace0()
  readings

// A square region of the plane with associated regions of data.
struct plane<a>
  // Each side length of the root square is `2 << root-exp`.
  root-exp : int
  // The low halves go up to -1, while the high sides start at 0.
  data : quadtree<a>

// Simplifier for boolean quadtrees.
fun simplify-bool(
  lolo : bool,
  lohi : bool,
  hilo : bool,
  hihi : bool
) : maybe<bool>
  if lolo == lohi && lolo == hilo && lolo == hihi
    then Just(lolo)
    else Nothing

// Half the side length of the root square.
fun side(plane : plane<a>) : int
  exp2(plane.root-exp)

// Folds a plane using the given function.
fun fold(plane : plane<a>, f : quadnode<a,b> -> e b) : e b
  plane.data.fold(f)

// Simplifies a plane using the given function.
fun simplify(plane : plane<a>, f : (a, a, a, a) -> e maybe<a>) : e plane<a>
  Plane(plane.root-exp, plane.data.simplify(f))

// Doubles the side length of the region the plane observes, filling new space
// with a default value.
fun grow(plane : plane<a>, default : a) : plane<a>
  val leaf = Quadtree(QTLeaf(default))
  val data' = Quadtree(match plane.data.unfix
    QTLeaf(a) -> QTLeaf(a)
    QTSplit(lolo, lohi, hilo, hihi) -> QTSplit(
      Quadtree(QTSplit(leaf, leaf, leaf, lolo)),
      Quadtree(QTSplit(leaf, leaf, lohi, leaf)),
      Quadtree(QTSplit(leaf, hilo, leaf, leaf)),
      Quadtree(QTSplit(hihi, leaf, leaf, leaf))
    )
  )
  Plane(plane.root-exp.inc, data')

// Aligns two planes to the same region.
fun align(
  lhs : plane<a>,
  rhs : plane<b>,
  l-default : a,
  r-default : b
) : (plane<a>, plane<b>)
  match compare(lhs.root-exp, rhs.root-exp)
    Lt -> align(
      lhs.grow(l-default).unsafe-decreasing,
      rhs,
      l-default,
      r-default
    )
    Eq -> (lhs, rhs)
    Gt -> align(
      lhs,
      rhs.grow(r-default).unsafe-decreasing,
      l-default,
      r-default
    )
  
// Joins two planes with the given function.
fun join(
  lhs : plane<a>,
  rhs : plane<b>,
  l-default : a,
  r-default : b,
  f : (a, b) -> e c
) : e plane<c>
  val (lhs', rhs') = align(lhs, rhs, l-default, r-default)
  Plane(
    lhs'.root-exp,
    join(lhs'.data, rhs'.data, f)
  )

// Creates a plane where a given box has one value and the rest of the plane has
// another.
fun as-plane(box : box) : plane<bool>
  val root-exp = log2([box.lo.x, box.lo.y, box.hi.x - 1, box.hi.y - 1].map(fn(n)
    if n < 0 then ~n + 1 else n
  ).maximum).default(0) + 1
  Plane(
    root-exp,
    unfold-search { box.split-rec(Point(0, 0), exp2(root-exp)) }
  )

// Creates a map of box coverage.
fun create-map(boxes : list<box>) : plane<bool>
  boxes.foldl(Plane(0, unit-quadtree(False)), fn(plane, box)
    join(plane, box.as-plane, False, False, (||))
  )

// Quadtree-searches along a diagonal-downwards line.
fun diagonal-down(x-plus-y : int, side : int) : quadtree-search<int> bool
  if x-plus-y < ~(side + side) || x-plus-y >= side + side - 1
    then False
    else if side == 0
      then True
      else diagonal-down(
        search(side, 0, 0, ~side),
        (side / 2).unsafe-decreasing
      )

fun exmain(input : string)
  val readings = parse(input.slice, pinput).expect("Invalid input")
  val box-map = readings.map(to45box).create-map.simplify(simplify-bool)
  println(box-map.data.debug(show))
  val query = Plane(
    box-map.side,
    unfold-search { diagonal-down(20, box-map.side) }
  )
  val pt1 = join(box-map, query, False, False, (&&)).simplify(
    simplify-bool
  ).fold(fn(q : quadnode<bool,int>)
    match q
      QTLeaf(b) -> b.int
      QTSplit(lolo, lohi, hilo, hihi) -> lolo + lohi + hilo + hihi
  )
  println(pt1)

fun main()
  run-main(exmain)
