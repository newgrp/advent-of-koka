import aok/fn
import aok/order
import aok/list
import aok/main
import aok/parse
import aok/tuple
import std/text/parse

// A packet.
type packet
  con Int(n : int)
  con List(xs : list<packet>)

fun compare-internal(lhs : packet, rhs : packet) : div order
  unsafe-no-exn {
    match (lhs, rhs)
      (Int(m), Int(n)) -> compare(m, n)
      (Int(_), List(_)) -> compare-internal(List([lhs]), rhs)
      (List(_), Int(_)) -> compare-internal(lhs, List([rhs]))
      (List(xs), List(ys)) ->
        lexicographical(zip(xs, ys).map(compare-internal.by-tuple)).or(
          compare(xs.length, ys.length)
        )
  }

// Compares two packets.
fun compare(lhs : packet, rhs : packet) : order
  unsafe-no-div { compare-internal(lhs, rhs) }

// A packet parser.
fun ppacket(elem : parser<<console|e>,packet>) : parser<<console|e>,packet>
  fn()
    val lst = fn()
      lit("[")
      val ps = optional([], {
        separated(elem, {
          whitespace0()
          lit(",")
          whitespace0()
        })
      })
      lit("]")
      List(ps)
    (Int * pnat) || lst

// Parses a packet.
fun parse-packet(line : string) : <abort,console> packet
  val depth = line.list.map(fn(c)
    match c
      '[' -> 1
      ']' -> -1
      _ -> 0
  ).scanl(0, (+)).maximum
  val parser = list(0, depth).foldl(
    fn() abort("Exceeded parsing depth"),
    fn(p, _) ppacket(p)
  )
  parse(line.slice, parser).expect("Invalid packet")

// Parses a sequence of pairs of packets.
fun parse-input(input : string) : <abort,console> list<(packet, packet)>
  input.lines.enumerate.filter-map(fn((i, line))
    with ctx("Line " ++ i.inc.show)
    if line.is-notempty
      then Just(parse-packet(line))
      else Nothing
  ).chunk-pairs

fun exmain(input : string)
  val pairs = parse-input(input)
  val pt1 = pairs.map-indexed(fn(i, (lhs, rhs))
    if compare(lhs, rhs) == Lt then i + 1 else 0
  ).sum
  println(pt1)

fun main()
  run-main(exmain)
