import aok/list
import aok/main
import std/text/parse

// Parses an input signal.
fun pinput() : parse string
  val buf = many(alpha)
  whitespace0()
  buf.string

// Finds the index of the start of the packet.
fun find-start-of-packet(buf : string) : maybe<int>
  val idx = buf.list.windows(4).index-of(fn(four)
    match four
      [a, b, c, d] ->
        a != b && a != c && a != d && b != c && b != d && c != d
      // Unreachable
      _ -> False
  )
  if idx != -1
    then Just(idx + 4)
    else Nothing

fun exmain(input : string)
  val buf = parse(input.slice, pinput).expect("invalid signal file")
  val sop = find-start-of-packet(buf).expect("no start of packet found")
  println(sop)

fun main()
  run-main(exmain)
