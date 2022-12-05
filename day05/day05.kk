import aok/main
import aok/parse
import std/text/parse

// Prints a list of stacks.
fun show(stacks : list<list<char>>) : string
  val top = stacks.map(length).maximum
  list(0, top - 1).reverse.map(fn(i)
    stacks.map(fn(st)
      st[i].map(fn(c) "[" ++ c.string ++ "]").default("   ")
    ).join(" ")
  ).join("\n") ++ "\n " ++ list(0, stacks.length - 1).map(show).join("   ")

// A crane move.
struct move
  count : int
  src : int
  dst : int

// Applies a move to a list of stacks.
fun apply(stacks : list<list<char>>, move : move) : <abort,exn> list<list<char>>
  with ctx("In stack\n" ++ stacks.show ++ "\n")
  val Move(count, src, dst) = move
  if src == dst then abort(
    "Source stack equal to target stack: " ++ (src + 1).show
  )

  var stackvec := stacks.vector
  if stackvec[src].length < count then abort(
    "Insufficient crates in source stack " ++
    (src + 1).show ++
    ", need " ++
    count.show
  )
  val (s, targets) = stackvec[src].split(stackvec[src].length - count)
  val d = stackvec[dst] ++ targets.reverse
  stackvec[src] := s
  stackvec[dst] := d
  stackvec.list


// Parses a crate.
fun crate() : parse char
  char('[')
  val c = alpha()
  char(']')
  c

// Parses a crate or a blank space.
fun maybe-crate() : parse maybe<char>
  val crt = fn() Just(crate())
  val spc = fn()
    lit("   ")
    Nothing
  crt || spc

// Parses the crates on a given row.
fun crate-row() : parse list<maybe<char>>
  separated(maybe-crate, fn() lit(" "))

// Parses all rows of crates.
fun crates() : parse list<list<maybe<char>>>
  separated(crate-row, fn() char('\n'))

// Parses a list of stack IDs.
fun stack-ids() : <abort,parse> list<int>
  separated(pnat, fn() lit("   ")).map-indexed(fn(i, n)
    if n != i + 1 then abort("Unexpected ID " ++ n.show)
    n
  )

// Parses the stacks of crates.
fun stacks() : <abort,parse> list<list<char>>
  val rows = crates()
  whitespace()
  val ids = stack-ids()

  rows.foldl(
    (0, [].replicate(ids.length)),
    fn((i, ss) : (int, list<_>), row : list<_>)
      with ctx("In row " ++ i.show)
      if row.length > ids.length then abort(
        "More stacks than IDs: " ++
        row.length.show ++
        " > " ++
        ids.length.show
      )
      (i + 1, zip(
        ss,
        row ++ Nothing.replicate(ss.length - row.length)
      ).map-indexed(fn(j, (s, mc))
        match mc
          Just(c) -> Cons(c, s)
          Nothing -> if !s.is-empty then abort(
            "Floating crate in stack " ++ j.show
          ) else Nil
      ))
  ).snd

// Parses a move.
fun move() : parse move
  lit("move ")
  val count = pnat()
  lit(" from ")
  val src = pnat()
  lit(" to ")
  val dst = pnat()
  Move(count, src - 1, dst - 1)

// Parses the input text.
fun pinput() : <abort,parse> (list<list<char>>, list<move>)
  val stacks = stacks()
  whitespace()
  val moves = separated(move, whitespace)
  whitespace0()
  (stacks, moves)

// Calculates the top crate on each stack after all moves.
fun calculate-tops(
  stacks : list<list<char>>,
  moves : list<move>
) : <abort,exn> list<char>
  moves.foldl((0, stacks), fn((i : int, stacks : list<list<char>>), move)
    with ctx("On move " ++ i.show)
    (i + 1, stacks.apply(move))
  ).snd.map-indexed(fn(i, st)
    with ctx("In stack " ++ i.show)
    st.last.expect("Empty stack at end")
  )

fun exmain(input : string)
  val (stacks, moves) = parse(
    input.slice,
    pinput
  ).expect("Failed to parse input text")
  val tops = calculate-tops(stacks, moves).string
  println(tops)

fun main()
  run-main(exmain)
