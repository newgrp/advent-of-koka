import aok/main
import std/num/int64

// Returns the priority of an item.
fun priority(item : char) : maybe<int>
  if 'a' <= item && item <= 'z'
    then Just(item.int - 'a'.int + 1)
    else if 'A' <= item && item <= 'Z'
      then Just(item.int - 'A'.int + 27)
      else Nothing

// A set of items.
struct itemset
  bitset : int64

// An empty `itemset`.
val empty = Itemset(0.int64)

// Returns an `itemset` containing just `c`.
fun singleton(c : char) : abort itemset
  Itemset(1.int64.shl(c.priority.expect("Invalid item " ++ c.string)))

// `itemset` union.
fun (||)(lhs : itemset, rhs : itemset) : itemset
  Itemset(lhs.bitset.or(rhs.bitset))

// `itemset` intersection.
fun (&&)(lhs : itemset, rhs : itemset) : itemset
  Itemset(lhs.bitset.and(rhs.bitset))

// Converts a string to an `itemset`.
fun itemset(s : string) : abort itemset
  s.list.map(singleton).foldl(empty, (||))

// Returns a list of the priorities of the items in the set.
fun priorities(s : itemset) : list<int>
  list(1, 52).filter-map(fn(p)
    if s.bitset.and(1.int64.shl(p)) != 0.int64
      then Just(p)
      else Nothing
  )

// Parses an input string into a list of rusksacks.
fun parse-input(input : string) : abort list<(string, string)>
  input.lines.filter-map(fn(line)
    if line.is-empty
      then Nothing
      else
        val (fst, snd) = line.list.split(line.list.length / 2)
        Just((fst.string, snd.string))
  )

// Sums the priorities of the unique shared elements in each rucksack.
fun priority-sum(rucksacks : list<(string, string)>) : abort int
  rucksacks.map-indexed(fn(i, sack)
    with ctx("In rucksack " ++ i.show ++ ": ")
    match (sack.fst.itemset && sack.snd.itemset).priorities
      [p] -> p
      ps -> abort("Expected exactly one shared item, found: " ++ ps.show)
  ).sum

fun exmain(input : string)
  val rucksacks = parse-input(input)
  val psum = priority-sum(rucksacks)
  println(psum)

fun main()
  run-main(exmain)
