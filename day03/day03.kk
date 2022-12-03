import aok/main
import std/num/int64

// Returns the priority of an item.
fun priority(item : char) : abort int
  if 'a' <= item && item <= 'z'
    then item.int - 'a'.int + 1
    else if 'A' <= item && item <= 'Z'
      then item.int - 'A'.int + 27
      else abort("Invalid item " ++ item.string)

// A set of items.
struct itemset
  bitset : int64

// An empty `itemset`.
val empty = Itemset(0.int64)

// Returns an `itemset` containing just `c`.
fun singleton(c : char) : abort itemset
  Itemset(1.int64.shl(c.priority))

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
    with ctx("In rucksack " ++ i.show)
    match (sack.fst.itemset && sack.snd.itemset).priorities
      [p] -> p
      ps -> abort("Expected exactly one shared item, found: " ++ ps.show)
  ).sum

// Collects a list of rucksacks into elf groups.
fun groups(rucksacks : list<(string, string)>) : abort list<((string, string), (string, string), (string, string))>
  val (grps, rst) = list(1, rucksacks.length / 3).foldl(([], rucksacks), fn((grps, rst), i)
    with ctx("In group " ++ i.show)
    val (grp, rst2) = rst.split(3)
    match grp
      [a, b, c] -> (Cons((a, b, c), grps), rst2)
      _ -> abort("split() misbehaved")
  )
  if !rst.is-empty then abort("Rucksack list length was not a multiple of 3, was " ++ rucksacks.length.show)
  grps.reverse

// Determines the badge priority of a group.
fun badge-priority(group : ((string, string), (string, string), (string, string))) : abort int
  val commons = (
    (group.fst.fst.itemset || group.fst.snd.itemset) &&
    (group.snd.fst.itemset || group.snd.snd.itemset) &&
    (group.thd.fst.itemset || group.thd.snd.itemset)
  )
  match commons.priorities
    [p] -> p
    ps -> abort("Expected exactly one common item in group, found: " ++ ps.show)

// Sums the badge priorities of a list of rucksacks.
fun badge-sum(rucksacks : list<(string, string)>) : abort int
  groups(rucksacks).map(badge-priority).sum

fun exmain(input : string)
  val rucksacks = parse-input(input)
  val psum = priority-sum(rucksacks)
  val bsum = badge-sum(rucksacks)
  println(psum)
  println(bsum)

fun main()
  run-main(exmain)
