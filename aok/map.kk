import aok/redblack
import std/num/float64
import std/num/int32
import std/num/int64

fun by-key(cmp : (k, k) -> order) : (((k, v), (k, v)) -> order)
  fn((a, _), (b, _)) cmp(a, b)

fun hint(cmp : (k, k) -> order, key : k) : ((k, v) -> order)
  fn((k, _)) cmp(key, k)

// A map from keys of one type to values of another.
pub value type rbmap<k, v>
  Map(
    tree : rbtree<(k, v)>,
    cmp : (k, k) -> order
  )

// Returns an empty map.
pub fun newmap(cmp : (k, k) -> order) : rbmap<k, v>
  Map(Leaf, cmp)

pub fun newmap-char() : rbmap<char, v>
  newmap(compare : (char, char) -> order)

pub fun newmap-float64() : rbmap<float64, v>
  newmap(compare : (float64, float64) -> order)

pub fun newmap-int() : rbmap<int, v>
  newmap(compare : (int, int) -> order)

pub fun newmap-int32() : rbmap<int32, v>
  newmap(compare : (int32, int32) -> order)

pub fun newmap-int64() : rbmap<int64, v>
  newmap(compare : (int64, int64) -> order)

pub fun newmap-string() : rbmap<string, v>
  newmap(compare : (string, string) -> order)

// A debug string showing the internals of a map.
pub fun debug(
  m : rbmap<k, v>,
  show-k : k -> string,
  show-v : v -> string
) : string
  match m
    Map(tree, _) -> "Map(" ++ tree.debug(fn((k, v))
      "(" ++ show-k(k) ++ ", " ++ show-v(v) ++ ")"
    ) ++ ")"

// The size of the map.
pub fun size(map : rbmap<k, v>) : int
  match map
    Map(tree, _) -> tree.size

// A list of the keys of a map in increasing order.
pub fun keys(m : rbmap<k, v>) : list<k>
  m.items.map(fst)

// A list of the values of a map in increasing order of keys.
pub fun values(m : rbmap<k, v>) : list<v>
  m.items.map(snd)

// A list of the key-value pairs of a map in increasing order of keys.
pub fun items(m : rbmap<k, v>) : list<(k, v)>
  match m
    Map(tree, _) -> tree.node-values

// Determines whether a map contains a key.
pub fun contains-key(m : rbmap<k, v>, key : k) : bool
  m.at(key).is-just

// Gets the value associated with a key.
pub fun at(m : rbmap<k, v>, key : k) : maybe<v>
  match m
    Map(tree, cmp) -> tree.find(cmp.hint(key)).map(snd)

// Inserts a key-value pair into a map, if the key doesn't already exist.
pub fun insert-or-keep(m : rbmap<k, v>, key : k, value : v) : rbmap<k, v>
  m.try-insert-or-keep(key, value).fst

// Inserts a key-value pair into a map, replacing the existing value if the key
// already exists.
pub fun insert-or-replace(m : rbmap<k, v>, key : k, value : v) : rbmap<k, v>
  m.try-insert-or-replace(key, value).fst

fun try-insert-internal(
  m : rbmap<k, v>,
  key : k,
  value : v,
  replace : bool
) : (rbmap<k, v>, maybe<v>)
  match m
    Map(tree, cmp) -> 
      val (t, prev) = tree.insert((key, value), cmp.by-key, replace)
      (Map(t, cmp), prev.map(snd))

// Inserts a key-value pair into a map, if the key doesn't already exist.
//
// Returns the existing value if the key already exists.
pub fun try-insert-or-keep(
  m : rbmap<k, v>,
  key : k,
  value : v
) : (rbmap<k, v>, maybe<v>)
  m.try-insert-internal(key, value, False)

// Inserts a key-value pair into a map, replacing the existing value if the key
// already exists.
//
// Returns the previous value if the key already exists.
pub fun try-insert-or-replace(
  m : rbmap<k, v>,
  key : k,
  value : v
) : (rbmap<k, v>, maybe<v>)
  m.try-insert-internal(key, value, True)

pub fn delete(m : rbmap<k, v>, key : k) : rbmap<k, v>
  m.try-delete(key).fst

// Deletes a key from a map.
//
// Returns the value if the key existed in the map.
pub fun try-delete(m : rbmap<k, v>, key : k) : (rbmap<k, v>, maybe<v>)
  match m
    Map(tree, cmp) ->
      val (t, elem) = tree.delete(cmp.hint(key))
      (Map(t, cmp), elem.map(snd))

// Merges two maps. Merges common values using the given function.
//
// Uses the comparator from the left map.
pub fun merge(lhs : rbmap<k, v>, rhs : rbmap<k, v>, merge : (v, v) -> v) : rbmap<k, v>
  match (lhs, rhs)
    (Map(lt, cmp), Map(rt, _)) -> Map(
      union(lt, rt, cmp.by-key, fn((kl, vl), (_, vr))
        (kl, merge(vl, vr))
      ),
    cmp)

// Pretty-prints a map using the given printers.
pub fun show(m : rbmap<k, v>, show-k : k -> string, show-v : v -> string) : string
  "{" ++ m.items.map(fn((k, v))
    show-k(k) ++ ": " ++ show-v(v)
  ).join(", ") ++ "}"
