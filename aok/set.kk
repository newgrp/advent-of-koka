import aok/redblack
import std/num/float64
import std/num/int32
import std/num/int64

// A set of values of the same type.
pub value type rbset<a>
  Set(
    tree : rbtree<a>,
    cmp : (a, a) -> order
  )

// Returns an empty set.
pub fun newset(cmp : (a, a) -> order) : rbset<a>
  Set(Leaf, cmp)

pub fun newset-char() : rbset<char>
  newset(compare : (char, char) -> order)

pub fun newset-float64() : rbset<float64>
  newset(compare : (float64, float64) -> order)

pub fun newset-int() : rbset<int>
  newset(compare : (int, int) -> order)

pub fun newset-int32() : rbset<int32>
  newset(compare : (int32, int32) -> order)

pub fun newset-int64() : rbset<int64>
  newset(compare : (int64, int64) -> order)

pub fun newset-string() : rbset<string>
  newset(compare : (string, string) -> order)

// Constructs a set from a list.
pub fun rbset(xs : list<a>, cmp : (a, a) -> order) : rbset<a>
  Set(xs.rbtree(cmp), cmp)

pub fun rbset(xs : list<char>) : rbset<char>
  xs.rbset(compare : (char, char) -> order)

pub fun rbset(xs : list<float64>) : rbset<float64>
  xs.rbset(compare : (float64, float64) -> order)

pub fun rbset(xs : list<int>) : rbset<int>
  xs.rbset(compare : (int, int) -> order)

pub fun rbset(xs : list<int32>) : rbset<int32>
  xs.rbset(compare : (int32, int32) -> order)

pub fun rbset(xs : list<int64>) : rbset<int64>
  xs.rbset(compare : (int64, int64) -> order)

pub fun rbset(xs : list<string>) : rbset<string>
  xs.rbset(compare : (string, string) -> order)

// A debug string showing the internals of a set.
pub fun debug(set : rbset<a>, show : a -> string) : string
  match set
    Set(tree, _) -> "Set(" ++ tree.debug(show) ++ ")"

pub fun debug(set : rbset<char>) : string
  set.debug(show)

pub fun debug(set : rbset<int>) : string
  set.debug(show)

pub fun debug(set : rbset<int32>) : string
  set.debug(show)

pub fun debug(set : rbset<int64>) : string
  set.debug(show)

pub fun debug(set : rbset<string>) : string
  set.debug(show)

// The size of the set.
pub fun size(set : rbset<a>) : int
  match set
    Set(tree, _) -> tree.size

// A list of the elements of a set in increasing order.
pub fun list(set : rbset<a>) : list<a>
  match set
    Set(tree, _) -> tree.node-values

// Determines whether a set contains an element.
pub fun contains(set : rbset<a>, key : a) : bool
  match set
    Set(tree, cmp) -> tree.find(fn(v) cmp(key, v)).is-just

// Inserts an element into a set, if it doesn't already exist.
pub fun insert(set : rbset<a>, key : a) : rbset<a>
  set.try-insert(key).fst

// Inserts an element into a set. Returns false in the second value if the
// element already existed.
pub fun try-insert(set : rbset<a>, key : a) : (rbset<a>, bool)
  match set
    Set(tree, cmp) -> 
      val (t, prev) = tree.insert(key, cmp, False)
      (Set(t, cmp), prev.is-nothing)

// Deletes an element from a set, if it exists.
pub fun delete(set : rbset<a>, key : a) : rbset<a>
  set.try-delete(key).fst

// Deletes an element from a set.
//
// Returns the deleted element, if it was found.
pub fun try-delete(set : rbset<a>, key : a) : (rbset<a>, maybe<a>)
  match set
    Set(tree, cmp) ->
      val (t, elem) = tree.delete(fn(v) cmp(key, v))
      (Set(t, cmp), elem)

// Takes the union of two sets. Uses the comparator from the left set.
pub fun (||)(lhs : rbset<a>, rhs : rbset<a>) : rbset<a>
  match (lhs, rhs)
    (Set(lt, cmp), Set(rt, _)) -> Set(union(lt, rt, cmp, fn(l, r) l), cmp)

// Pretty-prints a set using the given element printer.
pub fun show(set : rbset<a>, show : a -> string) : string
  "{" ++ set.list.map(show).join(", ") ++ "}"

pub fun show(set : rbset<char>) : string
  set.show(show)

pub fun show(set : rbset<int>) : string
  set.show(show)

pub fun show(set : rbset<int32>) : string
  set.show(show)

pub fun show(set : rbset<int64>) : string
  set.show(show)

pub fun show(set : rbset<string>) : string
  set.show(show)

// Takes the union of a list of sets.
pub fun union(ss : list<rbset<a>>, cmp : (a, a) -> order) : rbset<a>
  ss.foldl(newset(cmp), (||))
