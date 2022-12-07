import std/num/float64
import std/num/int32
import std/num/int64

fun lmap(e : either<a, b>, f : a -> e c) : e either<c, b>
  match e
    Left(a) -> Left(f(a))
    Right(b) -> Right(b)

type color
  con Black
  con Red

fun debug(color : color) : string
  match color
    Black -> "Black"
    Red -> "Red"

fun op(color : color) : color
  match color
    Black -> Red
    Red -> Black

// A red black tree.
type rbtree<a>
  con Leaf
  con Node(
    color : color,
    value : a,
    left : rbtree<a>,
    right : rbtree<a>
  )

// Changes the tree to a black node if it is not a leaf.
fun as-black(tree : rbtree<a>) : rbtree<a>
  match tree
    Leaf -> Leaf
    Node(_, value, left, right) -> Node(Black, value, left, right)

// A debug string showing the internal red-black tree structure.
fun debug(tree : rbtree<a>, show : a -> string) : string
  match tree
    Leaf -> "Leaf"
    Node(color, value, left, right) ->
      "Node(" ++ [
        color.debug,
        show(value),
        left.debug(show),
        right.debug(show)
      ].join(", ") ++ ")"

fun size-internal(tree : rbtree<a>) : div int
  match tree
    Leaf -> 0
    Node(_, _, left, right) -> 1 + left.size-internal + right.size-internal

// Returns the number of nodes in a red-black tree.
fun size(tree : rbtree<a>) : int
  unsafe-no-div(fn() tree.size-internal)

fun find-internal(tree : rbtree<a>, key : a, cmp : (a, a) -> order) : div maybe<a>
  match tree
    Leaf -> Nothing
    Node(_, value, left, right) -> match cmp(key, value)
      Lt -> left.find-internal(key, cmp)
      Eq -> Just(value)
      Gt -> right.find-internal(key, cmp)

// Searches a red-black tree for a node.
fun find(tree : rbtree<a>, key : a, cmp : (a, a) -> order) : maybe<a>
  unsafe-no-div(fn() tree.find-internal(key, cmp))

// Balances a black node of red-black that may have red child and red grandchild
// of that child. The red grandchild may also have a red child.
fun balance(tree : rbtree<a>) : rbtree<a>
  match tree
    // Along left spine.
    Node(Black, z, Node(Red, y, Node(Red, x, a, b), c), d) ->
      Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))

    // Left then right.
    Node(Black, z, Node(Red, x, a, Node(Red, y, b, c)), d) ->
      Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
    
    // Right then left.
    Node(Black, x, a, Node(Red, z, Node(Red, y, b, c), d)) ->
      Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))

    // Along right spine.
    Node(Black, x, a, Node(Red, y, b, Node(Red, z, c, d))) ->
      Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))

    _ -> tree

fun insert-internal(
  tree : rbtree<a>,
  key : a,
  cmp : (a, a) -> order
) : div either<rbtree<a>, a>
  match tree
    Leaf -> Left(Node(Red, key, Leaf, Leaf))
    Node(color, value, left, right) -> match cmp(key, value)
      Lt -> left.insert-internal(key, cmp).lmap(fn(l)
        Node(color, value, l, right).balance
      )
      Eq -> Right(value)
      Gt -> right.insert-internal(key, cmp).lmap(fn(r)
        Node(color, value, left, r).balance
      )

// Inserts an element into a red-black tree.
fun insert(
  tree : rbtree<a>,
  key : a,
  cmp : (a, a) -> order
) : either<rbtree<a>, a>
  unsafe-no-div(fn()
    tree.insert-internal(key, cmp)
  ).lmap(as-black)

// Unzips a red-black tree along the left spine.
fun unzip-left(
  tree : rbtree<a>,
  zipper : list<(color, a, rbtree<a>)>
) : div list<(color, a, rbtree<a>)>
  match tree
    Leaf -> zipper
    Node(color, value, left, right) -> (
      left.unzip-left(Cons((color, value, right), zipper))
    )

// Unzips a red-black tree along the right spine.
fun unzip-right(
  tree : rbtree<a>,
  zipper : list<(color, a, rbtree<a>)>
) : div list<(color, a, rbtree<a>)>
  match tree
    Leaf -> zipper
    Node(color, value, left, right) -> (
      right.unzip-right(Cons((color, value, left), zipper))
    )

// A site for joining two trees on a value.
struct joinsite<a>
  // A partial zipper of either the left or right tree along the opposite spine.
  // The other tree's zipper is implicitly `Nil`.
  zipper : either<list<(color, a, rbtree<a>)>, list<(color, a, rbtree<a>)>>

  // The rest of the left tree.
  left : rbtree<a>

  // The rest of the right tree.
  right : rbtree<a>

// Finds the site where two unzipped trees should be joined by zipping up their
// spine in parallel, matching up black nodes, until one entire tree has been
// reconstructed.
//
// This guarantees that the black-heights of the returned left and right trees
// are equal.
fun find-join-site(
  left-zipper : list<(color, a, rbtree<a>)>,
  left-tree : rbtree<a>,
  right-zipper : list<(color, a, rbtree<a>)>,
  right-tree : rbtree<a>
) : div joinsite<a>
  match (left-zipper, right-zipper)
    // Terminate at the end of either zipper.
    (_, Nil) -> Joinsite(Left(left-zipper), left-tree, right-tree)
    (Nil, _) -> Joinsite(Right(right-zipper), left-tree, right-tree)

    // Zip red nodes independently, but only if both zippers have nodes left.
    (_, Cons((Red, right-value, right-rchild), right-zipper1)) ->
      find-join-site(
        left-zipper,
        left-tree,
        right-zipper1,
        Node(Red, right-value, right-tree, right-rchild)
      )
    (Cons((Red, left-value, left-lchild), left-zipper1), _) ->
      find-join-site(
        left-zipper1,
        Node(Red, left-value, left-lchild, left-tree),
        right-zipper,
        right-tree
      )

    // Zip black nodes together.
    (
      Cons((Black, left-value, left-lchild), left-zipper1),
      Cons((Black, right-value, right-rchild), right-zipper1)
    ) ->
      find-join-site(
        left-zipper1,
        Node(Red, left-value, left-lchild, left-tree),
        right-zipper1,
        Node(Red, right-value, right-tree, right-rchild)
      )

// Zips a tree up the left spine, balancing at each step.
fun zip-balance-left(
  tree : rbtree<a>,
  zipper : list<(color, a, rbtree<a>)>
) : rbtree<a>
  zipper.foldl(tree, fn(curr, (color, value, right))
    Node(color, value, curr, right).balance
  )

// Zips a tree up the right spine, balancing at each step.
fun zip-balance-right(
  tree : rbtree<a>,
  zipper : list<(color, a, rbtree<a>)>
) : rbtree<a>
  zipper.foldl(tree, fn(curr, (color, value, left))
    Node(color, value, left, curr).balance
  )

// Joins two red-black trees on a given key. The input trees must be entirely
// less than and greater than the key, respectively.
fun join(left : rbtree<a>, key : a, right : rbtree<a>) : div rbtree<a>
  val Joinsite(zipper, left-part, right-part) = find-join-site(
    left.unzip-right(Nil),
    Leaf,
    right.unzip-left(Nil),
    Leaf
  )
  val join-base = Node(Red, key, left-part, right-part).balance
  match zipper
    Left(z) -> join-base.zip-balance-right(z).as-black
    Right(z) -> join-base.zip-balance-left(z).as-black

// Splits a red-black tree on a given key. The middle return value indicates
// whether the key existed in the original tree.
fun split(
  tree : rbtree<a>,
  key : a,
  cmp : (a, a) -> order
) : div (rbtree<a>, maybe<a>, rbtree<a>)
  match tree
    Leaf -> (Leaf, Nothing, Leaf)
    Node(_, value, left, right) -> match cmp(key, value)
      Lt ->
        val (left-left, m, left-right) = left.split(key, cmp)
        (left-left, m, join(left-right, value, right))
      Eq -> (left, Just(value), right)
      Gt ->
        val (right-left, m, right-right) = right.split(key, cmp)
        (join(left, value, right-left), m, right-right)

fun union-internal(
  left : rbtree<a>,
  right : rbtree<a>,
  cmp : (a, a) -> order,
  merge : (a, a) -> a
) : div rbtree<a>
  match right
    Leaf -> left
    Node(_, right-value, right-left, right-right) ->
      val (
        left-left,
        maybe-left-value,
        left-right
      ) = left.split(right-value, cmp)
      join(
        union-internal(left-left, right-left, cmp, merge),
        maybe-left-value.map(fn(x) merge(x, right-value)).default(right-value),
        union-internal(left-right, right-right, cmp, merge)
      )

// Returns the union of two red-black trees.
fun union(
  lhs : rbtree<a>,
  rhs : rbtree<a>,
  cmp : (a, a) -> order,
  merge : (a, a) -> a
) : rbtree<a>
  unsafe-no-div(fn() union-internal(lhs, rhs, cmp, merge))

fun node-values-internal(tree : rbtree<a>) : div list<a>
  match tree
    Leaf -> []
    Node(_, value, left, right) ->
      left.node-values-internal ++ [value] ++ right.node-values-internal

// Returns a list of the node values in a red-black tree.
fun node-values(tree : rbtree<a>) : list<a>
  unsafe-no-div(fn() tree.node-values-internal)

// A set of values of the same type.
pub type rbset<a>
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

// A debug string showing the internals of a set.
pub fun debug(set : rbset<a>, show : a -> string) : string
  match set
    Set(tree, _) -> "Set(" ++ tree.debug(show) ++ ")"

pub fun debug(set : rbset<int32>) : string
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
pub fun contains(set : rbset<a>, value : a) : bool
  match set
    Set(tree, cmp) -> tree.find(value, cmp).is-just

// Inserts an element into a set, if it doesn't already exist.
pub fun insert(set : rbset<a>, value : a) : rbset<a>
  set.try-insert(value).fst

// Inserts an element into a set. Returns false in the second value if the
// element already existed.
pub fun try-insert(set : rbset<a>, value : a) : (rbset<a>, bool)
  match set
    Set(tree, cmp) -> match tree.insert(value, cmp)
      Left(t) -> (Set(t, cmp), True)
      Right(_) -> (set, False)

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
