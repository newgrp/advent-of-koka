import aok/set
import aok/testing
import std/num/int32
import std/num/random

// A random list of distinct int32s.
fun random-list-shorter-than(maxsize : int) : random list<int32>
  val size = random-int() % maxsize
  list(1, size * 2).map(fn(_)
    random-int32()
  ).foldl([], fn(curr : list<int32>, x)
    if curr.length >= size || curr.any(fn(y) x == y)
      then curr
      else Cons(x, curr)
  )

// Expects a set to contain an element.
fun expect-contains(s : rbset<int32>, x : int32) : testcase ()
  expect(s.contains(x), s.show ++ " does not contain " ++ x.show)

// Expects a set to contain an element.
fun expect-not-contains(s : rbset<int32>, x : int32) : testcase ()
  expect(!s.contains(x), s.show ++ " contains " ++ x.show)

// Expects that a set can insert an element.
fun expect-can-insert(s : rbset<int32>, x : int32) : testcase()
  expect(s.try-insert(x).snd, s.show ++ " cannot insert " ++ x.show)

// Expects that a set cannot insert an element.
fun expect-cannot-insert(s : rbset<int32>, x : int32) : testcase()
  expect(!s.try-insert(x).snd, s.show ++ " can insert " ++ x.show)

// Expects a set to have a given size.
fun expect-size(s : rbset<int32>, n : int) : testcase ()
  expect(
    s.size == n,
    s.show ++ " has size " ++ s.size.show ++ " not " ++ n.show
  )

// Expects two sets to be equal.
fun expect-eq(lhs : rbset<int32>, rhs : rbset<int32>) : testcase ()
  expect(
    lhs.size == rhs.size && zip(lhs.list, rhs.list).all(fn((a, b)) a == b),
    lhs.show ++ " does not equal " ++ rhs.show
  )

fun test-newset-is-empty()
  with subtest("newset is empty")

  val x = 0.int32
  expect-not-contains(newset-int32(), x)

fun test-rbset-from-list()
  with subtest("construction from list works")

  val x = 0.int32
  val y = 1.int32
  val z = 2.int32

  val s = [x, y, z].rbset
  expect-contains(s, x)
  expect-contains(s, y)
  expect-contains(s, z)

fun test-rbset-from-list-random()
  with subtest("construction from random list works")

  val xs = random-list-shorter-than(20)
  val s = xs.rbset
  expect-size(s, xs.length)
  xs.foreach(fn(n) expect-contains(s, n))

fun test-insert-adds-elements()
  with subtest("insert adds elements")

  val x = 0.int32
  val y = 1.int32
  val z = 2.int32

  val s1 = newset-int32().insert(x)
  expect-contains(s1, x)
  expect-not-contains(s1, y)
  
  val s3 = newset-int32().insert(y).insert(z).insert(x)
  expect-contains(s3, x)
  expect-contains(s3, y)
  expect-contains(s3, z)

fun test-insert-is-idempotent()
  with subtest("insert is idempotent")

  val x = 0.int32
  val s = newset-int32().insert(x)
  val ss = s.insert(x)
  expect-eq(s, ss)

fun test-try-insert-reports-existing-elements()
  with subtest("try-insert reports existing elements")

  val x = 0.int32
  val y = 1.int32
  val s = newset-int32().insert(x)
  expect-cannot-insert(s, x)
  expect-can-insert(s, y)

fun test-random-inserts()
  with subtest("random inserts")

  val xs = random-list-shorter-than(20)
  val s = xs.foldl(newset-int32(), fn(s, n)
    expect-not-contains(s, n)
    expect-can-insert(s, n)
    s.insert(n)
  )
  expect-size(s, xs.length)
  xs.foreach(fn(n)
    expect-contains(s, n)
    expect-eq(s, s.insert(n))
    expect-cannot-insert(s, n)
  )

fun test-delete-removes-elements()
  with subtest("delete removes elements")

  val x = 0.int32
  val y = 1.int32
  val z = 2.int32

  val s = newset-int32().insert(x).insert(y).insert(z)

  val s1 = s.delete(x)
  expect-not-contains(s1, x)
  expect-contains(s1, y)

  val s2 = s.delete(x).delete(z)
  expect-not-contains(s1, x)
  expect-contains(s1, y)
  expect-not-contains(s1, z)

fun test-delete-is-idempotent()
  with subtest("delete is idempotent")

  val x = 0.int32
  val y = 1.int32
  val z = 2.int32

  val s = newset-int32().insert(x).insert(y).insert(z)

  val s1 = s.delete(x)
  val s2 = s1.delete(x)
  expect-eq(s2, s1)

fun test-try-delete-reports-absent-elements()
  with subtest("delete is idempotent")

  val x = 0.int32
  val y = 1.int32
  val z = 2.int32

  val s = newset-int32().insert(x).insert(y).insert(z)

  val (_, test1) = s.try-delete(x)
  expect(test1.is-just)

  val (s2, test2) = s.try-delete(4.int32)
  expect(test2.is-none)
  expect-eq(s2, s)

fun test-union-contains-all-elements-once()
  with subtest("union contains all elements once")

  val x = 0.int32
  val y = 1.int32
  val z = 2.int32

  val l = newset-int32().insert(x).insert(y)
  val r = newset-int32().insert(z).insert(y)
  val u = l || r

  with subctx("union = " ++ u.debug)
  expect-contains(u, x)
  expect-contains(u, y)
  expect-contains(u, z)
  expect-size(u, 3)

fun test-random-unions()
  with subtest("random unions")

  val x-only = random-list-shorter-than(20)
  val x-and-y = random-list-shorter-than(20)
  val y-only = random-list-shorter-than(20)

  val xs = x-only ++ x-and-y
  val ys = x-and-y ++ y-only

  val x-set = xs.foldl(newset-int32(), fn(s, n) s.insert(n))
  val y-set = ys.foldl(newset-int32(), fn(s, n) s.insert(n))
  val u-set = x-set || y-set

  expect-size(u-set, x-only.length + x-and-y.length + y-only.length)
  x-only.foreach(fn(e)
    with subctx("x-only")
    expect-contains(u-set, e)
  )
  x-and-y.foreach(fn(e)
    with subctx("x-and-y")
    expect-contains(u-set, e)
  )
  y-only.foreach(fn(e)
    with subctx("y-only")
    expect-contains(u-set, e)
  )


fun tests-all()
  with subctx("Set tests")
  with strong-random
  test-newset-is-empty()
  test-insert-adds-elements()
  test-insert-is-idempotent()
  test-try-insert-reports-existing-elements()
  test-random-inserts()
  test-union-contains-all-elements-once()
  test-random-unions()

fun main()
  testmain(tests-all)