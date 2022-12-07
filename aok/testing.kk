import std/num/random

// An effect for reporting test failures.
pub effect testcase
  // Reports a test error.
  ctl error(msg : string) : ()

  // Fails the current test.
  ctl fail() : a

// Appends a context message to each error message.
pub fun subctx(
  msg : string,
  action : () -> <testcase,testcase|e> a
) : <testcase|e> a
  with handler
    ctl error(m)
      resume(error(msg ++ ": " ++ m))
    ctl fail()
      fail()
  action()

// Appends a context message to each error message and catches test failures.
pub fun subtest(
  msg : string,
  action : () -> <testcase,testcase|e> a
) : <testcase|e> maybe<a>
  with handler
    ctl error(m)
      resume(error(msg ++ ": " ++ m))
    ctl fail()
      Nothing
    return(x)
      Just(x)
  action()

// Fails the test with the given message if the condition is not true.
pub fun assert(cond : bool, msg : string = "Assertion failed") : testcase ()
  if !cond then
    error(msg)
    fail()

// Emits an error the given message if the condition is not true.
pub fun expect(cond : bool, msg : string = "Expectation failed") : testcase ()
  if !cond then error(msg)

// A handler for tests that prints errors to the console. Also catches
// exceptions and turns them into errors.
pub fun testmain(action : () -> <console,exn,testcase|e> a) : <console|e> int
  val ok =
    with handler
      ctl error(m)
        resume(println(m))
        False
      ctl fail()
        False
      return(_)
        True
    with ctl throw-exn(e)
      error(e.show)
      fail()
    action().ignore
  if !ok then println("TESTS FAILED")
  ok.int
