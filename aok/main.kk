import std/os/env
import std/os/file
import std/os/path

// Aborts computation with an error message.
pub effect abort
  ctl abort(msg : string) : a

// Returns an `abort` handler that adds extra context to messages then re-aborts
// them.
fun ctx(msg : string, action : () -> <abort,abort|e> a) : <abort|e> a
  with ctl abort(m)
    abort(msg ++ m)
  action()

// Extracts the value from a `maybe` or aborts.
pub fun expect(m : maybe<a>, msg : string) : abort a
  match m
    Just(x) -> x
    Nothing -> abort(msg)

// Runs `main`, handling any exceptions and exit calls.
pub fun run-main(main : (string) -> <abort,console,exn> ()) : <console,fsys,ndet> ()
  with ctl abort(msg)
    println(msg)
  with ctl throw-exn(x : exception)
    abort(x.show)
  match get-args()
    [filename] -> main(read-text-file(filename.path))
    _ -> abort("Input file must be provided")
