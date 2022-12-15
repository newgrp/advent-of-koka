import std/os/env
import std/os/file
import std/os/path
import std/text/parse

// Aborts computation with an error message.
pub effect abort
  ctl abort(msg : string) : a

// Returns an `abort` handler that adds extra context to messages then re-aborts
// them.
fun ctx(msg : string, action : () -> <abort,abort|e> a) : <abort|e> a
  with ctl abort(m)
    abort(msg ++ ": " ++ m)
  action()

// Extracts the value from a `maybe` or aborts.
pub fun expect(m : maybe<a>, msg : string) : abort a
  match m
    Just(x) -> x
    Nothing -> abort(msg)

// Extracts the left value from an `either` or aborts.
pub fun expect-left(e : either<a, b>, msg : string) : abort a
  match e
    Left(a) -> a
    Right(_) -> abort(msg)

// Extracts the right value from an `either` or aborts.
pub fun expect-right(e : either<a, b>, msg : string) : abort b
  match e
    Left(_) -> abort(msg)
    Right(b) -> b

// Extracts the value from a `parse-error` or aborts.
pub fun expect(e : parse-error<a>, msg : string) : abort a
  with ctx(msg)
  match e
    ParseOk(result, _) -> result
    ParseError(m, rest) -> abort(m ++ " [remaining string \"" ++ rest.string ++ "\"]")

// Runs `main`, handling any exceptions and exit calls.
pub fun run-main(main : (string) -> <abort,console,exn> ()) : <console,fsys,ndet> ()
  with ctl abort(msg)
    println(msg)
  with ctl throw-exn(x : exception)
    abort(x.show)
  match get-args()
    [filename] -> main(read-text-file(filename.path))
    _ -> abort("Input file must be provided")
