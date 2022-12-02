import std/os/env
import std/os/file
import std/os/path

// Aborts computation with an error message.
effect abort
  ctl abort(msg : string) : a

// Runs `main`, handling any exceptions and exit calls.
fun run-main(main : (string) -> <abort,console,exn> ()) : <console,fsys,ndet> ()
  with ctl abort(msg)
    println(msg)
  with ctl throw-exn(x : exception)
    abort(x.show)
  match get-args()
    [filename] -> main(read-text-file(filename.path))
    _ -> abort("Input file must be provided")
