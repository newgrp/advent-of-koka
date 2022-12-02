// Aborts computation with an error message.
effect abort
  ctl abort(msg : string) : a

// Runs `main`, handling any exceptions and exit calls.
fun run-main(main : () -> <abort,console,exn|e> ()) : <console|e> ()
  with ctl abort(msg)
    println(msg)
  with ctl throw-exn(x : exception)
    abort(x.show)
  main()
