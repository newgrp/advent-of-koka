import aok/list
import aok/main
import aok/parse
import std/text/parse

// A valid `cd` command.
type cd-command
  con Root  // /
  con Up  // ..
  con Target(name : string)

// A filesystem node shown by `ls`.
type ls-entry
  con EntryFile(name : string, size : int)
  con EntryDir(name : string)

// A parsed shell command.
type command
  con Cd(cmd : cd-command)
  con Ls(nodes : list<ls-entry>)

// Parses a valid file or directory name.
fun filename() : parse string
  // I don't think the input will contain dashes and underscores, but let's be
  // flexible.
  chars-are("", fn(c)
    c.is-alpha || c.is-digit || c == '.' || c == '_' || c == '-'
  ).string

// Parses a `cd` command target.
fun cd() : parse cd-command
  val root = fn()
    lit("/")
    Root
  val up = fn()
    lit("..")
    Up
  val target = fn()
    Target(filename())

  choose([root, up, target])

// Parses a filesystem node returned by `ls`.
fun ls-entry() : parse ls-entry
  val file = fn()
    val size = pnat()
    lit(" ")
    val name = filename()
    EntryFile(name, size)
  val dir = fn()
    lit("dir ")
    EntryDir(filename())
  
  choose([file, dir])

// Parses a command.
fun command() : parse command
  val cd = fn()
    lit("cd ")
    Cd(cd())
  val ls = fn()
    lit("ls\n")
    Ls(separated(ls-entry, fn() lit("\n")))

  lit("$ ")
  choose([cd, ls])

// Parses the input text.
fun pinput() : parse list<command>
  val cmds = separated(command, fn() lit("\n"))
  whitespace0()
  cmds

// A node in a filesystem that is being mapped.
type fs-wip-node
  con WipFile(
    name : string,
    size : int
  )
  con WipDir(
    name : string,
    children : maybe<list<fs-wip-node>>
  )

// Constructs a WIP filesystem node from an `ls` entry.
fun to-fs(n : ls-entry) : fs-wip-node
  match n
    EntryFile(name, size) -> WipFile(name, size)
    EntryDir(name) -> WipDir(name, Nothing)

// A filesystem zipper.
struct fs-wip-zipper
  dirname : string
  left-siblings : list<fs-wip-node>
  right-siblings : list<fs-wip-node>

// Returns the current path as indicated by the zipper, without a trailing
// slash.
fun rootpath(zipper : list<fs-wip-zipper>) : string
  "/" ++ zipper.map(dirname).join("/")

// Zips up a filesystem to the parent directory.
fun zip-one(node : fs-wip-node, zip1 : fs-wip-zipper) : fs-wip-node
  WipDir(
    zip1.dirname,
    Just(zip1.left-siblings ++ [node] ++ zip1.right-siblings)
  )

// Zips up a filesystem from a starting node.
fun zip-all(node : fs-wip-node, zipper : list<fs-wip-zipper>) : fs-wip-node
  zipper.foldl(node, fn(n, z) n.zip-one(z))

// The current state of a walker mapping the filesystem.
struct fs-wip
  zipper : list<fs-wip-zipper>
  dirname : string
  children : maybe<list<fs-wip-node>>

// Returns the current path as indicated by the walker, without a trailing
// slash.
fun rootpath(fs : fs-wip) : string
  fs.zipper.rootpath ++ "/" ++ fs.dirname

// The current node of the walker.
fun node(fs : fs-wip) : fs-wip-node
  WipDir(fs.dirname, fs.children)

// Zips up a filesystem walker to the root.
fun zip-all(fs : fs-wip) : fs-wip-node
  fs.node.zip-all(fs.zipper)

// Applies a command to a filesystem walker.
fun apply(fs : fs-wip, cmd : command) : abort fs-wip
  with ctx("In " ++ fs.rootpath)
  match cmd
    Cd(Root) -> match fs.node.zip-all(fs.zipper)
      WipFile(_, _) -> abort("Root is a file")
      WipDir(name, children) -> Fs-wip([], name, children)
    Cd(Up) -> match fs.zipper
      // `cd ..` at `/` stays at `/`.
      Nil -> fs
      Cons(z, zs) -> match fs.node.zip-one(z)
        WipFile(_, _) -> abort("zip-one returned a file")
        WipDir(name, children) -> Fs-wip(zs, name, children)
    Cd(Target(t)) -> 
      val children = fs.children.expect(
        "Cannot cd into " ++ fs.dirname ++ " without prior ls"
      )
      match children.find-split(fn(ch) ch.name == t)
        Nothing -> abort("No such child " ++ t)
        Just((_, WipFile(_, _), _)) -> abort(
          "Cannot cd to " ++ t ++ ", is a file"
        )
        Just((left-siblings, WipDir(_, grandchildren), right-siblings)) ->
          Fs-wip(
            Cons(Fs-wip-zipper(
              fs.dirname,
              left-siblings,
              right-siblings
            ), fs.zipper),
            t,
            grandchildren
          )
    Ls(entries) -> match fs.children
      Nothing -> Fs-wip(fs.zipper, fs.dirname, Just(entries.map(to-fs)))
      Just(_) -> abort("Cannot ls in " ++ fs.dirname ++ " twice")

// A node in a fully mapped filesystem.
type fs-node<a>
  con File(
    name : string,
    size : int
  )
  con Dir(
    name : string,
    children : list<a>,
    total-size : int
  )

struct fs
  node : fs-node<fs>

// Returns the size or total size of the filesystem node.
fun total-size(fs : fs) : int
  match fs
    Fs(File(_, size)) -> size
    Fs(Dir(_, _, total-size)) -> total-size

fun finalize-internal(wip : fs-wip-node) : <abort,div> fs
  match wip
    WipFile(name, size) -> Fs(File(name, size))
    WipDir(name, children) ->
      val ch = children.expect(
        "Children are not populated"
      ).map(finalize-internal)
      Fs(Dir(name, ch, ch.map(total-size).sum))

// Expects all directories of the WIP filesystem to be populated.
fun finalize(wip : fs-wip-node) : abort fs
  unsafe-no-div(fn() wip.finalize-internal)

// Folds a filesystem using the given function.
fun fold(fs : fs, f : fs-node<a> -> e a) : e a
  match fs
    Fs(File(name, size)) -> f(File(name, size))
    Fs(Dir(name, children, size)) -> f(Dir(
      name,
      children.map(fn(ch) ch.fold(f)),
      size
    ))

fun exmain(input : string)
  val cmds = parse(input.slice, pinput).expect("Invalid input")
  val fs = cmds.foldl(Fs-wip([], "/", Nothing), apply).zip-all.finalize

  val total-up-to-tenk = unsafe-no-div(fn() fs.fold(fn(node)
    match node
      File(_, _) -> 0
      Dir(_, children, size) -> children.sum + (if size <= 100000
        then size
        else 0
      )
  ))

  println(total-up-to-tenk)

fun main()
  run-main(exmain)
