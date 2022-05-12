# Beep Boop Slack Learns OCaml

## Copying braindumps here from Slack
These need reformatting from Slack's braindead copy+paste!

OCAML TOOLS and SETUP
First of all, OCaml tooling sucks on Windows. There are a bunch of half-baked distributions most of which depend on Cygwin - suck, suck suck. If you're on Windows, this is a good excuse to set up WSL2 (Ubuntu is fine, or you can pick whatever distribution you prefer - "I use Arch btw").On Mac or Linux, things should be more straightforward. The instructions here are probably the way to go: https://ocaml.org/docs/up-and-running
They're going to suggest using a tool called opam, the OCaml Package Manager. Think of it as, depending on your background, some variation on npm, cargo + rustup, stack, dotnet, conda (misleading! misleading! death to Conda!) or what have you.Anyhow, once you've got opam installed, you can use it to install and manage your OCaml toolchain as described in the article.The second [CONNECTION TERMINATED: child screaming in background]

OCAML TOOLS and SETUP (cont'd)
... anyhow, there are a few tools which are going to come up over and over. We've covered opam: it's used like (part of) Rust's cargo and rustup or Node's npm: to install packages specific to OCaml, including the OCaml tools themselves as well as third-party libraries/packages.The second tool that's going to come up, although probably not until Real World OCaml, is dune. I love dune. I've never really used it, but the name is great. How could you not love dune? The spice must flow!dune is sort of like the other half of Rust's cargo, .NET's dotnet, or maybe like - uh - webpack if you're a survivor of Javascript Hell. It's a build system tailored for OCaml. We'll all learn about it together! Later. Because we're not going to need it for a while.The third tool which will come up is utop, which can be installed using opam. I'll talk about that in a minute.

The reason we don't care about dune yet is that we want to understand the basic tools of OCaml first: the interactive interpreter / REPL (called just ocaml), the bytecode compiler (called ocamlc), and the native "optimizing" compiler (called ocamlopt).ocaml-the-REPL kind of sucks. REPLs are great in general, and it has some nice features (similar to Haskell's GHCi, but not as nice) for querying types of expressions and so on, but it lacks good history or readline support and generally is just kind of janky. Enter utop! utop is a much nicer interactive REPL for OCaml: it has tab completion, history (including edit-and-re-evaluate of multiline history entries), color-coding, etc.

So especially for the OftVB book, we'll spend a lot of time in utop. But what about good old-fashioned writing code in a file and compiling it? No problem. OCaml has two distinct runtime systems: one is a bytecode interpreter (actually rather like Python, when you think about it) and the other is a native runtime. Both include garbage collection, standard library, interfaces to the OS, etc etc. Any OCaml program should be easily compiled to run on either. The historical pitch for the bytecode backend has been that compilation is fast and the result is "portable", which is bullshit, because I can't find a Windows OCaml bytecode runtime (ocamlrun is the program's name) worth a damn.Anyway, the native code compiler is slower to run, but produces native executables for your OS and architecture. On my OS (Arch on WSL) these binaries statically link all the OCaml bits and only have runtime dependencies on the system C runtime (libc) and math library (libm) - your mileage may vary. I'm guessing we have finer-grained control over this through command-line options, which is one thing I'm hoping we get to learn about!Amusingly, since these compilers are themselves OCaml programs, they can be compiled with either compiler, meaning there are really four tools (in my installation, at least):

    ocamlc.byte, a bytecode version of the bytecode compiler
    ocamlc.opt, a native version of the bytecode compiler, to which ocamlc is a symlink
    ocamlopt.byte, a bytecode version of the native compiler
    ocamlopt.opt, a native version of the native compiler, to which ocamlopt is a symlink

Any of these can be used to compile source code on disk like so:

[derrick@ALGEBRAIST scratch]$ cat >hello.ml <<END
> let () = print_string "hello world\n"
> END
[derrick@ALGEBRAIST scratch]$ ocamlc -o hello_byte hello.ml
[derrick@ALGEBRAIST scratch]$ ./hello_byte
hello world
[derrick@ALGEBRAIST scratch]$ ocamlopt -o hello_opt hello.ml
[derrick@ALGEBRAIST scratch]$ ./hello_opt
hello world

OCAML RESOURCES AND DOCUMENTATION
Here are some extra references we may want along the way. I found most of this stuff off the new improved "official" OCaml site which is, full disclosure, pretty enough that my brain latched onto the shiny object and decided "let's learn OCaml for real this time". (https://ocaml.org/)First, the OCaml language manual (https://v2.ocaml.org/releases/4.14/htmlman/index.html). It's fine! I've used it a few times to look up really detailed trivia without a clue about the broader language, way back when while consulting on a language design project. (I wanted to see, specifically, how BuckleScript compiled GADTs, IIRC, and thus needed to know how to write an example program using them.) Chapter 9 and 10 do most of the heavy lifting. Amusingly, Chapter 25, "The core library", covers something entirely different than Jane Street's Core library which we may get into with RWO.The official site also has a search bar for packages (probably the same as those installable from opam?). The standard library API reference is also finally really nice and searchable, including by types signature (https://v2.ocaml.org/releases/4.14/api/index.html). This is a cool feature which (maybe?) originated with the famous Haskell search engine "Hoogle". Just type something like string -> string -> string into the box and watch things like the search-and-replace and concatenation functions appear. (This search by type capability is, no shit, the #1 reason I think Python programmers are delusional - how can you search those docs? I can pretty reliably guess the type of a function if I know what it does - guessing what some rando named it is much harder!)Of course, that only covers the standard standard library (Stdlib). For Jane Street's libraries, we'll have to hit their (allegedly much poorer) documentation at https://v3.ocaml.org/p/base/v0.15.0/doc/index.html (and presumably somewhere similar for Core).

OK, that's (almost) enough spam! To recap, we'll be going throughOCAML BOOKS
OCaml from the Very Beginning (https://johnwhitington.net/ocamlfromtheverybeginning/index.html)followed by (parts or all of)Real World OCaml (https://dev.realworldocaml.org/toc.html)
