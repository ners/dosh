<img align="left" width="80" height="80" src="assets/dosh-minimal.svg" alt="dosh logo">

# dosh

The power of ~~capitalism~~ Haskell in your terminal!

## What have we got here?

`dosh` is a Haskell Read-Eval-Print Loop, or REPL for short.
While other REPLs for Haskell exist, this one aims to be good enough to replace Bash as a daily driver.

We offer:
- syntax highlighting
- advanced history interaction
- LSP-powered autocompletion and error detection

## Really? *Haskell* as a daily driver?

Why not? Haskell is an advanced functional programming language with an excellent blend of power and elegance that scales well as commands grow nontrivial.

> This is the Unix philosophy: Write programs that do one thing and do it well. Write
> programs to work together. Write programs to handle text streams, because that is a
> universal interface.

— <cite>Doug McIlroy</cite>

Aside from executing programs, an essential operation of the shell is to manipulate text streams that pass between programs.
Many programs output structured data, which Bash is [notoriously bad at handling](https://stackoverflow.com/a/45201229).

There are many alternatives to Bash, but they are all fundamentally boring shells. They tend to invent new domain specific languages which ultimately offer no real value as a programming language.

Instead of inventing a new shell language that can do slighty more than Bash, why not go the other way around and make an existing language usable as a shell?
And what language is more suitable than one that was quite literally invented as a testbed for novel uses such as this?

## Why is it named `dosh`?

Because our REPL has special handling of Haskell's [`do` notation](https://en.wikibooks.org/wiki/Haskell/do_notation).

In Haskell, the keyword `do` introduces a block of commands that evaluate sequentially and can depend on each other.
When the user enters a `do` block in `dosh`, the prompt changes to `do$`, which is also where the logo comes from.

I've also been advised to avoid overt references to Haskell in the name (e.g. `hashell`, `shellmonad`), as those might spook people.

## Prior art

This is not a novel idea, as evidenced by the abundance of Haskell libaries that provide shell primitives.
The only novelty of this project is a snazzy REPL around them.

- [turtle: Shell programming, Haskell-style](https://hackage.haskell.org/package/turtle)
- [shh: Simple shell scripting from Haskell](https://hackage.haskell.org/package/shh)
- [shelly: shell-like (systems) programming in Haskell](https://hackage.haskell.org/package/shelly)
- [ptGHCi](https://github.com/litxio/ptghci) is a high-powered REPL for Haskell, inspired by IPython
- [Using Haskell as my shell](https://las.rs/blog/haskell-as-shell.html) (2021) by Las Safin
- [Use Haskell for shell scripting](https://www.haskellforall.com/2015/01/use-haskell-for-shell-scripting.html) (2015) by Gabriella Gonzalez
