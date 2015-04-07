# decamp usage

This is a tutorial for using the `decamp` command line program.

First of all, if you haven't already,
[install decamp](https://github.com/pharpend/decamp#installation).

Let's make a new git repo

    ~ $ mkdir my_project
    ~ $ cd my_project
    ~/my_project $ git init
    ~/my_project $

We're going to write "Hello, world in Haskell". 

    ~/my_project $ cat > hello.hs
    main = print "hello, world"
    ~/my_project $

(Hit `C-d` to get out of `cat`)
Now, let's compile and run it:

    ~/my_project $ ghc hello.hs
    ~/my_project $ ./hello
    "hello, world"

Okay, cool. It works! Sort of. It prints out `"hello, world"`. Ideally, we would
want it to print out `hello, world`, without the quotes. The fix for this is
trivial, but, just for the hell of it, let's open a bug for the issue.

First, we need to initialize decamp

```
~/my_project $ decamp init
```
