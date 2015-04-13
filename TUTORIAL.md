# louse usage

This is a tutorial for using the `louse` command line program. You can look at
the actual file, or access this by running `louse tutorial`.

Louse follows the UNIX philosophy of doing one thing and doing it well. Louse
tracks bugs. It doesn't interface with your version control system. This is done
for two reasons:

1. It makes the code simpler.
2. It makes louse more portable. You can use louse with any version control
   system, or with no version control system if you want.

I'm planning on writing another program,
[git-exterminate](https://github.com/pharpend/git-exterminate), which will
interface with git.

**This tutorial is not finished**

## Getting started

First of all, if you haven't already,
[install louse](https://github.com/pharpend/louse#installation).

I'm assuming that you use some sort of UNIX system, like Linux, BSD, OS X, or
some sort of emulator on Windows (like cygwin).

We're going to create a trivial coding project with some bugs, and use louse to
manage the bugs. I'm not going to mention version control systems here.

Let's create the project:

    ~ $ mkdir my_project
    ~ $ cd my_project
    ~/my_project $

We're going to write hello, world in Haskell. Create the file
`~/my_project/hello.hs` with the following contents:

```haskell
main = print "hello, world"
```

Let's run the program.

    ~/my_project $ ghc hello.hs
    ~/my_project $ ./hello
    "hello, world"
    ~/my_project $ 

Okay, cool. It works! Sort of. It prints out `"hello, world"`. Ideally, we would
want it to print out `hello, world`, without the quotes, but this works for the
time being.

There are a couple of issues so far:

1.  If you are a Haskell programmer, you're likely pulling your hair out at the
    poor practices exhibited in that code.

2.  GHC likes to litter the working directory with garbage files. 

        ~/my_project $ ls
        hello hello.hi hello.hs hello.o
        ~/my_project $ 

    There are two solutions I can think of:
    
    1.  Use `cabal` to manage the project. This will solve both problems, but it
        will be somewhat cumbersome.
    2.  Use `runhaskell` instead of GHC.

    The list goes on and on.

At this point, we have code and some bugs. The bugs in question are pretty
trivial, but they're good enough for our demonstration.

## Breaking out louse

Let's initialize louse!

    ~/my_project $ louse init
    Alright, I'm going to ask you some questions about your project. If you make a mistake, type C-c C-c to cancel and run this command again.
    1. What is this project's name? (Will default to my_project) 
    2. What's the name of the project maintainer? (Leave empty for anonymous) Peter Harpending
    3. What's the email of the project maintainer? (Leave empty for anonymous) peter@harpending.org
    4. If the project has a home page, what is it? (Leave empty for no home page) 
    5. Give a one-line description of the project (Leave empty for no description): Sample project to demonstrate louse.
    ~/my_project $ ls -A
    .louse hello hello.hi hello.hs hello.o
    ~/my_project $

You'll notice that louse created the `.louse` directory. Let's look in the
`.louse` directory.

    ~/my_project $ ls -A .louse
    project.json
    ~/my_project $ 

Let's see what's in `project.json`:

```json
{
  "project-name": "my_project",
  "project-description": "Sample project to demonstrate louse.",
  "project-bugs": [],
  "project-maintainer": {
    "person-name": "Peter Harpending",
    "person-email": "peter@harpending.org"
  }
}
```

You don't have to be familiar with JSON's syntax to figure out what the file
is, or to edit it. There's nothing to stop you from editing it, but I don't
recommend it. Instead, use the `louse` program to modify it, otherwise things
could get hairy.

Nevertheless, if you want to edit it, you can access the JSON schemata with the
`louse schemata` command, so you don't make any errors.

    ~/my_project $ louse schemata --help
    Usage: louse schemata (COMMAND | COMMAND | COMMAND | COMMAND)
      Do stuff with schemata.
    
    Available options:
      -h,--help                Show this help text
      -h,--help                Show this help text
    
    Available commands:
      dir                      Show the directory in which the schemata are stored
      list                     List the available schemata
      path                     Show the directory in which the schemata are stored
      show                     Show a specific schema.

    ~/my_project $ 

I have no idea why `-h` is shown as an option twice. I think it's a bug in the
option parsing library I used. Oh well.

Let's add a bug:

    ~/my_project $ louse bug new
