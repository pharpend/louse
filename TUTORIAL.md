# decamp usage

This is a tutorial for using the `decamp` command line program. You can look at
the actual file, or access this by running `decamp tutorial`.

Decamp follows the UNIX philosophy of doing one thing and doing it well. Decamp
tracks bugs. It doesn't interface with your version control system. This is done
for two reasons:

1. It makes the code simpler.
2. It makes decamp more portable. You can use decamp with any version control
   system, or with no version control system if you want.

**This tutorial is not finished**

## Getting started

First of all, if you haven't already,
[install decamp](https://github.com/pharpend/decamp#installation).

Let's make a new git repo. You don't need to use a version control system with
decamp, but I would imagine that most people want to, so I'm including it in the
tutorial.

    ~ $ mkdir my_project
    ~ $ cd my_project
    ~/my_project $ git init
    Initialized empty Git repository in /home/pete/my_project/.git/
    ~/my_project $

We're going to write "Hello, world in Haskell". 

    ~/my_project $ cat > hello.hs
    main = print "hello, world"
    ~/my_project $

(Hit `C-d` to get out of `cat`). Now, let's compile and run it:

    ~/my_project $ ghc hello.hs
    ~/my_project $ ./hello
    "hello, world"
    ~/my_project $ 

Okay, cool. It works! Sort of. It prints out `"hello, world"`. Ideally, we would
want it to print out `hello, world`, without the quotes, but this works for the
time being.

Let's commit what we have so far

    ~/my_project $ git add hello.hs
    ~/my_project $ git commit -m "Initial commit, contains Haskell hello world"
    [master (root-commit) ab3e586] Initial commit, contains Haskell hello world
    1 file changed, 1 insertion(+)
    create mode 100644 hello.hs
    ~/my_project $

There are a couple of issues so far:

1.  If you are a Haskell programmer, you're likely pulling your hair out at the
    poor practices exhibited in that code.

2.  GHC likes to litter the working directory with garbage files. 

        ~/my_project $ ls
        hello hello.hi hello.hs hello.o
        ~/my_project $ 

    If you aren't really careful with your staging area, you might accidentally
    commit these garbage files.

        ~/my_project $ git status
        On branch master
        Untracked files:
        (use "git add <file>..." to include in what will be committed)

            hello
            hello.hi
            hello.o

        nothing added to commit but untracked files present (use "git add" to track)
        ~/my_project $

    There are any number of solutions here:

    1.  Put the file paths in a `.gitignore`. This will solve the git problem,
        but there will still be garbage files.
    2.  Use `cabal` to manage the project. This will solve both problems, but it
        will be somewhat cumbersome.
    3.  Use `runhaskell` instead of GHC.

    The list goes on and on.

At this point, we have code and some bugs. The bugs in question are pretty
trivial, but they're good enough for our demonstration.

## Breaking out decamp

Let's initialize decamp!

    ~/my_project $ decamp init
    Alright, I'm going to ask you some questions about your project. If you make a mistake, type C-c C-c to cancel and run this command again.

    1. What is this project's name? (Will default to my_project) 
    2. What's the name of the project maintainer? (Leave empty for anonymous) 
    3. What's the email of the project maintainer? (Leave empty for anonymous) 
    4. If the project has a home page, what is it? (Leave empty for no home page) 
    5. Give a one-line description of the project (Leave empty for no description): 
    ~/my_project $

You can answer the questions if you want. I left them blank, because I'm
lazy. You can always change them later.

    ~/my_project $ git status
    On branch master
    Untracked files:
    (use "git add <file>..." to include in what will be committed)

        .decamp/
        hello
        hello.hi
        hello.o

    nothing added to commit but untracked files present (use "git add" to track)
    ~/my_project $

You'll notice that decamp created the `.decamp` directory. Let's look in the
`.decamp` directory.

    ~/my_project $ ls -A .decamp
    project.yml
    ~/my_project $ 

Let's see what's in `project.yml`:

```yaml
project-name: ''
project-description: ''
project-homepage: ''
project-bugs: []
project-maintainer:
    person-name: ''
    person-email: ''
```

If you want to change any of those values, feel free to do so. If you want a
list of the schemata, you can run `decamp list-schemata`.
