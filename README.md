# louse

**Louse is unfinished. This document is for when it is finished.**

louse is a very simple distributed bug-tracking system. It literally
stores bug tracking information in a series of YAML files.

louse is licensed under the
[GNU General Public License, version 3](https://gnu.org/licenses/gpl). A
copy of the license can be found in the LICENSE file.

## Installation and Usage

Louse is written in Haskell. Because I haven't set up a good build
system yet, you need to [install the Haskell platform][0] before
installing .

**Note**: as of 2015-05-05, the latest Haskell platform ships with GHC
  7.8.3. Louse only supports GHC >=7.10, so you might have to
  [install GHC manually][1].

You can use `cabal` to install this

    git clone https://github.com/pharpend/louse.git
    cd louse
    cabal install

I've included a Makefile and configure script so that UNIX users can
have the satisfaction of typing

    ./configure && make && sudo make install

instead of `cabal install`

For a tutorial, you can see the [TUTORIAL.md file](TUTORIAL.md), or run
`louse --tutorial`. For a brief listing of the available commands, you
can run `louse --help`.

    louse v.0.1.0.0
    
    Usage: louse (--copyright | --license | --readme | --tutorial | --version |
                 COMMAND | COMMAND | COMMAND | COMMAND | COMMAND)
      A distributed bug tracker.
    
    Available options:
      -h,--help                Show this help text
      --copyright              Print the copyright.
      --license                Print the license (GPL version 3).
      --readme                 Print the README.
      --tutorial               Print the tutorial.
      --version                Print the version (0.1.0.0).
    
    Available commands:
      bug                      Do stuff with bugs.
      init                     Initialize louse.
      schema                   Do stuff with schemata.
      schemata                 Do stuff with schemata.
      status                   Initialize louse.

    For information on a specific command, run `louse COMMAND --help`, where COMMAND
    is one of the commands listed above.

## Inspiration

If you're developing open-source software, there are, in broad terms, 3
components:

1. The actual code
2. The bug tracker
3. Merge requests

I'm sure I'm forgetting something here.

The recent
[DDOS attack on GitHub](https://github.com/blog/1981-large-scale-ddos-attack-on-github-com)
revealed a sore point with distributed version control workflows: even
though the code itself is distributed, often the bug tracker is hosted
on a central server, which could go down. This is an effort to further
distribute the standard development workflow.

## Milestones

These are liable to change, but this is what I have so far.

### 0.1

1.  The following commands need to be implemented:
    
    * ~~`louse bug add`~~
    * ~~`louse bug close`~~
    * ~~`louse bug comment`~~ ish
    * ~~`louse bug edit`~~
    * ~~`louse bug delete`~~
    * ~~`louse bug list`~~
    * ~~`louse bug show`~~

2. The following commands need to be removed:
   
   * ~~`louse ppl`~~
   * ~~`louse schemata`~~

3. Rewrite the tutorial

4.  Replace `louse bug list` and `louse bug show` with a more flexible
    `louse query`.

    Additonally, factor out the query to be a central part of the
    `Read.hs` file, rather than just stuck on in `Bugs.hs`.

    The query should use the familiar SQL syntax

5.  Add a `louse config` command to edit the configuration file.

6.  Add support for common features like relations, tagging, and
    attachments.

### 0.3

Those which exist:

* Add support for GitHub issues API
* Add support for BitBucket issues API
* Add support for git.gnu.io issues API
* Add support for GitLab.com issues API
* Add support for Launchpad issues API
* Add support for Bugzilla issues API
* Add support for Debbugs API

### 1.0

1.  Proper bash completion. optparse-applicative claims to have it, but it
    doesn't. It lies!
2.  A plugin system. This would include support for custom issue tracker
    APIs.
3.  Support for indexing and marshaling to common database
    software. (the *SQLs).

## Schemata

The schemata for the YAML files are stored in the `res/schemata/`
directory.

You can also access the schemata with the `louse` tool.

    $ louse schemata list
    bug
    comment
    person
    project
    $ louse schema show comment
    name: comment
    type: object
    properties:
      comment-person:
        required: false
        type: string
        description: The id of the person associated with this comment.
      comment-text:
        required: true
        type: string
        description: The comment itself

## Contact

Email: <peter@harpending.org>
IRC: `pharpend` on FreeNode and OFTC.

[0]: https://github.com/bitemyapp/learnhaskell#getting-set-up
[1]: https://www.haskell.org/ghc/download_ghc_7_10_1#binaries
