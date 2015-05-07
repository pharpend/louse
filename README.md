# louse

**Louse is unfinished. This document is for when it is finished.**

louse is a very simple distributed bug-tracking system. It literally
stores bug tracking information in a series of JSON files.

The schema for the JSON file is stored in the
`res/schemata/project.json`. The other schemata (for people, comments,
individual bugs, etc) are stored in the `res/schemata/` directory.

You can also access the schemata with the `louse` tool.

    $ louse schemata list
    bug
    comment
    person
    project
    $ louse schemata show comment
    {
      "name": "comment",
      "type": "object",
      "properties": {
        "comment-person": {
          "required": false,
          "type": "object",
          "description": "The person associated with this comment. Should fit the person schema."
        },
        "comment-text": {
          "required": true,
          "type": "string",
          "description": "The comment itself"
        }
      }
    }

louse is licensed under the
[GNU General Public License, version 3](https://gnu.org/licenses/gpl). A
copy of the license can be found in the LICENSE file.

## Installation and Usage

You need to install the Haskell platform before installing louse. Chris
Allen maintains an
[exhaustive guide](https://github.com/bitemyapp/learnhaskell#getting-set-up)
on installing the Haskell platform. on every operating system under the
sun.

**Note**: as of 2015-05-05, the latest Haskell platform ships with GHC
  7.8.3. Louse only supports GHC >=7.10, so you have to
  [install GHC manually][1].

I haven't published a version on Hackage (the Haskell package
repository) yet, so you'll have to use the git version. To download and
compile louse, run:

```
git clone https://github.com/pharpend/louse.git
cd louse
cabal update
cabal install -j
```

For a tutorial, you can see the [TUTORIAL.md file](TUTORIAL.md), or run
`louse --tutorial`. For a brief listing of the available commands, you
can run `louse --help`.

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

The following commands need to be implemented:

* `louse bug add`
* `louse bug close`
* `louse bug comment`
* `louse bug delete`
* `louse bug list`
* `louse bug show`

The following commands need to be removed:

* `louse ppl`
* `louse schemata`

These will probably go in another tool:

* Add support for GitHub issues API
* Add support for BitBucket issues API
* Add support for git.gnu.io issues API
* Add support for GitLab.com issues API
* Add support for Launchpad issues API
* Add support for Bugzilla issues API
* Add support for Custom issue tracker API
* Add support for Custom issue tracker API

## Contact

Email: <peter@harpending.org>
IRC: `pharpend` on FreeNode and OFTC.

[1]: https://www.haskell.org/ghc/download_ghc_7_10_1#binaries
