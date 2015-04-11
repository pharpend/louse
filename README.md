# decamp

decamp is a very simple distributed bug-tracking system. It literally stores
bugs in a JSON file. The idea is pretty portable, although this implementation
is written in Haskell.

The schema for the JSON file is stored in the `res/schemata/project.json`. The
other schemata (for people, comments, individual bugs, etc) are stored in the
`res/schemata/` directory.

You can also access the schemata with the `decamp` tool.

    $ decamp schemata list
    bug
    comment
    person
    project
    $ decamp schemata show comment
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

Or, with highlighting:

```json
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
```


I can't really copyright an idea, but this particular implementation of it is
Copyright (C) 2015 Peter Harpending, and licensed under the
[GNU General Public License, version >=3](https://gnu.org/licenses/gpl). A copy
of the license can be found in the LICENSE file.

## Installation and Usage

You need to install the Haskell platform before installing decamp. Chris Allen
maintains an
[exhaustive guide](https://github.com/bitemyapp/learnhaskell#getting-set-up) on
installing the Haskell platform.

I haven't published a version on Hackage (the Haskell package repository) yet,
so you'll have to use the git version. To download and compile decamp, run:

```
git clone ssh://git@github.com/pharpend/decamp.git
cd decamp
cabal update
cabal install
```

For a tutorial, you can see the [TUTORIAL.md file](TUTORIAL.md), or run `decamp
--tutorial`. For a brief listing of the available commands, you can run `decamp
--help`.

## Inspiration

If you're developing software with Git, there are, in broad terms, 4 components:

1. The actual code
2. The bug tracker
3. Merge requests
4. Continuous integration

I'm sure I'm forgetting something here.

The recent
[DDOS attack on GitHub](https://github.com/blog/1981-large-scale-ddos-attack-on-github-com)
revealed a sore point with distributed version control workflows: even though
the code itself is distributed, often the bug tracker is hosted on a central
server, which could go down. This is an effort to further distribute the
standard development workflow.

I can't figure out how to distribute continuous integration, so I'll leave that
to someone else.

## Nomenclature

The SFC has a trademark on the term "Git", so I can't use it in the name of my
program without their permission. I looked in the thesaurus for synonyms to
"git", and "decamp" came up. It seems appropriate, because camping is vaguely
reminiscent of free/open-source developers collaborating.

## Contact

You can email me at `<peter@harpending.org>`, or contact me via IRC. I am
`pharpend` on FreeNode and OFTC.
