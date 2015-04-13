# louse

louse is a very simple distributed bug-tracking system. It literally
stores bugs in a JSON file. The idea is pretty portable, although this
implementation is written in Haskell.

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

You need to install the Haskell platform before installing louse. Chris Allen
maintains an
[exhaustive guide](https://github.com/bitemyapp/learnhaskell#getting-set-up) on
installing the Haskell platform.

I haven't published a version on Hackage (the Haskell package repository) yet,
so you'll have to use the git version. To download and compile louse, run:

```
git clone https://notabug.org/pharpend/louse.git
cd louse
cabal update
cabal install
```

For a tutorial, you can see the [TUTORIAL.md file](TUTORIAL.md), or run `louse
--tutorial`. For a brief listing of the available commands, you can run `louse
--help`.

## Directory structure

The current directory structure looks like this:

    project/
        .louse/
            project.json

It occurs to me though, that just one JSON file is a bit hard to version
control. If two louse branches make different changes, they'll
inevitably conflict.

That is, if two separate people add bugs, they'll likely add them to the
same spot in the JSON file, which means a merge conflict. With this new
system, if two people add two files with different names, there is
unlikely to be a merge conflict.

Even though louse doesn't interface with version control, projects that
will use louse probably also use version control, so I have to keep
version control in mind during the design process.

Thus, I'm probably going to switch to a directory structure more like this:

    project/
        .louse/
            project.json
            people/
                58ec25555613cfced0171391bc483ee77a2fd317.json
                74a2c0d30d1125a4952a92a0d5b534546d4967ba.json
                d787eed545773f27b23622e8ade69bf1ecd90528.json
                f19cec87632be50bffb463314ea0c7a76ec4aca6.json
                ...
            bugs/
                43d66424f5d84470aabedc2cc369ab8cf0ba19ee.json
                8c36dd07c13861be9d8634eeb97b0024785b541a.json
                92de71a16a94012f53c0f09de214a2235ee32fa0.json
                a97816ef1d66507695698fb41dd6887cc8869c2d.json
                ...

which is much more VCS-friendly. 

This also has the added benefit of "factoring out" individual people.

At the moment, the identifiers are randomly generated. In the future, I'll
probably add some sort of hashing system.

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

The SFC has a trademark on the term "Git", so I can't use it in the name
of my program without their permission. I looked in the thesaurus for
synonyms to "git", and "louse" came up. It seems appropriate, because
camping is vaguely reminiscent of free/open-source developers
collaborating.

## Contact

You can email me at `<peter@harpending.org>`, or contact me via IRC. I am
`pharpend` on FreeNode and OFTC.
