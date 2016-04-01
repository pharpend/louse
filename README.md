# louse

Louse is a bug tracker client. It interfaces with remote bug trackers
(e.g. GitHub issues), and provides a local client to interact with the
issues. Louse is written in Haskell and licensed under the GNU General
Public License, version 3 or later, as published by the Free Software
Foundation (AGPL). You can read a copy of the AGPL in the [LICENSE][lic]
file.

## Building & running

You need to install [stack][stk] and [git][git-install] to build this
program.

    git clone https://gitlab.com/pharpend/louse.git
    cd louse
    stack setup
    stack build
    
The program isn't very interesting right now. To run it, you can run 

    stack exec -- louse

You can run the test suite (which again, has no tests) with

    stack build --test
    
## Contacting the author

* To submit a bug report, please use the
  [GitLab issue tracker][gl-issues]. If you don't want to make a GitLab
  account for whatever reason, you can email me at
  `peter@harpending.org`.
* To submit changes, you can open a merge request on GitLab, or email
  `peter@harpending.org`.
* My email is `peter@harpending.org` if you haven't picked that up yet.
* You can contact me on IRC. I go by `pharpend` on freenode.net.

[git-install]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[gh-mirror]: https://github.com/pharpend/louse
[gl-issues]: https://gitlab.com/pharpend/louse/issues
[lic]: LICENSE
[stk]: http://docs.haskellstack.org/en/stable/README/
