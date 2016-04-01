# louse

louse is a simple command line bug tracker interface. Louse will fetch
bug information from remote bug trackers, and allow you to browse it on
your machine.

## Installation and Usage

First, install [the Haskell Stack][stk] and [git][git]. Then, run these
commands in a terminal

    git clone https://github.com/pharpend/louse.git
    cd louse
    stack setup
    stack build

Louse is still in development, so this won't install louse in your
`$PATH`. If you want to install louse in your `$PATH`, run

    stack build --copy-bins
    
Louse is not really useful yet, but you can run the executable via

    stack exec -- louse

## Contact

* IRC: `pharpend` on FreeNode
* Email: `peter@harpending.org`

[git]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[stk]: http://docs.haskellstack.org/en/stable/README/
