INSTALLFLAGS=-j

all: update build

clean:
	cabal clean

update:
	cabal update

build:
	cabal install ${INSTALLFLAGS}
	cd bin && ghc louse.hs

install:
	cp bin/louse /usr/local/bin/louse

uninstall:
	rm /usr/local/bin/louse
