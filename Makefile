INSTALLFLAGS=-j
LIBONLY=-flib_only

all: update build-exe

clean:
	cabal clean

update:
	cabal update

build-lib:
	cabal install ${INSTALLFLAGS} ${LIBONLY}

build-exe: build-lib
	cd bin && ghc louse.hs

build:
	cabal install

install:
	cp bin/louse /usr/local/bin/louse

uninstall:
	rm /usr/local/bin/louse
