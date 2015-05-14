INSTALLFLAGS=-j

all: update build

clean:
	cabal clean

update:
	cabal update

build:
	cabal install ${INSTALLFLAGS}

install:
	cp dist/build/louse/louse /usr/local/bin/louse

uninstall:
	rm /usr/local/bin/louse
