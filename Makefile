# This makefile is only needed for developing or otherwise modifying the
# package.  For users of the package, simply use Cabal.

all: build

clean:
	rm -fr \
	    src/Blas/Primitive/blas.h \
	    src/Blas/Primitive/Safe_.chs \
	    src/Blas/Primitive/Safe.hs \
	    src/Blas/Primitive/Safe.chs.tmp \
	    src/Blas/Primitive/Unsafe_.chs \
	    src/Blas/Primitive/Unsafe.hs \
	    src/Blas/Primitive/Unsafe.chs.tmp \
	    src/Blas/Generic/Safe.hs \
	    src/Blas/Generic/Unsafe.hs
	cabal clean

build: \
    src/Blas/Primitive/Safe.hs \
    src/Blas/Primitive/Unsafe.hs \
    src/Blas/Generic/Safe.hs \
    src/Blas/Generic/Unsafe.hs
	cabal build

doc: build
	cabal haddock

# download the header from Netlib
src/Blas/Primitive/blas.h:
	mkdir -p src/Blas/Primitive
	curl >$@ -L http://netlib.org/clapack/CLAPACK-3.1.1.1/BLAS/WRAP/cblas.h

src/Blas/Primitive/Safe.hs: \
    src/Blas/Primitive/blas.h \
    src/Blas/Primitive/Safe_.chs
	c2hs blas.h src/Blas/Primitive/Safe_.chs
	tools/post-process src/Blas/Primitive/Safe_.hs
	mv src/Blas/Primitive/Safe_.hs src/Blas/Primitive/Safe.hs

src/Blas/Primitive/Unsafe.hs: \
    src/Blas/Primitive/blas.h \
    src/Blas/Primitive/Unsafe_.chs
	c2hs blas.h src/Blas/Primitive/Unsafe_.chs
	tools/post-process src/Blas/Primitive/Unsafe_.hs
	mv src/Blas/Primitive/Unsafe_.hs src/Blas/Primitive/Unsafe.hs

# the trailing underscore is to prevent Cabal from running c2hs on them
src/Blas/Primitive/Safe_.chs \
src/Blas/Primitive/Unsafe_.chs: \
    src/Blas/Primitive.in \
    src/Blas/Primitive/Safe.chs.tmp \
    src/Blas/Primitive/Unsafe.chs.tmp \
    tools/common.rb
	tools/rpp >/dev/null src/Blas/Primitive.in
	mv src/Blas/Primitive/Safe.chs   src/Blas/Primitive/Safe_.chs
	mv src/Blas/Primitive/Unsafe.chs src/Blas/Primitive/Unsafe_.chs

src/Blas/Primitive/Safe.chs.tmp \
src/Blas/Primitive/Unsafe.chs.tmp: \
    tools/generate-ffi \
    tools/Defs.hs
	mkdir -p src/Blas/Primitive
	cd tools && ./generate-ffi

src/Blas/Generic/Safe.hs \
src/Blas/Generic/Unsafe.hs: \
    src/Blas/Generic.in \
    tools/common.rb
	mkdir -p src/Blas/Generic
	tools/rpp >/dev/null src/Blas/Generic.in
