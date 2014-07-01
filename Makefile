all: build

clean:
	rm -fr \
	    src/Blas/Primitive/Safe.chs \
	    src/Blas/Primitive/Safe.chs.tmp \
	    src/Blas/Primitive/Unsafe.chs \
	    src/Blas/Primitive/Unsafe.chs.tmp \
	    src/Blas/Generic/Safe.hs \
	    src/Blas/Generic/Unsafe.hs
	cabal clean

build: \
    src/Blas/Primitive/Safe.chs \
    src/Blas/Primitive/Unsafe.chs \
    src/Blas/Generic/Safe.hs \
    src/Blas/Generic/Unsafe.hs
	cabal build

doc: build
	cabal haddock

src/Blas/Primitive/Safe.chs \
src/Blas/Primitive/Unsafe.chs: \
    src/Blas/Primitive.in \
    src/Blas/Primitive/Safe.chs.tmp \
    src/Blas/Primitive/Unsafe.chs.tmp \
    tools/common.rb
	tools/rpp >/dev/null src/Blas/Primitive.in

src/Blas/Primitive/Safe.chs.tmp \
src/Blas/Primitive/Unsafe.chs.tmp: \
    tools/generate-ffi \
    tools/Defs.hs
	cd tools && ./generate-ffi

src/Blas/Generic/Safe.hs \
src/Blas/Generic/Unsafe.hs: \
    src/Blas/Generic.in \
    tools/common.rb
	tools/rpp >/dev/null src/Blas/Generic.in
