all: build

clean:
	cabal clean

build: \
    src/Blas/Primitive/Safe.chs \
    src/Blas/Primitive/Unsafe.chs \
    src/Blas/Generic/Unsafe.hs
	cabal build

doc: build
	cabal haddock

src/Blas/Primitive/Safe.chs \
src/Blas/Primitive/Unsafe.chs: \
    tools/generate-ffi tools/Defs.hs tools/Mod.hs
	cd tools && ./generate-ffi

src/Blas/Generic/Unsafe.hs: src/Blas/Generic/Safe.hs
	sed >src/Blas/Generic/Unsafe.hs 's/\.Safe/\.Unsafe/' \
	     src/Blas/Generic/Safe.hs
