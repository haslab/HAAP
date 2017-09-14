
# Installation

To install HAAP in a new sandbox simply run:
```
cabal sandbox init
cabal install HAAP.cabal
```

For CodeWorld animations you need to have `ghcjs` installed:
1. install `ghcjs` from https://github.com/commercialhaskell/stack/blob/master/doc/ghcjs.md
2. add `ghcjs` to your path
3. install the codeworld packages
```
cabal install --ghcjs codeworld-haap-api
cabal install --ghcjs codeworld-haap-base
```

# Example

You can experiment with a minimalistic example.

```
cd example
cabal exec -- ghc Example.hs
open _site/index.html
```




