
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

# Examples

You can experiment with an example.

```
cabal exec -- ghci examples/<example>/<example>.hs
> main
```

You can find pre-compiled examples at:
* [helloworld](https://hpacheco.github.io/HAAP/examples/helloworld/_site/helloworld.html).
* [minimalistic](https://hpacheco.github.io/HAAP/examples/minimalistic/_site/minimalistic.html).




