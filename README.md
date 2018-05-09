
# Installation

## Manual

To install HAAP in a new sandbox, simply run:
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
```

## Docker

To try HAAP in a new Docker container, simply run:
```
docker run -i -t hugopacheco/haap:latest /bin/bash
```

# Examples

You can experiment with an example.

```
cd examples/<example>
cabal --sandbox-config-file=../../cabal.sandbox.config exec -- ghc helloworld.hs
cabal exec -- ghci examples/<example>/<example>.hs
> main
```

You can find pre-compiled examples at:
* [helloworld](https://hpacheco.github.io/HAAP/examples/helloworld/_site/index.html).
* [minimalistic](https://hpacheco.github.io/HAAP/examples/minimalistic/_site/index.html).
* [plab](https://hpacheco.github.io/HAAP/examples/plab/_site/index.html).




