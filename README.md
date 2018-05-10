
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
cabal install --ghcjs packages/codeworld-haap-api
```

## Docker

To try HAAP in a new Docker container, simply run:
```
docker run -i -t hugopacheco/haap:latest /bin/bash
```

# Examples

You can then try our examples.

## helloworld

A simple hello world example with a simple Hspec specification and Hakyll webpage generation:

```
cd examples/helloworld
cp ../../cabal.sandbox.config .
cabal exec -- ghc helloworld.hs
./helloworld
```

You can find the generated [helloworld](https://hpacheco.github.io/HAAP/examples/helloworld/site/index.html) example here.

## minimalistic

A minimal example that showcases code analysis tools and CodeWorld integration:

```
cd examples/minimalistic
cp ../../cabal.sandbox.config .
cabal exec -- ghc minimalistics.hs
./minimalistic
./minimalistic
```

You can find the generated [minimalistic](https://hpacheco.github.io/HAAP/examples/minimalistic/site/index.html) example here.

## plab

A mockup of the [Laboratórios de Informática I](https://haslab.github.io/Teaching/LI1/) class feedback page.

```
cd examples/plab
cp ../../cabal.sandbox.config .
cabal exec -- ghc plab.hs -ioracle
./plab
./plab
```

You can find the generated [plab](https://hpacheco.github.io/HAAP/examples/plab/site/index.html) example here.





