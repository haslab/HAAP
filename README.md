
# Installation

## Manual

To install HAAP in a new sandbox, simply run:
```
cabal sandbox init
cabal install HAAP.cabal
```

For [CodeWorld](https://github.com/google/codeworld) animations you need to have [ghcjs](https://github.com/ghcjs/ghcjs) installed:
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

A simple hello world example with a simple [Hspec](https://hspec.github.io/) specification and [Hakyll](https://jaspervdj.be/hakyll/) webpage generation:

```
cd examples/helloworld
cp ../../cabal.sandbox.config .
cabal exec -- ghc helloworld.hs
./helloworld
```

Check the generated [helloworld](https://hpacheco.github.io/HAAP/examples/helloworld/site/index.html) example.

## minimalistic

A minimal example that showcases code analysis tools and [CodeWorld](https://github.com/google/codeworld) integration:

```
cd examples/minimalistic
cp ../../cabal.sandbox.config .
cabal exec -- ghc minimalistics.hs
./minimalistic
./minimalistic
```

Check the generated [minimalistic](https://hpacheco.github.io/HAAP/examples/minimalistic/site/index.html) example.

## xterm

An example that showcases [xterm.js](https://xtermjs.org/) integration:

```
cd examples/xterm
cp ../../cabal.sandbox.config .
cabal exec -- ghc xterm.hs
./xterm
```

Check the generated [minimalistic](https://hpacheco.github.io/HAAP/examples/minimalistic/site/index.html) example.

## plab

A mockup of the [Laboratórios de Informática I](https://haslab.github.io/Teaching/LI1/) class feedback page.

```
cd examples/plab
cp ../../cabal.sandbox.config .
cabal exec -- ghc plab.hs -ioracle
./plab
./plab
```

Check the generated [plab](https://hpacheco.github.io/HAAP/examples/plab/site/index.html) example.





