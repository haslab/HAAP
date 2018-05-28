
# Installation

## Manual

To install HAAP in a new sandbox, simply run:
```
cabal sandbox init
cabal sandbox add-source packages/*
cabal install HAAP.cabal
```

For JavaScript animations you need to have [ghcjs](https://github.com/ghcjs/ghcjs) installed:
1. install `ghcjs` from https://github.com/commercialhaskell/stack/blob/master/doc/ghcjs.md
2. add `ghcjs` to your path
3. install the HAAP ghcjs packages
```
cabal install --ghcjs packages/codeworld-haap-api packages/ghcjs-extras packages/Xterm
```

## Docker

To try HAAP in a new Docker container, simply run:
```
docker run -i -t hugopacheco/haap:latest /bin/bash
```
Since this container has no X window system, it is often useful to bind a directory in your host system, say `~/Desktop`, to a path inside your container, say `/Desktop`, by running the container with the additional argument:
```
docker run -v ~/Desktop:/Desktop -i -t hugopacheco/haap:latest /bin/bash
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

The resulting page is generated at `site/index.html`.

Check the pre-generated result of this [helloworld](https://hpacheco.github.io/HAAP/examples/helloworld/site/index.html) example.

## minimalistic

A minimal example that showcases code analysis tools and [CodeWorld](https://github.com/google/codeworld) integration:

```
cd examples/minimalistic
cp ../../cabal.sandbox.config .
cabal exec -- ghc minimalistic.hs
./minimalistic
./minimalistic
```

The resulting page is generated at `site/index.html`.

Check the pre-generated result of this [minimalistic](https://hpacheco.github.io/HAAP/examples/minimalistic/site/index.html) example.

## xterm

An example that showcases [xterm.js](https://xtermjs.org/) integration:

```
cd examples/xterm
cp ../../cabal.sandbox.config .
cabal exec -- ghc xterm.hs
./xterm
```

Check the pre-generated result of this [xterm](https://hpacheco.github.io/HAAP/examples/xterm/site/index.html) example.

## gameworker

An example that showcases a [CodeWorld](https://github.com/google/codeworld) game using JavaScript Web Workers:

```
cd examples/gameworker
cp ../../cabal.sandbox.config .
cabal exec -- ghc gameworker.hs
./gameworker
```

Check the pre-generated result of this [gameworker](https://hpacheco.github.io/HAAP/examples/gameworker/site/index.html) example.

## plab

A mockup of the [Laboratórios de Informática I](https://haslab.github.io/Teaching/LI1/) class feedback page.

```
cd examples/plab
cp ../../cabal.sandbox.config .
cabal exec -- ghc plab.hs -ioracle
./plab
./plab
```

Check the pre-generated result of this [plab](https://hpacheco.github.io/HAAP/examples/plab/site/index.html) example.





