# Getting Started Guide

The ICFP18 artifact for paper #42 is bundled as a Docker image that contains a working installation of the Haskell Platform, GHCJS, and the Haskell Automated Assessment Platform (HAAP) that supported the grading and feedback of students of the Programming Laboratories I course unit described in the experience report.

* [Dropbox]()
* 

As a generic platform, HAAP is modeled as a collection of plugins and scripting libraries that automate the assessment process.
A concrete HAAP program is then the analog of a batch script that generates feedback in various forms.

Since we will provide sample HAAP scripts that highlight HAAP's visual feedback capabilities, and the Docker container has no X window system, we recommend binding a directory in the host system, say ~/Desktop, to a path inside the container, say /Desktop, by running the container with the arguments:

## Step-by-Step Instructions

We provide a series of incrementally more complex HAAP scripts that highlight HAAP's visual feedback capabilities.

## Examples

You can then try our examples. These rely on different HAAP plugins and were collected from parts of concrete HAAP instantiations throughout the years. Some were used as examples in the ICFP18 paper, and are identified as so.

### helloworld

A simple hello world example with a simple [Hspec](https://hspec.github.io/) specification and [Hakyll](https://jaspervdj.be/hakyll/) webpage generation:

```
cd examples/helloworld
cabal exec -- ghc helloworld.hs
./helloworld
```

The resulting page is generated at `_site/index.html`.

Check the pre-generated result of this [helloworld](https://haslab.github.io/HAAP/examples/helloworld/site/index.html) example.

### minimalistic

A minimal example that showcases code analysis tools and [CodeWorld](https://github.com/google/codeworld) integration. This example is the same described in Section 4.1 of the paper, and Fig. 2 is actually a code snippet from `minimalistic.hs`. It can be installed and run as:

```
cd examples/minimalistic
cabal exec -- ghc minimalistic.hs
./minimalistic
./minimalistic
```

Tournament history is preserved between executions; executing `minimalist` twice will result in two tournaments. The resulting page is generated at `_site/index.html`. 

Check the pre-generated result of this [minimalistic](https://haslab.github.io/HAAP/examples/minimalistic/site/index.html) example.

### xterm

An example that showcases [xterm.js](https://xtermjs.org/) integration:

```
cd examples/xterm
cp ../../cabal.sandbox.config .
cabal exec -- ghc xterm.hs
./xterm
```

The resulting page is generated at `_site/index.html`.

Check the pre-generated result of this [xterm](https://haslab.github.io/HAAP/examples/xterm/site/index.html) example.

### gameworker

An example that showcases a [CodeWorld](https://github.com/google/codeworld) game using JavaScript Web Workers:

```
cd examples/gameworker
cabal exec -- ghc gameworker.hs
./gameworker
```

The resulting page is generated at `_site/index.html`.
Note that, by default, some browsers (such as Google Chrome) may block local JavaScript Web Workers for security reasons.

Check the pre-generated result of this [gameworker](https://haslab.github.io/HAAP/examples/gameworker/site/index.html) example.

### plab

A mockup of the [Laboratórios de Informática I](https://haslab.github.io/Teaching/LI1/) class feedback page. This is essentially the 17/18 instantiation of HAAP described in Section 4.2 of the paper. It can be installed and run as:

```
cd examples/plab
cabal exec -- ghc plab.hs -ioracle
./plab
./plab
```

Tournament history is preserved between executions; executing `plab` twice will result in two tournaments. The resulting page is generated at `_site/index.html`.

Check the pre-generated result of this [plab](https://haslab.github.io/HAAP/examples/plab/site/index.html) example.


