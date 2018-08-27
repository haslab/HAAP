# Getting Started Guide

We have prepared a pre-built Docker image that contains a working installation of the Haskell Platform, GHCJS, and the _Haskell Automated Assessment Platform_ (HAAP) that supported the grading and feedback of students of the _Programming Laboratories I_ course unit described in the [ICFP18 experience report](https://dl.acm.org/citation.cfm?id=3236777).

Our [repository](https://github.com/haslab/HAAP) contains the complete HAAP sources (including its core plugins, HTML templates, OpenGL/JavaScript bindings, etc) and the Dockerfile used to build this image.

As a generic platform, HAAP is modeled as a collection of plugins and scripting libraries that automate the assessment process. A concrete HAAP program is then the analog of a batch script that generates feedback in various forms.

Since we will provide sample HAAP scripts that highlight HAAP's visual feedback capabilities, and the Docker image has no X window system, we recommend binding a directory in the host system, say `~/Desktop`, to a path inside the container, say `/Desktop`, by running the image with the arguments:

```
docker run -v ~/Desktop:/Desktop -i -t haslab/haap /bin/bash
```
## Step-by-Step Instructions

We showcase HAAP through a series of incrementally more complex examples that highlight its visual feedback capabilities and its relevance as a supporting tool throughout our 5-year experiment.

### How to Compile the Examples

Our examples rely on different HAAP plugins and were collected from parts of concrete HAAP instantiations throughout the years. Some were used as examples in the ICFP18 paper, and are identified as such below. Thus, these examples reuse legacy external code (like instructor oracles and student submissions) that is particular to the yearly instantiation and is not an integral part of HAAP’s libraries.

A more complete account on its instantiations, providing a unified view of its potential, can be found in the HAAP-powered [Hall of Fame](https://haslab.github.io/Teaching/LI1/) of the course unit. It collects student submissions to acknowledge the best students for their work, and to inspire new students starting their assignments.

In general, each example `[example]` can be installed and run with the following instructions:

```
cd examples/[example]
cabal exec -- ghc [example].hs
./[example]
./[example]
```

This will compile a concrete HAAP instantiation described in `[example].hs` and then execute it. Some examples may require additional parameters. All examples use the `Hakyll` plugin to generate a static webpage with the associated feedback at `[example]/_site/index.html`. Pre-generated versions of the examples feedback are also available at `[example]/site/index.html`.

Certain plugins (e.g., the `tournament` one) preserve history between executions (using a suitable `database` plugin); executing `[example]` twice exercises this functionality (e.g., two tournaments in the leaderboard). 

### Example 1: helloworld

A simple “hello world” example with a simple [Hspec](https://hspec.github.io/) specification and [Hakyll](https://jaspervdj.be/hakyll/) webpage generation. This example is the same described in Section 4.1 of the paper, and Fig. 2 is actually a code snippet from `helloworld.hs`.

The example relies on the `Hspec` and `Hakyll` plugins provided by HAAP to generate task-level [feedback](https://haslab.github.io/HAAP/examples/helloworld/site/spec.html).
### Example 2: minimalistic

A minimal example that showcases basic usage of code analysis tools, [CodeWorld](https://github.com/google/codeworld) integration and task correctness for continuous feedback (Section 4.2.1). 

The example uses the `Hspec` plugin to provide both task-level [feedback](https://haslab.github.io/HAAP/examples/minimalistic/site/spec.html) and global feedback both through the [`ranking`](https://haslab.github.io/HAAP/examples/ /site/ranks.html) and [`tournament`](https://haslab.github.io/HAAP/examples/minimalistic/site/torneio/tourneys.html) plugins. The `database` plugin allows the history of tournaments to be preserved. Group-level feedback regarding code quality is provided through the [`Haddock`](https://haslab.github.io/HAAP/examples/minimalistic/site/doc.html), [`Hlint`](https://haslab.github.io/HAAP/examples/minimalistic/site/hlint.html), [`Homplexity`](https://haslab.github.io/HAAP/examples/minimalistic/site/homplexity.html) and [`hpc`](https://haslab.github.io/HAAP/examples/minimalistic/site/hpc/HPCTest/hpc_index.html) plugins.

Finally, the example also shows two basic usages of the `CodeWorld` plugin that allow students to graphically consult the oracle, compiled with GHCJS, [one](https://haslab.github.io/HAAP/examples/minimalistic/site/codeworld/MMDraw.jsexe/run.html) for testing expected pairs of input/output, and another [one](https://haslab.github.io/HAAP/examples/minimalistic/site/codeworld/MMGame.jsexe/run.html) for interactive simulation.

All outputs are connected by the `Hakkyll` plugin.
### Example 3: xterm

An example that showcases [xterm.js](https://xtermjs.org/) integration, used to animate the final student tournament for our [13/14 project](https://haslab.github.io/Teaching/LI1/1314_tournament_final/torneio.html).

The example relies on the `xterm` plugin to generate an interactive [match](https://haslab.github.io/HAAP/examples/xterm/site/xterm/Game.jsexe/run.html) through a terminal-like interface, compiled with GHCJS.
### Example 4: gameworker

An example that showcases an interactive game from our 16/17 project. It uses the `CodeWorld` plugin to [animate](https://haslab.github.io/HAAP/examples/gameworker/site/gameworker/Game.jsexe/run.html) matches, where each works executes the submission of a student compiled by GHCJS.

Note that, by default, some browsers (such as Google Chrome) may block local JavaScript Web Workers for security reasons. We recommend using Firefox or Safari, for example, to overcome this issue.
### Example 5: plab

A mockup of the [Laboratórios de Informática I](https://haslab.github.io/Teaching/LI1/) class feedback page. This is essentially the 17/18 instantiation of HAAP described in Section 4.2 of the paper. It provides task-level, group-level and global continuous feedback (Section 4.2.1) for a small set of solutions with a few errors to illustrate how their feedback is provided. Installing requires an additional `ghc` parameter, and can be done with:

```
cabal exec -- ghc plab.hs -ioracle
```

The example uses the `Hspec` plugin to provide task-level [feedback](https://haslab.github.io/HAAP/examples/plab/site/grupos/180/t1.html) for various tasks. The expected outcome of each test can also be consulted [graphically](https://haslab.github.io/HAAP/examples/plab/site/grupos/180/t1/1/run.html) by relying on the `CodeWorld` plugin that runs the oracle compiled by GHCJS.

Group-level feedback regarding code quality is provided through the [`Haddock`](https://haslab.github.io/HAAP/examples/plab/site/grupos/180/doc.html), [`Hlint`](https://haslab.github.io/HAAP/examples/plab/site/grupos/180/hlint.html), [`Homplexity`](https://haslab.github.io/HAAP/examples/plab/site/grupos/180/homplexity.html) and [`hpc`](https://haslab.github.io/HAAP/examples/plab/site/grupos/180/hpcT1/RunT1/hpc_index.html) plugins.

Global feedback is provided through the [`ranking`](https://haslab.github.io/HAAP/examples/plab/site/ranks/t3.html) and [`tournament`](https://haslab.github.io/HAAP/examples/plab/site/tourneys/t6/tourneys.html) plugins on top of `Hspec`. The `database` plugin allows the history of tournaments to be preserved. The combination with the `CodeWorld` plugin allows matches to be replayed.

Finally, the example also presents several usages of the `CodeWorld` plugin that allow students to graphically consult the oracle, compiled with GHCJS, [either](https://haslab.github.io/HAAP/examples/plab/site/mapviewer/MapViewer.jsexe/run.html) for testing expected pairs of input/output or an interactive [simulator](https://haslab.github.io/HAAP/examples/plab/site/grupos/180/collisionsimulator/CollisionSimulator.jsexe/run.html).

All outputs are connected by the `Hakkyll` plugin.


