FROM hugopacheco/ghcjs:latest
MAINTAINER Hugo Pacheco <hpacheco@gmail.com>

# install essential packages
RUN apt-get update
RUN apt-get -qqy install pkg-config libpcre3-dev

# install HAAP
WORKDIR $HOME
RUN git clone https://github.com/haslab/HAAP
WORKDIR $HOME/HAAP
RUN cabal update
RUN cp ../ghcjs/cabal.sandbox.config cabal.sandbox.config
RUN cabal sandbox add-source $HOME/HAAP/packages/*
RUN cabal install brittany haddock hlint /home/HAAP/packages/SourceGraph/SourceGraph.cabal /home/HAAP/packages/Xterm/Xterm.cabal /home/HAAP/packages/homplexity/homplexity.cabal /home/HAAP/packages/debug/debug.cabal /home/HAAP/packages/hakyll/hakyll.cabal /home/HAAP/HAAP.cabal /home/HAAP/packages/gloss-window /home/HAAP/packages/snap-exports /home/HAAP/packages/snaplet-acid-state /home/HAAP/packages/snap-extras --disable-documentation

# install HAAP ghcjs packages
WORKDIR $HOME
RUN cabal install --ghcjs /home/HAAP/packages/codeworld-haap-api /home/HAAP/packages/gloss-window /home/HAAP/packages/Xterm /home/HAAP/packages/ghcjs-extras QuickCheck GenericPretty safe xml parsec --disable-documentation

# prepare examples
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/helloworld/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/minimalistic/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/xterm/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/gameworker/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/plab/cabal.sandbox.config

WORKDIR $HOME/HAAP


