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
RUN cabal install brittany haddock hlint homplexity $HOME/HAAP/HAAP.cabal $HOME/HAAP/packages/gloss-window $HOME/HAAP/packages/snap-exports $HOME/HAAP/packages/snaplet-acid-state-0.2.8 $HOME/HAAP/packages/snap-extras-0.12.2.1 --disable-documentation

# install HAAP ghcjs packages
WORKDIR $HOME
RUN cabal install --ghcjs $HOME/HAAP/packages/codeworld-haap-api $HOME/HAAP/packages/gloss-window $HOME/HAAP/packages/Xterm $HOME/HAAP/packages/ghcjs-extras $HOME/HAAP/packages/ghcjs-fetch-0.1.0.1 QuickCheck GenericPretty safe xml parsec --disable-documentation

# prepare examples
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/helloworld/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/minimalistic/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/xterm/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/gameworker/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/plab/cabal.sandbox.config

WORKDIR $HOME/HAAP


