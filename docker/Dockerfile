FROM hugopacheco/ghcjs:latest
MAINTAINER Hugo Pacheco <hpacheco@gmail.com>

ENV HOME /home
RUN echo "export PATH=/home/.cabal/bin/:/home/.cabal-sandbox/bin:$PATH" >> /root/.profile
RUN . /root/.profile

WORKDIR $HOME
RUN git clone https://github.com/haslab/HAAP
WORKDIR $HOME/HAAP
RUN cabal update
RUN cp ../ghcjs/cabal.sandbox.config cabal.sandbox.config

RUN cabal sandbox init
RUN cabal sandbox add-source $HOME/HAAP/packages/*
# ADD cabal.config $HOME/cabal.config
RUN cabal install $HOME/HAAP/packages/Hoed-extras brittany haddock hlint homplexity $HOME/HAAP/HAAP.cabal $HOME/HAAP/packages/gloss-window $HOME/HAAP/packages/snap-exports $HOME/HAAP/packages/snaplet-acid-state-0.2.8 $HOME/HAAP/packages/snap-extras-0.12.2.1 --disable-documentation
RUN rm -rf $HOME/ghc-8.0-2017-02-05-lts-7.19-9007019.tar.gz

WORKDIR $HOME/ghcjs
RUN cabal install --ghcjs $HOME/HAAP/packages/codeworld-haap-api $HOME/HAAP/packages/gloss-window $HOME/HAAP/packages/Xterm $HOME/HAAP/packages/ghcjs-extras QuickCheck GenericPretty safe xml parsec --disable-documentation

RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/helloworld/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/minimalistic/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/xterm/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/gameworker/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/plab/cabal.sandbox.config

WORKDIR $HOME/HAAP


