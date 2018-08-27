FROM ubuntu:16.04
MAINTAINER Hugo Pacheco <hpacheco@gmail.com>

RUN apt-get update
RUN apt-get -qqy install locales build-essential autoconf git nodejs nodejs-legacy zlib1g-dev libgmp-dev freeglut3-dev libtinfo-dev wget vim unzip

RUN echo "LANG=en_AU.UTF-8" > /etc/default/locale
RUN echo "en_AU.UTF-8 UTF-8" > /etc/locale.gen
RUN /usr/sbin/locale-gen
ENV LANG en_AU.UTF-8
ENV LC_ALL C.UTF-8

ENV HOME /home
ENV PATH /home/.cabal/bin:/home/.cabal-sandbox/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN echo "export PATH=/home/.cabal/bin/:/home/.cabal-sandbox/bin:$PATH" >> /root/.profile
RUN . /root/.profile

WORKDIR $HOME
RUN wget https://www.haskell.org/platform/download/8.0.1/haskell-platform-8.0.1-unknown-posix--minimal-x86_64.tar.gz
RUN tar -xvf haskell-platform-8.0.1-unknown-posix--minimal-x86_64.tar.gz
RUN ./install-haskell-platform.sh
RUN rm -rf $HOME/haskell-platform-8.0.1-unknown-posix--minimal-x86_64.tar.gz
RUN rm -rf $HOME/hp-usr-local.tar.gz
RUN rm -rf $HOME/install-haskell-platform.sh

RUN cabal update

WORKDIR $HOME
RUN wget http://ghcjs.tolysz.org/ghc-8.0-2017-02-05-lts-7.19-9007019.tar.gz
RUN tar -xvf ghc-8.0-2017-02-05-lts-7.19-9007019.tar.gz
RUN git clone https://github.com/haslab/HAAP

RUN cabal sandbox init
RUN cabal sandbox add-source $HOME/HAAP/packages/*
ADD cabal.config $HOME/cabal.config
RUN cabal install ghcjs-0.2.1.9007019/ghcjs.cabal HAAP/HAAP.cabal brittany haddock hlint homplexity $HOME/HAAP/packages/gloss-window $HOME/HAAP/packages/snap-exports $HOME/HAAP/packages/snaplet-acid-state-0.2.8 $HOME/HAAP/packages/snap-extras-0.12.2.1 $HOME/HAAP/packages/debug-0.1.1 $HOME/HAAP/packages/Hoed-extras --disable-documentation
RUN rm -rf $HOME/ghc-8.0-2017-02-05-lts-7.19-9007019.tar.gz

WORKDIR $HOME/ghcjs-0.2.1.9007019
ADD ghcjs.config $HOME/ghcjs-0.2.1.9007019/cabal.config
RUN ghcjs-boot

WORKDIR $HOME/ghcjs-0.2.1.9007019
RUN cabal install --ghcjs $HOME/HAAP/packages/codeworld-haap-api $HOME/HAAP/packages/gloss-window $HOME/HAAP/packages/Xterm $HOME/HAAP/packages/ghcjs-extras QuickCheck GenericPretty safe xml parsec --disable-documentation

RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/helloworld/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/minimalistic/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/xterm/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/gameworker/cabal.sandbox.config
RUN ln -s $HOME/cabal.sandbox.config $HOME/HAAP/examples/plab/cabal.sandbox.config

WORKDIR $HOME/HAAP


