FROM ubuntu:16.04
MAINTAINER Hugo Pacheco <hpacheco@gmail.com>

# install essential packages
RUN apt-get update
RUN apt-get -qqy install curl python3 locales build-essential autoconf git zlib1g-dev libgmp-dev freeglut3-dev libtinfo-dev wget vim unzip
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN apt-get -qqy install -y nodejs

#configure locale
RUN echo "LANG=en_AU.UTF-8" > /etc/default/locale
RUN echo "en_AU.UTF-8 UTF-8" > /etc/locale.gen
RUN /usr/sbin/locale-gen
ENV LANG en_AU.UTF-8
ENV LC_ALL C.UTF-8

#configure path
ENV HOME /home
ENV PATH /home/.cabal/bin:/home/ghcjs/.cabal-sandbox/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN echo "export PATH=/home/.cabal/bin/:/home/ghcjs/.cabal-sandbox/bin:$PATH" >> /root/.profile
RUN . /root/.profile

#install Haskell
WORKDIR $HOME
RUN wget https://haskell.org/platform/download/8.4.2/haskell-platform-8.4.2-unknown-posix--core-x86_64.tar.gz
RUN tar -xvf haskell-platform-8.4.2-unknown-posix--core-x86_64.tar.gz
RUN ./install-haskell-platform.sh
RUN rm -rf haskell-platform-8.4.2-unknown-posix--core-x86_64.tar.gz hp-usr-local.tar.gz install-haskell-platform.sh

#install GHCJS
WORKDIR $HOME
RUN git clone https://github.com/ghcjs/ghcjs.git
WORKDIR $HOME/ghcjs
RUN git checkout ghc-8.4
RUN git submodule update --init --recursive
RUN ./utils/makePackages.sh -j8
RUN ./utils/makeSandbox.sh -j8
RUN cabal install --disable-documentation
RUN ghcjs-boot -s ./lib/boot






