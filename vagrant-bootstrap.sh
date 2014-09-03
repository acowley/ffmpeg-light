#!/usr/bin/env bash

# Install FFmpeg dependencies
apt-get update
apt-get -y install autoconf automake build-essential libass-dev \
  libfreetype6-dev libgpac-dev libtheora-dev libtool libvorbis-dev \
  pkg-config texi2html zlib1g-dev yasm unzip libmp3lame-dev

# Install support for add-apt-repository for working with PPAs
apt-get install -y python-software-properties software-properties-common

# Get GHC and cabal-install from Herbert's PPA
add-apt-repository ppa:hvr/ghc
sed 's/ trusty / precise /' /etc/apt/sources.list.d/hvr-ghc-trusty.list > /etc/apt/sources.list.d/hvr-ghc-precise.list
apt-get update
apt-get install -y ghc-7.8.3
apt-get install -y cabal-install-1.20

# cabal-install bash completion
wget https://raw.githubusercontent.com/haskell/cabal/master/cabal-install/bash-completion/cabal -O /etc/bash_completion.d/cabal

# Setup PATH, run cabal update, and configure SSH
su vagrant <<EOF
echo "PATH=/home/vagrant/.cabal/bin:/opt/ghc/7.8.3/bin:/opt/cabal/1.20/bin:$PATH" >> ~/.bashrc
export PATH=/home/vagrant/.cabal/bin:/opt/ghc/7.8.3/bin:/opt/cabal/1.20/bin:$PATH
/opt/cabal/1.20/bin/cabal update
# /opt/cabal/1.20/bin/cabal install cabal-install
EOF
