#! /bin/sh
set -e
sudo apt-get update
sudo apt-get install -y \
        autoconf \
        build-essential \
        expect \
        itcl3 \
        libreadline-dev \
        libtool \
        tcl-dev
cd /vagrant
autoreconf -fvi
./configure
make
sudo make install
