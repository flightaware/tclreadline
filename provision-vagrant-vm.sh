#! /bin/sh
sudo apt-get install -y
        autoconf
        build-essential
        expect
        libreadline6
        libreadline6-dev
        libtool
        tcl
        tcl-dev
cd /vagrant
autoreconf -fvi
./configure --with-tcl-includes=/usr/include/tcl
make
sudo make install
