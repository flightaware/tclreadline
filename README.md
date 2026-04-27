![Linux CI](https://github.com/flightaware/tclreadline/workflows/Linux%20CI/badge.svg)
![Mac CI](https://github.com/flightaware/tclreadline/workflows/Mac%20CI/badge.svg)

# tclreadline -- gnu readline for tcl

Copyright (c) 1998 - 2014, Johannes Zellner <johannes@zellner.org>
This software is copyright under the BSD license.



tclreadline


Introduction
===============

This directory contains the sources and documentation for tclreadline,
which builds a connection between tcl and the gnu readline.

Documentation
================

The tclreadline.n nroff man page in this release contains the reference
manual entries for tclreadline.  If you only want to use tclreadline as
a tool for interactive script development,  you don't have to read this
manual page at all.  Simply change your .tclshrc according to the section
later in this file.

Compiling and installing tclreadline
=======================================

This release will probably only build under UNIX (Linux).

Before trying to compile tclreadline you should do the following things:

1. Make sure you have tcl 8.0 or higher.
   tclreadline relies on a proper tcl installation:
   It uses the tclConfig.sh file, which should reside somewhere
   in /usr/local/lib/ or /usr/local/lib/tcl8.0/...

2. Make sure you have gnu readline 2.2 or higher.
   tclreadline uses the gnu readline callback handler, which
   wasn't implemented in early releases.

3. Build with either autotools (TEA) or meson:

   **Autotools (TEA):**
   ```sh
   autoconf    # Only needed if building from git, releases have configure baked in
   ./configure
   make
   make install
   ```
   You may have to specify the location of your tcl installation
   if it is not in a standard location. You can do this with a
   `--with-tcl=DIR` option to configure, where DIR is the location
   of the tclConfig.sh file. To build without Tk (which is only needed
   for wishrl) you can use the `--without-tk` option.

   **Meson:**
   ```sh
   meson setup build --buildtype=release
   meson install -C build
   ```
   If Tcl is not found automatically, point pkg-config at it:
   ```sh
   PKG_CONFIG_PATH=/path/to/tcl/lib/pkgconfig meson setup build
   ```
   Meson can also build Tcl from source as a fallback if no
   system Tcl is found (via the included subproject wraps).

4. Optionally (or additionally) you can build the executables
   tclshrl and / or wishrl which are a readline enhanced replacement
   for tclsh and wish.

   **Autotools:**
   ```sh
   ./configure --enable-tclshrl --enable-wishrl
   ```

   **Meson:**
   ```sh
   meson setup build -Dtclshrl=true -Dwishrl=true
   ```

   (or one of these if you want just tclshrl or wishrl).

Using tclreadline for interactive tcl scripting.
================================================

copy the sample.tclshrc to $HOME/.tclshrc. If you use another interpreter
like wish, you should copy the file sample.tclshrc to $HOME/.wishrc
(or whatever the manual page of your interpreter says.) If you have
installed tclreadline properly, you are just ready to start:
start your favorite interpreter. The tclreadlineSetup.tcl script
does the rest.
