## -*- automake -*-
## FILE: "/home/joze/src/tclreadline/Makefile.am"
## LAST MODIFICATION: "Mon, 03 Jul 2000 14:46:10 +0200 (joze)"
## (C) 2000 by Johannes Zellner, <johannes@zellner.org>
## $Id$
## ---
## tclreadline -- gnu readline for tcl
## http://www.zellner.org/tclreadline/
## Copyright (c) 1998 - 2000, Johannes Zellner <johannes@zellner.org>
## This software is copyright under the BSD license.
## ---

## AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libtclreadline.la

libtclreadline_la_SOURCES = config.h tclreadline.c tclreadline.h
INCLUDES = -I$(TCL_INCLUDE_DIR) -I$(READLINE_INCLUDE_DIR)

## libtclreadline_la_LIBADD = $(LIBS)
libtclreadline_la_LDFLAGS = -release $(MAJOR).$(MINOR).$(PATCHLEVEL)

tclrldir = @TCLRL_DIR@
tclrl_SCRIPTS = pkgIndex.tcl tclreadlineCompleter.tcl tclreadlineInit.tcl tclreadlineSetup.tcl
INSTALL_SCRIPT = ${INSTALL_DATA}

EXTRA_DIST =                 \
    tclreadlineCompleter.tcl \
    aux/config.guess         \
    aux/config.sub           \
    aux/install-sh           \
    aux/ltconfig             \
    aux/ltmain.sh            \
    aux/missing              \
    aux/mkinstalldirs

man_MANS = tclreadline.n

