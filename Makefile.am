## -*- automake -*-
## FILE: Makefile.am
## $Id$
## ---
## tclreadline -- gnu readline for tcl
## https://github.com/flightaware/tclreadline/
## Copyright (c) 1998 - 2014, Johannes Zellner <johannes@zellner.org>
## This software is copyright under the BSD license.
## ---

## AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libtclreadline.la

libtclreadline_la_SOURCES = config.h tclreadline.c tclreadline.h
include_HEADERS = tclreadline.h

AM_CPPFLAGS = -I$(TCL_INCLUDE_DIR) -I$(READLINE_INCLUDE_DIR)

if !USE_TCL_STUBS
    libtclreadline_la_CFLAGS = -Wall
else
    libtclreadline_la_CFLAGS = -Wall -DUSE_TCL_STUBS
endif
if !USE_TCL_STUBS
    libtclreadline_la_LIBADD = $(TCL_LIB_SPEC)
else
    libtclreadline_la_LIBADD = $(TCL_STUB_LIB_SPEC)
endif
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

if STATIC_TCLSHRL
static_tclshrl = tclshrl
tclshrl_LDADD = libtclreadline.la $(TCL_LIB_SPEC)
tclshrl_LDFLAGS = -static
endif

if STATIC_WISHRL
static_wishrl = wishrl
wishrl_LDADD = libtclreadline.la $(TCL_LIB_SPEC) $(TK_LIB_SPEC)
wishrl_LDFLAGS = -static
endif

bin_PROGRAMS = $(static_tclshrl) $(static_wishrl)
tclshrl_SOURCES = tclshrl.c
wishrl_SOURCES = wishrl.c

man_MANS = tclreadline.n

