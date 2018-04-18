dnl -*- autoconf -*-
dnl FILE: configure.in
dnl $Id$
dnl ---
dnl tclreadline -- gnu readline for tcl
dnl http://www.zellner.org/tclreadline/
dnl Copyright (c) 1998 - 2014, Johannes Zellner <johannes@zellner.org>
dnl This software is copyright under the BSD license.
dnl ---

AC_INIT([tclreadline], [2.3.1])
AC_CONFIG_SRCDIR([tclreadline.c])
AC_CONFIG_HEADERS(config.h)
AC_PREREQ(2.13)
AC_REVISION($Revision$)
AC_CONFIG_AUX_DIR(./aux)

MAJOR="`echo 'AC_PACKAGE_VERSION' | cut -d. -f 1`"
MINOR="`echo 'AC_PACKAGE_VERSION' | cut -d. -f 2`"
PATCHLEVEL="`echo 'AC_PACKAGE_VERSION' | cut -d. -f 3`"
VERSION=$MAJOR.$MINOR
PATCHLEVEL_STR=$VERSION.$PATCHLEVEL

AC_SUBST(MAJOR)
AC_SUBST(MINOR)
AC_SUBST(PATCHLEVEL)
AC_SUBST(VERSION)
AC_SUBST(PATCHLEVEL_STR)

AM_INIT_AUTOMAKE([foreign])


AC_PROG_MAKE_SET

dnl AM_INIT_AUTOMAKE(tclreadline, $VERSION)

AC_CANONICAL_HOST
AC_PROG_CC
AC_EXEEXT
dnl AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
AC_SUBST(LIBTOOL_DEPS)

AC_C_CONST
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_PROG_LN_S


# -- WHICH TCL TO USE
AC_ARG_WITH(
    tcl,
    [  --with-tcl=DIR          where to look for tclConfig.sh],
    tcl_search=$withval,
    tcl_search=""
)

AC_MSG_CHECKING([which tclConfig.sh to use])
TCL_LIB_DIR=""
for dir in $tcl_search /usr/lib /usr/local/lib $exec_prefix/lib /usr/local/lib/unix /opt/tcl/lib; do
    if test -r $dir/tclConfig.sh; then
        TCL_LIB_DIR=$dir
        break
    fi
done

if test -z "$TCL_LIB_DIR"; then
    AC_MSG_ERROR(Can't find Tcl libraries.  Use --with-tcl to specify the directory containing tclConfig.sh on your system.)
fi

. $TCL_LIB_DIR/tclConfig.sh
AC_MSG_RESULT($TCL_LIB_DIR/tclConfig.sh)
AC_MSG_CHECKING([for your tcl version])
AC_MSG_RESULT([$TCL_VERSION, patchlevel $TCL_PATCH_LEVEL])

# Check, if tcl_version is > 8.0
if test $TCL_MAJOR_VERSION -lt 8; then
    AC_MSG_ERROR(need tcl 8.0 or higher.)
fi


# -----------------------------------------------------------------------
#   Set up a new default --prefix.
# -----------------------------------------------------------------------

# this is the default anyway:
# AC_PREFIX_DEFAULT(/usr/local)

if test "${prefix}" = "NONE"; then
    prefix=$TCL_PREFIX
fi
TCLRL_LIBDIR="${prefix}/lib"
AC_SUBST(TCLRL_LIBDIR)
TCLRL_DIR="${TCLRL_LIBDIR}/tclreadline$VERSION"
AC_SUBST(TCLRL_DIR)

# HEADERS
AC_HEADER_STDC
AC_CHECK_HEADERS(string.h)

#--------------------------------------------------------------------
#   Check for tcl.h
#--------------------------------------------------------------------

AC_ARG_WITH(tcl-includes,
            [  --with-tcl-includes=DIR where to look for tcl.h],
            tcl_includes=$withval, tcl_includes=$prefix/include)

AC_MSG_CHECKING([where to look for tcl.h])
TCL_INCLUDE_DIR=""
for dir in $tcl_includes $prefix/include/ ${TCL_INCLUDE_SPEC#-I} /usr/include; do
    if test -r $dir/tcl.h; then
        TCL_INCLUDE_DIR=$dir
        break
    fi
done

if test -z "$TCL_INCLUDE_DIR"; then
    AC_MSG_ERROR([
  Can't find tcl.h.  Use --with-tcl-includes to specify the directory
  containing tcl.h on your system.])
else
    AC_MSG_RESULT($TCL_INCLUDE_DIR/tcl.h)
fi
#--------------------------------------------------------------------
#   Check for readline.h
#--------------------------------------------------------------------

AC_ARG_WITH(readline-includes,
            [  --with-readline-includes=DIR readline include files in DIR],
            rl_includes=$withval, rl_includes="")

dnl look for readline/readline.h
dnl          ^^^^^^^^
AC_MSG_CHECKING([where to look for readline.h])
READLINE_INCLUDE_DIR=""
for dir in $rl_includes /usr/local/include /usr/include ; do
    if test -r $dir/readline/readline.h; then
        READLINE_INCLUDE_DIR=$dir
        break
    fi
done

dnl look directly in the include dirs for readline.h
if test -z "$READLINE_INCLUDE_DIR"; then
    for dir in $rl_includes /usr/local/include /usr/include ; do
    if test -r $dir/readline.h; then
        READLINE_INCLUDE_DIR=$dir
        AC_DEFINE_UNQUOTED(READLINE_LIBRARY, 1, [ Define if we have libreadline. ])
        break
    fi
    done
fi

if test -z "$READLINE_INCLUDE_DIR"; then
    AC_MSG_ERROR([
  Can't find readline.h.
  Use --with-readline-includes to specify the directory
  containing readline.h on your system.])
fi

AC_ARG_WITH(readline-library,
    [  --with-readline-library=DIR
          lib spec to readline (e.g. '-L/usr/local/lib -lreadline')],
    LIBS="$LIBS $withval",
    AC_SEARCH_LIBS(rl_callback_read_char, readline, ,
        AC_MSG_RESULT([
            Your readline version does not support readline's alternate interface.
            Please upgrade to readline >= 2.2 and retry.
        ])
        exit
    )
)



# check for readline's rl_executing_macro
# (could be macro.c's private variable _rl_executing_macro).

AC_CHECK_DECL(rl_executing_macro
, AC_DEFINE(EXECUTING_MACRO_NAME, rl_executing_macro,
  [ Define the name of the executing macro variable in libreadline. ])
, AC_MSG_CHECKING([for _rl_executing_macro in -lreadline])
  AC_TRY_LINK(,[
    extern char* _rl_executing_macro;
    _rl_executing_macro = (char*) 0;
  ]
  , AC_MSG_RESULT(yes)
    AC_DEFINE(EXECUTING_MACRO_HACK, 1,
    [ Define if EXECUTING_MACRO_NAME is resolved in libreadline. ])
    AC_DEFINE(EXECUTING_MACRO_NAME, _rl_executing_macro,
    [ Define the name of the executing macro variable in libreadline. ])
  , AC_MSG_RESULT(no))
, [AC_INCLUDES_DEFAULT
#include "$READLINE_INCLUDE_DIR/readline.h"])

# check for readline's rl_cleanup_after_signal

AC_MSG_CHECKING([for rl_cleanup_after_signal() in -lreadline])
AC_TRY_LINK(,[
    extern void rl_cleanup_after_signal();
    rl_cleanup_after_signal();
],
    AC_MSG_RESULT(yes);
    AC_DEFINE(CLEANUP_AFER_SIGNAL, 1,
    [ Define if rl_cleanup_after_signal is resolved in libreadline. ]),
    AC_MSG_RESULT(no))


AC_MSG_CHECKING([for the readline version number])
AC_TRY_RUN([
#include <stdio.h>
int main () {
    FILE *fp = fopen("conftestversion", "w");
    extern char *rl_library_version;
    fprintf(fp, "%s", rl_library_version);
    close(fp);
    return 0;
}],
    READLINE_VERSION=`cat conftestversion`
    AC_MSG_RESULT($READLINE_VERSION),
    AC_MSG_RESULT(failed),
    AC_MSG_RESULT(no cross compiling)
)


AC_ARG_ENABLE(tclshrl,
    [  --enable-tclshrl        build statically linked tclshrl],
    [dnl action if given
    case "${enableval}" in
        yes) enable_static=true ;;
        no)  enable_static=false ;;
        *)   AC_MSG_ERROR(bad value ${enableval} for --enable-static) ;;
    esac
    ],
    [dnl action if not given
    enable_static=false
    ]
)
AM_CONDITIONAL(STATIC_TCLSHRL, test x$enable_static = xtrue)

AC_ARG_ENABLE(wishrl,
    [  --enable-wishrl         build statically linked wishrl],
    [dnl action if given
    case "${enableval}" in
        yes)
        enable_static=true
        dnl source the tkConfig.sh which defines TK_LIB_SPEC
        . $TCL_LIB_DIR/tkConfig.sh
        AC_SUBST(TK_LIB_SPEC)
        ;;
        no)  enable_static=false ;;
        *)   AC_MSG_ERROR(bad value ${enableval} for --enable-static) ;;
    esac
    ],
    [dnl action if not given
    enable_static=false
    ]
)
AM_CONDITIONAL(STATIC_WISHRL, test x$enable_static = xtrue)


AC_SUBST(TCL_INCLUDE_DIR)
AC_SUBST(TCL_LIB_SPEC)
AC_SUBST(READLINE_INCLUDE_DIR)
AC_OUTPUT(Makefile tclreadline.h tclreadlineInit.tcl tclreadlineSetup.tcl tclreadline.n pkgIndex.tcl)
