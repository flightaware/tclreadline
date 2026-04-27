#
# Include the TEA standard macro set
#

builtin(include,tclconfig/tcl.m4)

#
# Add here whatever m4 macros you want to define for your package
#

#------------------------------------------------------------------------
# HIGHEST_C_STANDARD
#
#   Determines the highest C standard supported by the compiler.
#   Tests standards in descending order: C23, C17, C11, C99, C90.
#
# Arguments:
#   None
#
# Results:
#   Sets the following variables:
#     C_STD_CFLAGS - Compiler flags for the highest supported standard
#     C_STD_VERSION - The highest supported standard (e.g., "c23", "c17")
#------------------------------------------------------------------------
AC_DEFUN([HIGHEST_C_STANDARD], [
    AC_MSG_CHECKING([for highest supported C standard])

    # Save current CFLAGS
    SAVE_CFLAGS_STD="$CFLAGS"
    SAVE_LIBS_STD="$LIBS"
    LIBS=""

    # Test standards in descending order
    for std in c23 c17 c11 c99 c90; do
        CFLAGS="-std=$std"
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])], [
            C_STD_VERSION="$std"
            C_STD_CFLAGS="-std=$std"
            break
        ], [])
    done

    # Restore CFLAGS and LIBS
    CFLAGS="$SAVE_CFLAGS_STD"
    LIBS="$SAVE_LIBS_STD"

    if test -n "$C_STD_VERSION"; then
        AC_MSG_RESULT([$C_STD_VERSION])
    else
        AC_MSG_RESULT([none, using compiler default])
        C_STD_CFLAGS=""
        C_STD_VERSION="default"
    fi

    AC_SUBST(C_STD_CFLAGS)
    AC_SUBST(C_STD_VERSION)
])

#------------------------------------------------------------------------
# CHECK_C23_EMBED
#
#   Checks if the compiler supports the C23 #embed directive.
#
# Arguments:
#   None
#
# Results:
#   Defines HAVE_C23_EMBED if #embed is supported
#   Sets the variable have_c23_embed to "yes" or "no"
#------------------------------------------------------------------------
AC_DEFUN([CHECK_C23_EMBED], [
    AC_MSG_CHECKING([if compiler supports @%:@embed directive])

    # Create a temporary test file to embed
    echo -n "test" > conftest_embed.txt

    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        const char embedded_data[] = {
        #embed "conftest_embed.txt"
            ,0
        };
    ]], [[
        return sizeof(embedded_data) != 5;
    ]])], [
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_C23_EMBED], [1], [Define if compiler supports C23 @%:@embed directive])
        have_c23_embed=yes
    ], [
        AC_MSG_RESULT([no])
        have_c23_embed=no
    ])

    # Clean up
    rm -f conftest_embed.txt
])

#------------------------------------------------------------------------
# CHECK_ZIPFS
#
#  Checks if the Tcl library has zipfs support.
#
# Arguments:
#  None
#
# Results:
#  Defines HAVE_ZIPFS if zipfs support is available
#  Sets the variable have_zipfs to "yes" or "no"
#------------------------------------------------------------------------
AC_DEFUN([CHECK_ZIPFS], [
    AC_MSG_CHECKING([for zipfs support in Tcl])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #undef USE_TCL_STUBS 
        #include <tcl.h>
    ]], [[
        TclZipfs_Mount(NULL, NULL, NULL, NULL);
        return 0;
    ]])], [
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_ZIPFS], [1], [Define if Tcl has zipfs support])
        have_zipfs=yes
    ], [
        AC_MSG_RESULT([no])
        have_zipfs=no
    ])
])

#------------------------------------------------------------------------
# CHECK_EMBED_BINARY
#
#   Checks if the toolchain supports embedding binary files into object files
#
# Arguments:
#   None
#
# Results:
#   Sets the following variables:
#     OBJCOPY - The objcopy tool found, or "no" if not found
#     have_embed_binary - "yes" if embedding is supported, "no" otherwise
#
#------------------------------------------------------------------------
AC_DEFUN([CHECK_EMBED_BINARY], [
    # Check if objcopy is available
    AC_CHECK_TOOL([OBJCOPY], [objcopy], [no])
    AC_SUBST(OBJCOPY)

    have_embed_binary=no
    AC_MSG_CHECKING([whether the toolchain supports embedding files in objects])
    if test "$OBJCOPY" != "no"; then
        # Step 1: Create an empty object file
        if $CC -x c -c -o conftest_embed.o /dev/null >&5; then
            # Step 2: Try to embed data using objcopy
            if echo -ne 'working\0' | $OBJCOPY \
                --add-section .embed=/dev/stdin \
                --set-section-flags .embed=data,alloc,load \
                --add-symbol _embed_data=.embed:0,global,object \
                conftest_embed.o 2>&5
            then
                # Step 3: Try to compile and link a test program that uses the embedded data
                hold_cflags=$CFLAGS;  CFLAGS=""
                hold_ldlags=$LDFLAGS; LDFLAGS=""
                hold_libs=$LIBS;      LIBS="conftest_embed.o"
                AC_LINK_IFELSE([AC_LANG_PROGRAM([[
                    #include <string.h>
                    extern const char _embed_data;
                    const char* embed_data = &_embed_data;
                ]],[[
                    return strcmp(embed_data, "working") != 0;
                ]])], [
                    if test "$cross_compiling" = "yes"; then
                        # Cross-compiling: assume it works if we got this far
                        AC_MSG_RESULT([yes (cross-compiling, assumed)])
                        have_embed_binary=yes
                    else
                        if ./conftest$EXEEXT >&5; then
                            AC_MSG_RESULT([yes])
                            have_embed_binary=yes
                        else
                            AC_MSG_RESULT([no (runtime test failed)])
                        fi
                    fi
                ], [
                    AC_MSG_RESULT([no (linking failed)])
                ])
                LIBS=$hold_libs
                LDFLAGS=$hold_ldlags
                CFLAGS=$hold_cflags
            else
                AC_MSG_RESULT([no (objcopy failed)])
            fi

            # Clean up
            rm -f conftest_embed.o
        else
            AC_MSG_RESULT([no (compilation failed)])
        fi
    else
        AC_MSG_RESULT([no (objcopy not found)])
    fi
])

#------------------------------------------------------------------------
# SHIM_TCL_STATICLIBRARY
#
#  Defines Tcl_StaticLibrary as Tcl_StaticPackage for old Tcl versions
#
# Arguments:
#  None
#
# Results:
#  Defines Tcl_StaticLibrary as Tcl_StaticPackage if needed for
#  compatibility with older Tcl releases.
#------------------------------------------------------------------------
AC_DEFUN([SHIM_TCL_STATICLIBRARY], [
    AC_MSG_CHECKING([whether Tcl_StaticLibrary is spelled Tcl_StaticPackage in this Tcl])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #undef USE_TCL_STUBS
        #include <tcl.h>
    ]], [[
        Tcl_StaticLibrary(NULL, NULL, NULL, NULL);
        return 0;
    ]])], [
        AC_MSG_RESULT([no])
    ], [
        AC_MSG_RESULT([yes])
        AC_DEFINE([Tcl_StaticLibrary], [Tcl_StaticPackage], [Define as Tcl_StaticPackage for old Tcl versions])
    ])
])

#------------------------------------------------------------------------
# CHECK_BUILD_TCLSH
#
#  Checks if the build environment can run the Tcl shell (tclsh)
#
# Arguments:
#   None
#
# Results:
#   Sets HAVE_BUILD_TCLSH to "yes" or "no"
#   Sets BUILD_TCLSH to the path of the tclsh program (if HAVE_BUILD_TCLSH is "yes")
#------------------------------------------------------------------------
AC_DEFUN([CHECK_BUILD_TCLSH], [
    AC_MSG_CHECKING([whether the build environment can run tcl scripts])
    HAVE_BUILD_TCLSH=no

    if test -n "$BUILD_TCLSH"; then
        try_tclsh=$BUILD_TCLSH
    elif test -x "$TCLSH_PROG"; then
        try_tclsh=$TCLSH_PROG
    else
        try_tclsh=$(which tclsh 2>/dev/null)
    fi

    if test "$(echo "puts working" | "$try_tclsh" 2>&5)" = "working"; then
        HAVE_BUILD_TCLSH=yes
        BUILD_TCLSH=$try_tclsh
        AC_SUBST(BUILD_TCLSH)
    fi
    AC_MSG_RESULT([$HAVE_BUILD_TCLSH: ${BUILD_TCLSH:-not found}])
    AC_SUBST(HAVE_BUILD_TCLSH)
])

#------------------------------------------------------------------------
#
# TCL_SCRIPT_IFELSE
#
#  Runs a Tcl script and executes one of two code blocks depending on success
#  and whether a TCLSH is available in the build environment (as determined by
#  CHECK_BUILD_TCLSH).
#
# Arguments:
#  $1 - Tcl script to run
#  $2 - Code block to execute if the script succeeds
#  $3 - Code block to execute if the script fails
#
# Results:
#  Executes the appropriate code block
#------------------------------------------------------------------------
AC_DEFUN([TCL_SCRIPT_IFELSE], [[
    prog="if {[catch {$1} e]} {puts stderr \$e; exit 1}"
    if test $HAVE_BUILD_TCLSH = "yes" && echo "$prog" | "$BUILD_TCLSH" 2>&5; then
        eval "$2"
    else
        echo "failed tcl script was:" >&5
        echo "$prog" >&5
        eval "$3"
    fi
]])

