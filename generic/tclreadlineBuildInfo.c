/*
 * tclreadlineInfo.c --
 *
 * Implements the pkginfo and build-info commands for the extension.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tclreadline.h"
#include "tclreadlineUuid.h"
#include <string.h>

#undef STRINGIFY
#  define STRINGIFY(x) STRINGIFY1(x)
#  define STRINGIFY1(x) #x

static const char *buildData =
		    PACKAGE_VERSION "+" STRINGIFY(TCLREADLINE_VERSION_UUID)
#if defined(__clang__) && defined(__clang_major__)
			    ".clang-" STRINGIFY(__clang_major__)
#if __clang_minor__ < 10
			    "0"
#endif
			    STRINGIFY(__clang_minor__)
#endif
#if defined(__cplusplus) && !defined(__OBJC__)
			    ".cplusplus"
#endif
#ifndef NDEBUG
			    ".debug"
#endif
#if !defined(__clang__) && !defined(__INTEL_COMPILER) && defined(__GNUC__)
			    ".gcc-" STRINGIFY(__GNUC__)
#if __GNUC_MINOR__ < 10
			    "0"
#endif
			    STRINGIFY(__GNUC_MINOR__)
#endif
#ifdef __INTEL_COMPILER
			    ".icc-" STRINGIFY(__INTEL_COMPILER)
#endif
#ifdef TCL_MEM_DEBUG
			    ".memdebug"
#endif
#if defined(_MSC_VER)
			    ".msvc-" STRINGIFY(_MSC_VER)
#endif
#ifdef USE_NMAKE
			    ".nmake"
#endif
#ifndef TCL_CFG_OPTIMIZED
			    ".no-optimize"
#endif
#ifdef __OBJC__
			    ".objective-c"
#if defined(__cplusplus)
			    "plusplus"
#endif
#endif
#ifdef TCL_CFG_PROFILED
			    ".profile"
#endif
#ifdef PURIFY
			    ".purify"
#endif
#ifdef STATIC_BUILD
			    ".static"
#endif
    ;


/*
 * MyExtensionBuildInfoObjCmd --
 *
 *	Implements build-info command. This is cloned from the Tcl core
 *	implementation. One could have just used the Tcl implementation
 *	via the Tcl_CommandInfo interface but there is not guarantee that
 *	the "clientData" parameter which is not public will take a different
 *	form a future Tcl release. So we duplicate the code.
 *
 * Results:
 *	Standard Tcl result code.
 */
int
BuildInfoObjCmd(
    void *clientData,
    Tcl_Interp *interp,		/* Current interpreter. */
    int objc,		/* Number of arguments. */
    Tcl_Obj *const objv[])	/* Argument objects. */
{
    char buf[80];
    const char *arg, *p, *q;
    Tcl_Size len;
    int idx;
    static const char *identifiers[] = {
	"commit", "compiler", "patchlevel", "version", NULL
    };
    enum Identifiers {
	ID_COMMIT, ID_COMPILER, ID_PATCHLEVEL, ID_VERSION, ID_OTHER
    };

    if (objc > 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "?option?");
	return TCL_ERROR;
    } else if (objc < 2) {
	Tcl_SetObjResult(interp, Tcl_NewStringObj(buildData, -1));
	return TCL_OK;
    }

    /*
     * Query for a specific piece of build info
     */

    if (Tcl_GetIndexFromObj(NULL, objv[1], identifiers, NULL, TCL_EXACT,
	    &idx) != TCL_OK) {
	idx = ID_OTHER;
    }

    switch (idx) {
    case ID_PATCHLEVEL:
	if ((p = strchr(buildData, '+')) != NULL) {
	    memcpy(buf, buildData, p - buildData);
	    buf[p - buildData] = '\0';
	    Tcl_SetObjResult(interp, Tcl_NewStringObj(buf, -1));
	}
	return TCL_OK;
    case ID_VERSION:
	if ((p = strchr(buildData, '.')) != NULL) {
	    const char *r = strchr(p++, '+');
	    q = strchr(p, '.');
	    p = (q < r) ? q : r;
	}
	if (p != NULL) {
	    memcpy(buf, buildData, p - buildData);
	    buf[p - buildData] = '\0';
	    Tcl_SetObjResult(interp, Tcl_NewStringObj(buf, -1));
	}
	return TCL_OK;
    case ID_COMMIT:
	if ((p = strchr(buildData, '+')) != NULL) {
	    if ((q = strchr(p++, '.')) != NULL) {
		memcpy(buf, p, q - p);
		buf[q - p] = '\0';
		Tcl_SetObjResult(interp, Tcl_NewStringObj(buf, -1));
	    } else {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(p, -1));
	    }
	}
	return TCL_OK;
    case ID_COMPILER:
	for (p = strchr(buildData, '.'); p++; p = strchr(p, '.')) {
	    /*
	     * Does the word begin with one of the standard prefixes?
	     */
	    if (!strncmp(p, "clang-", 6)
		    || !strncmp(p, "gcc-", 4)
		    || !strncmp(p, "icc-", 4)
		    || !strncmp(p, "msvc-", 5)) {
		if ((q = strchr(p, '.')) != NULL) {
		    memcpy(buf, p, q - p);
		    buf[q - p] = '\0';
		    Tcl_SetObjResult(interp, Tcl_NewStringObj(buf, -1));
		} else {
		    Tcl_SetObjResult(interp, Tcl_NewStringObj(p, -1));
		}
		return TCL_OK;
	    }
	}
	break;
    default:		/* Boolean test for other identifiers' presence */
	arg = Tcl_GetStringFromObj(objv[1], &len);
	for (p = strchr(buildData, '.'); p++; p = strchr(p, '.')) {
	    if (!strncmp(p, arg, len)
		    && ((p[len] == '.') || (p[len] == '-') || (p[len] == '\0'))) {
		if (p[len] == '-') {
		    p += len;
		    q = strchr(++p, '.');
		    if (!q) {
			q = p + strlen(p);
		    }
		    memcpy(buf, p, q - p);
		    buf[q - p] = '\0';
		    Tcl_SetObjResult(interp, Tcl_NewStringObj(buf, -1));
		} else {
		    Tcl_SetObjResult(interp, Tcl_NewBooleanObj(1));
		}
		return TCL_OK;
	    }
	}
    }
    Tcl_SetObjResult(interp, Tcl_NewBooleanObj(0));
    return TCL_OK;
}

