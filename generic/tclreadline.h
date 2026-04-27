 /* ================================================================== *
  * FILE: tclreadline.h.in
  * $Id$
  * ---
  * tclreadline -- gnu readline for tcl
  * https://github.com/flightaware/tclreadline/
  * Copyright (c) 1998 - 2014, Johannes Zellner <johannes@zellner.org>
  * This software is copyright under the BSD license.
  * ================================================================== */

#ifndef TCLREADLINE_H_
#define TCLREADLINE_H_

#if HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef NOSTUBS
# undef USE_TCL_STUBS
# undef USE_TK_STUBS
#endif

#include <tcl.h>

#if (TCL_MAJOR_VERSION < 8) || (TCL_MAJOR_VERSION == 8 && TCL_MINOR_VERSION < 7)
# undef Tcl_Size
  typedef int Tcl_Size;
# define Tcl_GetSizeIntFromObj Tcl_GetIntFromObj
# define Tcl_NewSizeIntObj Tcl_NewIntObj
# define TCL_SIZE_MAX      INT_MAX
# define TCL_SIZE_MODIFIER ""
#endif

static inline void replace_tclobj(Tcl_Obj** target, Tcl_Obj* replacement)
{
    Tcl_Obj*	old = *target;

    *target = replacement;
    if (*target) Tcl_IncrRefCount(*target);
    if (old) {
        Tcl_DecrRefCount(old);
        old = NULL;
    }
}

#ifdef __cplusplus
extern "C" {
#endif
Tcl_ObjCmdProc BuildInfoObjCmd;

DLLEXPORT int Tclreadline_Init(Tcl_Interp *interp);
DLLEXPORT int Tclreadline_SafeInit(Tcl_Interp *interp);
#ifdef __cplusplus
}
#endif

#endif /* TCLREADLINE_H_ */
