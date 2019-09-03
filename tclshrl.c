 /* ================================================================== *
  * FILE: tclshrl.c
  * $Id$
  * ---
  * tclreadline -- gnu readline for tcl
  * https://github.com/flightaware/tclreadline/
  * Copyright (c) 1998 - 2014, Johannes Zellner <johannes@zellner.org>
  * This software is copyright under the BSD license.
  * ================================================================== */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stdlib.h>
#include <tcl.h>
#include <tclreadline.h>

extern int Tclreadline_Init(Tcl_Interp *interp);
extern int Tclreadline_SafeInit(Tcl_Interp *interp);

int
TclreadlineAppInit(Tcl_Interp* interp)
{
    char file[0xff];
    int status;
    if (TCL_ERROR == Tcl_Init(interp)) {
        return TCL_ERROR;
    }
    if (TCL_ERROR == Tclreadline_Init(interp)) {
        return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "tclreadline",
                      Tclreadline_Init, Tclreadline_SafeInit);
#if (TCL_MAJOR_VERSION == 7) && (TCL_MINOR_VERSION == 4)
    tcl_RcFileName = "~/.tclshrc";
#else
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.tclshrc", TCL_GLOBAL_ONLY);
#endif
    sprintf(file, "%s/tclreadlineInit.tcl", TCLRL_LIBRARY);
    if ((status = Tcl_EvalFile(interp, file))) {
        fprintf(stderr, "(TclreadlineAppInit) unable to eval %s\n", file);
        exit (EXIT_FAILURE);
    }
    return TCL_OK;
}

int
main(int argc, char *argv[])
{
    Tcl_Main(argc, argv, TclreadlineAppInit);
    return EXIT_SUCCESS;
}
