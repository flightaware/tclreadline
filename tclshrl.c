
 /* ==================================================================

    FILE: "/home/joze/src/tclreadline/tclshrl.c"
    LAST MODIFICATION: "Thu Dec 16 21:51:19 1999 (joze)"
    (C) 1998, 1999 by Johannes Zellner, <johannes@zellner.org>
    $Id$
    ---

    tclreadline -- gnu readline for tcl
    Copyright (C) 1999  Johannes Zellner

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

    <johannes@zellner.org>, http://www.zellner.org/tclreadline/

    ================================================================== */  

#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif

#include <tcl.h>
#include <tclreadline.h>

#if 0
#include <assert.h>
#endif

extern int Tclreadline_Init(Tcl_Interp *interp);
extern int Tclreadline_SafeInit(Tcl_Interp *interp);

int
TclreadlineAppInit(Tcl_Interp* interp)
{
    char file[0xff];
    int status;
#if 0
    assert(Tcl_InitStubs(interp, TCL_VERSION, 0));
#endif
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
	exit (1);
    }
    return TCL_OK;
}

int
main(int argc, char *argv[])
{
    Tcl_Main(argc, argv, TclreadlineAppInit);
    return 0;
}
