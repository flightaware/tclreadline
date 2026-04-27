 /* ================================================================== *
  * FILE: shrl.c
  * $Id$
  * ---
  * tclreadline -- gnu readline for tcl
  * https://github.com/flightaware/tclreadline/
  * Copyright (c) 1998 - 2014, Johannes Zellner <johannes@zellner.org>
  * This software is copyright under the BSD license.
  * ================================================================== */

#include "tclreadline.h"
#include <stdlib.h>
#if IS_WISHRL
#   include <tk.h>
#endif

#if USE_EMBED_METHOD == EMBED_METHOD_C23_EMBED
static const char setupscript[] = {
#embed "../library/tclreadlineSetup.tcl"
    ,0
};

static const char completerscript[] = {
#embed "../library/tclreadlineCompleter.tcl"
    ,0
};
#elif USE_EMBED_METHOD == EMBED_METHOD_OBJCOPY
/* Symbols defined by objcopy embedding, see makefile target
 * $(SHELL_INIT_SCRIPT_OBJS) */
extern const char _TclReadlineInitScript_tclreadlineSetup_tcl;
extern const char _TclReadlineInitScript_tclreadlineCompleter_tcl;
const char* setupscript = &_TclReadlineInitScript_tclreadlineSetup_tcl;
const char* completerscript = &_TclReadlineInitScript_tclreadlineCompleter_tcl;
#elif USE_EMBED_METHOD == EMBED_METHOD_CHAR_ARRAY
extern const char embed_tclreadlineSetup_tcl[];
extern const char embed_tclreadlineCompleter_tcl[];
const char* setupscript = embed_tclreadlineSetup_tcl;
const char* completerscript = embed_tclreadlineCompleter_tcl;
#endif

int
TclreadlineAppInit(Tcl_Interp* interp)
{
    int code = TCL_OK;

#define TEST_OK(cmd)    if (TCL_OK != (code = cmd)) goto finally;
#define EVAL(script)    TEST_OK(Tcl_EvalEx(interp, script, -1, TCL_EVAL_GLOBAL|TCL_EVAL_DIRECT))

    TEST_OK(Tcl_Init(interp));
#if IS_WISHRL
    TEST_OK(Tk_Init(interp));
#endif
    TEST_OK(Tclreadline_Init(interp));

    Tcl_StaticLibrary(interp, "tclreadline", Tclreadline_Init, Tclreadline_SafeInit);

    Tcl_SetVar(interp, "tcl_rcFileName", "~/.tclshrc", TCL_GLOBAL_ONLY);

#if USE_EMBED_METHOD == EMBED_METHOD_C23_EMBED || USE_EMBED_METHOD == EMBED_METHOD_OBJCOPY || USE_EMBED_METHOD == EMBED_METHOD_CHAR_ARRAY
#   define LOAD_INIT_SCRIPT(name, file)     EVAL(name)
#elif USE_EMBED_METHOD == EMBED_METHOD_ZIPFS
#   define LOAD_INIT_SCRIPT(name, file)     EVAL("source [zipfs root]app/" file)
#elif USE_EMBED_METHOD == EMBED_METHOD_FINDLIBRARY
#   define LOAD_INIT_SCRIPT(name, file)     EVAL("source [file join $tclreadline_library " file "]")
    /* Use tcl_findLibrary to locate and source the init script, which sets tclreadline_library */
    EVAL(
        "if {[file readable [file join [pwd] library tclreadlineSetup.tcl]]} {\n"
        "   set ::tclreadline::library [file join [pwd] library]\n"
        "}\n"
        "tcl_findLibrary tclreadline " TCLRL_VERSION_STR " " PACKAGE_VERSION " tclreadlineSetup.tcl TCLREADLINE_LIBRARY ::tclreadline::library"
    );

    /* Copy to the old location for backwards compatibility */
    EVAL("set tclreadline_library $::tclreadline::library");
#else
#   error "No valid USE_EMBED_METHOD defined"
#endif

    LOAD_INIT_SCRIPT(setupscript,      "tclreadlineSetup.tcl");
    LOAD_INIT_SCRIPT(completerscript,  "tclreadlineCompleter.tcl");
#undef LOAD_INIT_SCRIPT

    EVAL("::tclreadline::readline customcompleter ::tclreadline::ScriptCompleter");

finally:
    return code;
}

int
main(int argc, char *argv[])
{
#if HAVE_ZIPFS
    TclZipfs_AppHook(&argc, &argv);
#endif

#if IS_WISHRL
    Tk_Main(argc, argv, TclreadlineAppInit);
#else
    Tcl_Main(argc, argv, TclreadlineAppInit);
#endif

    return EXIT_SUCCESS;
}
