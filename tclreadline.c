
 /* ==================================================================
    FILE: "/home/joze/src/tclreadline/tclreadline.c"
    LAST MODIFICATION: "Sat, 01 Jul 2000 23:47:30 +0200 (joze)"
    (C) 1998 - 2000 by Johannes Zellner, <johannes@zellner.org>
    $Id$
    ---
    tclreadline -- gnu readline for tcl
    http://www.zellner.org/tclreadline/
    Copyright (c) 1998 - 2000, Johannes Zellner <johannes@zellner.org>
    This software is copyright under the BSD license.
    ================================================================== */  

#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif

#include <tcl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined (READLINE_LIBRARY)
#   include <readline.h>
#   include <history.h>
#else
#   include <readline/readline.h>
#   include <readline/history.h>
#endif


/*
 * this prototype is missing
 * in readline.h
 */
void rl_extend_line_buffer(int len);

#ifdef EXECUTING_MACRO_HACK
/**
 * this prototype is private in readline's file `macro.c'.
 * We need it here to decide, if we should read more
 * characters from a macro. Dirty, but it should work.
 */
extern char* _rl_executing_macro;
#endif

#include "tclreadline.h"

#define MALLOC(size) Tcl_Alloc((int) size)
#define FREE(ptr) if (ptr) { Tcl_Free((char*) ptr); ptr = 0; }

enum {
    _CMD_SET     = (1 << 0),
    _CMD_GET     = (1 << 1)
};


typedef struct cmds_t {
    struct cmds_t* prev;
    char**         cmd;
    struct cmds_t* next;
} cmds_t;


#define ISWHITE(c) ((' ' == c) || ('\t' == c) || ('\n' == c))

/* forward declarations. */
static char* stripleft(char* in);
static char* stripright(char* in);
static char* stripwhite(char* in);
static int TclReadlineLineComplete(void);
static void TclReadlineTerminate(int state);
static char* TclReadlineQuote(char* text, char* quotechars);
static int TclReadlineCmd(ClientData clientData, Tcl_Interp* interp, int argc, char** argv);
static void TclReadlineReadHandler(ClientData clientData, int mask);
static void TclReadlineLineCompleteHandler(char* ptr);
static int TclReadlineInitialize(Tcl_Interp* interp, char* historyfile);
static int blank_line(char* str);
static char** TclReadlineCompletion(char* text, int start, int end);
static char* TclReadline0generator(char* text, int state);
static char* TclReadlineKnownCommands(char* text, int state, int mode);
static int TclReadlineParse(char** args, int maxargs, char* buf);

/* must be non-static */
int Tclreadline_SafeInit(Tcl_Interp* interp);
int Tclreadline_Init(Tcl_Interp* interp);


enum { 
    LINE_PENDING = -1,
    LINE_EOF = (1 << 8),
    LINE_COMPLETE = (1 << 9)
};

/**
 * global variables
 */
static int tclrl_state = TCL_OK;
static char* tclrl_eof_string = (char*) NULL;
static char* tclrl_custom_completer = (char*) NULL;
static char* tclrl_last_line = (char*) NULL;
static int tclrl_use_builtin_completer = 1;
static int tclrl_history_length = -1;
Tcl_Interp* tclrl_interp = (Tcl_Interp*) NULL;

static char* tclrl_license =
"   Copyright (c) 1998 - 2000, Johannes Zellner <johannes@zellner.org>\n"
"   All rights reserved.\n"
"   \n"
"   Redistribution and use in source and binary forms, with or without\n"
"   modification, are permitted provided that the following conditions\n"
"   are met:\n"
"   \n"
"     * Redistributions of source code must retain the above copyright\n"
"       notice, this list of conditions and the following disclaimer.\n"
"     * Redistributions in binary form must reproduce the above copyright\n"
"       notice, this list of conditions and the following disclaimer in the\n"
"       documentation and/or other materials provided with the distribution.\n"
"     * Neither the name of Johannes Zellner nor the names of contributors\n"
"       to this software may be used to endorse or promote products derived\n"
"       from this software without specific prior written permission.\n"
"       \n"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n"
"   ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n"
"   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n"
"   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR\n"
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,\n"
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,\n"
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR\n"
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF\n"
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING\n"
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n"
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.";



static char*
stripleft(char* in)
{
    char* ptr = in;
    while (*ptr && *ptr <= ' ')
	ptr++;
    if (in != ptr)
	memmove(in, ptr, strlen(ptr) + 1);
    return in;
}

static char*
stripright(char* in)
{
    char* ptr;
    for (ptr = strchr(in, '\0') - 1; ptr >= in && *ptr <= ' '; ptr--)
	*ptr = '\0';
    return in;
}

static char*
stripwhite(char* in)
{
    stripleft(in);
    stripright(in);
    return in;
}

static int
TclReadlineLineComplete(void)
{
    return !(tclrl_state == LINE_PENDING);
}

static void
TclReadlineTerminate(int state)
{
    tclrl_state = state;
    rl_callback_handler_remove();
}

static char*
TclReadlineQuote(char* text, char* quotechars)
{
    char* ptr;
    char* result_c;
    int i, len = strlen(quotechars);
    Tcl_DString result;

    Tcl_DStringInit(&result);
    for (ptr = text; ptr && *ptr; ptr++) {
	for (i = 0; i < len; i++) {
	    if (quotechars[i] == *ptr) {
		Tcl_DStringAppend(&result, "\\", 1);
		break;
	    }
	}
	Tcl_DStringAppend(&result, ptr, 1);
    }
    result_c = strdup(Tcl_DStringValue(&result));
    return result_c;
}

static int
TclReadlineCmd(
    ClientData  clientData,
    Tcl_Interp* interp,     /* Current interpreter */
    int         argc,       /* Number of arguments */
    char**      argv        /* Argument strings    */
	      )
{
    int i, obj_idx, status;
    Tcl_Obj** objv = (Tcl_Obj**) MALLOC((argc + 1) * sizeof(Tcl_Obj *));

    static char *subCmds[] = {
	"read", "initialize", "write", "add", "complete",
	"customcompleter", "builtincompleter", "eofchar",
	"reset-terminal", "bell",
	(char *) NULL
    };
    enum SubCmdIdx {
	TCLRL_READ, TCLRL_INITIALIZE, TCLRL_WRITE, TCLRL_ADD, TCLRL_COMPLETE,
	TCLRL_CUSTOMCOMPLETER, TCLRL_BUILTINCOMPLETER, TCLRL_EOFCHAR,
	TCLRL_RESET_TERMINAL, TCLRL_BELL
    };


    Tcl_ResetResult(interp); /* clear the result space */

    for (i = 0;  i < argc;  i++) {
	Tcl_Obj* objPtr = Tcl_NewStringObj(argv[i], -1);
	Tcl_IncrRefCount(objPtr);
	objv[i] = objPtr;
    }
    objv[argc] = 0; /* terminate */

    if (argc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "option ?arg arg ...?");
	return TCL_ERROR;
    }

    status = Tcl_GetIndexFromObj
    (interp, objv[1], subCmds, "option", 0, (int *) &obj_idx);

    if (status != TCL_OK) {
	FREE(objv)
	return status;
    }

    switch (obj_idx) {

	case TCLRL_READ:

	    rl_callback_handler_install(argc == 3 ? argv[2] : "%",
		TclReadlineLineCompleteHandler);

	    Tcl_CreateFileHandler(0, TCL_READABLE,
		TclReadlineReadHandler, (ClientData) NULL);

	    /**
	     * Main Loop.
	     * XXX each modification of the global variables
	     *     which terminates the main loop must call
	     *     rl_callback_handler_remove() to leave
	     *     readline in a defined state.          XXX
	     */
	    tclrl_state = LINE_PENDING;

	    while (!TclReadlineLineComplete()) {
#ifdef EXECUTING_MACRO_HACK
		/**
		 * check first, if more characters are
		 * available from _rl_executing_macro,
		 * because Tcl_DoOneEvent() will (naturally)
		 * not detect this `event'.
		 */
		if (_rl_executing_macro)
		    TclReadlineReadHandler((ClientData) NULL, TCL_READABLE);
		else
#endif
		    Tcl_DoOneEvent(TCL_ALL_EVENTS);
	    }

	    Tcl_DeleteFileHandler(0);

	    switch (tclrl_state) {

		case LINE_COMPLETE:

		    return TCL_OK;
		    /* NOTREACHED */
		    break;

		case LINE_EOF:
		    if (tclrl_eof_string)
			return Tcl_Eval(interp, tclrl_eof_string);
		    else
			return TCL_OK;
		    /* NOTREACHED */
		    break;

		default:
		    return tclrl_state;
		    /* NOTREACHED */
		    break;
	    }
	    break;

	case TCLRL_INITIALIZE:
	    if (3 != argc) {
		Tcl_WrongNumArgs(interp, 2, objv, "historyfile");
		return TCL_ERROR;
	    } else {
		return TclReadlineInitialize(interp, argv[2]);
	    }
	    break;

	case TCLRL_WRITE:
	    if (3 != argc) {
		Tcl_WrongNumArgs(interp, 2, objv, "historyfile");
		return TCL_ERROR;
	    }  else if (write_history(argv[2])) {
		Tcl_AppendResult(interp, "unable to write history to `",
		    argv[2], "'\n", (char*) NULL);
		return TCL_ERROR;
	    }
	    if (tclrl_history_length >= 0) {
		history_truncate_file(argv[2], tclrl_history_length);
	    }
	    return TCL_OK;
	    break;

	case TCLRL_ADD:
	    if (3 != argc) {
		Tcl_WrongNumArgs(interp, 2, objv, "completerLine");
		return TCL_ERROR;
	    } else if (TclReadlineKnownCommands(argv[2], (int) 0, _CMD_SET)) {
		Tcl_AppendResult(interp, "unable to add command \"",
		    argv[2], "\"\n", (char*) NULL);
	    }
	    break;

	case TCLRL_COMPLETE:
	    if (3 != argc) {
		Tcl_WrongNumArgs(interp, 2, objv, "line");
		return TCL_ERROR;
	    } else if (Tcl_CommandComplete(argv[2])) {
		Tcl_AppendResult(interp, "1", (char*) NULL);
	    } else {
		Tcl_AppendResult(interp, "0", (char*) NULL);
	    }
	    break;

	case TCLRL_CUSTOMCOMPLETER:
	    if (argc > 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "?scriptCompleter?");
		return TCL_ERROR;
	    } else if (3 == argc) {
		if (tclrl_custom_completer)
		    FREE(tclrl_custom_completer);
		if (!blank_line(argv[2]))
		    tclrl_custom_completer = stripwhite(strdup(argv[2]));
	    }
	    Tcl_AppendResult(interp, tclrl_custom_completer, (char*) NULL);
	    break;

	case TCLRL_BUILTINCOMPLETER:
	    if (argc > 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "?boolean?");
		return TCL_ERROR;
	    } else if (3 == argc) {
		int bool = tclrl_use_builtin_completer;
		if (TCL_OK != Tcl_GetBoolean(interp, argv[2], &bool)) {
		    Tcl_AppendResult(interp,
			"wrong # args: should be a boolean value.",
			(char*) NULL);
		    return TCL_ERROR;
		} else {
		    tclrl_use_builtin_completer = bool;
		}
	    }
	    Tcl_AppendResult(interp, tclrl_use_builtin_completer ? "1" : "0",
		(char*) NULL);
	    break;

	case TCLRL_EOFCHAR:
	    if (argc > 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "?script?");
		return TCL_ERROR;
	    } else if (3 == argc) {
		if (tclrl_eof_string)
		    FREE(tclrl_eof_string);
		if (!blank_line(argv[2]))
		    tclrl_eof_string = stripwhite(strdup(argv[2]));
	    }
	    Tcl_AppendResult(interp, tclrl_eof_string, (char*) NULL);
	    break;

	case TCLRL_RESET_TERMINAL:
	    /* TODO: add this to the completer */
	    if (argc > 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "?terminal-name?");
		return TCL_ERROR;
	    }
	    if (3 == argc) {
		/*
		 * - tcl8.0 doesn't have Tcl_GetString()
		 * - rl_reset_terminal() might be defined
		 *   to take no arguments. This might produce
		 *   a compiler warning.
		 */
		rl_reset_terminal(Tcl_GetStringFromObj(objv[2], (int*) NULL));
#ifdef CLEANUP_AFER_SIGNAL
	    } else {
		rl_cleanup_after_signal();
#endif
	    }
	    break;

	case TCLRL_BELL:
	    /*
	     * ring the terminal bell obeying the current
	     * settings -- audible or visible.
	     */
	    ding();
	    break;

	default:
	    goto BAD_COMMAND;
	    /* NOTREACHED */
	    break;
    }

    return TCL_OK;

BAD_COMMAND:
    Tcl_AppendResult(interp,
	"wrong # args: should be \"readline option ?arg ...?\"",
	(char*) NULL);
    return TCL_ERROR;

}

static void
TclReadlineReadHandler(ClientData clientData, int mask)
{
    if (mask & TCL_READABLE) {
#ifdef EXECUTING_MACRO_HACK
	do {
#endif
	    rl_callback_read_char();
#ifdef EXECUTING_MACRO_HACK
	    /**
	     * check, if we're inside a macro and
	     * if so, read all macro characters
	     * until the next eol.
	     */
	} while (_rl_executing_macro && !TclReadlineLineComplete());
#endif
    }
}

static void
TclReadlineLineCompleteHandler(char* ptr)
{
    if (!ptr) { /* <c-d> */

	TclReadlineTerminate(LINE_EOF);

    } else {

	/**
	 * From version 0.9.3 upwards, all lines are
	 * returned, even empty lines. (Only non-empty
	 * lines are stuffed in readline's history.)
	 * The calling script is responsible for handling
	 * empty strings.
	 */

	char* expansion = (char*) NULL;
	int status = history_expand(ptr, &expansion);

	if (status >= 1) {
	    /* TODO: make this a valid tcl output */
	    printf("%s\n", expansion);
	} else if (-1 == status) {
	    Tcl_AppendResult
	    (tclrl_interp, "error in history expansion\n", (char*) NULL);
	    TclReadlineTerminate(TCL_ERROR);
	}
	/**
	 * TODO: status == 2 ...
	 */

	Tcl_AppendResult(tclrl_interp, expansion, (char*) NULL);

#ifdef EXECUTING_MACRO_HACK
	/**
	 * don't stuff macro lines
	 * into readline's history.
	 */
	if(!_rl_executing_macro) {
#endif
	    /**
	     * don't stuff empty lines
	     * into readline's history.
	     * don't stuff twice the same
	     * line into readline's history.
	     */
	    if (expansion && *expansion && (!tclrl_last_line ||
		    strcmp(tclrl_last_line, expansion))) {
		add_history(expansion);
	    }
	    if (tclrl_last_line)
		free(tclrl_last_line);
	    tclrl_last_line = strdup(expansion);
#ifdef EXECUTING_MACRO_HACK
	}
#endif
	/**
	 * tell the calling routines to terminate.
	 */
	TclReadlineTerminate(LINE_COMPLETE);
	FREE(ptr);
	FREE(expansion);
    }
}

int
Tclreadline_SafeInit(Tcl_Interp *interp)
{
    return Tclreadline_Init(interp);
}

int
Tclreadline_Init(Tcl_Interp *interp)
{
    int status;
    Tcl_CreateCommand(interp, "::tclreadline::readline", TclReadlineCmd,
	(ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    tclrl_interp = interp;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::historyLength",
		(char*) &tclrl_history_length, TCL_LINK_INT)))
	return status;

    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::library",
		(char*) &tclrl_library, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
	return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::version",
		(char*) &TCLRL_VERSION, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
	return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::patchLevel",
		(char*) &TCLRL_PATCHLEVEL, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
	return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::license",
		(char*) &tclrl_license, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
	return status;

    if (TCL_OK != (status = Tcl_LinkVar(interp, "tclreadline_library",
		(char*) &tclrl_library, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
	return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "tclreadline_version",
		(char*) &TCLRL_VERSION, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
	return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "tclreadline_patchLevel",
		(char*) &TCLRL_PATCHLEVEL, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
	return status;

    return Tcl_PkgProvide(interp, "tclreadline", TCLRL_VERSION);
}

static int
TclReadlineInitialize(Tcl_Interp* interp, char* historyfile)
{
    rl_readline_name = "tclreadline";
    /*    rl_special_prefixes = "${\"["; */
    rl_special_prefixes = "$";
    /**
     * default is " \t\n\"\\'`@$><=;|&{("
     * removed "(" <-- arrays
     * removed "{" <-- `${' variables 
     * removed "<" <-- completion lists with < ... >
     * added "[]"
     * added "}"
     */
    /* 11.Sep rl_basic_word_break_characters = " \t\n\"\\@$}=;|&[]"; */
    /* besser (11. Sept) 2. (removed \") */
    /* rl_basic_word_break_characters = " \t\n\\@$}=;|&[]"; */
    /* besser (11. Sept) 3. (removed }) */
    rl_basic_word_break_characters = " \t\n\\@$=;|&[]";
#if 0
    rl_basic_quote_characters = "\"{"; /* XXX ??? XXX */
    rl_completer_quote_characters = "\"";
#endif
    /*
       rl_filename_quote_characters
       = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

       rl_filename_quoting_function
       = (CPFunction*) TclReadlineFilenameQuotingFunction;
     */
    /*
       rl_filename_quoting_desired = 1;
     */

    using_history();
    if (!tclrl_eof_string)
	tclrl_eof_string = strdup("puts {}; exit");

    /*
     * try to read historyfile in home
     * directory. If this failes, this
     * is *not* an error.
     */
    rl_attempted_completion_function = (CPPFunction *) TclReadlineCompletion;
    if (read_history(historyfile)) {
	if (write_history(historyfile)) {
	    Tcl_AppendResult (interp, "warning: `",
		historyfile, "' is not writable.", (char*) NULL);
	}
    }
    return TCL_OK;
}

static int
blank_line(char* str)
{
    char* ptr;
    for (ptr = str; ptr && *ptr; ptr++) {
	if (!ISWHITE(*ptr))
	    return 0;
    }
    return 1;
}

static char**
TclReadlineCompletion(char* text, int start, int end)
{
    char** matches = (char**) NULL;
    int status;
    rl_completion_append_character = ' '; /* reset, just in case ... */

    if (text && ('!' == text[0]
	    || (start && rl_line_buffer[start - 1] == '!' /* for '$' */))) {
	char* expansion = (char*) NULL;
	int oldlen = strlen(rl_line_buffer);
	status = history_expand(rl_line_buffer, &expansion);
	if (status >= 1) {
	    rl_extend_line_buffer(strlen(expansion) + 1);
	    strcpy(rl_line_buffer, expansion);
	    rl_end = strlen(expansion);
	    rl_point += strlen(expansion) - oldlen;
	    FREE(expansion);
	    /*
	     * TODO:
	     * because we return 0 == matches,
	     * the filename completer will still beep.
	     rl_inhibit_completion = 1;
	     */
	    return matches;
	}
	FREE(expansion);
    }

    if (tclrl_custom_completer) {
	char start_s[BUFSIZ], end_s[BUFSIZ];
	Tcl_Obj* obj;
	Tcl_Obj** objv;
	int objc;
	int state;
	char* quoted_text = TclReadlineQuote(text, "$[]{}\"");
	char* quoted_rl_line_buffer
	= TclReadlineQuote(rl_line_buffer, "$[]{}\"");
	sprintf(start_s, "%d", start);
	sprintf(end_s, "%d", end);
	Tcl_ResetResult(tclrl_interp); /* clear result space */
	state = Tcl_VarEval(tclrl_interp, tclrl_custom_completer,
	    " \"", quoted_text, "\" ", start_s, " ", end_s,
	    " \"", quoted_rl_line_buffer, "\"", (char*) NULL);
	FREE(quoted_text);
	FREE(quoted_rl_line_buffer);
	if (TCL_OK != state) {
	    Tcl_AppendResult (tclrl_interp, " `", tclrl_custom_completer,
		" \"", quoted_text, "\" ", start_s, " ", end_s,
		" \"", quoted_rl_line_buffer, "\"' failed.", (char*) NULL);
	    TclReadlineTerminate(state);
	    return matches;
	}
	obj = Tcl_GetObjResult(tclrl_interp);
	status = Tcl_ListObjGetElements(tclrl_interp, obj, &objc, &objv);
	if (TCL_OK != status)
	    return matches;

	if (objc) {
	    int i, length;
	    matches = (char**) MALLOC(sizeof(char*) * (objc + 1));
	    for (i = 0; i < objc; i++) {
		matches[i] = strdup(Tcl_GetStringFromObj(objv[i], &length));
		if (1 == objc && !strlen(matches[i])) {
		    FREE(matches[i]);
		    FREE(matches);
		    Tcl_ResetResult(tclrl_interp); /* clear result space */
		    return (char**) NULL;
		}
	    }

	    /**
	     * this is a special one:
	     * if the script returns exactly two arguments
	     * and the second argument is the empty string,
	     * the rl_completion_append_character is set
	     * temporaryly to NULL.
	     */
	    if (2 == objc && !strlen(matches[1])) {
		i--;
		FREE(matches[1]);
		rl_completion_append_character = '\0';
	    }

	    matches[i] = (char*) NULL; /* terminate */
	}
	Tcl_ResetResult(tclrl_interp); /* clear result space */
    }

    if (!matches && tclrl_use_builtin_completer) {
	matches = completion_matches(text, TclReadline0generator);
    }

    return matches;
}

static char*
TclReadline0generator(char* text, int state)
{
    return TclReadlineKnownCommands(text, state, _CMD_GET);
}

static char*
TclReadlineKnownCommands(char* text, int state, int mode)
{
    static int len;
    static cmds_t *cmds = (cmds_t *) NULL, *new;
    char* tmp;
    char* args[256];
    int i, argc;
    char** name;

    char* local_line = (char*) NULL;
    int sub;


    switch (mode) {

	case _CMD_SET:

	    new = (cmds_t *) MALLOC(sizeof(cmds_t));
	    new->next = (cmds_t *) NULL;

	    if (!cmds) {
		cmds = new;
		cmds->prev = new;
	    }
	    else {
		cmds->prev->next = new;
		cmds->prev = new;
	    }

	    tmp = strdup(text);
	    argc = TclReadlineParse(args, sizeof(args), tmp);

	    new->cmd = (char**) MALLOC(sizeof(char*) * (argc + 1));

	    for (i = 0; i < argc; i++)
		new->cmd[i] = args[i];

	    new->cmd[argc] = (char*) NULL;

	    return (char*) NULL;
	    break;


	case _CMD_GET:

	    local_line = strdup(rl_line_buffer);
	    sub = TclReadlineParse(args, sizeof(args), local_line);

	    if (0 == sub || (1 == sub && '\0' != text[0])) {
		if (!state) {
		    new = cmds;
		    len = strlen(text);
		}
		while (new && (name = new->cmd)) {
		    new = new->next;
		    if (!strncmp(name[0], text, len))
			return strdup(name[0]);
		}
		return (char*) NULL;
	    } else {

		if (!state) {

		    new = cmds;
		    len = strlen(text);

		    while (new && (name = new->cmd)) {
			if (!strcmp(name[0], args[0]))
			    break;
			new = new->next;
		    }

		    if (!new)
			return (char*) NULL;

		    for (i = 0; new->cmd[i]; i++) /* EMPTY */;

		    if (sub < i && !strncmp(new->cmd[sub], text, len))
			return strdup(new->cmd[sub]);
		    else
			return (char*) NULL;

		}
		else
		    return (char*) NULL;

		/* NOTREACHED */
		break;
	    }


	default:
	    return (char*) NULL;
	    break;

    }
    /* NOTREACHED */
}

static int
TclReadlineParse(char** args, int maxargs, char* buf)
{
    int nr = 0;

    while (*buf != '\0' && nr < maxargs) {
	/*
	 * Strip whitespace.  Use nulls, so
	 * that the previous argument is terminated
	 * automatically.
	 */
	while (ISWHITE(*buf))
	    *buf++ = '\0';

	if (!(*buf)) /* don't count the terminating NULL */
	    break;

	*args++ = buf;
	nr++;

	while (('\0' != *buf) && !ISWHITE(*buf))
	    buf++;
    }

    *args = '\0';
    return nr;
}
