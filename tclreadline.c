
 /* ==================================================================

    FILE: "/diska/home/joze/src/tclreadline/tclreadline.c"
    LAST MODIFICATION: "Wed Aug 25 16:53:28 1999 (joze)"
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


#include <tcl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define READLINE_LIBRARY
#include <readline.h>
#include <history.h>

#include <tclreadline.h>

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

/*
 * forward declarations.
 */
char* stripleft(char* in);
char* stripright(char* in);
char* stripwhite(char* in);
char* TclReadlineQuote(char* text, char* quotechars);
int TclReadlineCmd(ClientData clientData, Tcl_Interp* interp,
    int argc, char** argv);
void TclReadlineDataAvailableHandler(ClientData clientData, int mask);
void TclReadlineLineCompleteHandler(char* ptr);
int Tclreadline_SafeInit(Tcl_Interp* interp);
int Tclreadline_Init(Tcl_Interp* interp);
char* TclReadlineInitialize(char* historyfile);
int blank_line(char* str);
char** TclReadlineCompletion(char* text, int start, int end);
char* TclReadline0generator(char* text, int state);
char* TclReadlineKnownCommands(char* text, int state, int mode);
int TclReadlineParse(char** args, int maxargs, char* buf);

enum { 
    LINE_PENDING,
    LINE_EOF,
    LINE_COMPLETE
};

/**
 * global variables
 */
static int tclrl_line_complete = LINE_PENDING;
static int tclrl_state = TCL_OK;
static char* tclrl_eof_string = (char*) NULL;
static char* tclrl_line = (char*) NULL;
static char* tclrl_custom_completer = (char*) NULL;
static int tclrl_use_builtin_completer = 1;
Tcl_Interp* tclrl_interp = (Tcl_Interp*) NULL;


char*
stripleft(char* in)
{
    char* ptr = in;
    while (*ptr && *ptr <= ' ')
        ptr++;
    if (in != ptr)
        memmove(in, ptr, strlen(ptr) + 1);
    return in;
}

char*
stripright(char* in)
{
    char* ptr;
    for (ptr = strchr(in, '\0') - 1; ptr >= in && *ptr <= ' '; ptr--)
        *ptr = '\0';
    return in;
}

char*
stripwhite(char* in)
{
    stripleft(in);
    stripright(in);
    return in;
}

char*
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

int
TclReadlineCmd(
    ClientData  clientData, /* Main window associated with interpreter  */
    Tcl_Interp* interp,     /* Current interpreter                      */
    int         argc,       /* Number of arguments                      */
    char**      argv        /* Argument strings                         */
)
{
    int		c, length;
    
    if (argc < 2)
        goto BAD_COMMAND;

    c = argv[1][0];
    length = strlen(argv[1]);

    if (c == 'r'  && strncmp(argv[1], "read", length) == 0) {
        
        char* expansion = (char*) NULL;
        int status;
        
        tclrl_line_complete = LINE_PENDING;
        tclrl_state = TCL_OK;
        rl_callback_handler_install(argc == 3 ? argv[2] : "%",
                TclReadlineLineCompleteHandler);

        Tcl_CreateFileHandler(0, TCL_READABLE,
                TclReadlineDataAvailableHandler, (ClientData) NULL);

        while (LINE_PENDING == tclrl_line_complete && TCL_OK == tclrl_state) {
            Tcl_DoOneEvent(0);
        }

        Tcl_DeleteFileHandler(0);

	if ((LINE_EOF == tclrl_line_complete) && tclrl_eof_string) {
	    Tcl_Eval(interp, tclrl_eof_string);
            return tclrl_state;
	}

        status = history_expand(tclrl_line, &expansion);
        if (status == 1)
            printf("%s\n", expansion);
        else if (status == -1)
            Tcl_AppendResult(interp, "error in history expansion\n",
                    (char*) NULL);
        
        if (expansion && *expansion)
            add_history(expansion);

        Tcl_AppendResult(interp, expansion, (char*) NULL);

        FREE(tclrl_line);
        FREE(expansion);
        return tclrl_state;
    } else if (c == 'i'  && strncmp(argv[1], "initialize", length) == 0) {
        if (3 != argc)
            goto BAD_COMMAND;
        else
            Tcl_AppendResult(interp, TclReadlineInitialize(argv[2]),
                    (char*) NULL);
    } else if (c == 'w'  && strncmp(argv[1], "write", length) == 0) {
        if (3 != argc)
            goto BAD_COMMAND;
        else if (write_history(argv[2]))
            Tcl_AppendResult(interp, "unable to write history to \"",
                    argv[2], "\"\n", (char*) NULL);
    } else if (c == 'a'  && strncmp(argv[1], "add", length) == 0) {
        if (3 != argc)
            goto BAD_COMMAND;
        else if (TclReadlineKnownCommands(argv[2], (int) 0, _CMD_SET))
            Tcl_AppendResult(interp, "unable to add command \"",
                    argv[2], "\"\n", (char*) NULL);
    } else if (c == 'c'  && strncmp(argv[1], "complete", length) == 0) {
        if (3 != argc)
            goto BAD_COMMAND;
        else if (Tcl_CommandComplete(argv[2]))
            Tcl_AppendResult(interp, "1", (char*) NULL);
        else
            Tcl_AppendResult(interp, "0", (char*) NULL);
    } else if (c == 'c'  && strncmp(argv[1], "customcompleter", length) == 0) {
        if (3 != argc && 2 != argc)
            goto BAD_COMMAND;
        if (3 == argc) {
            if (tclrl_custom_completer)
                FREE(tclrl_custom_completer);
            if (!blank_line(argv[2]))
                tclrl_custom_completer = stripwhite(strdup(argv[2]));
        }
        Tcl_AppendResult (interp, tclrl_custom_completer, (char*) NULL);
    } else if (c == 'b'  && strncmp(argv[1], "builtincompleter", length) == 0) {
        int bool = tclrl_use_builtin_completer;
        if (3 != argc && 2 != argc)
            goto BAD_COMMAND;
        if (3 == argc) {
            if (TCL_OK != Tcl_GetBoolean(interp, argv[2], &bool)) {
                Tcl_AppendResult(interp,
                    "wrong # args: should be a boolean value.", (char*) NULL);
                return TCL_ERROR;
            } else {
                tclrl_use_builtin_completer = bool;
            }
        }
        Tcl_AppendResult(interp, tclrl_use_builtin_completer ? "1" : "0",
            (char*) NULL);
    } else if (c == 'e'  && strncmp(argv[1], "eofchar", length) == 0) {
        if (3 != argc && 2 != argc)
            goto BAD_COMMAND;
        if (3 == argc) {
            if (tclrl_eof_string)
                FREE(tclrl_eof_string);
            if (!blank_line(argv[2]))
                tclrl_eof_string = stripwhite(strdup(argv[2]));
        }
        Tcl_AppendResult(interp, tclrl_eof_string, (char*) NULL);
    } else {
        goto BAD_COMMAND;
    }

    return TCL_OK;

BAD_COMMAND:
    Tcl_AppendResult(interp,
        "wrong # args: should be \"readline option ?arg ...?\"",
        (char*) NULL);
    return TCL_ERROR;

}

void
TclReadlineDataAvailableHandler(ClientData clientData, int mask)
{
#if 0
    fprintf(stderr, "(TclReadlineDataAvailableHandler) mask = %d\n",  mask);
#endif
    if (mask & TCL_READABLE) {
        while (0 == rl_done)
            rl_callback_read_char();
    }
}

void
TclReadlineLineCompleteHandler(char* ptr)
{
#if 1
    if (!ptr) { /* <c-d> */
        tclrl_line_complete = LINE_EOF;
        rl_callback_handler_remove();
    } else if (*ptr) {
        tclrl_line_complete = LINE_COMPLETE;
        rl_callback_handler_remove();
        tclrl_line = ptr;
    }
#else
    if (ptr && *ptr) {
        tclrl_line_complete = 1;
        rl_callback_handler_remove ();
        tclrl_line = ptr;
    }
#endif
}

int
Tclreadline_SafeInit(Tcl_Interp *interp)
{
    return Tclreadline_Init(interp);
}

int
Tclreadline_Init(Tcl_Interp *interp)
{
    Tcl_CreateCommand(interp, "::tclreadline::readline", TclReadlineCmd,
	    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    tclrl_interp = interp;
    return Tcl_PkgProvide(interp, "tclreadline", TCLREADLINE_VERSION);
}

char*
TclReadlineInitialize(char* historyfile)
{
    rl_readline_name = "tclreadline";
    /* rl_special_prefixes = "${\""; */
    rl_special_prefixes = "${";
    /**
     * default is " \t\n\"\\'`@$><=;|&{("
     * removed "{("
     * added "[]}"
     */
    rl_basic_word_break_characters = " \t\n\"\\'`@$><=;|&[]}";
    /* rl_completer_quote_characters = "\""; */

    using_history();
    if (!tclrl_eof_string)
        tclrl_eof_string = strdup("puts {}; exit");

    /*
     * try to read historyfile in home
     * directory. If this failes, this
     * is *not* an error.
     */
    rl_attempted_completion_function = (CPPFunction *) TclReadlineCompletion;
    if (read_history(historyfile))
        return "unable to read history file";
    
    else
        return "";
}

int
blank_line(char* str)
{
    char* ptr;
    for (ptr = str; ptr && *ptr; ptr++) {
        if (!ISWHITE(*ptr))
            return 0;
    }
    return 1;
}

char**
TclReadlineCompletion(char* text, int start, int end)
{
    char** matches = (char**) NULL;

#if 0
    fprintf(stderr, "DEBUG> TclReadlineCompletion: text=|%s|\n", text);
    fprintf(stderr, "DEBUG> TclReadlineCompletion: start=|%d|\n", start);
    fprintf(stderr, "DEBUG> TclReadlineCompletion: end=|%d|\n", end);
#endif

    if (tclrl_custom_completer) {
        char start_s[BUFSIZ], end_s[BUFSIZ];
        Tcl_Obj* obj;
        Tcl_Obj** objv;
        int objc;
        char* quoted_text = TclReadlineQuote(text, "$[]{}\"");
        char* quoted_rl_line_buffer
            = TclReadlineQuote(rl_line_buffer, "$[]{}\"");
        /*
        fprintf (stderr, "(TclReadlineCompletion) rl_line_buffer = |%s|\n",
            rl_line_buffer);
        fprintf (stderr, "(TclReadlineCompletion) quoted_rl_line_buffer = |%s|\n",
            quoted_rl_line_buffer);
        fprintf (stderr, "(TclReadlineCompletion) text = |%s|\n", text);
        fprintf (stderr, "(TclReadlineCompletion) quoted_text = |%s|\n",
            quoted_text);
        */
        sprintf(start_s, "%d", start);
        sprintf(end_s, "%d", end);
        Tcl_ResetResult(tclrl_interp); /* clear result space */
        tclrl_state = Tcl_VarEval(tclrl_interp, tclrl_custom_completer,
            " \"", quoted_text, "\" ", start_s, " ", end_s,
            " \"", quoted_rl_line_buffer, "\"", (char*) NULL);
        FREE(quoted_text);
        FREE(quoted_rl_line_buffer);
        if (TCL_OK != tclrl_state) {
            fprintf (stderr, "%s\n", Tcl_GetStringResult(tclrl_interp));
            /*
            Tcl_AppendResult (tclrl_interp, "`", tclrl_custom_completer,
                " {", text, "} ", start_s, " ", end_s,
                " {", rl_line_buffer, "}' failed.", (char*) NULL);
            */
            return matches;
        }
        obj = Tcl_GetObjResult(tclrl_interp);
        Tcl_ListObjGetElements(tclrl_interp, obj, &objc, &objv);
        /* fprintf (stderr, "(TclReadlineCompletion) objc = %d\n", objc); */
        if (objc) {
            int i, length /* not used */;
            matches = (char**) MALLOC(sizeof(char*) * (objc + 1));
            for (i = 0; i < objc; i++) {
                matches[i] = strdup(Tcl_GetStringFromObj(objv[i], &length));
                /*
                fprintf (stderr, "(TclReadlineCompletion) matches[%d] = |%s|\n",
                    i, matches[i]);
                */
            }
            matches[i] = (char*) NULL; /* terminate */
        }
        Tcl_ResetResult(tclrl_interp); /* clear result space */
    }

    if (!matches && tclrl_use_builtin_completer) {
        matches = completion_matches(text, TclReadline0generator);
    }
#if 0
    {
        char **ptr;
        for (ptr = matches; ptr && *ptr; ptr++)
            fprintf (stderr, "(TclReadlineCompletion) |%s|\n", *ptr);
    }
#endif
    
    return matches;
}

char*
TclReadline0generator(char* text, int state)
{
    return TclReadlineKnownCommands(text, state, _CMD_GET);
}

char*
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
            /*
             * fprintf (stderr, "(TclReadlineKnownCommands) state=%d\n", state);
             */

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

#if 0
                    fprintf (stderr,
                        "(TclReadlineKnownCommands) _CMD_SUB_GET = |%s| %d %d\n",
                        text, state, mode);
#endif

                    /*
                    len = strlen(local_line);
                    stripright(local_line);
                    */

#if 0
                    if (len != strlen(local_line)) {
#endif
#if 0
                    } else {
                        sub = TclReadlineParse(args, sizeof(args), local_line) - 1;
                    }
#endif
#if 0
                    {
                        int i;
                        fprintf (stderr, "\n");
                        for (i = 0; i < sub; i++)
                            fprintf (stderr, "|%s|\n", args[i]);
                    }

                    fprintf (stderr,
                        "(TclReadlineKnownCommands) args[sub - 1] = |%s|\n",
                        args[sub - 1]);
#endif
#if 0
                    if (sub > 0 && args[sub - 1] && *(args[sub - 1]) == '$') {
                        char* var = strdup(args[sub - 1] + 1);
                        char* ptr = var;
                        int varlen;
                        if (var)
                            varlen = strlen(var);
                        else
                            return (char*) NULL;
                        if (!varlen) {
                            FREE(var);
                            return (char* ) NULL;
                        } else if ('{' == var[0]) {
                            ptr++;
                            varlen--;
                        }
                        fprintf (stderr, "(TclReadlineKnownCommands) $\n");
                        FREE(var);
                        return (char* ) NULL;
                    }
#endif

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

int
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
