
 /* ==================================================================

    FILE: "/home/joze/src/tclreadline/tclreadline.c"
    LAST MODIFICATION: "Mon Sep 13 02:21:35 1999 (joze)"
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

/**
 * this prototype is missing
 * in readline.h
 */
void rl_extend_line_buffer(int len);

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

/*
 * forward declarations.
 */
char* stripleft(char* in);
char* stripright(char* in);
char* stripwhite(char* in);
char* TclReadlineQuote(char* text, char* quotechars);
int TclReadlineCmd(ClientData clientData, Tcl_Interp* interp,
    int argc, char** argv);
int TclReadlineEventHook(void);
void TclReadlineReadHandler(ClientData clientData, int mask);
void TclReadlineWriteHandler(ClientData clientData, int mask);
void TclReadlineLineCompleteHandler(char* ptr);
int Tclreadline_SafeInit(Tcl_Interp* interp);
int Tclreadline_Init(Tcl_Interp* interp);
char *TclReadlineFilenameQuotingFunction(
    char *text, int match_type, char* quote_ptr);
int TclReadlineInitialize(Tcl_Interp* interp, char* historyfile);
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
static int tclrl_history_length = -1;
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
        
#if 1
        tclrl_line_complete = LINE_PENDING;
        tclrl_state = TCL_OK;
        rl_callback_handler_install(argc == 3 ? argv[2] : "%",
                TclReadlineLineCompleteHandler);

        Tcl_CreateFileHandler(0, TCL_READABLE,
                TclReadlineReadHandler, (ClientData) NULL);
        /*
        Tcl_CreateFileHandler(1, TCL_WRITABLE,
                TclReadlineWriteHandler, (ClientData) NULL);
        */

        /**
         * Main Loop.
         * XXX each modification of the global variables
         *     which terminates the main loop must call
         *     rl_callback_handler_remove() to leave
         *     readline in a defined state.          XXX
         */
        while (LINE_PENDING == tclrl_line_complete
            && TCL_OK == tclrl_state && !rl_done) {
            Tcl_DoOneEvent(TCL_ALL_EVENTS);
            /*
            Tcl_DoOneEvent(0);
            fprintf (stderr, "(TclReadlineCmd) \n");
            rl_inhibit_completion = 0;
            */
        }
        Tcl_DeleteFileHandler(0);

        if (TCL_OK != tclrl_state)
            return tclrl_state; /* !! */

	if ((LINE_EOF == tclrl_line_complete) && tclrl_eof_string) {
	    Tcl_Eval(interp, tclrl_eof_string);
            return tclrl_state;
	}
#else
        rl_event_hook = TclReadlineEventHook;
        tclrl_line = readline(argc == 3 ? argv[2] : "%");
#endif

        status = history_expand(tclrl_line, &expansion);
        if (status >= 1) {
#if 0
            Tcl_Channel channel = Tcl_MakeFileChannel(stdout, TCL_WRITABLE);
            /* Tcl_RegisterChannel(interp, channel); */
            (void) Tcl_WriteChars(channel, expansion, -1);
            Tcl_Flush(channel);
            Tcl_Close(interp, channel);
#else
            printf("%s\n", expansion);
#endif
        }
        else if (status == -1) {
            Tcl_AppendResult
                (interp, "error in history expansion\n", (char*) NULL);
            return tclrl_state;
        }
        /**
         * TODO: status == 2 ...
         */
        
        if (expansion && *expansion)
            add_history(expansion);

        Tcl_SetResult(interp, expansion, TCL_VOLATILE);

        FREE(tclrl_line);
        FREE(expansion);
        return tclrl_state;
    } else if (c == 'i'  && strncmp(argv[1], "initialize", length) == 0) {
        if (3 != argc)
            goto BAD_COMMAND;
        else
            return TclReadlineInitialize(interp, argv[2]);
    } else if (c == 'w'  && strncmp(argv[1], "write", length) == 0) {
        if (3 != argc) {
            goto BAD_COMMAND;
        }  else if (write_history(argv[2])) {
            Tcl_AppendResult(interp, "unable to write history to `",
                    argv[2], "'\n", (char*) NULL);
            return TCL_ERROR;
        }
        if (tclrl_history_length >= 0) {
            history_truncate_file(argv[2], tclrl_history_length);
        }
        return TCL_OK;
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
        Tcl_AppendResult(interp, tclrl_custom_completer, (char*) NULL);
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

int TclReadlineEventHook(void)
{
    Tcl_DoOneEvent(TCL_ALL_EVENTS | TCL_DONT_WAIT);
    /*
        TCL_DONT_WAIT
        TCL_WINDOW_EVENTS
        TCL_FILE_EVENTS
        TCL_TIMER_EVENTS
        TCL_IDLE_EVENTS
        TCL_ALL_EVENTS
    */
    return TCL_OK;
}
void
TclReadlineReadHandler(ClientData clientData, int mask)
{
#if 0
    fprintf(stderr, "(TclReadlineReadHandler) mask = %d\n",  mask);
#endif
    if (mask & TCL_READABLE) {
        /*
        fprintf(stderr, "(TclReadlineReadHandler) mask = readable\n");
        rl_event_hook = TclReadlineEventHook;
        while (!rl_done) {
        */
            rl_callback_read_char();
        /*
        }
        fflush(stdin);
        */
    }
}

void
TclReadlineWriteHandler(ClientData clientData, int mask)
{
    if (mask & TCL_WRITABLE) {
        /*
        fprintf(stderr, "(TclReadlineReadHandler) mask = writable\n");
        */
        fflush(stdout);
        rl_redisplay();
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
        rl_callback_handler_remove();
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
    int status;
    Tcl_CreateCommand(interp, "::tclreadline::readline", TclReadlineCmd,
	    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    tclrl_interp = interp;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::historyLength",
                (char*) &tclrl_history_length, TCL_LINK_INT)))
        return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::library",
         (char*) &TCLRL_LIBRARY, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
        return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::version",
         (char*) &TCLRL_VERSION, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
        return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "::tclreadline::patchLevel",
         (char*) &TCLRL_PATCHLEVEL, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
        return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "tclreadline_library",
         (char*) &TCLRL_LIBRARY, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
        return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "tclreadline_version",
         (char*) &TCLRL_VERSION, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
        return status;
    if (TCL_OK != (status = Tcl_LinkVar(interp, "tclreadline_patchLevel",
         (char*) &TCLRL_PATCHLEVEL, TCL_LINK_STRING | TCL_LINK_READ_ONLY)))
        return status;
    return Tcl_PkgProvide(interp, "tclreadline", TCLRL_VERSION);
}

#if 0
char *
TclReadlineFilenameQuotingFunction
(char *filename, int match_type, char* quote_ptr)
{
    char *res = (char*) malloc(sizeof(char) * (strlen(filename) + 2));
    int i = 0;
    fprintf (stderr, "(TclReadlineFilenameQuotingFunction) \n");
    if (quote_ptr && *quote_ptr) {
        *res = *quote_ptr;                     /* leading quote */
        i++;
    }
    strcpy (res + i, filename);              /* name          */
#if 0
    fprintf (stderr, "(Tclreadline_Init) filename=|%s|\n", filename);
    fprintf (stderr, "(Tclreadline_Init) *quote_ptr=|%c|\n", *quote_ptr);
#endif
    if (quote_ptr && '{' == *quote_ptr) {
        *quote_ptr = '}';
    }
    return res;

#if 0
    switch (match_type) {
        case SINGLE_MATCH:
            break;
        default:
    }
#endif
}
#endif

int
TclReadlineInitialize(Tcl_Interp* interp, char* historyfile)
{
    rl_readline_name = "tclreadline";
    //    rl_special_prefixes = "${\"[";
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
    // rl_basic_quote_characters = "\"{"; // XXX ??? XXX
    // rl_completer_quote_characters = "\"";
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
    int status;
    // rl_attempted_completion_over = 0;
    rl_completion_append_character = ' '; /* reset, just in case ... */

#if 0
    fprintf(stderr, "DEBUG> TclReadlineCompletion: text=|%s|\n", text);
    fprintf(stderr, "DEBUG> TclReadlineCompletion: start=|%d|\n", start);
    fprintf(stderr, "DEBUG> TclReadlineCompletion: end=|%d|\n", end);
#endif

#if 0
    char* history_event = (char*) NULL;
    if (text) {
        if ('!' == text[0])
            history_event = strdup(text);
        else if (start && rl_line_buffer[start - 1] == '!' /* for '$' */) {
            int len = strlen(text);
            history_event = strncpy((char*) malloc(sizeof(char) * (len + 1)),
                rl_line_buffer[start - 1], len);
            history_event[len] = '\0'; /* terminate */
        }
    }
    if (history_event)
#endif

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
            /* rl_redisplay(); */
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
        char* quoted_text = TclReadlineQuote(text, "$[]{}\"");
        char* quoted_rl_line_buffer
            = TclReadlineQuote(rl_line_buffer, "$[]{}\"");
#if 0
        fprintf (stderr, "(TclReadlineCompletion) rl_line_buffer = |%s|\n",
            rl_line_buffer);
        fprintf (stderr, "(TclReadlineCompletion) quoted_rl_line_buffer = |%s|\n",
            quoted_rl_line_buffer);
        fprintf (stderr, "(TclReadlineCompletion) text = |%s|\n", text);
        fprintf (stderr, "(TclReadlineCompletion) quoted_text = |%s|\n",
            quoted_text);
#endif
        sprintf(start_s, "%d", start);
        sprintf(end_s, "%d", end);
        Tcl_ResetResult(tclrl_interp); /* clear result space */
        tclrl_state = Tcl_VarEval(tclrl_interp, tclrl_custom_completer,
            " \"", quoted_text, "\" ", start_s, " ", end_s,
            " \"", quoted_rl_line_buffer, "\"", (char*) NULL);
        FREE(quoted_text);
        FREE(quoted_rl_line_buffer);
        if (TCL_OK != tclrl_state) {
            rl_callback_handler_remove();
            Tcl_AppendResult (tclrl_interp, " `", tclrl_custom_completer,
                " \"", quoted_text, "\" ", start_s, " ", end_s,
                " \"", quoted_rl_line_buffer, "\"' failed.", (char*) NULL);
            return matches;
        }
#if 0
        fprintf(stderr, "\nscript returned |%s|\n",
            Tcl_GetStringResult(tclrl_interp));
#endif
        obj = Tcl_GetObjResult(tclrl_interp);
        status = Tcl_ListObjGetElements(tclrl_interp, obj, &objc, &objv);
        if (TCL_OK != status)
            return matches;
        /* fprintf (stderr, "(TclReadlineCompletion) objc = %d\n", objc); */
        if (objc) {
            int i, length;
            matches = (char**) MALLOC(sizeof(char*) * (objc + 1));
            for (i = 0; i < objc; i++) {
                matches[i] = strdup(Tcl_GetStringFromObj(objv[i], &length));
                if (1 == objc && !strlen(matches[i])) {
                    // rl_attempted_completion_over = 1;
                    FREE(matches[i]);
                    FREE(matches);
                    return (char**) NULL;
                }
                /*
                fprintf (stderr, "(TclReadlineCompletion) len[%s]=%d\n",
                    matches[i], strlen(matches[i]));
                */
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
             * fprintf (stderr, "(TclReadlineKnownCommands) text = |%s|\n", text);
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
