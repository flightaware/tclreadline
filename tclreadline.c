
 /* ==================================================================

    FILE: "/home/joze/src/tclreadline/tclreadline.c"
    LAST MODIFICATION: "Sat Aug 21 17:44:13 1999 (joze)"
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
#define FREE(ptr) if (ptr) Tcl_Free((char *) ptr)

enum {
    _CMD_SET     = (1 << 0),
    _CMD_GET     = (1 << 1),
    _CMD_SUB_GET = (1 << 2)
};


typedef struct cmds_t {
    struct cmds_t  *prev;
    char          **cmd;
    struct cmds_t  *next;
} cmds_t;


#define STRIPLEFT(ptr)          \
do {                            \
    char *tmp = ptr;            \
    while (*tmp && *tmp <= ' ') \
        tmp ++;                 \
    strcpy(ptr, tmp);           \
} while (0)

#define STRIPRIGHT(ptr)                                \
do {                                                   \
    char *__tmp__;                                     \
    for (__tmp__ = strchr(ptr, '\0') - 1;              \
         __tmp__ >= ptr && *__tmp__ <= ' '; __tmp__--) \
        *__tmp__ = '\0';                               \
} while (0)

#define STRIPWHITE(ptr) \
do {                    \
    STRIPLEFT(ptr);     \
    STRIPRIGHT(ptr);    \
} while (0)


/*
 * forward declarations.
 */
int TclReadlineCmd(ClientData clientData, Tcl_Interp *interp,
    int argc, char **argv);
void TclReadlineDataAvailableHandler(ClientData clientData, int mask);
void TclReadlineLineCompleteHandler(char *ptr);
int Tclreadline_SafeInit(Tcl_Interp *interp);
int Tclreadline_Init(Tcl_Interp *interp);
char* TclReadlineInitialize(char *historyfile);
char** TclReadlineCompletion(char *text, int start, int end);
char* TclReadline0generator(char *text, int state);
char* TclReadline1generator(char *text, int state);
char* TclReadlineKnownCommands(char *text, int state, int mode);
int TclReadlineParse(char **args, int maxargs, char *buf);

enum { 
    LINE_PENDING,
    LINE_CONTROL_D,
    LINE_COMPLETE
};

static int line_complete = LINE_PENDING;
static char *line = (char *) NULL;


int
TclReadlineCmd(
    ClientData  clientData, /* Main window associated with interpreter  */
    Tcl_Interp* interp,     /* Current interpreter                      */
    int         argc,       /* Number of arguments                      */
    char**      argv        /* Argument strings                         */
)
{
    int		c, length;
    
    if (argc < 2) {
	interp->result = "wrong # args";
	for (c = 0; c < argc; c++)
	    fprintf(stderr, "argv[%d] = %s\n", c, argv[c]);
	return TCL_ERROR;
    }

    c = argv[1][0];
    length = strlen(argv[1]);

    if (c == 'r'  && strncmp(argv[1], "read", length) == 0) {
        
        char *expansion = (char *) NULL;
        int status;
        
        line_complete = LINE_PENDING;
        rl_callback_handler_install(argc == 3 ? argv[2] : "%",
                TclReadlineLineCompleteHandler);

        Tcl_CreateFileHandler(0, TCL_READABLE,
                TclReadlineDataAvailableHandler, (ClientData) NULL);

        while (LINE_PENDING == line_complete) {
            Tcl_DoOneEvent(0);
        }

        Tcl_DeleteFileHandler(0);

        /*
	if (LINE_CONTROL_D == line_complete) {
	    Tcl_Eval(interp, "puts {}; exit");
	}
        */

        status = history_expand(line, &expansion);
        if (status == 1)
            printf("%s\n", expansion);
        else if (status == -1)
            Tcl_AppendResult(interp, "error in history expansion\n",
                    (char *) NULL);
        
        if (expansion && *expansion)
            add_history(expansion);

        Tcl_AppendResult(interp, expansion, (char *) NULL);

        FREE(line);
        FREE(expansion);
        return TCL_OK;
    }
    else if (c == 'i'  && strncmp(argv[1], "initialize", length) == 0) {
        if (argc != 3)
            goto BAD_COMMAND;
        else
            Tcl_AppendResult(interp, TclReadlineInitialize(argv[2]),
                    (char *) NULL);
    }
    else if (c == 'w'  && strncmp(argv[1], "write", length) == 0) {
        if (argc != 3)
            goto BAD_COMMAND;
        else if (write_history(argv[2]))
            Tcl_AppendResult(interp, "unable to write history to \"",
                    argv[2], "\"\n", (char *) NULL);
    }
    else if (c == 'a'  && strncmp(argv[1], "add", length) == 0) {
        if (argc != 3)
            goto BAD_COMMAND;
        else if (TclReadlineKnownCommands(argv[2], (int) 0, _CMD_SET))
            Tcl_AppendResult(interp, "unable to add command \"",
                    argv[2], "\"\n", (char *) NULL);
    }
    else if (c == 'c'  && strncmp(argv[1], "complete", length) == 0) {
        if (argc != 3)
            goto BAD_COMMAND;
        else if (Tcl_CommandComplete(argv[2]))
            Tcl_AppendResult(interp, "1", (char *) NULL);
        else
            Tcl_AppendResult(interp, "0", (char *) NULL);
    }
    else
        goto BAD_COMMAND;


    return TCL_OK;

BAD_COMMAND:
    Tcl_AppendResult (interp,
            "wrong # args: should be \"readline option ?arg ...?\"",
	    (char *) NULL);
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
TclReadlineLineCompleteHandler(char *ptr)
{
#if 0
    fprintf(stderr, "(TclReadlineLineCompleteHandler)  ptr = %p\n",  ptr);
    if (ptr)
    fprintf(stderr, "(TclReadlineLineCompleteHandler) *ptr = %p\n", *ptr);
    if (!ptr) { /* <c-d> */
        line_complete = LINE_CONTROL_D;
        rl_callback_handler_remove();
    } else if (*ptr) {
        line_complete = LINE_COMPLETE;
        rl_callback_handler_remove();
        line = ptr;
    }
#endif
    if (ptr && *ptr) {
        line_complete = 1;
        rl_callback_handler_remove ();
        line = ptr;
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
    Tcl_CreateCommand(interp, "::tclreadline::readline", TclReadlineCmd,
	    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    return Tcl_PkgProvide(interp, "tclreadline", TCLREADLINE_VERSION);
}

char*
TclReadlineInitialize(char* historyfile)
{

    rl_readline_name = "tclreadline";
    using_history();

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

char**
TclReadlineCompletion(char* text, int start, int end)
{
    char** matches = (char **) NULL;
    static char local_line[BUFSIZ];
       
    strcpy(local_line, rl_line_buffer);
    
    STRIPWHITE(local_line);
    
    /*
    fprintf(stderr, "DEBUG> TclReadlineCompletion: text=|%s|\n", text);
    fprintf(stderr, "DEBUG> TclReadlineCompletion: start=|%d|\n", start);
    fprintf(stderr, "DEBUG> TclReadlineCompletion: end=|%d|\n", end);
    */

    if (start == 0 || !strlen(local_line))
        matches = completion_matches(text, TclReadline0generator);
    else
        matches = completion_matches(text, TclReadline1generator);
    
    return matches;
}

char*
TclReadline0generator(char* text, int state)
{
    return TclReadlineKnownCommands(text, state, _CMD_GET);
}

char*
TclReadline1generator(char* text, int state)
{
    return TclReadlineKnownCommands(text, state, _CMD_SUB_GET);
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

            new->cmd = (char **) MALLOC(sizeof(char *) * (argc + 1));

            for (i = 0; i < argc; i++)
                new->cmd[i] = args[i];

            new->cmd[argc] = (char *) NULL;

            return (char *) NULL;
            break;


        case _CMD_GET:


            if (!state) {
                new = cmds;
                len = strlen(text);
            }

            while (new && (name = new->cmd)) {
                new = new->next;
                if (!strncmp(name[0], text, len))
                    return strdup(name[0]);
            }

            return (char *) NULL;
            break;

        case _CMD_SUB_GET:
            

            if (!state) {

                int sub;
                char *local_line = strdup(rl_line_buffer);
                
                len = strlen(local_line);
                STRIPRIGHT(local_line);

                if (len != strlen(local_line))
                    sub = TclReadlineParse(args, sizeof(args), local_line);
                else
                    sub = TclReadlineParse(args, sizeof(args), local_line) - 1;
                
                new = cmds;
                len = strlen(text);
                
                while (new && (name = new->cmd)) {
                    if (!strcmp(name[0], args[0]))
                        break;
                    new = new->next;
                }
                
                if (!new)
                    return (char *) NULL;

                for (i = 0; new->cmd[i]; i++) /* EMPTY */;

                if (sub < i && !strncmp(new->cmd[sub], text, len))
                    return strdup(new->cmd[sub]);
                else
                   return (char *) NULL;

            }
            else
                return (char *) NULL;

            /* NOTREACHED */
            break;


        default:
            return (char *) NULL;
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
        while ((*buf == ' ') || (*buf == '\t') || (*buf == '\n'))
            *buf++ = '\0';

        if (!(*buf)) /* don't count the terminating NULL */
            break;

        *args++ = buf;
        nr++;

        while ((*buf!='\0') && (*buf!=' ') && (*buf!='\t') && (*buf!='\n'))
            buf++;
    }

    *args = '\0';
    return nr;
}
