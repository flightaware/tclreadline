#!/usr/local/bin/tclsh
# ==================================================================
# FILE: "/home/joze/tmp/tclreadline/tclreadlineSetup.tcl"
# LAST MODIFIED: "Mon Oct 05 15:15:42 1998 (joze)"
# (c) 1998 by Johannes Zellner
# Johannes.Zellner@physik.uni-karlsruhe.de
# $Id$
# ================================================================== 

package provide tclreadline 1.0

proc unknown args {

    global auto_noexec auto_noload env unknown_pending tcl_interactive
    global errorCode errorInfo

    # Save the values of errorCode and errorInfo variables, since they
    # may get modified if caught errors occur below.  The variables will
    # be restored just before re-executing the missing command.

    set savedErrorCode $errorCode
    set savedErrorInfo $errorInfo
    set name [lindex $args 0]
    if ![info exists auto_noload] {
        #
        # Make sure we're not trying to load the same proc twice.
        #
        if [info exists unknown_pending($name)] {
            return -code error "self-referential recursion in \"unknown\" for command \"$name\"";
        }
        set unknown_pending($name) pending;
        set ret [catch {auto_load $name [uplevel 1 {namespace current}]} msg]
        unset unknown_pending($name);
        if {$ret != 0} {
            return -code $ret -errorcode $errorCode \
                "error while autoloading \"$name\": $msg"
        }
        if ![array size unknown_pending] {
            unset unknown_pending
        }
        if $msg {
            set errorCode $savedErrorCode
            set errorInfo $savedErrorInfo
            set code [catch {uplevel 1 $args} msg]
            if {$code ==  1} {
                #
                # Strip the last five lines off the error stack (they're
                # from the "uplevel" command).
                #

                set new [split $errorInfo \n]
                set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
                return -code error -errorcode $errorCode \
                        -errorinfo $new $msg
            } else {
                return -code $code $msg
            }
        }
    }

    # REMOVED THE [info script] TEST (joze, SEP 98)
    if {([info level] == 1) \
            && [info exists tcl_interactive] && $tcl_interactive} {
        if ![info exists auto_noexec] {
            set new [auto_execok $name]
            if {$new != ""} {
                set errorCode $savedErrorCode
                set errorInfo $savedErrorInfo
                set redir ""
                if {[info commands console] == ""} {
                    set redir ">&@stdout <@stdin"
                }
                # LOOK FOR GLOB STUFF IN $ARGS (joze, SEP 98)
                return [uplevel eval exec $redir $new \
                    [::tclreadline::Glob [lrange $args 1 end]]]
            }
        }
        set errorCode $savedErrorCode
        set errorInfo $savedErrorInfo
        if {$name == "!!"} {
            set newcmd [history event]
        } elseif {[regexp {^!(.+)$} $name dummy event]} {
            set newcmd [history event $event]
        } elseif {[regexp {^\^([^^]*)\^([^^]*)\^?$} $name dummy old new]} {
            set newcmd [history event -1]
            catch {regsub -all -- $old $newcmd $new newcmd}
        }
        if [info exists newcmd] {
            tclLog $newcmd
            history change $newcmd 0
            return [uplevel $newcmd]
        }

        set ret [catch {set cmds [info commands $name*]} msg]
        if {[string compare $name "::"] == 0} {
            set name ""
        }
        if {$ret != 0} {
            return -code $ret -errorcode $errorCode \
                "error in unknown while checking if \"$name\" is a unique command abbreviation: $msg"
        }
        if {[llength $cmds] == 1} {
            return [uplevel [lreplace $args 0 0 $cmds]]
        }
        if {[llength $cmds] != 0} {
            if {$name == ""} {
                return -code error "empty command name \"\""
            } else {
                return -code error \
                        "ambiguous command name \"$name\": [lsort $cmds]"
            }
        }
    }
    return -code error "invalid command name \"$name\""
}

namespace eval tclreadline:: {
    namespace export Setup Glob Loop InitCmds InitTclCmds InitTkCmds
}

proc ::tclreadline::Setup {} {

    uplevel #0 {


        if {[info exists tk_version]} {
            set readlinePrompt "\[0;94mwish$tk_version\[0m"
        } else {
            set readlinePrompt "\[0;91mtclsh$tcl_version\[0m"
        }

        proc ls {args} {
            if {[exec uname -s] == "Linux"} {
                eval exec ls --color -FC [::tclreadline::Glob $args]
            } else {
                eval exec ls -FC [::tclreadline::Glob $args]
            }
        }

        if {[info procs cd] == ""} {
            catch {rename ::tclreadline::Cd ""}
            rename cd ::tclreadline::Cd
            proc cd {args} {
                if {[catch {eval ::tclreadline::Cd $args} message]} {
                    puts stderr "$message"
                }
                ls
            }
        }

        if {[info procs exit] == ""} {

            catch {rename ::tclreadline::Exit ""}
            rename exit ::tclreadline::Exit

            proc exit {args} {

                catch {::tclreadline::readline write 
                    [::tclreadline::HistoryFileGet]}

                if [catch "eval ::tclreadline::Exit $args" message] {
                    puts stderr "error:"
                    puts stderr "$message"
                }
                # NOTREACHED
            }
        }



        global pi
        set pi 3.1415926535897931
        set tcl_precision 17


        global env
        variable historyfile

        if {[info exists env(HOME)]} {
            set historyfile  $env(HOME)/.tclsh-history
            set env(INPUTRC) $env(HOME)/.tclsh-inputrc
        } else {
            set historyfile  .tclsh-history
        }
        set msg [::tclreadline::readline initialize $historyfile]
        if {$msg != ""} {
            puts stderr "$msg"
        }

        ::tclreadline::InitCmds
    }

    rename ::tclreadline::Setup ""
}

proc ::tclreadline::HistoryFileGet {} {
    variable historyfile
    puts stderr "::tclreadline::HistoryFileGet: entered"
    return $historyfile
}

proc ::tclreadline::Glob {string} {

    set commandstring ""
    foreach name $string {
        set replace [glob -nocomplain -- $name]
        if {$replace == ""} {
            lappend commandstring $name
        } else {
            lappend commandstring $replace
        }
    }
    return $commandstring
}



proc ::tclreadline::Loop {} {

    ::tclreadline::Setup

    uplevel #0 {
        while {1} {
            if {[catch {
                if {[info exists tk_version]} {
                    update
                }
                regsub $env(HOME) [pwd] "" prompt2
                set line [::tclreadline::readline read "$readlinePrompt \[$prompt2\]"]
                set result [eval $line]
                if {$result != ""} {
                    puts $result
                }
                set result ""
            } msg]} {
#                puts stderr $msg
            }
        }
    }
}


proc ::tclreadline::InitCmds {} {
    global tcl_version tk_version
    if {[info exists tcl_version]} {
        ::tclreadline::InitTclCmds
    }
    if {[info exists tk_version]} {
        ::tclreadline::InitTkCmds
    }
    rename tclreadline::InitCmds ""
}

proc ::tclreadline::InitTclCmds {} {
::tclreadline::readline add "after option ?arg arg ...?"
::tclreadline::readline add "append varName ?value value ...?"
::tclreadline::readline add "array option arrayName ?arg ...?"
::tclreadline::readline add "binary option ?arg arg ...?"
::tclreadline::readline add "catch command ?varName?"
::tclreadline::readline add "clock option ?arg ...?"
::tclreadline::readline add "close channelId"
::tclreadline::readline add "eof channelId"
::tclreadline::readline add "error message ?errorInfo? ?errorCode?"
::tclreadline::readline add "eval arg ?arg ...?"
::tclreadline::readline add "exec ?switches? arg ?arg ...?"
::tclreadline::readline add "expr arg ?arg ...?"
::tclreadline::readline add "fblocked channelId"
::tclreadline::readline add "fconfigure channelId ?optionName? ?value? ?optionName value?..."
::tclreadline::readline add "fcopy input output ?-size size? ?-command callback?"
::tclreadline::readline add "file option ?arg ...?"
::tclreadline::readline add "fileevent channelId event ?script?"
::tclreadline::readline add "flush channelId"
::tclreadline::readline add "for start test next command"
::tclreadline::readline add "foreach varList list ?varList list ...? command"
::tclreadline::readline add "format formatString ?arg arg ...?"
::tclreadline::readline add "gets channelId ?varName?"
::tclreadline::readline add "glob ?switches? name ?name ...?"
::tclreadline::readline add "global varName ?varName ...?"
::tclreadline::readline add "incr varName ?increment?"
::tclreadline::readline add "info option ?arg arg ...?"
::tclreadline::readline add "interp cmd ?arg ...?"
::tclreadline::readline add "join list ?joinString?"
::tclreadline::readline add "lappend varName ?value value ...?"
::tclreadline::readline add "lindex list index"
::tclreadline::readline add "linsert list index element ?element ...?"
::tclreadline::readline add "llength list"
::tclreadline::readline add "load fileName ?packageName? ?interp?"
::tclreadline::readline add "lrange list first last"
::tclreadline::readline add "lreplace list first last ?element element ...?"
::tclreadline::readline add "lsearch ?mode? list pattern"
::tclreadline::readline add "lsort ?options? list"
::tclreadline::readline add "namespace subcommand ?arg ...?"
::tclreadline::readline add "open fileName ?access? ?permissions?"
::tclreadline::readline add "package option ?arg arg ...?"
::tclreadline::readline add "proc name args body"
::tclreadline::readline add "puts ?-nonewline? ?channelId? string"
::tclreadline::readline add "read ?-nonewline? channelId"
::tclreadline::readline add "regexp ?switches? exp string ?matchVar? ?subMatchVar subMatchVar ...?"
::tclreadline::readline add "regsub ?switches? exp string subSpec varName"
::tclreadline::readline add "rename oldName newName"
::tclreadline::readline add "scan string format ?varName varName ...?"
::tclreadline::readline add "seek channelId offset ?origin?"
::tclreadline::readline add "set varName ?newValue?"
::tclreadline::readline add "socket ?-myaddr addr? ?-myport myport? ?-async? host port"
::tclreadline::readline add "socket -server command ?-myaddr addr? port"
::tclreadline::readline add "source fileName"
::tclreadline::readline add "split string ?splitChars?"
::tclreadline::readline add "string option arg ?arg ...?"
::tclreadline::readline add "subst ?-nobackslashes? ?-nocommands? ?-novariables? string"
::tclreadline::readline add "switch ?switches? string pattern body ... ?default body?"
::tclreadline::readline add "tell channelId"
::tclreadline::readline add "time command ?count?"
::tclreadline::readline add "trace option \[arg arg ...\]"
::tclreadline::readline add "unset varName ?varName ...?"
::tclreadline::readline add "uplevel ?level? command ?arg ...?"
::tclreadline::readline add "upvar ?level? otherVar localVar ?otherVar localVar ...?"
::tclreadline::readline add "vwait name"
::tclreadline::readline add "while test command"
rename tclreadline::InitTclCmds ""

}

proc ::tclreadline::InitTkCmds {} {
::tclreadline::readline add "bind window ?pattern? ?command?"
::tclreadline::readline add "bindtags window ?tags?"
::tclreadline::readline add "button pathName ?options?"
::tclreadline::readline add "canvas pathName ?options?"
::tclreadline::readline add "checkbutton pathName ?options?"
::tclreadline::readline add "clipboard option ?arg arg ...?"
::tclreadline::readline add "entry pathName ?options?"
::tclreadline::readline add "event option ?arg1?"
::tclreadline::readline add "font option ?arg?"
::tclreadline::readline add "frame pathName ?options?"
::tclreadline::readline add "grab option ?arg arg ...?"
::tclreadline::readline add "grid option arg ?arg ...?"
::tclreadline::readline add "image option ?args?"
::tclreadline::readline add "label pathName ?options?"
::tclreadline::readline add "listbox pathName ?options?"
::tclreadline::readline add "lower window ?belowThis?"
::tclreadline::readline add "menu pathName ?options?"
::tclreadline::readline add "menubutton pathName ?options?"
::tclreadline::readline add "message pathName ?options?"
::tclreadline::readline add "option cmd arg ?arg ...?"
::tclreadline::readline add "pack option arg ?arg ...?"
::tclreadline::readline add "radiobutton pathName ?options?"
::tclreadline::readline add "raise window ?aboveThis?"
::tclreadline::readline add "scale pathName ?options?"
::tclreadline::readline add "scrollbar pathName ?options?"
::tclreadline::readline add "selection option ?arg arg ...?"
::tclreadline::readline add "send ?options? interpName arg ?arg ...?"
::tclreadline::readline add "text pathName ?options?"
::tclreadline::readline add "tk option ?arg?"
::tclreadline::readline add "tkwait variable|visibility|window name"
::tclreadline::readline add "toplevel pathName ?options?"
::tclreadline::readline add "winfo option ?arg?"
::tclreadline::readline add "wm option window ?arg ...?"
rename tclreadline::InitTkCmds ""
}




# ***** invalid command name "Tcl"
# ***** invalid command name "bgerror"
# ***** invalid command name "contents"
# ***** invalid command name "filename"
# ***** invalid command name "http"
# ***** wrong # args: no expression after "if" argument
# ***** invalid command name "library"
# ***** 4940
# ***** invalid command name "pkgMkIndex"
# ***** /home/joze
# ***** invalid command name "registry"
# ***** invalid command name "resource"
# ***** invalid command name "safe"
# ***** wrong # args: should be either:
# ***** invalid command name "tclvars"
# ***** invalid command name ""
