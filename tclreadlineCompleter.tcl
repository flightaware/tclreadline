# -*- tclsh -*-
# FILE: "/disk01/home/joze/src/tclreadline/tclreadlineCompleter.tcl"
# LAST MODIFICATION: "Thu Sep 16 02:53:18 1999 (joze)"
# (C) 1998, 1999 by Johannes Zellner, <johannes@zellner.org>
# $Id$
# ---
#
# tclreadline -- gnu readline for tcl
# Copyright (C) 1999  Johannes Zellner
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# johannes@zellner.org
# http://www.zellner.org/tclreadline/
#
# ================================================================== 


# TODO:
#
#	- tcltest is missing
#	- better completion for CompleteListFromList:
#	  RemoveUsedOptions ...
#	- namespace eval fred {... <-- continue with a 
#								   substitution in fred.
#
#



namespace eval tclreadline {

namespace export \
TryFromList CompleteFromList DisplayHints Rehash \
PreviousWord CommandCompletion RemoveUsedOptions \
HostList ChannelId InChannelId OutChannelId \
Lindex Llength CompleteBoolean

# set tclreadline::trace to 1, if you
# want to enable explicit trace calls.
#
variable trace

# set tclreadline::trace_procs to 1, if you
# want to enable tracing every entry to a proc.
#
variable trace_procs

if {[info exists trace_procs] && $trace_procs} {
	::proc proc {name arguments body} {
		::proc $name $arguments [subst -nocommands {
			TraceText [lrange [info level 0] 1 end]
			$body
		}]
	}
} else { ;# !$trace_procs
	catch {rename ::tclreadline::proc ""}
}

if {[info exists trace] && $trace} {

	::proc TraceReconf {args} {
		eval .tclreadline_trace.scroll set $args
		.tclreadline_trace.text see end
	}

	::proc AssureTraceWindow {} {
		variable trace
		if {![info exists trace]} {
			return 0
		}
		if {!$trace} {
			return 0
		}
		if {![winfo exists .tclreadline_trace.text]} {
			toplevel .tclreadline_trace
			text .tclreadline_trace.text \
			-yscrollcommand { tclreadline::TraceReconf }
			scrollbar .tclreadline_trace.scroll \
			-orient vertical \
			-command { .tclreadline_trace.text yview }
			pack .tclreadline_trace.text -side left -expand yes -fill both
			pack .tclreadline_trace.scroll -side right -expand yes -fill y
		} else {
			raise .tclreadline_trace
		}
		return 1
	}

	::proc TraceVar vT {
		if {![AssureTraceWindow]} {
			return
		}
		upvar $vT v
		if {[info exists v]} {
			.tclreadline_trace.text insert end \
			"([lindex [info level -1] 0]) $vT=|$v|\n"
		}
		# silently ignore unset variables.
	}

	::proc TraceText txt {
		if {![AssureTraceWindow]} {
			return
		}
		.tclreadline_trace.text insert end \
		[format {%32s %s} ([lindex [info level -1] 0]) $txt\n]
	}

} else {
	::proc TraceReconf args {}
	::proc AssureTraceWindow args {}
	::proc TraceVar args {}
	::proc TraceText args {}
}

#**
# TryFromList will return an empty string, if
# the text typed so far does not match any of the
# elements in list. This might be used to allow
# subsequent filename completion by the builtin
# completer.
# If inhibit is non-zero, the result will be
# formatted such that readline will not insert
# a space after a complete (single) match.
#
proc TryFromList {text lst {allow ""} {inhibit 0}} {

	# puts stderr "(CompleteFromList) \ntext=|$text|"
	# puts stderr "(CompleteFromList) lst=|$lst|"
	set pre [GetQuotedPrefix ${text}]
	set matches [MatchesFromList $text $lst $allow]

	# puts stderr "(CompleteFromList) matches=|$matches|"
	if {1 == [llength $matches]} { ; # unique match
		# puts stderr \nunique=$matches\n
		# puts stderr "\n|${pre}${matches}[Right ${pre}]|\n"
		set null [string index $matches 0]
		if {"<" == $null || "?" == $null} {
			set completion [string trim "[list $text] $lst"]
		} else {
			set completion [string trim ${pre}${matches}[Right ${pre}]]
		}
		if {$inhibit} {
			return [list $completion {}]
		} else {
			return $completion
		}
	} elseif {"" != ${matches}} {
		# puts stderr \nmore=$matches\n
		set longest [CompleteLongest ${matches}]
		# puts stderr longest=|$longest|
		if {"" == $longest} {
			return [string trim "[list $text] ${matches}"]
		} else {
			return [string trim "${pre}${longest} ${matches}"]
		}
	} else {
		return ""; # nothing to complete
	}
}

#**
# CompleteFromList will never return an empty string.
# completes, if a completion can be done, or ring
# the bell if not. If inhibit is non-zero, the result
# will be formatted such that readline will not insert
# a space after a complete (single) match.
#
proc CompleteFromList {text lst {inhibit 0}} {
	set result [TryFromList ${text} ${lst} "" $inhibit]
	if {![llength ${result}]} {
		Alert
		# return [string trim [list ${text}] ${lst}"]
		if {[llength ${lst}]} {
			return [string trim "${text} ${lst}"]
		} else {
			return [string trim [list ${text} {}]]
		}
	} else {
		return ${result}
	}
}

#**
# CompleteBoolean does a CompleteFromList
# with a list of all valid boolean values.
#
proc CompleteBoolean {text} {
	return [CompleteFromList $text {yes no true false 1 0}]
}

#**
# build a list of all executables which can be
# found in $env(PATH). This is (naturally) a bit
# slow, and should not called frequently. Instead
# it is a good idea to check if the variable
# `executables' exists and then just use it's
# content instead of calling Rehash.
# (see complete(exec)).
# 
proc Rehash {} {

	global env
	variable executables

	if {![info exists env] || ![array exists env]} {
		return
	}
	if {![info exists env(PATH)]} {
		return
	}

	set executables 0
	foreach dir [split $env(PATH) :] {
		if {[catch [list set files [glob -nocomplain ${dir}/*]]]} { continue }
		foreach file $files {
			if {[file executable $file]} {
				lappend executables [file tail ${file}]
			}
		}
	}
}

#**
# build a list hosts from the /etc/hosts file.
# this is only done once. This is sort of a
# dirty hack, /etc/hosts is hardcoded ...
# But on the other side, if the user supplies
# a valid host table in tclreadline::hosts
# before entering the event loop, this proc
# will return this list.
# 
proc HostList {} {
	# read the host table only once.
	#
	variable hosts
	if {![info exists hosts]} {
		catch {
			set hosts ""
			set id [open /etc/hosts r]
			if {0 != ${id}} {
				while {-1 != [gets ${id} line]} {
					regsub {#.*} ${line} {} line
					if {[llength ${line}] >= 2} {
						lappend hosts [lindex ${line} 1]
					}
				}
				close ${id} 
			} 
		}
	}
	return ${hosts} 
}

#**
# never return an empty string, never complete.
# This is useful for showing options lists for example.
#
proc DisplayHints {lst} {
	return [string trim "{} ${lst}"]
}

#**
# find (partial) matches for `text' in `lst'. Ring
# the bell and return the whole list, if the user
# tries to complete ?..? options or <..> hints.
#
# MatchesFromList returns a list which is not suitable
# for passing to the readline completer. Thus,
# MatchesFromList should not be called directly but
# from formatting routines as TryFromList.
#
proc MatchesFromList {text lst {allow ""}} {
	set result ""
	set text [StripPrefix $text]
	set null [string index $text 0]
	foreach char {< ?} {
		if {$char == $null && -1 == [string first $char $allow]} {
			Alert
			return $lst
		}
	}
	# puts stderr "(MatchesFromList) text=$text"
	# puts stderr "(MatchesFromList) lst=$lst"
	foreach word $lst {
		if {[string match ${text}* ${word}]} {
			lappend result ${word}
		}
	}
	return [string trim $result]
}

#**
# invoke cmd with a (hopefully) invalid string and
# parse the error message to get an option list.
#
# @param cmd
# @return list of options for cmd
# @date Sep-14-1999
#
proc TrySubCmds {cmd} {
	set trystring ____
	set result ""
	if [catch {set result [eval ${cmd} ${trystring}]} msg] {
		set tcmd [string trim ${cmd}]
		if {[regexp {bad *option.*____.*: *must *be( .*$)} ${msg} all raw]} {
			regsub -all -- , ${raw} { } raw
			set len [llength ${raw}]
			set len_2 [expr ${len} - 2]
			for {set i 0} {${i} < ${len}} {incr i} {
				set word [lindex ${raw} ${i}]
				if {"or" != ${word} && ${i} != ${len_2}} {
					lappend result ${word}
				}

			}
		} elseif {[regexp "wrong # args: should be \"${tcmd}\(.*\)\"" \
			${msg} all hint]
		} {
			set result [string trim $hint]
		} else {
			# check, if it's a blt error msg ...
			#
			set msglst [split ${msg} \n]
			foreach line ${msglst} {
				if {[regexp "${tcmd}\[ \t\]\+\(\[^ \t\]*\)\[^:\]*$" \
					${line} all sub]
				} {
					lappend result [list ${sub}]
				}
			}
		}
	}
	return ${result}
}

proc FirstNonOption {line} {
	set expr_pos 1
	foreach word [lrange ${line} 1 end] {; # 0 is the command itself
		if {"-" != [string index ${word} 0]} {
			break
		} else {
			incr expr_pos
		}
	}
	return ${expr_pos}
}

proc RemoveUsedOptions {line opts {terminate {}}} {
	if {[llength ${terminate}]} {
		if {[regexp -- ${terminate} ${line}]} {
			return ""
			# return ${terminate}
		}
	}
	set new ""
	foreach word ${opts} {
		if {-1 == [string first ${word} ${line}]} {
			lappend new ${word}
		}
	}
	return [string trim ${new}]
}

proc Alert {} {
	puts -nonewline \a
	flush stdout
}

#**
# get the longest common completion
# e.g. str == {tcl_version tclreadline_version tclreadline_library}
# --> [CompleteLongest ${str}] == "tcl"
#
proc CompleteLongest {str} {
	# puts stderr str=$str
	set match0 [lindex ${str} 0]
	set len0 [string length $match0]
	set no_matches [llength ${str}]
	set part ""
	for {set i 0} {$i < $len0} {incr i} {
		set char [string index $match0 $i]
		for {set j 1} {$j < $no_matches} {incr j} {
			if {$char != [string index [lindex ${str} $j] $i]} {
				break
			}
		}
		if {$j < $no_matches} {
			break
		} else {
			append part $char
		}
	}
	# puts stderr part=$part
	return ${part}
}

proc SplitLine {start line} {
	set depth 0
	# puts stderr SplitLine
	for {set i $start} {$i >= 0} {incr i -1} {
		set c [string index $line $i]
		if {{;} == $c} {
			incr i; # discard command break character
			return [list [expr $start - $i] [string range $line $i end]]
		} elseif {{]} == $c} {
			incr depth
		} elseif {{[} == $c} {
			incr depth -1
			if {$depth < 0} {
				incr i; # discard command break character
				return [list [expr $start - $i] [string range $line $i end]]
			}
		}
	}
	return ""
}

proc IsWhite {char} {
	if {" " == $char || "\n" == $char || "\t" == $char} {
		return 1
	} else {
		return 0
	}
}

proc PreviousWord {start line} {
	incr start -1
	set found 0
	for {set i $start} {$i > 0} {incr i -1} {
		set c [string index $line $i]
		if {${found} && [IsWhite $c]} {
			break
		} elseif {!${found} && ![IsWhite $c]} {
			set found 1
		}
	}
	return [string trim [string range ${line} $i $start]]
}

proc Quote {value left} {
	set right [Right ${left}]
	if {1 < [llength $value] && "" == $right} {
		return [list \"${value}\"]
	} else {
		return [list ${left}${value}${right}]
	}
}

# the following two channel proc's make use of
# the brandnew (Sep 99) `file channels' command
# but have some fallback behaviour for older
# tcl version.
#
proc InChannelId {text {switches ""}} {
	if [catch {set chs [file channels]}] {
		set chs {stdin}
	}
	set result ""
	foreach ch $chs {
		if {![catch {fileevent $ch readable}]} {
			lappend result $ch
		}
	}
	return [ChannelId ${text} <inChannel> $result $switches]
}

proc OutChannelId {text {switches ""}} {
	if [catch {set chs [file channels]}] {
		set chs {stdout stderr}
	}
	set result ""
	foreach ch $chs {
		if {![catch {fileevent $ch writable}]} {
			lappend result $ch
		}
	}
	return [ChannelId ${text} <outChannel> $result $switches]
}

proc ChannelId {text {descript <channelId>} {chs ""} {switches ""}} {
	if {"" == ${chs}} {
		# the `file channels' command is present
		# only in pretty new versions.
		#
		if [catch {set chs [file channels]}] {
			set chs {stdin stdout stderr}
		}
	}
	if {[llength [set channel [TryFromList ${text} "${chs} ${switches}"]]]} {
		return ${channel}
	} else {
		return [DisplayHints [string trim "${descript} ${switches}"]]
	}
}

proc QuoteQuotes {line} {
	regsub -all -- \" $line {\"} line
	regsub -all -- \{ $line {\{} line; # \}\} (keep the editor happy)
	return $line
}

#**
# get the word position.
# @return the word position
# @note will returned modified values.
# @sa EventuallyEvaluateFirst
# @date Sep-06-1999
#
# % p<TAB>
# % bla put<TAB> $b
# % put<TAB> $b
# part  == put
# start == 0
# end   == 3
# line  == "put $b"
# [PartPosition] should return 0
#
proc PartPosition {partT startT endT lineT} {

	upvar $partT part $startT start $endT end $lineT line
	EventuallyEvaluateFirst part start end line
	return [Llength [string range $line 0 [expr $start - 1]]]

# 
#     set local_start [expr $start - 1]
#     set local_start_chr [string index $line $local_start]
#     if {"\"" == $local_start_chr || "\{" == $local_start_chr} {
#         incr local_start -1
#     }
# 
#     set pre_text [QuoteQuotes [string range $line 0 $local_start]]
#     return [llength $pre_text]
# 
}

proc Right {left} {
	# puts left=$left
	if {"\"" == $left} {
		return "\""
	} elseif {"\\\"" == $left} {
		return "\\\""
	} elseif {"\{" == $left} {
		return "\}"
	} elseif {"\\\{" == $left} {
		return "\\\}"
	}
	return ""
}

proc GetQuotedPrefix {text} {
	set null [string index $text 0]
	if {"\"" == $null || "\{" == $null} {
		return \\$null
	} else {
		return {}
	}
}

proc CountChar {line char} {
	# puts stderr char=|$char|
	set found 0
	set pos 0
	while {-1 != [set pos [string first $char $line $pos]]} {
		incr pos
		incr found
	}
	return $found
}

#**
# make a proper tcl list from an icomplete
# string, that is: remove the junk. This is
# complementary to `IncompleteListRemainder'.
# e.g.:
#       for {set i 1} "
#  -->  for {set i 1}
#
proc ProperList {line} {
	set last [expr [string length $line] - 1]
	for {set i $last} {$i >= 0} {incr i -1} {
		if {![catch {llength [string range $line 0 $i]}]} {
			break
		}
	}
	return [string range $line 0 $i]
}

#**
# return the last part of a line which
# prevents the line from beeing a list.
# This is complementary to `ProperList'.
#
proc IncompleteListRemainder {line} {
	set last [expr [string length $line] - 1]
	for {set i $last} {$i >= 0} {incr i -1} {
		if {![catch {llength [string range $line 0 $i]}]} {
			break
		}
	}
	incr i
	return [string range $line $i end]
}

#**
# save `lindex'. works also for non-complete lines
# with opening parentheses or quotes.
# usage as `lindex'.
# Eventually returns the Rest of an incomplete line,
# if the index is `end' or == [Llength $line].
#
proc Lindex {line pos} {
	if {[catch [list set sub [lindex $line $pos]]]} {
		if {"end" == $pos || [Llength $line] == $pos} {
			return [IncompleteListRemainder $line]
		}
		set line [ProperList $line]
		# puts stderr \nproper_line=|$proper_line|
		if {[catch [list set sub [lindex $line $pos]]]} { return {} }
	}
	return $sub
}

#**
# save `llength' (see above).
#
proc Llength {line} {
	if {[catch [list set len [llength $line]]]} {
		set line [ProperList $line]
		if {[catch [list set len [llength $line]]]} { return {} }
	}
	# puts stderr \nline=$line
	return $len
}

proc StripPrefix {text} {
	# puts "(StripPrefix) text=|$text|"
	set null [string index $text 0]
	if {"\"" == $null || "\{" == $null} {
		return [string range $text 1 end]
	} else {
		return $text
	}
}

proc VarCompletion {text {level -1}} {
	if {-1 == ${level}} {
		set level [info level]
	} else {
		incr level
	}
	set pre [GetQuotedPrefix ${text}]
	set var [StripPrefix ${text}]
	# puts stderr "(VarCompletion) pre=|$pre|"
	# puts stderr "(VarCompletion) var=|$var|"

	# arrays
	#
	if {[regexp {([^(]*)\((.*)} ${var} all array name]} {
		set names [uplevel ${level} array names ${array} ${name}*]
		if {1 == [llength $names]} { ; # unique match
			return "${array}(${names})"
		} elseif {"" != ${names}} {
			return "${array}([CompleteLongest ${names}] ${names}"
		} else {
			return ""; # nothing to complete
		}
	}

	# non-arrays
	#
	regsub ":$" ${var} "::" var
	set namespaces [namespace children :: ${var}*]
	if {[llength ${namespaces}] && "::" != [string range ${var} 0 1]} {
		foreach name ${namespaces} {
			regsub "^::" ${name} "" name
			if {[string length ${name}]} {
				lappend new ${name}::
			}
		}
		set namespaces ${new}
		unset new
	}
	set matches \
	[string trim "[uplevel ${level} info vars ${var}*] ${namespaces}"]
	if {1 == [llength $matches]} { ; # unique match

		# check if this unique match is an
		# array name, (whith no "(" yet).
		#
		if {[uplevel ${level} array exists $matches]} {
			return [VarCompletion ${matches}( ${level}]; # recursion
		} else {
			return ${pre}${matches}[Right ${pre}]
		}
	} elseif {"" != $matches} { ; # more than one match
		  return [CompleteFromList ${text} ${matches}]
	} else {
		return ""; # nothing to complete
	}
}

proc CompleteControlStatement {text start end line pos mod pre new_line} {
	set pre [GetQuotedPrefix ${pre}]
	set cmd [Lindex $new_line 0]
	set diff [expr \
	[string length $line] - [string length $new_line]]
	if {$diff == [expr $start + 1]} {
		set mod1 $mod
	} else {
		set mod1 $text
		set pre ""
	}
	set new_end [expr $end - $diff]
	set new_start [expr $new_end - [string length $mod1]]
	# puts ""
	# puts new_start=$new_start
	# puts new_end=$new_end
	# puts new_line=$new_line
	# puts mod1=$mod1
	if {$new_start < 0} {
		return ""; # when does this occur?
	}
	# puts stderr ""
	# puts stderr start=|$start|
	# puts stderr end=|$end|
	# puts stderr mod=|$mod|
	# puts stderr new_start=|$new_start|
	# puts stderr new_end=|$new_end|
	# puts stderr new_line=|$new_line|
	# puts stderr ""
	set res [ScriptCompleter $mod1 $new_start $new_end $new_line]
	# puts stderr \n\${pre}\${res}=|${pre}${res}|
	if {[string length [Lindex ${res} 0]]} {
		return ${pre}${res}
	} else {
		return ${res}
	}
	return ""
}

proc BraceOrCommand {text start end line pos mod} {
	if {![string length [Lindex $line $pos]]} {
		return [list \{ {}]; # \}
	} else {
		set new_line [string trim [IncompleteListRemainder $line]]
		if {![regexp {^([\{\"])(.*)$} $new_line all pre new_line]} {
			set pre ""
		}
		return [CompleteControlStatement $text \
		$start $end $line $pos $mod $pre $new_line]
	}
}

proc FullQualifiedMatches {qualifier matchlist} {
	set new ""
	if {"" != $qualifier && ![regexp ::$ $qualifier]} {
		append qualifier ::
	}
	foreach entry ${matchlist} {
		set full ${qualifier}${entry}
		if {"" != [namespace which ${full}]} {
			lappend new ${full}
		}
	}
	return ${new}
}

proc ProcsOnlyCompletion {cmd} {
	return [CommandCompletion ${cmd} procs]
}

proc CommandsOnlyCompletion {cmd} {
	return [CommandCompletion ${cmd} commands]
}

proc CommandCompletion {cmd {action both} {spc ::}} {
	# get the leading colons in `cmd'.
	regexp {^:*} ${cmd} pre
	return [CommandCompletionWithPre $cmd $action $spc $pre]
}

proc CommandCompletionWithPre {cmd action spc pre} {
	# puts stderr "(CommandCompletion) cmd=|$cmd|"
	# puts stderr "(CommandCompletion) action=|$action|"
	# puts stderr "(CommandCompletion) spc=|$spc|"

	set cmd [StripPrefix ${cmd}]
	set quali [namespace qualifiers ${cmd}]
	if {[string length ${quali}]} {
		# puts stderr \nquali=|$quali|
		set matches [CommandCompletionWithPre \
		[namespace tail ${cmd}] ${action} ${spc}${quali} ${pre}]
		# puts stderr \nmatches1=|$matches|
		return $matches
	}
	set cmd [string trim ${cmd}]*
	# puts stderr \ncmd=|$cmd|\n
	if {"procs" != ${action}} {
		set all_commands [namespace eval $spc [list info commands ${cmd}]]
		# puts stderr all_commands=|$all_commands|
		set commands ""
		foreach command $all_commands {
			if {[namespace eval $spc [list namespace origin $command]] == \
				[namespace eval $spc [list namespace which $command]]} {
				lappend commands $command
			}
		}
	} else {
		set commands ""
	}
	if {"commands" != ${action}} {
		set all_procs [namespace eval $spc [list info procs ${cmd}]]
		# puts stderr procs=|$procs|
		set procs ""
		foreach proc $all_procs {
			if {[namespace eval $spc [list namespace origin $proc]] == \
				[namespace eval $spc [list namespace which $proc]]} {
				lappend procs $proc
			}
		}
	} else {
		set procs ""
	}
	set matches [namespace eval $spc concat ${commands} ${procs}]
	set namespaces [namespace children $spc ${cmd}]

	if {![llength ${matches}] && 1 == [llength ${namespaces}]} {
		set matches [CommandCompletionWithPre {} ${action} ${namespaces} ${pre}]
		# puts stderr \nmatches=|$matches|
		return $matches
	}

	# make `namespaces' having exactly
	# the same number of colons as `cmd'.
	#
	regsub -all {^:*} $spc $pre spc

	set matches [FullQualifiedMatches ${spc} ${matches}]
	# puts stderr \nmatches3=|$matches|
	return [string trim "${matches} ${namespaces}"]
}

#**
# check, if the first argument starts with a '['
# and must be evaluated before continuing.
# NOTE: trims the `line'.
#       eventually modifies all arguments.
# DATE: Sep-06-1999
#
proc EventuallyEvaluateFirst {partT startT endT lineT} {
	# return; # disabled
	upvar $partT part $startT start $endT end $lineT line

	set oldlen [string length ${line}]
	# set line [string trim ${line}]
	set line [string trimleft ${line}]
	set diff [expr [string length $line] - $oldlen]
	incr start $diff
	incr end $diff

	set char [string index ${line} 0]
	if {{[} != ${char} && {$} != ${char}} {return}

	set pos 0
	while {-1 != [set idx [string first {]} ${line} ${pos}]]} {
		set cmd [string range ${line} 0 ${idx}]
		if {[info complete ${cmd}]} {
			break;
		}
		set pos [expr ${idx} + 1]
	}

	if {![info exists cmd]} {return}
	if {![info complete ${cmd}]} {return}
	set cmd [string range ${cmd} 1 [expr [string length ${cmd}] - 2]]
	set rest [string range ${line} [expr ${idx} + 1] end]

	if {[catch [list set result [string trim [eval ${cmd}]]]]} {return}

	set line ${result}${rest}
	set diff [expr [string length ${result}] - ([string length ${cmd}] + 2)]
	incr start ${diff}
	incr end ${diff}
}

# if the line entered so far is
# % puts $b<TAB>
# part  == $b
# start == 5
# end   == 7
# line  == "$puts $b"
#
proc ScriptCompleter {part start end line} {

	# puts stderr "(ScriptCompleter) |$part| $start $end |$line|"

	# if the character before the cursor is a terminating
	# quote and the user wants completion, we insert a white
	# space here.
	#
	set char [string index $line [expr $end - 1]]
	if {"\}" == $char} {
		append $part " "
		return [list $part]
	}

	if {{$} == [string index $part 0]} {

		# check for a !$ history event
		#
		if {$start > 0} {
			if {{!} == [string index $line [expr $start - 1]]} {
				return ""
			}
		}
		# variable completion. Check first, if the
		# variable starts with a plain `$' or should
		# be enclosed in braces.
		#
		set var [string range $part 1 end]

		# check if $var is an array name, which
		# already has already a "(" somewhere inside.
		#
		if {"" != [set vc [VarCompletion $var]]} {
			if {"" == [lindex $vc 0]} {
				return "\$ [lrange ${vc} 1 end]"
			} else {
				return \$${vc}
			}
			# puts stderr vc=|$vc|
		} else {
			return ""
		}

	# SCENARIO:
	#
	# % puts bla; put<TAB> $b
	# part  == put
	# start == 10
	# end   == 13
	# line  == "puts bla; put $b"
	# [SplitLine] --> {1 " put $b"} == sub
	# new_start = [lindex $sub 0] == 1
	# new_end   = [expr $end - ($start - $new_start)] == 4
	# new_part  == $part == put
	# new_line  = [lindex $sub 1] == " put $b"
	# 
	} elseif {"" != [set sub [SplitLine $start $line]]} {

		set new_start [lindex $sub 0]
		set new_end [expr $end - ($start - $new_start)]
		set new_line [lindex $sub 1]
		# puts stderr "(SplitLine) $new_start $new_end $new_line"
		return [ScriptCompleter $part $new_start $new_end $new_line]

	} elseif {0 == [set pos [PartPosition part start end line]]} {

		# puts stderr "(PartPosition) $part $start $end $line"
		set all [CommandCompletion ${part}]
		# puts stderr "(ScriptCompleter) all=$all"
		#puts \nmatches=$matches\n
		# return [Format $all $part]
		return [TryFromList $part $all]

	} else {

		# try to use $pos further ...
		# puts stderr |$line|
		#
		if {"." == [string index [string trim ${line}] 0]} {
			set alias WIDGET
			set namespc ""
		} else {

			# the double `lindex' strips {} or quotes.
			# the subst enables variables containing
			# command names.
			#
			set alias [uplevel [info level] \
			subst [lindex [lindex [QuoteQuotes ${line}] 0] 0]]

			# make `alias' a fully qualified name.
			# this can raise an error, if alias is
			# no valid command.
			#
			if {[catch [list set alias [namespace origin $alias]]]} {
				return ""
			}

			# strip leading ::'s.
			#
			regsub -all {^::} $alias {} alias
			set namespc [namespace qualifiers $alias]
			set alias [namespace tail $alias]
		}

		foreach cmd [list ${alias} tclreadline_complete_unknown] {
			# puts stderr ${namespc}complete(${cmd})
			if {"" != [namespace eval ::tclreadline::${namespc} \
				[list info procs complete(${cmd})]]
			} {
				# puts found=|complete($cmd)|
				# to be more error-proof, we check here,
				# if complete($cmd) takes exactly 5 arguments.
				#
				if {6 != [set arguments [llength \
					[namespace eval ::tclreadline::${namespc} \
					[list info args complete($cmd)]]]]
				} {
					error [list complete(${cmd}) takes ${arguments} \
					arguments, but should take exactly 6.]
				}

				# remove leading quotes
				#
				set mod [StripPrefix $part]
				# puts stderr mod=$mod

				if {[catch [list set script_result \
					[namespace eval ::tclreadline::${namespc} \
					[list complete(${cmd}) $part $start $end $line $pos $mod]]]\
					::tclreadline::errorMsg]
				} {
					error [list error during evaluation of `complete(${cmd})']
				}
				# puts stderr \nscript_result=|${script_result}|
				return ${script_result}
			}
			# set namespc ""; # no qualifiers for tclreadline_complete_unknown
		}

		# as we've reached here no valid specific completer
		# was found. Check, if it's a proc and return the
		# arguments.
		#
		if {[string length [uplevel [info level] info proc $alias]]} {
			set args [uplevel [info level] info args $alias]
			set arg [lindex $args [expr $pos - 1]]
			if {"" != $arg && "args" != $arg} {
				if {[uplevel [info level] info default $alias $arg junk]} {
					return [DisplayHints ?$arg?]
				} else {
					return [DisplayHints <$arg>]
				}
			}
		}


		# Ok, also no proc. Try to do the same as for widgets now:
		# try to complete from the option table if the subcommand
		# is `configure' or `cget' otherwise try to get further
		# subcommands.
		#
		return [CompleteFromOptionsOrSubCmds $part $start $end $line $pos]
	}
	error "{NOTREACHED (this is probably an error)}"
}


# explicit command completers
#

# -------------------------------------
#                 TCL
# -------------------------------------

proc complete(after) {text start end line pos mod} {
	set sub [Lindex $line 1]
	# puts \npos=$pos
	switch -- $pos {
		1 {
			return [CompleteFromList ${text} {<ms> cancel idle info}]
		}
		2 {
			switch -- $sub {
				cancel {
					return [CompleteFromList $text "<script> [after info]"]
				}
				idle {
					return [DisplayHints <script>]
				}
				info {
					return [CompleteFromList $text [after info]]
				}
				default { return [DisplayHints ?script?] }
			}
		}
		default {
			switch -- $sub {
				info { return [DisplayHints {}] }
				default { return [DisplayHints ?script?] }
			}
		}
	}
	return ""
}

proc complete(append) {text start end line pos mod} {
	switch -- $pos {
		1       { return [VarCompletion ${text}] }
		default { return [DisplayHints ?value?] }
	}
	return ""
}

proc complete(array) {text start end line pos mod} {
	switch -- $pos {
		1 {
			set cmds {
				anymore donesearch exists get names
				nextelement set size startsearch
			}
			return [CompleteFromList $text $cmds]
		}
		2 {
			set matches ""
			# set vars [uplevel [info level] info vars ${mod}*]
			#
			# better: this displays a list of array names if the
			# user inters with something which cannot be matched.
			# The matching against `text' is done by CompleteFromList.
			#
			set vars [uplevel [info level] info vars]
			foreach var ${vars} {
				if {[uplevel [info level] array exists ${var}]} {
					lappend matches ${var}
				}
			}
			return [CompleteFromList ${text} ${matches}]
		}
		3 {
			set cmd [Lindex $line 1]
			set array_name [Lindex $line 2]
			switch -- $cmd {
				get -
				names {
					set pattern [Lindex $line 3]
					set matches [uplevel [info level] \
					array names ${array_name} ${pattern}*]
					if {![llength $matches]} {
						return [DisplayHints ?pattern?]
					} else {
						return [CompleteFromList ${text} ${matches}]
					}
				}
				anymore -
				donesearch -
				nextelement { return [DisplayHints <searchId>] }
			}
		}
	}
	return ""
}

# proc complete(bgerror) {text start end line pos mod} {
# }

proc complete(binary) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			return [CompleteFromList $text {format scan}]
		}
		2 {
			switch -- $cmd {
				format { return [DisplayHints <formatString>] }
				scan   { return [DisplayHints <string>] }
			}
		}
		3 {
			switch -- $cmd {
				format { return [DisplayHints ?arg?] }
				scan   { return [DisplayHints <formatString>] }
			}
		}
		default {
			switch -- $cmd {
				format { return [DisplayHints ?arg?] }
				scan   { return [DisplayHints ?varName?] }
			}
		}
	}
	return ""
}

# proc complete(break) {text start end line pos mod} {
# }

proc complete(catch) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <script>] }
		2 { return [DisplayHints ?varName?] }
	}
	return ""
}

proc complete(cd) {text start end line pos mod} {
	return ""
}

proc complete(clock) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			return [CompleteFromList $text {clicks format scan seconds}]
		}
		2 {
			switch -- $cmd {
				format  { return [DisplayHints <clockValue>] }
				scan    { return [DisplayHints <dateString>] }
				clicks  -
				seconds {}
			}
		}
		3 -
		5 {
			switch -- $cmd {
				format {
					set subcmds [RemoveUsedOptions $line {-format -gmt}]
					return [TryFromList $text $subcmds]
				}
				scan {
					set subcmds [RemoveUsedOptions $line {-base -gmt}]
					return [TryFromList $text $subcmds]
				}
				clicks  -
				seconds {}
			}
		}
		4 -
		6 {
			set sub [Lindex $line [expr $pos - 1]]
			switch -- $cmd {
				format {
					switch -- $sub {
						-format { return [DisplayHints <string>] }
						-gmt    { return [DisplayHints <boolean>] }
					}
				}
				scan {
					switch -- $sub {
						-base { return [DisplayHints <clockVal>] }
						-gmt  { return [DisplayHints <boolean>] }
					}
				}
				clicks  -
				seconds {}
			}
		}
	}
	return ""
}

proc complete(close) {text start end line pos mod} {
	switch -- $pos {
		1 { return [ChannelId $text] }
	}
	return ""
}

proc complete(concat) {text start end line pos mod} {
	return [DisplayHints ?arg?]
}

# proc complete(continue) {text start end line pos mod} {
# }

# proc complete(dde) {text start end line pos mod} {
#     We're not on windoze here ...
# }

proc complete(encoding) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			return [CompleteFromList $text {convertfrom convertto names system}]
		}
		2 {
			switch -- $cmd {
				convertfrom -
				convertto -
				system {
					return [CompleteFromList ${text} [encoding names]]
				}
			}
		}
		3 {
			switch -- $cmd {
				convertfrom { return [DisplayHints <data>] }
				convertto { return [DisplayHints <string>] }
			}
		}
	}
	return ""
}

proc complete(eof) {text start end line pos mod} {
	switch -- $pos {
		1 { return [InChannelId $text] }
	}
	return ""
}

proc complete(error) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <message>] }
		2 { return [DisplayHints ?info?] }
		3 { return [DisplayHints ?code?] }
	}
	return ""
}

proc complete(eval) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <arg>] }
		default { return [DisplayHints ?arg?] }
	}
	return ""
}

proc complete(exec) {text start end line pos mod} {
	set redir [list | |& < <@ << > 2> >& >> 2>> >>& >@ 2>@ >&@]
	variable executables
	if {![info exists executables]} {
		Rehash
	}
	switch -- $pos {
		1 {
			return [TryFromList $text "-keepnewline -- $executables"]
		}
		default {
			set prev [PreviousWord ${start} ${line}]
			if {"-keepnewline" == $prev && 2 == $pos} {
				return [TryFromList $text "-- $executables"]
			}
			switch -exact -- $prev {
				| -
				|& { return [TryFromList $text $executables] }
				< -
				> -
				2> -
				>& -
				>> -
				2>> -
				>>& { return "" }
				<@ -
				>@ -
				2>@ -
				>&@ { return [ChannelId $text] }
				<< { return [DisplayHints <value>] }
				default { return [TryFromList $text $redir "<>"] }
			}
		}
	}
	return ""
}

proc complete(exit) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints ?returnCode?] }
	}
	return ""
}

proc complete(expr) {text start end line pos mod} {
	set left $text
	set right ""
	set substitution [regexp -- {(.*)(\(.*)} $text all left right]; #-)

	set cmds {
		- + ~ !  * / % + - << >> < > <= >= == != & ^ | && || <x?y:z>
		acos    cos     hypot   sinh 
		asin    cosh    log     sqrt 
		atan    exp     log10   tan 
		atan2   floor   pow     tanh 
		ceil    fmod    sin     abs 
		double  int     rand    round 
		srand 
	}

	if {[info tclversion] >= 8.2} {
		set end end
	} else {
		set end [expr [string length $text] - 1]
	}
	if {")" == [string index $text $end] && -1 != [lsearch $cmds $left]} {
		return "$text "; # append a space after a closing ')'
	}

	switch -- $left {
		rand { return "rand() " }

		abs  -
		acos -
		asin -
		atan -
		ceil  -
		cos -
		cosh -
		double -
		exp -
		floor -
		int -
		log -
		log10 -
		round  -
		sin  -
		sinh  -
		sqrt  -
		srand  -
		tan  -
		tanh { return [DisplayHints <value>] }


		atan2 -
		fmod -
		hypot -
		pow { return [DisplayHints <value>,<value>] }
	}

	set completions [TryFromList $left $cmds <>]
	if {1 == [llength $completions]} {
		if {!$substitution} {
			if {"rand" == $completions} {
				return "rand() "; # rand() takes no arguments
			}
			append completions (; #-)
			return [list $completions {}]
		}
	} else {
		return $completions
	}
	return ""
}

proc complete(fblocked) {text start end line pos mod} {
	switch -- $pos {
		1 { return [InChannelId $text] }
	}
	return ""
}

proc complete(fconfigure) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			return [ChannelId ${text}]
		}
		default {
			set option [PreviousWord ${start} ${line}]
			switch -- $option {
				-blocking {
					return [CompleteBoolean ${text}]
				}
				-buffering {
					return [CompleteFromList ${text} {full line none}]
				}
				-buffersize {
					if {![llength ${text}]} {
						return [DisplayHints <newSize>]
					}
				}
				-encoding {
					return [CompleteFromList ${text} [encoding names]]
				}
				-eofchar {
					return [DisplayHints {\{<inChar>\ <outChar>\}}]
				}
				-translation {
					return [CompleteFromList ${text} {auto binary cr crlf lf}]
				}
				default {return [CompleteFromList $text \
					[RemoveUsedOptions $line {
					-blocking -buffering -buffersize
					-encoding -eofchar -translation}]]
				}
			}
		}
	}
	return ""
}

proc complete(fcopy) {text start end line pos mod} {
	switch -- $pos {
		1 {
			return [InChannelId ${text}]
		}
		2 {
			return [OutChannelId ${text}]
		}
		default {
			set option [PreviousWord ${start} ${line}]
			switch -- $option {
				-size    { return [DisplayHints <size>] }
				-command { return [DisplayHints <callback>] }
				default  { return [CompleteFromList $text \
					[RemoveUsedOptions $line {-size -command}]]
				}
			}
		}
	}
	return ""
}

proc complete(file) {text start end line pos mod} {
	switch -- $pos {
		1 {
			set cmds {
				atime attributes copy delete dirname executable exists
				extension isdirectory isfile join lstat mkdir mtime
				nativename owned pathtype readable readlink rename
				rootname size split stat tail type volumes writable
			}
			return [TryFromList $text $cmds]
		}
		2 {
			set cmd [Lindex $line 1]
			switch -- $cmd {
				atime -
				attributes -
				dirname -
				executable -
				exists -
				extension -
				isdirectory -
				isfile -
				join -
				lstat -
				mtime -
				mkdir -
				nativename -
				owned -
				pathtype -
				readable -
				readlink -
				rootname -
				size -
				split -
				stat -
				tail -
				type -
				volumes -
				writable {
					return ""
				}

				copy -
				delete -
				rename {
					# return [TryFromList $text "-force [glob *]"]
					# this is not perfect. The  `-force' and `--'
					# options will not be displayed.
					return ""
				}
			}
		}
	}
	return ""
}

proc complete(fileevent) {text start end line pos mod} {
	switch -- $pos {
		1 {
			return [ChannelId ${text}]
		}
		2 {
			return [CompleteFromList ${text} {readable writable}]
		}
		3 {
			return [DisplayHints ?script?]
		}
	}
	return ""
}

proc complete(flush) {text start end line pos mod} {
	switch -- $pos {
		1 { return [OutChannelId ${text}] }
	}
	return ""
}

proc complete(for) {text start end line pos mod} {
	switch -- $pos {
		1 -
		2 -
		3 -
		4 {
			return [BraceOrCommand $text $start $end $line $pos $mod]
		}
	}
	return ""
}

proc complete(foreach) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <varname>] }
		2 { return [DisplayHints <list>] }
		default {
			if {[expr $pos % 2]} {
				return [DisplayHints [list ?varname? <body>]]
			} else {
				return [DisplayHints ?list?]
			}
		}
	}
	return ""
}

proc complete(format) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <formatString>] }
		default { return [DisplayHints ?arg?] }
	}
	return ""
}

proc complete(gets) {text start end line pos mod} {
	switch -- $pos {
		1 { return [InChannelId ${text}] }
		2 { return [VarCompletion ${text}]}
	}
	return ""
}

proc complete(glob) {text start end line pos mod} {
	switch -- $pos {
		1 {
			# This also is not perfect.
			# This will not display the options as hints!
			set matches [TryFromList ${text} {-nocomplain --}]
			if {[llength [string trim ${text}]] && [llength ${matches}]} {
				return ${matches}
			}
		}
	}
	return ""
}

proc complete(global) {text start end line pos mod} {
	return [VarCompletion ${text}]
}

proc complete(history) {text start end line pos mod} {
	switch -- $pos {
		1 {
			set cmds {add change clear event info keep nextid redo}
			return [TryFromList $text $cmds]
		}
		2 {
			set cmd [Lindex $line 1]
			switch -- $cmd {
				add { return [DisplayHints <command>] }
				change { return [DisplayHints <newValue>] }

				info -
				keep { return [DisplayHints ?count?] }

				event -
				redo { return [DisplayHints ?event?] }

				clear -
				nextid { return "" }
			}
		}
	}
	return ""
}

# --- HTTP PACKAGE ---

# create a http namespace inside
# tclreadline and import some commands.
#
namespace eval http {
	catch {
		namespace import \
		::tclreadline::DisplayHints ::tclreadline::PreviousWord \
		::tclreadline::CompleteFromList ::tclreadline::CommandCompletion \
		::tclreadline::RemoveUsedOptions ::tclreadline::HostList \
		::tclreadline::ChannelId ::tclreadline::Lindex \
		::tclreadline::CompleteBoolean
	}
}

proc http::complete(config) {text start end line pos mod} {
	set prev [PreviousWord ${start} ${line}]
	switch -- $prev {
		-accept { return [DisplayHints <mimetypes>] }
		-proxyhost {
			return [CompleteFromList $text [HostList]]
		}
		-proxyport { return [DisplayHints <number>] }
		-proxyfilter {
			return [CompleteFromList $text [CommandCompletion $text]]
		}
		-useragent { return [DisplayHints <string>] }
		default {
			return [CompleteFromList $text [RemoveUsedOptions $line {
				-accept -proxyhost -proxyport -proxyfilter -useragent
			}]]
		}
	}
	return ""
}

proc http::complete(geturl) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <url>] }
		default {
			set prev [PreviousWord ${start} ${line}]
			switch -- $prev {
				-blocksize { return [DisplayHints <size>] }
				-channel { return [ChannelId ${text}] }
				-command -
				-handler -
				-progress {
					return [CompleteFromList $text [CommandCompletion $text]]
				}
				-headers { return [DisplayHints <keyvaluelist>] }
				-query { return [DisplayHints <query>] }
				-timeout { return [DisplayHints <milliseconds>] }
				-validate { return [CompleteBoolean $text] }
				default {
					return [CompleteFromList $text [RemoveUsedOptions $line {
						-blocksize -channel -command -handler -headers
						-progress -query -timeout -validate
					}]]
				}
			}
		}
	}
	return ""
}

proc http::complete(formatQuery) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <key>] }
		2 { return [DisplayHints <value>] }
		default {
			switch [expr $pos % 2] {
				0 { return [DisplayHints ?value?] }
				1 { return [DisplayHints ?key?] }
			}
		}
	}
	return ""
}

proc http::complete(reset) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <token>] }
		2 { return [DisplayHints ?why?] }
	}
	return ""
}

# the unknown proc handles the rest
#
proc \
http::complete(tclreadline_complete_unknown) {text start end line pos mod} {
	set cmd [Lindex $line 0]
	regsub -all {^.*::} $cmd "" cmd
	switch -- $pos {
		1 {
			switch -- $cmd {
				reset -
				wait -
				data -
				status -
				code -
				size -
				cleanup {
					return [DisplayHints <token>]
				}
			}
		}
	}
	return ""
}

# --- END OF HTTP PACKAGE ---

proc complete(if) {text start end line pos mod} {
	# we don't offer the completion `then':
	# it's optional, more difficult to parse
	# and who uses it anyway?
	#
	switch -- $pos {
		1 -
		2 {
			return [BraceOrCommand $text $start $end $line $pos $mod]
		}
		default {
			set prev [PreviousWord ${start} ${line}]
			switch -- $prev {
				then -
				else -
				elseif {
					return [BraceOrCommand \
					$text $start $end $line $pos $mod]
				}
				default {
					if {-1 == [lsearch [ProperList $line] else]} {
						return [CompleteFromList $text {else elseif}]
					}
				}
			}
		}
	}
	return ""
}

proc complete(incr) {text start end line pos mod} {
	switch -- $pos {
		1 {
			set matches [uplevel [info level] info vars ${mod}*]
			set integers ""
			# check for integers
			#
			foreach match $matches {
				if {[uplevel [info level] array exists $match]} {
					continue
				}
				if {[regexp {^[0-9]+$} [uplevel [info level] set $match]]} {
					lappend integers $match
				}
			}
			return [CompleteFromList ${text} ${integers}]
		}
		2 { return [DisplayHints ?increment?] }
	}
	return ""
}

proc complete(info) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			set cmds {
				args body cmdcount commands complete default exists
				globals hostname level library loaded locals nameofexecutable
				patchlevel procs script sharedlibextension tclversion vars}
			return [CompleteFromList $text $cmds]
		}
		2 {
			switch -- $cmd {
				args -
				body -
				default -
				procs { return [complete(proc) ${text} 0 0 ${line} 1 ${mod}] }
				complete { return [DisplayHints <command>] }
				level { return [DisplayHints ?number?] }
				loaded { return [DisplayHints ?interp?] }
				commands -
				exists -
				globals -
				locals -
				vars {
					if {"exists" == $cmd} {
						set do vars
					} else {
						set do $cmd
					}
					# puts stderr [list complete(info) level = [info level]]
					return \
					[CompleteFromList ${text} [uplevel [info level] info ${do}]]
				}
			}
		}
		3 {
			switch -- $cmd {
				default {
					set proc [Lindex $line 2]
					return [CompleteFromList ${text} \
					[uplevel [info level] info args $proc]]
				}
				default {}
			}
		}
		4 {
			switch -- $cmd {
				default {
					return [VarCompletion ${text}]
				}
				default {}
			}
		}
	}
	return ""
}

proc complete(interp) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			set cmds {
				alias aliases create delete eval exists expose hide hidden
				invokehidden issafe marktrusted share slaves target transfer}
			return [TryFromList $text $cmds]
		}
		2 {
			switch -- $cmd {

				create {
					set cmds [RemoveUsedOptions ${line} {-save --} {--}]
					if {[llength $cmds]} {
						return [CompleteFromList $text "$cmds ?path?"]
					} else {
						return [DisplayHints ?path?]
					}
				}

				eval -
				exists -
				expose -
				hide -
				hidden -
				invokehidden -
				marktrusted -
				target { return [CompleteFromList ${text} [interp slaves]] }

				aliases -
				delete -
				issafe -
				slaves { return [CompleteFromList ${text} [interp slaves]] }

				alias -
				share -
				transfer { return [DisplayHints <srcPath>] }
			}
		}
		3 {
			switch -- $cmd {

				alias { return [DisplayHints <srcCmd>] }

				create {
					set cmds [RemoveUsedOptions ${line} {-save --} {--}]
					if {[llength $cmds]} {
						return [CompleteFromList $text "$cmds ?path?"]
					} else {
						return [DisplayHints ?path?]
					}
				}

				eval { return [DisplayHints <arg>] }
				delete { return [CompleteFromList ${text} [interp slaves]] }

				expose { return [DisplayHints <hiddenName>] }
				hide { return [DisplayHints <exposedCmdName>] }

				invokehidden {
					return \
					[CompleteFromList $text {?-global? <hiddenCmdName>}]
				}

				target { return [DisplayHints <alias>] }

				exists {}
				hidden {}
				marktrusted {}
				aliases {}
				issafe {}
				slaves {}

				share -
				transfer {return [ChannelId ${text}]}
			}
		}
		4 {
			switch -- $cmd {

				alias { return [DisplayHints <targetPath>] }
				eval { return [DisplayHints ?arg?] }

				invokehidden {
					return [CompleteFromList $text {<hiddenCmdName> ?arg?}]
				}

				create {
					set cmds [RemoveUsedOptions ${line} {-save --} {--}]
					if {[llength $cmds]} {
						return [CompleteFromList $text "$cmds ?path?"]
					} else {
						return [DisplayHints ?path?]
					}
				}

				expose { return [DisplayHints ?exposedCmdName?] }
				hide { return [DisplayHints ?hiddenCmdName?] }

				share -
				transfer { return [CompleteFromList ${text} [interp slaves]] }
			}
		}
		5 {
			switch -- $cmd {

				alias { return [DisplayHints <targetCmd>] }
				invokehidden -
				eval { return [DisplayHints ?arg?] }

				expose { return [DisplayHints ?exposedCmdName?] }
				hide { return [DisplayHints ?hiddenCmdName?] }

				share -
				transfer { return [CompleteFromList ${text} [interp slaves]] }
			}
		}
	}
	return ""
}

proc complete(join) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <list>] }
		2 { return [DisplayHints ?joinString?] }
	}
	return ""
}

proc complete(lappend) {text start end line pos mod} {
	switch -- $pos {
		1 { return [VarCompletion ${text}] }
		default { return [DisplayHints ?value?] }
	}
	return ""
}

# the following routines are described in the
# `library' man page.
# --- LIBRARY ---

proc complete(auto_execok) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <cmd>] }
	}
	return ""
}

proc complete(auto_load) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <cmd>] }
	}
	return ""
}

proc complete(auto_mkindex) {text start end line pos mod} {
	switch -- $pos {
		1 { return "" }
		default { return [DisplayHints ?pattern?] }
	}
	return ""
}

# proc complete(auto_reset) {text start end line pos mod} {
# }

proc complete(tcl_findLibrary) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <basename>] }
		2 { return [DisplayHints <version>] }
		3 { return [DisplayHints <patch>] }
		4 { return [DisplayHints <initScript>] }
		5 { return [DisplayHints <enVarName>] }
		6 { return [DisplayHints <varName>] }
	}
	return ""
}

proc complete(parray) {text start end line pos mod} {
	switch -- $pos {
		1 {
			set vars [uplevel [info level] info vars]
			foreach var ${vars} {
				if {[uplevel [info level] array exists ${var}]} {
					lappend matches ${var}
				}
			}
			return [CompleteFromList $text $matches]
		}
	}
	return ""
}

proc complete(tcl_endOfWord) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <str>] }
		2 { return [DisplayHints <start>] }
	}
	return ""
}

proc complete(tcl_startOfNextWord) {text start end line pos mod} {
	return [complete(tcl_endOfWord) $text $start $end $line $pos $mod]
}

proc complete(tcl_startOfPreviousWord) {text start end line pos mod} {
	return [complete(tcl_endOfWord) $text $start $end $line $pos $mod]
}

proc complete(tcl_wordBreakAfter) {text start end line pos mod} {
	return [complete(tcl_endOfWord) $text $start $end $line $pos $mod]
}

proc complete(tcl_wordBreakBefore) {text start end line pos mod} {
	return [complete(tcl_endOfWord) $text $start $end $line $pos $mod]
}

# --- END OF `LIBRARY' ---

proc complete(lindex) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <list>] }
		2 { return [DisplayHints <index>] }
	}
	return ""
}

proc complete(linsert) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <list>] }
		2 { return [DisplayHints <index>] }
		3 { return [DisplayHints <element>] }
		default { return [DisplayHints ?element?] }
	}
	return ""
}

proc complete(list) {text start end line pos mod} {
	return [DisplayHints ?arg?]
}

proc complete(llength) {text start end line pos mod} {
	switch -- $pos {
		1 {
			return [DisplayHints <list>]
		}
	}
	return ""
}

proc complete(load) {text start end line pos mod} {
	switch -- $pos {
		1 {
			return ""; # filename
		}
		2 {
			if {![llength ${mod}]} {
				return [DisplayHints ?packageName?]
			}
		}
		3 {
			if {![llength ${mod}]} {
				return [DisplayHints ?interp?]
			}
		}
	}
	return ""
}

proc complete(lrange) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <list>] }
		2 { return [DisplayHints <first>] }
		3 { return [DisplayHints <last>] }
	}
	return ""
}

proc complete(lreplace) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <list>] }
		2 { return [DisplayHints <first>] }
		3 { return [DisplayHints <last>] }
		default { return [DisplayHints ?element?] }
	}
	return ""
}

proc complete(lsearch) {text start end line pos mod} {
	set options {-exact -glob -regexp}
	switch -- $pos {
		1 {
			return [CompleteFromList ${text} "$options <list>"]
		}
		2 -
		3 -
		4 {
			set sub [Lindex $line 1]
			if {-1 != [lsearch $options $sub]} {
				incr pos -1
			}
			switch -- $pos {
				1 { return [DisplayHints <list>] }
				2 { return [DisplayHints <pattern>] }
			}
		}
	}
	return ""
}

proc complete(lsort) {text start end line pos mod} {
	set options [RemoveUsedOptions ${line} {
		-ascii -dictionary -integer -real -command
		-increasing -decreasing -index <list>
	}]
	switch -- $pos {
		1 { return [CompleteFromList ${text} ${options}] }
		default {
			switch -- [PreviousWord ${start} ${line}] {
				-command {
					return [CompleteFromList $text [CommandCompletion $text]]
				}
				-index { return [DisplayHints <index>] }
				default { return [CompleteFromList ${text} ${options}] }
			}
		}
	}
	return ""
}

# --- MSGCAT PACKAGE ---

# create a msgcat namespace inside
# tclreadline and import some commands.
#
namespace eval msgcat {
	catch {namespace import ::tclreadline::DisplayHints}
}

proc msgcat::complete(mc) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <src-string>] }
	}
	return ""
}

proc msgcat::complete(mclocale) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints ?newLocale?] }
	}
	return ""
}

# proc msgcat::complete(mcpreferences) {text start end line pos mod} {
# }

proc msgcat::complete(mcload) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <dirname>] }
	}
	return ""
}

proc msgcat::complete(mcset) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <locale>] }
		2 { return [DisplayHints <src-string>] }
		3 { return [DisplayHints ?translate-string?] }
	}
	return ""
}

proc msgcat::complete(mcunknown) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <locale>] }
		2 { return [DisplayHints <src-string>] }
	}
	return ""
}

# --- END OF MSGCAT PACKAGE ---

# TODO import ! -force
proc complete(namespace) {text start end line pos mod} {
	# TODO dosn't work ???
	set space_matches [namespace children :: [string trim ${mod}*]]
	# puts \nspace_matches=|${space_matches}|
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			set cmds {
				children code current delete eval export forget
				import inscope origin parent qualifiers tail which}
			return [TryFromList $text $cmds]
		}
		2 {
			switch -- $cmd {
				children -
				delete -
				eval -
				inscope -
				forget -
				parent -
				qualifiers -
				tail {
					regsub {^([^:])} ${mod} {::\1} mod; # full qual. name
					return [TryFromList ${mod} $space_matches]
				}
				code { return [DisplayHints <script> ] }
				current {}
				export { return [CompleteFromList ${text} {-clear ?pattern?}] }
				import {
					if {"-" != [string index ${mod} 0]} {
						regsub {^([^:])} ${mod} {::\1} mod; # full qual. name
					}
					return [CompleteFromList ${mod} "-force $space_matches"]
				}
				origin { return [DisplayHints <command>] }
				# qualifiers -
				# tail { return [DisplayHints <string>] }
				which { return [CompleteFromList ${mod} {
					-command -variable <name>}] }
			}
		}
		3 {
			switch -- $cmd {
				children -
				export -
				forget -
				import { return [DisplayHints ?pattern?] }
				delete { return [TryFromList ${mod} $space_matches] }
				eval -
				inscope {
					return [BraceOrCommand \
					$text $start $end $line $pos $mod]
				}
				which { return [CompleteFromList ${mod} {-variable <name>}] }
			}
		}
		4 {
			switch -- $cmd {
				export -
				forget -
				import { return [DisplayHints ?pattern?] }
				delete { return [TryFromList ${mod} $space_matches] }
				eval -
				inscope { return [DisplayHints ?arg?] }
				which { return [CompleteFromList ${mod} {<name>}] }
			}
		}
	}
	return ""
}

proc complete(open) {text start end line pos mod} {
		# 2 { return [DisplayHints ?access?] }
	switch -- $pos {
		2 {
			set access {r r+ w w+ a a+ 
				RDONLY WRONLY RDWR APPEND CREAT EXCL NOCTTY NONBLOCK TRUNC}
			return [CompleteFromList ${text} $access]
		}
		3 { return [DisplayHints ?permissions?] }
	}
	return ""
}

proc complete(package) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			set cmds {
				forget ifneeded names present provide require
				unknown vcompare versions vsatisfies}
			return [TryFromList $text $cmds]
		}
		2 {
			switch -- $cmd {
				forget -
				ifneeded -
				provide -
				versions { return [CompleteFromList ${mod} [package names]] }
				present -
				require {
					return [CompleteFromList ${mod} "-exact [package names]"] }
				names {}
				unknown { return [DisplayHints ?command?] }
				vcompare -
				vsatisfies { return [DisplayHints <version1>] }
			}
		}
		3 {
			set versions ""
			catch [list set versions [package versions [Lindex $line 2]]]
			switch -- $cmd {
				forget {}
				ifneeded {
					if {"" != $versions} {
						return [CompleteFromList ${text} $versions]
					} else {
						return [DisplayHints <version>]
					}
				}
				provide {
					if {"" != $versions} {
						return [CompleteFromList ${text} $versions]
					} else {
						return [DisplayHints ?version?]
					}
				}
				versions {}
				present -
				require {
					if {"-exact" == [PreviousWord ${start} ${line}]} {
						return [CompleteFromList ${mod} [package names]]
					} else {
						if {"" != $versions} {
							return [CompleteFromList ${text} $versions]
						} else {
							return [DisplayHints ?version?]
						}
					}
				}
				names {}
				unknown {}
				vcompare -
				vsatisfies { return [DisplayHints <version2>] }
			}
		}
	}
	return ""
}

proc complete(pid) {text start end line pos mod} {
	switch -- $pos {
		1 { return [ChannelId ${text}] }
	}
}

proc complete(pkg_mkIndex) {text start end line pos mod} {
	set cmds [RemoveUsedOptions ${line} {-direct -load -verbose -- <dir>} {--}]
	set res [string trim [TryFromList $text $cmds]]
	set prev [PreviousWord ${start} ${line}]
	if {"-load" == $prev} {
		return [DisplayHints <pkgPat>]
	} elseif {"--" == $prev} {
		return [TryFromList ${text} <dir>]
	}
	return ${res}
}

proc complete(proc) {text start end line pos mod} {
	switch -- $pos {
		1 {
			set known_procs [ProcsOnlyCompletion ${text}]
			return [CompleteFromList ${text} ${known_procs}]
		}
		2 {
			set proc [Lindex $line 1]
			if {[catch {set args [uplevel [info level] info args ${proc}]}]} {
				return [DisplayHints <args>]
			} else {
				return [list "\{${args}\}"]
			}
		}
		3 {
			if {![string length [Lindex $line $pos]]} {
				return [list \{ {}]; # \}
			} else {
				# return [DisplayHints <body>]
				return [BraceOrCommand $text $start $end $line $pos $mod]
			}
		}
	}
	return ""
}

proc complete(puts) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			return [OutChannelId ${text} "-nonewline"]
		}
		2 {
			switch -- $cmd {
				-nonewline { return [OutChannelId ${text}] }
				default { return [DisplayHints <string>] }
			}
		}
		3 {
			switch -- $cmd {
				-nonewline { return [DisplayHints <string>] }
			}
		}
	}
	return ""
}

# proc complete(pwd) {text start end line pos mod} {
# }

proc complete(read) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 {
			return [InChannelId ${text} "-nonewline"]
		}
		2 {
			switch -- $cmd {
				-nonewline { return [InChannelId ${text}] }
				default { return [DisplayHints <numChars>] }
			}
		}
	}
	return ""
}

proc complete(regexp) {text start end line pos mod} {
	set prev [PreviousWord ${start} ${line}]
	if {[llength ${prev}] && "--" != $prev && \
		("-" == [string index ${prev} 0] || 1 == $pos)} {
		set cmds [RemoveUsedOptions ${line} {
			-nocase -indices -expanded -line 
			-linestop -lineanchor -about <expression> --} {--}]
		if {[llength ${cmds}]} {
			return [string trim [CompleteFromList $text $cmds]]
		}
	} else {
		set virtual_pos [expr ${pos} - [FirstNonOption ${line}]]
		switch -- ${virtual_pos} {
			0 { return [DisplayHints <string>] }
			1 { return [DisplayHints ?matchVar?] }
			default { return [DisplayHints ?subMatchVar?] }
		}
	}
	return ""
}

# proc complete(regexp) {text start end line pos mod} {
#     We're not on windoze here ...
# }

proc complete(regsub) {text start end line pos mod} {
	set prev [PreviousWord ${start} ${line}]
	if {[llength ${prev}] && "--" != $prev && \
		("-" == [string index ${prev} 0] || 1 == $pos)} {
		set cmds [RemoveUsedOptions ${line} {
			-all -nocase --} {--}]
		if {[llength ${cmds}]} {
			return [string trim [CompleteFromList $text $cmds]]
		}
	} else {
		set virtual_pos [expr ${pos} - [FirstNonOption ${line}]]
		switch -- ${virtual_pos} {
			0 { return [DisplayHints <expression>] }
			1 { return [DisplayHints <string>] }
			2 { return [DisplayHints <subSpec>] }
			3 { return [DisplayHints <varName>] }
		}
	}
	return ""
}

proc complete(rename) {text start end line pos mod} {
	switch -- $pos {
		1 {
			return [CompleteFromList $text [CommandCompletion $text]]
		}
		2 {
			return [DisplayHints <newName>]
		}
	}
	return ""
}

# proc complete(resource) {text start end line pos mod} {
#     This is not a mac ...
# }

proc complete(return) {text start end line pos mod} {
	# TODO this is not perfect yet
	set cmds {-code -errorinfo -errorcode ?string?}
	set res [PreviousWord ${start} ${line}]
	switch -- ${res} {
		-errorinfo { return [DisplayHints <info>] }
		-code -
		-errorcode {
			set codes {ok error return break continue}
			return [TryFromList ${mod} ${codes}]
		}
	}
	return [CompleteFromList ${text} [RemoveUsedOptions ${line} ${cmds}]]
}

# --- SAFE PACKAGE ---

# create a safe namespace inside
# tclreadline and import some commands.
#
namespace eval safe {
	catch {
		namespace import \
		::tclreadline::DisplayHints ::tclreadline::PreviousWord \
		::tclreadline::CompleteFromList ::tclreadline::CommandCompletion \
		::tclreadline::RemoveUsedOptions ::tclreadline::HostList \
		::tclreadline::ChannelId ::tclreadline::Lindex \
		::tclreadline::CompleteBoolean
	}
	variable opts
	set opts {
		-accessPath -statics -noStatics -nested -nestedLoadOk -deleteHook
	}
	proc SlaveOrOpts {text start line pos slave} {
		set prev [PreviousWord ${start} ${line}]
		variable opts
		if {$pos > 1} {
			set slave ""
		}
		switch -- $prev {
			-accessPath { return [DisplayHints <directoryList>] }
			-statics { return [CompleteBoolean $text] }
			-nested { return [CompleteBoolean $text] }
			-deleteHook { return [DisplayHints <script>] }
			default {
				return [CompleteFromList ${text} \
				[RemoveUsedOptions ${line} "${opts} $slave"]]
			}
		}
	}
}

proc safe::complete(interpCreate) {text start end line pos mod} {
	return [SlaveOrOpts $text $start $line $pos ?slave?]
}

proc safe::complete(interpInit) {text start end line pos mod} {
	return [SlaveOrOpts $text $start $line $pos [interp slaves]]
}

proc safe::complete(interpConfigure) {text start end line pos mod} {
	return [SlaveOrOpts $text $start $line $pos [interp slaves]]
}

proc safe::complete(interpDelete) {text start end line pos mod} {
	return [CompleteFromList $text [interp slaves]]
}

proc safe::complete(interpAddToAccessPath) {text start end line pos mod} {
	switch -- $pos {
		1 { return [CompleteFromList $text [interp slaves]] }
	}
}

proc safe::complete(interpFindInAccessPath) {text start end line pos mod} {
	switch -- $pos {
		1 { return [CompleteFromList $text [interp slaves]] }
	}
}

proc safe::complete(setLogCmd) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints ?cmd?] }
		default { return [DisplayHints ?arg?] }
	}
}

# --- END OF SAFE PACKAGE ---

proc complete(scan) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <string>] }
		2 { return [DisplayHints <format>] }
		default { return [VarCompletion ${text}] }
	}
	return ""
}

proc complete(seek) {text start end line pos mod} {
	switch -- $pos {
		1 { return [ChannelId ${text}] }
		2 { return [DisplayHints <offset>] }
		3 { return [TryFromList ${text} {start current end}] }
	}
	return ""
}

proc complete(set) {text start end line pos mod} {
	switch -- $pos {
		1 { return [VarCompletion ${text}] }
		2 {
			if {$text == "" || $text == "\"" || $text == "\{"} {
				set line [QuoteQuotes $line]
				if {[catch [list set value [list [uplevel [info level] \
					set [Lindex $line 1]]]] msg]
				} {
					return ""
				} else {
					return [Quote $value ${text}]
				}
			}
		}
	}
	return ""
}

proc complete(socket) {text start end line pos mod} {
	set cmd [Lindex ${line} 1]
	set prev [PreviousWord ${start} ${line}]
	if {"-server" == ${cmd}} {
		# server sockets
		#
		switch -- $pos {
			2 { return [DisplayHints <command>] }
			default {
				if {"-myaddr" == $prev} {
					return [DisplayHints <addr>]
				} else {
					return [CompleteFromList ${mod} \
					[RemoveUsedOptions $line {-myaddr -error -sockname <port>}]]
				}
			}
		}
	} else {
		# client sockets
		#
		switch -- ${prev} {
			-myaddr { return [DisplayHints <addr>] }
			-myport { return [DisplayHints <port>] }
		}

		set hosts [HostList]
		set cmds {-myaddr -myport -async -myaddr -error -sockname -peername}
		if {$pos <= 1} {
			lappend cmds -server
		}
		set cmds [RemoveUsedOptions $line $cmds]
		if {-1 != [lsearch $hosts $prev]} {
			return [DisplayHints <port>]
		} else {
			return [CompleteFromList ${mod} [concat ${cmds} ${hosts}]]
		}
	}
	return ""
}

# proc complete(source) {text start end line pos mod} {
# }

proc complete(split) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <string>] }
		2 { return [DisplayHints ?splitChars?] }
	}
}

proc complete(string) {text start end line pos mod} {
	set cmd [Lindex ${line} 1]
	set prev [PreviousWord ${start} ${line}]
	set cmds {
		bytelength compare equal first index is last length map match
		range repeat replace tolower toupper totitle trim trimleft
		trimright wordend wordstart}
	switch -- $pos {
		1 {
			return [CompleteFromList ${text} ${cmds}]
		}
		2 {
			switch -- $cmd {
				compare -
				equal {
					return [CompleteFromList ${text} {
						-nocase -length <string> }]
				}

				first -
				last { return [DisplayHints <string1>] }

				map { return [CompleteFromList ${text} {-nocase <charMap>]} }
				match { return [CompleteFromList ${text} {-nocase <pattern>]} }

				is {
					return [CompleteFromList ${text} {
						alnum alpha ascii boolean control digit double 
						false graph integer lower print punct space 
						true upper wordchar xdigit 
					}]
				}

				bytelength -
				index -
				length -
				range -
				repeat -
				replace -
				tolower -
				totitle -
				toupper -
				trim -
				trimleft -
				trimright -
				wordend -
				wordstart { return [DisplayHints <string>] }
			}
		}
		3 {
			switch -- $cmd {
				compare -
				equal {
					if {"-length" == $prev} {
						return [DisplayHints <int>]
					}
					return [CompleteFromList ${text} \
					[RemoveUsedOptions $line {-nocase -length <string>}]]
				}

				first -
				last { return [DisplayHints <string2>] }

				map {
					if {"-nocase" == $prev} {
						return [DisplayHints <charMap>]
					} else {
						return [DisplayHints <string>]
					}
				}
				match {
					if {"-nocase" == $prev} {
						return [DisplayHints <pattern>]
					} else {
						return [DisplayHints <string>]
					}
				}

				is {
					return [CompleteFromList ${text} \
					[RemoveUsedOptions $line {-strict -failindex <string>}]]
				}

				bytelength {}
				index -
				wordend -
				wordstart { return [DisplayHints <charIndex>] }
				range -
				replace { return [DisplayHints <first>] }
				repeat { return [DisplayHints <count>] }
				tolower -
				totitle -
				toupper { return [DisplayHints ?first?] }
				trim -
				trimleft -
				trimright { return [DisplayHints ?chars?] }
			}
		}
		4 {
			switch -- $cmd {
				compare -
				equal {
					if {"-length" == $prev} {
						return [DisplayHints <int>]
					}
					return [CompleteFromList ${text} \
					[RemoveUsedOptions $line {-nocase -length <string>}]]
				}

				first -
				last { return [DisplayHints ?startIndex?] }

				map -
				match { return [DisplayHints <string>] }

				is {
					if {"-failindex" == $prev} {
						return [VarCompletion ${text}]
					}
					return [CompleteFromList ${text} \
					[RemoveUsedOptions $line {-strict -failindex <string>}]]
				}

				bytelength {}
				index {}
				length {}
				range -
				replace { return [DisplayHints <last>] }
				repeat {}
				tolower -
				totitle -
				toupper { return [DisplayHints ?last?] }
				trim -
				trimleft -
				trimright {}
				wordend -
				wordstart {}
			}
		}
		default {
			switch -- $cmd {
				compare -
				equal {
					if {"-length" == $prev} {
						return [DisplayHints <int>]
					}
					return [CompleteFromList ${text} \
					[RemoveUsedOptions $line {-nocase -length <string>}]]
				}

				is {
					if {"-failindex" == $prev} {
						return [VarCompletion ${text}]
					}
					return [CompleteFromList ${text} \
					[RemoveUsedOptions $line {-strict -failindex <string>}]]
				}

				replace { return [DisplayHints ?newString?] }
			}
		}
	}
	return ""
}

proc complete(subst) {text start end line pos mod} {
	return [CompleteFromList ${text} [RemoveUsedOptions $line {
		-nobackslashes -nocommands -novariables <string>}]]
}

proc complete(switch) {text start end line pos mod} {
	set prev [PreviousWord ${start} ${line}]
	if {[llength ${prev}] && "--" != $prev && \
		("-" == [string index ${prev} 0] || 1 == $pos)} {
		set cmds [RemoveUsedOptions ${line} {
			-exact -glob -regexp --} {--}]
		if {[llength ${cmds}]} {
			return [string trim [CompleteFromList $text $cmds]]
		}
	} else {
		set virtual_pos [expr ${pos} - [FirstNonOption ${line}]]
		switch -- ${virtual_pos} {
			0 { return [DisplayHints <string>] }
			1 { return [DisplayHints <pattern>] }
			2 { return [DisplayHints <body>] }
			default { 
				switch [expr $virtual_pos % 2] {
					0 { return [DisplayHints ?body?] }
					1 { return [DisplayHints ?pattern?] }
				}
			}
		}
	}
	return ""
}

# --- TCLREADLINE PACKAGE ---

# create a tclreadline namespace inside
# tclreadline and import some commands.
#
namespace eval tclreadline {
	catch {
		namespace import \
		::tclreadline::DisplayHints \
		::tclreadline::CompleteFromList \
		::tclreadline::Lindex
	}
}

proc tclreadline::complete(readline) {text start end line pos mod} {
	set cmd [Lindex $line 1]
	switch -- $pos {
		1 { return [CompleteFromList ${text} {
			read initialize write add complete
			customcompleter builtincompleter eofchar}]
		}
		2 {
			switch -- $cmd {
				read {}
				initialize {}
				write {}
				add { return [DisplayHints <completerLine>] }
				completer { return [DisplayHints <line>] }
				customcompleter { return [DisplayHints ?scriptCompleter?] }
				builtincompleter { return [DisplayHints ?boolean?] }
				eofchar { return [DisplayHints ?script?] }
			}
		}
	}
	return ""
}

# --- END OF TCLREADLINE PACKAGE ---

proc complete(tell) {text start end line pos mod} {
	switch -- $pos {
		1 { return [ChannelId ${text}] }
	}
	return ""
}

proc complete(time) {text start end line pos mod} {
	switch -- $pos {
		1 { return [DisplayHints <script>] }
		2 { return [DisplayHints ?count?] }
	}
	return ""
}

proc complete(trace) {text start end line pos mod} {
	set cmd [Lindex ${line} 1]
	switch -- $pos {
		1 {
			return [CompleteFromList ${mod} {variable vdelete vinfo}]
		}
		2 {
			return [CompleteFromList ${text} \
			[uplevel [info level] info vars "${mod}*"]]
		}
		3 {
			switch -- $cmd {
				variable -
				vdelete { return [CompleteFromList ${text} {r w u}] }
			}
		}
		4 {
			switch -- $cmd {
				variable -
				vdelete {
					return [CompleteFromList $text [CommandCompletion $text]]
				}
			}
		}
	}
	return ""
}

proc complete(unknown) {text start end line pos mod} {
	switch -- $pos {
		1 {
			return [CompleteFromList $text [CommandCompletion $text]]
		}
		default { return [DisplayHints ?arg?] }
	}
	return ""
}

proc complete(unset) {text start end line pos mod} {
	return [VarCompletion ${text}]
}

proc complete(update) {text start end line pos mod} {
	switch -- $pos {
		1 { return idletasks }
	}
	return ""
}

proc complete(uplevel) {text start end line pos mod} {
	set one [Lindex ${line} 1]
	switch -- $pos {
		1 {
			return [CompleteFromList $text "?level? [CommandCompletion $text]"]
		}
		2 {
			if {"#" == [string index $one 0] || [regexp {^[0-9]*$} $one]} {
				return [CompleteFromList $text [CommandCompletion $text]]
			} else {
				return [DisplayHints ?arg?]
			}
		}
		default { return [DisplayHints ?arg?] }
	}
	return ""
}

proc complete(upvar) {text start end line pos mod} {
	set one [Lindex ${line} 1]
	switch -- $pos {
		1 {
			return [DisplayHints {?level? <otherVar>}]
		}
		2 {
			if {"#" == [string index $one 0] || [regexp {^[0-9]*$} $one]} {
				return [DisplayHints <otherVar>]
			} else {
				return [DisplayHints <myVar>]
			}
		}
		3 {
			if {"#" == [string index $one 0] || [regexp {^[0-9]*$} $one]} {
				return [DisplayHints <myVar>]
			} else {
				return [DisplayHints ?otherVar?]
			}
		}
		default {
			set virtual_pos $pos
			if {"#" == [string index $one 0] || [regexp {^[0-9]*$} $one]} {
				incr virtual_pos
			}
			switch [expr $virtual_pos % 2] {
				0 { return [DisplayHints ?myVar?] }
				1 { return [DisplayHints ?otherVar?] }
			}
		}
	}
	return ""
}

proc complete(variable) {text start end line pos mod} {
	set modulo [expr $pos % 2]
	switch -- $modulo {
		1 { return [VarCompletion ${text}] }
		0 {
			if {$text == "" || $text == "\"" || $text == "\{"} {
				set line [QuoteQuotes $line]
				if {[catch [list set value [list [uplevel [info level] \
					set [PreviousWord $start $line]]]] msg]
				} {
					return ""
				} else {
					return [Quote $value ${text}]
				}
			}
		}
	}
	return ""
}

proc complete(vwait) {text start end line pos mod} {
	switch -- $pos {
		1 { return [VarCompletion ${mod}] }
	}
	return ""
}

proc complete(while) {text start end line pos mod} {
	switch -- $pos {
		1 -
		2 {
			return [BraceOrCommand $text $start $end $line $pos $mod]
		}
	}
	return ""
}

# -------------------------------------
#                  TK
# -------------------------------------

# GENERIC WIDGET CONFIGURATION

proc WidgetChildren {{pattern .}} {
	regsub {^([^\.])} ${pattern} {\.\1} pattern
	if {![string length ${pattern}]} {
		set pattern .
	}
	if {[winfo exists ${pattern}]} {
		return [concat ${pattern} [winfo children ${pattern}]]
	} else {
		regsub {.[^.]*$} $pattern {} pattern
		if {[winfo exists ${pattern}]} {
			return [concat ${pattern} [winfo children ${pattern}]]
		} else {
			return ""
		}
	}
}

proc WidgetDescendants {{pattern .}} {
	set tree [WidgetChildren ${pattern}]
	foreach widget $tree {
		append tree " [WidgetDescendants $widget]"
	}
	return $tree
}

proc ToplevelWindows {} {
	set children [WidgetChildren ""]
	set toplevels ""
	foreach widget $children {
		set toplevel [winfo toplevel $widget]
		if {-1 == [lsearch $toplevels $toplevel]} {
			lappend toplevels $toplevel
		}
	}
	return $toplevels
}

#**
# try to get options for commands which
# allow `configure' (cget).
# @param command.
# @param optionsT where the table will be stored.
# @return number of options
# @date Sep-14-1999
#
proc OptionTable {cmd optionsT} {
	upvar $optionsT options
	# first we build an option table
	#
	if {[catch [list set option_table [eval ${cmd}]] msg]} {
		return 0
	}
	foreach optline ${option_table} {
		if {5 != [llength ${optline}]} continue else {
			lappend options(switches) [lindex ${optline} 0]
			lappend options(value)    [lindex ${optline} 4]
		}
	}
	return [llength ${option_table}]
}

#**
# try to complete a `cmd configure|cget ..' from the command's options.
# @param text start line cmd, standard tclreadlineCompleter arguments.
# @return a tclreadline completer formatted string.
# @date Sep-14-1999
#
proc CompleteFromOptions {text start line} {

	# check if either `configure' or `cget' is present.
	#
	set lst [ProperList ${line}]
	foreach keyword {configure cget} {
		set idx [lsearch ${lst} ${keyword}]
		if {-1 != ${idx}} {
			break
		}
	}
	if {-1 == ${idx}} {
		return
	}
	set cmd [lrange ${lst} 0 ${idx}]
	# puts stderr cmd=|$cmd|
	if {0 < [OptionTable ${cmd} options]} {

		set prev [PreviousWord ${start} ${line}]
		if {-1 != [set found [lsearch -exact $options(switches) ${prev}]]} {

			# complete only if the user has not
			# already entered something here.
			#
			if {![llength ${text}]} {
				# use this double list to quote option
				# values which have to be quoted.
				#
				return [list [list [lindex $options(value) ${found}]]]
			}

		} else {
			return [CompleteFromList ${text} \
			[RemoveUsedOptions ${line} $options(switches)]]
		}
	}
	return ""
}

proc CompleteFromOptionsOrSubCmds {text start end line pos} {
	set from_opts [CompleteFromOptions ${text} ${start} ${line}]
	if {[string length ${from_opts}]} {
		return ${from_opts}
	} else {
		set cmds [TrySubCmds [lrange [ProperList ${line}] 0 [expr $pos - 1]]]
		# puts stderr cmds=|$cmds|
		if {[llength ${cmds}]} {
			return [TryFromList ${text} ${cmds}]
		}
	}
	return ""
}

proc complete(WIDGET) {text start end line pos mod} {
	# set widget [Lindex ${line} 0]
	# set cmds [TrySubCmds ${widget}]
	# if {[llength ${cmds}]} {
	# 	return [TryFromList ${mod} ${cmds}]
	# }
	return [CompleteFromOptionsOrSubCmds ${text} ${start} ${end} ${line} ${pos}]
}

# SPECIFIC TK COMMAND COMPLETERS

proc complete(bell) {text start end line pos mod} {
	switch -- ${pos} {
		1 { return [CompleteFromList ${text} -displayof] }
		2 {
			if {"-displayof" == [PreviousWord ${start} ${line}]} {
				return [CompleteFromList ${text} [ToplevelWindows]]
			}
		}
	}
}

proc EventuallyInsertLeadingDot {text fallback} {
	if {![string length ${text}]} {
		return [list . {}]
	} else {
		return [DisplayHints $fallback]
	}
}

#**
# TODO: shit. make this better!
# @param text, a std completer argument (current word).
# @param fullpart, the full text of the current position.
# @param lst, the list to complete from.
# @param pre, leading `quote'.
# @param sep, word separator.
# @param post, trailing `quote'.
# @return a formatted completer string.
# @date Sep-15-1999
#
proc CompleteListFromList {text fullpart lst pre sep post} {

	# puts stderr ""
	# puts stderr text=|$text|
	# puts stderr lst=|$lst|
	# puts stderr pre=|$pre|
	# puts stderr sep=|$sep|
	# puts stderr post=|$post|

	if {![string length ${fullpart}]} {

		# nothing typed so far. Insert a $pre
		# and inhibit further completion.
		#
		return [list ${pre} {}]

	} elseif {[regexp ${post} ${text}]} {

		# finalize, append the post and a space.
		#
		set diff \
		[expr [CountChar ${fullpart} ${pre}] - [CountChar ${fullpart} ${post}]]
		for {set i 0} {${i} < ${diff}} {incr i} {
			append text ${post}
		}
		append text " "
		return ${text}

	} elseif {![regexp -- ^\(.*\[${pre}${sep}\]\)\(\[^${pre}${sep}\]*\)$ \
		${text} all left right]
	} {
		set left {}
		set right ${text}
	}

	# TraceVar left
	# TraceVar right

	# puts stderr \nleft=|$left|
	# puts stderr \nright=|$right|
	set exact_matches [MatchesFromList ${right} ${lst}]
	# TODO this is awkward. Think of making it better!
	#
	if {1 == [llength ${exact_matches}] && -1 != [lsearch ${lst} ${right}]
	} {
		#set completion [CompleteFromList ${right} [list ${sep} ${post}] 1]
		return [list ${left}${right}${sep} {}]
	} else {
		set completion [CompleteFromList ${right} ${lst} 1]
	}
	# puts stderr \ncompletion=|$completion|
	if {![string length [lindex $completion 0]]} {
		return [concat [list ${left}] [lrange $completion 1 end]]
	} elseif {[string length ${left}]} {
		return [list ${left}]${completion}
	} else {
		return ${completion}
	}
	return ""
}

proc complete(bind) {text start end line pos mod} {
	switch -- ${pos} {
		1 {
			set widgets [WidgetChildren ${text}]
			set toplevels [ToplevelWindows]
			if {[catch {set toplevelClass [winfo class .]}]} {
				set toplevelClass ""
			}
			set rest {
				Button Canvas Checkbutton Entry Frame Label
				Listbox Menu Menubutton Message Radiobutton
				Scale Scrollbar Text
				all
			}
			return [CompleteFromList ${text} \
			[concat ${toplevels} ${widgets} ${toplevelClass} $rest]]
		}
		2 {
			set modifiers {
				Alt Control Shift Lock Double Triple
				B1 B2 B3 B4 B5 Button1 Button2 Button3 Button4 Button5
				M M1 M2 M3 M4 M5        
				Meta Mod1 Mod2 Mod3 Mod4 Mod5
			}
			set events {
				Activate Button ButtonPress ButtonRelease
				Circulate Colormap Configure Deactivate Destroy
				Enter Expose FocusIn FocusOut Gravity
				Key KeyPress KeyRelease Leave Map Motion
				MouseWheel Property Reparent Unmap Visibility
			}
			set sequence [concat ${modifiers} ${events}]
			return [CompleteListFromList ${text} \
			[Lindex $line 2] ${sequence} < - >]
		}
		3 {
			# return [DisplayHints {<script> <+script>}]
			return [BraceOrCommand ${text} \
			${start} ${end} ${line} ${pos} ${mod}]
		}
	}
	return ""
}

proc complete(bindtags) {text start end line pos mod} {
	switch -- ${pos} {
		1 { return [CompleteFromList ${text} [WidgetChildren ${text}]] }
		2 {
			return [CompleteListFromList ${text} [Lindex ${line} 2] \
			[bindtags [Lindex ${line} 1]] \{ { } \}]
		}
	}
	return ""
}

proc CompleteWidgetConfigurations {text start line lst} {
	prev [PreviousWord ${start} ${line}]
}

proc complete(button) {text start end line pos mod} {
	switch -- ${pos} {
		1 { return [EventuallyInsertLeadingDot ${text} <pathName>] }
		default {
			return [CompleteWidgetConfigurations ${text} {
			}]
		}
	}
	return ""
}

proc complete(image) {text start end line pos mod} {
	set sub [Lindex ${line} 1]
	switch -- ${pos} {
		1 { return [CompleteFromList ${text} [TrySubCmds image]] }
		2 {
			switch -- ${sub} {
				create { return [CompleteFromList ${text} [image types]] }
				delete -
				height -
				type -
				width { return [CompleteFromList ${text} [image names]] }
				names {}
				types {}
			}
		}
		3 {
			switch -- ${sub} {
				create {
					set type [Lindex ${line} 2]
					switch -- ${type} {
						bitmap {
							return [CompleteFromList ${text} {
								?name? -background -data -file
								-foreground -maskdata -maskfile
							}]
						}
						photo {
							# TODO
						}
						default {}
					}
				}
				default {}
			}
		}
		default {
			switch -- ${sub} {
				create {
					set type [Lindex ${line} 2]
					set prev [PreviousWord ${start} ${line}]
					# puts stderr prev=$prev
					switch -- ${type} {
						bitmap {
							switch -- ${prev} {
								-background -
								-foreground { return [DisplayHints <color>] }
								-data -
								-maskdata { return [DisplayHints <string>] }
								-file -
								-maskfile { return "" }
								default {
									return [CompleteFromList ${text} \
									[RemoveUsedOptions ${line} {
										-background -data -file
										-foreground -maskdata -maskfile
									}]]
								}
							}
						}
						photo {
							# TODO
						}
					}
				}
			}
		}
	}
}

proc complete(winfo) {text start end line pos mod} {
	set cmd [lindex ${line} 1]
	switch -- ${pos} {
		1 {
			set cmds [TrySubCmds winfo]
			if {[llength ${cmds}]} {
				return [TryFromList ${text} ${cmds}]
			}
		}
		2 {
			return [TryFromList ${text} [WidgetChildren ${text}]]
		}
	}
	return ""
}

}; # namespace tclreadline
