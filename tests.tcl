#! /usr/bin/env tclsh
# -*- tclsh -*-
# FILE: tests.tcl
# $Id$
# ---
# tclreadline -- gnu readline for tcl
# http://www.zellner.org/tclreadline/
# Copyright (c) 1998 - 2014, Johannes Zellner <johannes@zellner.org>
# Copyright (c) 2016, dbohdan <dbohdan@dbohdan.com>
# This software is copyright under the BSD license.
# ---

package require Expect
package require tcltest

::tcltest::configure {*}$::argv

::tcltest::testConstraint itcl3 [expr {
    ![catch { package require Itcl 3.0 }]
}]
::tcltest::testConstraint itcl4 [expr {
    ![catch { package require itcl 4.0 }]
}]
::tcltest::testConstraint itcl [expr {
    [::tcltest::testConstraint itcl3] || [::tcltest::testConstraint itcl4]
}]
::tcltest::testConstraint tcloo [expr {
    ![catch { package require TclOO 1.0 }]
}]

set prompt {% }
set tclshrc [file join $::env(HOME) .tclshrc]

proc setup {} {
    match_max 100000
    set ::timeout 2
    expect_before {
        timeout {
            error {timed out}
        }
        invalid {
            error {invalid command}
        }
    }

    if {[file exists $::tclshrc]} {
        file rename $::tclshrc ${::tclshrc}.renamed
    }

    trap {
        send \x03
        cleanup
        exit 1
    } {SIGINT SIGHUP}

    puts \n----
    uplevel #0 spawn [info nameofexecutable]
    send "package require tclreadline\r"
    expect -exact $::prompt
    send "::tclreadline::Loop\r"
    expect -exact \]
}

proc cleanup {} {
    send \rexit\r

    if {![file exists $::tclshrc] && [file exists $::tclshrc.renamed]} {
        file rename $::tclshrc.renamed ${::tclshrc}
    }
}

proc send_nl args {
    send [join $args \r]\r
}

tcltest::test basic-1.1 {::tclreadline::Loop} \
        -setup setup \
        -cleanup cleanup \
        -body {}

tcltest::test namespace-1.1 {namespace path completion} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl \
            {namespace eval ::foo {}} \
            {namespace eval ::foo::bar {}}
    expect -exact \]
    send "namespace eval ::foo\t"
    expect -exact ::bar
}

tcltest::test variable-1.1 {variable namespace path completion} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl \
            {namespace eval ::bar {}} \
            {set ::bar::baz value1}
    expect -exact \]
    send "set ::bar\t"
    expect -exact ::baz
}

tcltest::test variable-1.2 {partial variable name completion after '$'} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl {set foo 5}
    expect -exact \]
    send "lindex \$f\t"
    expect -exact foo
    send \r
    expect 5
}

tcltest::test variable-1.3 {empty variable name completion after '$'} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl {set bar 5}
    expect -exact \]
    send "puts \$\t"
    sleep 0.2
    send \t
    expect -exact bar
}

tcltest::test itcl-1.1 {itcl method name completion} \
        -constraints itcl \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl {
        # Prefer [incr Tcl] 4.
        if {[catch {package require itcl}]} {
            package require Itcl
        }
    }
    send_nl {
        ::itcl::class cls {
            public variable opt
            method aleph {} {}
            method beth {} {}
        }
    }
    expect -exact \]
    send_nl {cls obj}
    expect -exact \]
    send "obj a\t"
    sleep 0.2
    send \t
    expect -exact aleph
}

tcltest::test itcl-2.1 {itcl method argument hints} \
        -constraints itcl \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl {
        # Prefer [incr Tcl] 4.
        if {[catch {package require itcl}]} {
            package require Itcl
        }
    }
    send_nl {
        ::itcl::class cls {
            method foo {bar baz quux asdf} {}
        }
    }
    expect -exact \]
    send_nl {cls obj}
    expect -exact \]
    send "obj foo \t"
    sleep 0.2
    send \t
    expect -exact <bar>
    send "arg1 \t"
    sleep 0.2
    send \t
    expect -exact <baz>
    send "arg2 \t"
    sleep 0.2
    send \t
    expect -exact <quux>
    send "arg3 \t"
    sleep 0.2
    send \t
    expect -exact <asdf>
}

tcltest::test itcl-3.1 {itcl option name completion for cget and configure} \
        -constraints itcl \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl {
        # Prefer [incr Tcl] 4.
        if {[catch {package require itcl}]} {
            package require Itcl
        }
    }
    send_nl {
        ::itcl::class cls {
            public variable opt
            method aleph {} {}
            method beth {} {}
        }
    }
    expect -exact \]
    send_nl {cls obj}
    expect -exact \]
    send "obj cget -o\t"
    sleep 0.2
    send \t
    expect -exact -opt
    send \r
    send "obj configure -o\t"
    expect -exact -opt
}

tcltest::test itcl-4.1 {itcl wrong method name} \
        -constraints itcl \
        -setup setup \
        -cleanup cleanup \
        -body {
    set ::timeout 1
    send_nl {
        # Prefer [incr Tcl] 4.
        if {[catch {package require itcl}]} {
            package require Itcl
        }
    }
    send_nl {
        ::itcl::class cls {
            method foo {} {}
        }
    }
    expect -exact \]
    send_nl {cls obj}
    expect -exact \]
    send "obj nope \t"
    expect {
        {error during evaluation} {
            error {error during evaluation}
        }
    }
} -returnCodes 1 -result {timed out}

tcltest::test tcloo-1.1 {TclOO method name completion} \
        -constraints tcloo \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl {package require TclOO}
    send_nl {::oo::class create cls { method bar {} {}; method baz {} {} }}
    expect -exact \]
    send_nl {set quux [cls new]}
    expect -exact \]
    send "\$quux \t"
    sleep 0.2
    send \t\r
    expect -glob "*bar*baz*"
}

tcltest::test tcloo-2.1 {TclOO method argument hints} \
        -constraints tcloo \
        -setup setup \
        -cleanup cleanup \
        -body {
    send_nl {
        ::oo::class create ::cls {
            method foo {bar baz quux asdf} {}
        }
    }
    expect -exact \]
    send_nl {set obj [::cls new]}
    expect -exact \]
    send "\$obj foo \t"
    sleep 0.2
    send \t
    expect -exact <bar>
    send "arg1 \t"
    sleep 0.2
    send \t
    expect -exact <baz>
    send "arg2 \t"
    sleep 0.2
    send \t
    expect -exact <quux>
    send "arg3 \t"
    sleep 0.2
    send \t
    expect -exact <asdf>
}

tcltest::test tcloo-3.1 {TclOO wrong method name} \
        -constraints tcloo \
        -setup setup \
        -cleanup cleanup \
        -body {
    set ::timeout 1
    send_nl {
        ::oo::class create cls {
            method foo {} {}
        }
    }
    expect -exact \]
    send_nl {set obj [cls new]}
    expect -exact \]
    send "\$obj nope \t"
    sleep 0.2
    send \t

    expect {
        {error during evaluation} {
            error {error during evaluation}
        }
    }
} -returnCodes 1 -result {timed out}

# Exit with a nonzero status if there are failed tests.
set failed [expr {$tcltest::numTests(Failed) > 0}]

tcltest::cleanupTests
if {$failed} {
    exit 1
}
