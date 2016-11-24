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
    uplevel #0 spawn tclsh
    send "package require tclreadline\r"
    expect -exact $::prompt
}

proc cleanup {} {
    send \rexit\r

    if {![file exists $::tclshrc] && [file exists $::tclshrc.renamed]} {
        file rename $::tclshrc.renamed ${::tclshrc}
    }
}

tcltest::test basic-1.1 {require tclreadline} \
        -setup setup \
        -cleanup cleanup \
        -body {}

tcltest::test basic-1.2 {::tclreadline::Loop} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send "::tclreadline::Loop\r"
    expect -exact \]
}

tcltest::test namespace-1.1 {namespace path completion} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send "::tclreadline::Loop\r"
    expect -exact \]
    send "namespace eval ::foo {}\rnamespace eval ::foo::bar {}\r"
    expect -exact \]
    send "namespace eval ::foo\t"
    expect -exact "::bar"
}

tcltest::test variable-1.1 {variable namespace path completion} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send "::tclreadline::Loop\r"
    expect -exact \]
    send "namespace eval ::bar {}\rset ::bar::baz value1\r"
    expect -exact \]
    send "set ::bar\t"
    expect -exact "::baz"
}

tcltest::test variable-1.2 {partial variable name completion after '$'} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send "::tclreadline::Loop\r"
    expect -exact \]
    send "set foo 5\r"
    expect -exact \]
    send "lindex \$f\t"
    expect -exact "foo"
    send \r
    expect 5
}

tcltest::test variable-1.3 {empty variable name completion after '$'} \
        -setup setup \
        -cleanup cleanup \
        -body {
    send "::tclreadline::Loop\r"
    expect -exact \]
    send "set bar 5\r"
    expect -exact \]
    send "puts \$\t"
    sleep 0.2
    send \t
    expect -exact "bar"
}

# Exit with a nonzero status if there are failed tests.
set failed [expr {$tcltest::numTests(Failed) > 0}]

tcltest::cleanupTests
if {$failed} {
    exit 1
}
