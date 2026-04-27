package require tcltest

# Test configuration options that may be set are:
# (currently none)

tcltest::configure -testdir [file dirname [file normalize [info script]]]
# Avoid running temp Emacs and others
tcltest::configure -notfile "*#*"

if {[info exists env(TEMP)]} {
    tcltest::configure -tmpdir $::env(TEMP)/cffi-test/[clock seconds]
} else {
    if {[file exists /tmp] && [file isdirectory /tmp]} {
	tcltest::configure -tmpdir /tmp/cffi-test/[clock seconds]
    } else {
	error "Unable to figure out TEMP directory. Please set the TEMP env var"
    }
}

tcltest::configure {*}$argv

# ERROR_ON_FAILURES for github actions
set ErrorOnFailures [info exists env(ERROR_ON_FAILURES)]
# NOTE: Do NOT unset ERROR_ON_FAILURES if recursing to subdirectories
unset -nocomplain env(ERROR_ON_FAILURES)
if {[tcltest::runAllTests] && $ErrorOnFailures} {exit 1}

# if calling direct only (avoid rewrite exit if inlined or interactive):
if { [info exists ::argv0] && [file tail $::argv0] eq [file tail [info script]]
     && !([info exists ::tcl_interactive] && $::tcl_interactive)
 } {
    proc exit args {}
}

