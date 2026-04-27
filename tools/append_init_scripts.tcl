set here [file dirname [file normalize [info script]]]
lassign $argv out in files
set l {}
foreach e $files {
    lappend l [file join $here .. $e] [file tail $e]
}
# Can't use $out for both in and out - produces a corrupt image
zipfs lmkimg zipfs_$out $l pw $in
file rename -force zipfs_$out $out
