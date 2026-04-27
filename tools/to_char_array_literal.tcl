set source_name [lindex $argv 0]
chan configure stdin -translation binary
set count -1
puts "const char embed_[string map {. _} [file tail $source_name]]\[\] = {[join [lmap c [regexp -all -inline .. [binary encode hex [read stdin]]] {
    format %s%d \
        [if {[incr count]%18==0} {return -level 0 \n\t}] \
        0x$c
}] {,}],0\n};"
#は
