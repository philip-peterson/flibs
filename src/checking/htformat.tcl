# htformat.tcl --
#    Turn Fortran code into HTML-files:
#    - mark links to modules, subroutines and types
#    - doing the same for functions is tough!
#    - highlight comments and headers
#

# fileContents --
#     Get the contents of the source file, first preprocessing step
#
# Arguments:
#     filename        Name of the file to read
#
# Result:
#     Contents after replacing &, < and > by their HTML encoding
#
proc fileContents {filename} {

    set infile [open $filename]
    set contents [string map {< &lt; > &gt; & &amp;} [read $infile]]
    close $infile

    return $contents
}

# convertToHtml --
#     Convert the contents of a source file to HTML-format
#
# Arguments:
#     filename        Name of the source file to read
#
# Result:
#     None
#
# Side effects:
#     A new file is written with the same name but extension .html.
#     This contains the source code marked with HTML formats.
#
proc convertToHtml {filename} {

    set contents [fileContents $filename]

    set outfile [open "[file rootname $filename].html" w]

    puts $outfile "<html>
<head>
    <title>$filename</title>
    <link rel=\"stylesheet\" type=\"text/css\" href=\"fortran.css\">
</head>
<body>"

    set comment -1
    set routine  0
    foreach line [split $contents \n] {
        switch -re $line {
            {^ *!} {
                if { $comment == -1 } {
                    set comment 1
                    puts $outfile "<pre class=\"comment\">"
                }
                if { $comment == 0 } {
                    set comment 1
                    puts $outfile "</pre>"
                    puts $outfile "<pre class=\"comment\">"
                }
                puts $outfile $line
            }
            {^ *program} -
            {^ *subroutine} -
            {^ *module}     -
            { function [a-zA-Z0-9_]+ *\(} {
                if { $comment == 1 || $code == 1 } {
                    set comment 0
                    puts $outfile "</pre>"
                }
                set routine 1
                set contin  0
                set code    0
                puts $outfile "<pre class=\"routine\">"
                regsub {(program +|subroutine +|module +|function +)([a-zA-Z0-9_]+)} \
                    $line {\1<a name="\2">\2</a>} line
                puts $outfile $line
                if { [regexp {& *$} $line] } {
                    set contin 1
                } else {
                    puts $outfile "</pre>"
                }
            }
            {(^|[ )])call } -
            {(^ *)use }     {
                if { $comment == 1 } {
                    set comment 0
                    puts $outfile "</pre>"
                    puts $outfile "<pre class=\"code\">"
                }
                if { $code == 0 } {
                    puts $outfile "<pre class=\"code\">"
                }
                set comment 1
                regsub {(call +|use +)([a-zA-Z0-9_]+)} $line {\1<a href="#\2">\2</a>} line
                puts $outfile $line
            }
            default {
                if { $comment == 1 } {
                    set comment 0
                    puts $outfile "</pre>"
                    puts $outfile "<pre class=\"code\">"
                }
                if { $routine == 1 } {
                    if { $contin == 1 } {
                        if { ! [regexp {& *$} $line] } {
                            set contin 0
                            puts $outfile $line
                            puts $outfile "</pre>"
                            puts $outfile "<pre class=\"code\">"
                            continue
                        }
                    } else {
                        set routine 0
                        puts $outfile "<pre class=\"code\">"
                    }
                }
                set code 1
                puts $outfile "$line"
            }
        }
    }

    if { $comment == 1 } {
        set comment 0
        puts $outfile "</pre>"
    }
    puts $outfile "</body>"
    puts $outfile "</html>"
}

# main --
#     Test

convertToHtml test_w.f90
