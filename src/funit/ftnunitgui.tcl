# ftnunitgui.tcl --
#     Straightforward GUI for Ftnunit:
#     - Select and run tests
#
#     TODO:
#     Properly detect failure in test by examining
#     the contents of ftnunit.lst. Current heuristic
#     seems to do fine though.
#
#
package require Tk 8.5

# interrogateExecutable --
#     Ask the executable to provide a list of test cases
#
# Arguments:
#     executable     Name of the executable
#
# Returns:
#     Nothing
#
# Side effects:
#     Test cases listed
#
proc interrogateExecutable {executable} {
    global tree
    global output
    global items

    if { [info exists items] } {
        foreach item $items {
            $tree delete $item
        }
        $output delete 1.0 end
    }

    set outfile [open "ftnunit.run" w]
    puts $outfile "LIST"
    close $outfile

    if { [catch {
        set cases [exec $executable]
    } msg] } {
        tk_messageBox -icon error -message "Error retrieving test cases:\n$msg" -type ok
        return
    }

    set columnWidth 0
    set treeFont [::ttk::style lookup TTreeview -font]

    set items {}
    set allCases [$tree insert {} end -text "All cases" -image $::notrun]
    lappend items $allCases
    foreach case [split $cases \n] {
        lappend items [$tree insert $allCases end -text $case -image $::notrun]
        set textWidth [font measure $treeFont $case]
        if { $textWidth > $columnWidth } {
            set columnWidth $textWidth
        }
    }
    $tree column #0 -width [expr {70+$columnWidth}]
}


# setTestSelection --
#     Store the selected test case
#
# Arguments:
#     x         X-coordinate of mouse click
#     y         Y-coordinate
#
# Returns:
#     Nothing
#
# Side effects:
#     Variable selected set
#
proc setTestSelection {x y} {
    global selected
    global tree

    set selected [$tree selection]
}


# runSelectedTest --
#     Run the selected test and catch the output
#
# Arguments:
#     None
#
# Returns:
#     Nothing
#
# Side effects:
#     Output window filled with output from test
#
proc runSelectedTest {} {
    global tree
    global output
    global selected
    global executable
    global items

    if { ! [info exists selected] } {
        return
    }
    if { $selected != [lindex $items 0] } {
        file delete -force "ftnunit.lst"
        set   outfile [open "ftnunit.run" w]
        puts  $outfile [expr {[string trimleft [string range [$tree selection] 1 end] 0] - 1}]
        close $outfile

        $output delete 1.0 end
        $output insert end "Test case: [$tree item $selected -text]\n\n" title

        if { [catch {
            set result [exec $executable]
            set img    $::okay
            if { [string match "*ASSERTION FAILED*" $result] } {
                set result [string map {"ASSERTION FAILED\n" ""} $result]
                set img    $::failed
            }
            $output insert end $result
            $tree item $selected -image $img
        } msg] } {
            $output insert end "Error: " error "\n$msg"
            $tree item $selected -image $::failed
        }
    } else {
        $output delete 1.0 end
        set success 1

        foreach item [lrange $items 1 end] {
            file delete -force "ftnunit.lst"
            set   outfile [open "ftnunit.run" w]
            puts  $outfile [expr {[string trimleft [string range $item 1 end] 0] - 1}]
            close $outfile

            $output insert end "Test case: [$tree item $item -text]\n\n" title

            if { [catch {
                set result [exec $executable]
                set img    $::okay
                if { [string match "*ASSERTION FAILED*" $result] } {
                    set result [string map {"ASSERTION FAILED" ""} $result]
                    set img    $::failed
                }
                $tree item $item -image $img
                $output insert end $result
            } msg] } {
                $output insert end "Error: " error "\n$msg"
                $tree item $item -image $::failed
                set success 0
            }
            $output insert end "\n\n"
        }

        if { $success } {
            $tree item [lindex $items 0] -image $::okay
        } else {
            $tree item [lindex $items 0] -image $::failed
        }
    }
}


# selectExecutable --
#     Select the executable to be tested
#
# Arguments:
#     name                Name of the executable file (optional)
#
# Returns:
#     Nothing
#
# Side effects:
#     Test program is interrogated and the list of tests is filled
#
proc selectExecutable {{name {}}} {
    global tree
    global executable

    if { $name eq "" } {
        if { $::tcl_platform(platform) eq "windows" } {
            set newname [tk_getOpenFile -title "Program to test" -filetypes {{{Executables} {.exe}}}]
        } else {
            set newname [tk_getOpenFile -title "Program to test" -filetypes {{{Executables} {*}}}]
        }
        if { $newname != "" } {
            set executable $newname
        }
    }

    if { $executable ne "" } {
        wm title . "Ftnunit: [file tail $executable]"
        interrogateExecutable $executable
    } else {
        wm title . "Ftnunit: -- select program to test --"
    }
}


# setupWindow --
#     Set up the main window
#
# Arguments:
#     None
#
# Returns:
#     Nothing
#
# Side effects:
#     Main window with treeview widget and output window
#
proc setupWindow {} {
    global tree
    global output

    set left       [::ttk::frame    .left]
    set right      [::ttk::frame    .right]
    set tree       [::ttk::treeview .left.tree -xscrollcommand [list .left.xtreescroll set] -yscrollcommand [list .left.ytreescroll set] -selectmode browse]
    set output     [text .right.output -xscrollcommand [list .right.xoutputscroll set] -yscrollcommand [list .right.youtputscroll set]]

    set xtreescroll [::ttk::scrollbar .left.xtreescroll    -command [list .left.tree xview]    -orient horizontal]
    set ytreescroll [::ttk::scrollbar .left.ytreescroll    -command [list .left.tree yview]    -orient vertical]
    set xoutput     [::ttk::scrollbar .right.xoutputscroll -command [list .right.output xview] -orient horizontal]
    set youtput     [::ttk::scrollbar .right.youtputscroll -command [list .right.output yview] -orient vertical]

    set buttonrow    [::ttk::frame     .right.buttons]
    set runbutton    [::ttk::button    .right.buttons.run   -text Run   -command [list runSelectedTest]]
    set clearbutton  [::ttk::button    .right.buttons.clear -text Clear -command [list $output delete 1.0 end]]

    grid $tree        $ytreescroll -sticky news
    grid $xtreescroll              -sticky news
    grid $output      $youtput     -sticky news
    grid $xoutput                  -sticky news

    grid $runbutton $clearbutton   -sticky news
    grid $buttonrow                -sticky news

    grid $left $right              -sticky news
    grid rowconfigure    $left  0 -weight 1
    grid columnconfigure $left  1 -weight 1
    grid rowconfigure    $right 0 -weight 1
    grid columnconfigure $right 0 -weight 1

    grid rowconfigure    .      0 -weight 1
    grid columnconfigure .      1 -weight 1

    set data_failed 47494638376114001400910000000000ff0000ffffffffffff2c0000000014001400000247848fa911e20f1f0b66c63b29c8575a237098258021e9318f79aa8ee8b06d03c74094a1eb8dd7fba8fb41722e888ce363c16a261f0ce47b912ac1e16753eccca8da9e4601561400003b
    set data_okay 47494638376114001400910000000000ffffff00ff00ffffff2c0000000014001400000244848fa9c1ed2ea230af3619a8ad58efdb011f1431de8865e2184882b9a225ccbacd69a5b4fcdeeb4c02aa5a3a467148141e3db664cf177c1d9120d713fab05e618aaea200003b
    set data_notrun 47494638376114001400910000000000ffffffbebebeffffff2c0000000014001400000241848fa9c1eddf0c9c4fd6046d14bc83eb00dd287c911988e4885aeaea9dcc0b73a65bb3f39dc729df2bed66c156ab670cd550bfca8ad9bc2032500ac84a5168b705003b

    set data_failed  [binary format H* $data_failed]
    set data_okay    [binary format H* $data_okay  ]
    set data_notrun  [binary format H* $data_notrun]

    set ::okay   [image create photo okay   -format gif -data $data_okay  ]
    set ::failed [image create photo failed -format gif -data $data_failed]
    set ::notrun [image create photo notrun -format gif -data $data_notrun]

    $tree heading #0 -text "Test cases"

    bind $tree <<TreeviewSelect>> [list setTestSelection %X %Y]

    $output configure -font [::ttk::style lookup TTreeview -font]
    $output tag configure title -font       "Helvetica 12 bold"
    $output tag configure error -foreground red

    menu .menu -tearoff 0

    menu .menu.file -tearoff 0
    .menu      add cascade -label File -menu .menu.file -underline 0
    .menu.file add command -label "Open ..." -command [list selectExecutable]
    .menu.file add separator
    .menu.file add command -label "Exit" -command exit

    . configure -menu .menu
    wm title . "Ftnunit: -- select program to test --"
}


# main --
#     Set up the main window and get the thing going
#
setupWindow

if { [llength $argv] > 0 } {
    set executable [lindex $argv 0]
    selectExecutable $executable
}
