# images_gui.tcl --
#     Create the images for the Ftnunit GUI
#
package require Img

pack [canvas .c1 -width 20 -height 20 -highlightthickness 0 -bg white]
.c1 create line      0  0 19 19 -fill red -width 3
.c1 create line      0 19 19  0 -fill red -width 3
.c1 create rectangle 0  0 19 19 -outline black

pack [canvas .c2 -width 20 -height 20 -highlightthickness 0 -bg white]
.c2 create line      3 12 10 19 17  0 -fill green -width 3
.c2 create rectangle 0  0 19 19       -outline black

pack [canvas .c3 -width 20 -height 20 -highlightthickness 0 -bg white]
.c3 create oval      3  3 16 16 -fill gray
.c3 create rectangle 0  0 19 19 -outline black

tkwait visibility .c3
after 1000 {
    set pause 1
}
vwait pause

set outfile [open "image.data" w]

foreach img {failed okay notrun} cnv {.c1 .c2 .c3} {
    set imgcmd [image create photo $img -data $cnv -format window]

    $imgcmd write "$img.gif" -format gif

    set infile [open "$img.gif" r]

    fconfigure $infile -encoding binary
    set data [read $infile]
    close $infile

    set hexstring ""
    binary scan $data H* hexstring
    puts $outfile "set data_$img $hexstring"
}

close $outfile

# Test
if {0} {
    set infile [open "failed.data" r]
    gets $infile data
    close $infile
    set hexdata [binary format H* $data]

    set newimage [image create photo newimage -data $hexdata -format gif]
    pack [canvas .c4 -width 20 -height 20 -highlightthickness 0]
    .c4 create image 0 0 -image $newimage -anchor nw
    .c4 create rectangle 0 0 19 19
}

