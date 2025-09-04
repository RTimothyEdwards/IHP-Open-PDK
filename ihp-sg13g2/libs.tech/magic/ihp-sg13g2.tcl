#-----------------------------------------------------
# Magic/TCL design kit for IHP ihp-sg13g2
#-----------------------------------------------------
# Tim Edwards
# Revision 0.2	ALPHA   8/28/2025
# Currently a work in progress, expected completion
# date of January 2026.
#-----------------------------------------------------
#
# Devices in the model files, with notes about
# implementation as generated devices in magic:
#
# Capacitors:
#
# cap_cmim	(basic MiM cap, L and W, contact options)
# cap_rfcmim	(cmim with RF option; draws guard ring under the device, feed width)
#
# Diodes:
#
# dantenna	(basic n-diode, L and W)
# dpantenna	(basic p-diode, L and W)
#
# Resistors:
#
# rsil		(low-R silicided poly resistor, L and W)
# rppd		(medium-R P+ unsalicided poly resistor, L and W)
# rhigh		(high-R N- unsalicided poly resistor, L and W)
#
# Bipolars:
#
# npn13g2	(bipolar, emitter L and W)
# npn13g2l	(bipolar, emitter L and W)
# npn13g2v	(bipolar, emitter L and W)
# pnpMPA	(bipolar, emitter L and W)
#
# MOSFETs:
#
# sg13_hv_nmos	(mosfet, channel L and W, NF, M, RF option)
# sg13_hv_pmos	(mosfet, channel L and W, NF, M, RF option)
# sg13_lv_nmos	(mosfet, channel L and W, NF, M, RF option)
# sg13_lv_pmos	(mosfet, channel L and W, NF, M, RF option)
#
# Varactors:
#
# sg13_hv_svaricap (varactor, L and W, M)
#
# Fixed-layout devices:
#
# diodevdd_2kv	(fixed layout, array options only)
# diodevdd_4kv	(fixed layout, array options only)
# diodevss_2kv	(fixed layout, array options only)
# diodevss_4kv	(fixed layout, array options only)
# nmoscl_2	(clamp mosfet, fixed layout)
# nmoscl_4	(clamp mosfet, fixed layout)
#
# Miscellaneous:
#
# bondpad	(octagon or square, bond or probe, diameter)
# isobox	(deep nwell area, length and width)
# inductor	(2-terminal, width, spacing, diameter, windings)
# balun		(3-terminal, width, spacing, diameter, windings)
# seal ring	(length and width)

# Devices not part of the device generator:
# cparasitic	(device model for parasitic capacitance)
# Rparasitic	(device model for parasitic resistance)
# npn13g2_5t	(bipolar, emitter L and W, nonphysical 5th terminal)
# npn13g2l_5t	(bipolar, emitter L and W, nonphysical 5th terminal)
# npn13g2v_5t	(bipolar, emitter L and W, nonphysical 5th terminal)
# ptap1		(p-tap contact, treated as resistor device, L and W)
# ntap1		(n-tap contact, treated as resistor device, L and W)

if {[catch {set TECHPATH $env(PDK_ROOT)}]} {
    set TECHPATH /usr/share/pdk
}
if [catch {set PDKPATH}] {set PDKPATH ${TECHPATH}/ihp-sg13g2}
set PDKNAME ihp-sg13g2
# "sg13g2" is the namespace used for all devices
set PDKNAMESPACE sg13g2
puts stdout "Loading ihp-sg13g2 Device Generator Menu ..."

# Initialize toolkit menus to the wrapper window

global Opts
namespace eval sg13g2 {}

# Set the window callback
if [catch {set Opts(callback)}] {set Opts(callback) ""}
set Opts(callback) [subst {sg13g2::addtechmenu \$framename; $Opts(callback)}]

# if {![info exists Opts(cmdentry)]} {set Opts(cmdentry) 1}

# Set options specific to this PDK
set Opts(hidelocked) 1
set Opts(hidespecial) 0

# Wrap the closewrapper procedure so that closing the last
# window is equivalent to quitting.
if {[info commands closewrapper] == "closewrapper"} {
   rename closewrapper closewrapperonly
   proc closewrapper { framename } {
      if {[llength [windownames all]] <= 1} {
         magic::quit
      } else {
         closewrapperonly $framename
      }
   }
}

# Remove maze router layers from the toolbar by locking them
catch {tech lock fence,magnet,rotate}

namespace eval sg13g2 {
    namespace path {::tcl::mathop ::tcl::mathfunc}

    set ruleset [dict create]

    # Process DRC rules (magic style)

    dict set ruleset poly_surround    0.07      ;# Poly surrounds contact
    dict set ruleset diff_surround    0.07      ;# Diffusion surrounds contact
    dict set ruleset gate_to_diffcont 0.19      ;# Gate to diffusion contact center
    dict set ruleset gate_to_polycont 0.22      ;# Gate to poly contact center
    dict set ruleset gate_extension   0.18      ;# Poly extension beyond gate
    dict set ruleset diff_extension   0.18      ;# Diffusion extension beyond gate
    dict set ruleset contact_size     0.16      ;# Minimum contact size
    dict set ruleset via_size         0.19      ;# Minimum via size
    dict set ruleset metal_surround   0.05      ;# Metal 1 exension over contact
    dict set ruleset sub_surround     0.31      ;# Sub/well surrounds diffusion
    dict set ruleset diff_spacing     0.21      ;# Diffusion spacing rule
    dict set ruleset poly_spacing     0.18      ;# Poly spacing rule
    dict set ruleset diff_poly_space  0.07      ;# Diffusion to poly spacing rule
    dict set ruleset diff_gate_space  0.07      ;# Diffusion to gate poly spacing rule
    dict set ruleset metal_spacing    0.18      ;# Metal 1 spacing rule
    dict set ruleset mmetal_spacing   0.21      ;# Metal spacing rule (metal 2 to 5)
    dict set ruleset res_to_cont      0.20      ;# resistor to contact center
    dict set ruleset res_diff_spacing 0.18      ;# resistor to guard ring
}

#-----------------------------------------------------
# magic::addtechmenu
#-----------------------------------------------------

proc sg13g2::addtechmenu {framename} {
   global Winopts Opts
   
   # Check for difference between magic 8.1.125 and earlier, and 8.1.126 and later
   if {[catch {${framename}.titlebar cget -height}]} {
      set layoutframe ${framename}.pane.top
   } else {
      set layoutframe ${framename}
   }

   # List of devices is long.  Divide into two sections for active and passive deivces
   magic::add_toolkit_menu $layoutframe "Devices" pdk1

   magic::add_toolkit_command $layoutframe "nmos (MOSFET)" \
	    "magic::gencell sg13g2::sg13_lv_nmos" pdk1
   magic::add_toolkit_command $layoutframe "pmos (MOSFET)" \
	    "magic::gencell sg13g2::sg13_lv_pmos" pdk1

   magic::add_toolkit_separator	$layoutframe pdk1
   magic::add_toolkit_command $layoutframe "n-diode" \
	    "magic::gencell sg13g2::dantenna" pdk1
   magic::add_toolkit_command $layoutframe "p-diode" \
	    "magic::gencell sg13g2::dpantenna" pdk1
   magic::add_toolkit_command $layoutframe "schottky" \
	    "magic::gencell sg13g2::schottky" pdk1
   magic::add_toolkit_separator	$layoutframe pdk1

   magic::add_toolkit_command $layoutframe "NPN" \
	    "magic::gencell sg13g2::npn13g2" pdk1
   magic::add_toolkit_command $layoutframe "PNP" \
	    "magic::gencell sg13g2::pnpMPA" pdk1
   magic::add_toolkit_separator	$layoutframe pdk1

   magic::add_toolkit_command $layoutframe "poly resistor - 7 Ohm/sq" \
	    "magic::gencell sg13g2::rsil" pdk1
   magic::add_toolkit_command $layoutframe "poly resistor - 260 Ohm/sq" \
	    "magic::gencell sg13g2::rppd" pdk1
   magic::add_toolkit_command $layoutframe "poly resistor - 1360 Ohm/sq" \
	    "magic::gencell sg13g2::rhigh" pdk1
   magic::add_toolkit_command $layoutframe "metal resistor" \
	    "magic::gencell sg13g2::rm1" pdk1
   magic::add_toolkit_separator	$layoutframe pdk1

   magic::add_toolkit_command $layoutframe "MiM cap - 1.5fF/um^2" \
	    "magic::gencell sg13g2::cap_cmim" pdk1
   magic::add_toolkit_separator	$layoutframe pdk1

   magic::add_toolkit_command $layoutframe "substrate contact (1.8V)" \
	    "sg13g2::subconn_draw" pdk1
   magic::add_toolkit_command $layoutframe "substrate guard ring (1.8V)" \
	    "sg13g2::subconn_guard_draw" pdk1
   magic::add_toolkit_command $layoutframe "substrate contact (5.0V)" \
	    "sg13g2::hvsubconn_draw" pdk1
   magic::add_toolkit_command $layoutframe "substrate guard ring (5.0V)" \
	    "sg13g2::hvsubconn_guard_draw" pdk1
   magic::add_toolkit_command $layoutframe "deep n-well region (1.8V)" \
	    "sg13g2::deep_nwell_draw" pdk1
   magic::add_toolkit_command $layoutframe "deep n-well region (5.0V)" \
	    "sg13g2::hvdeep_nwell_draw" pdk1
   magic::add_toolkit_command $layoutframe "n-well region with guard ring (1.8V)" \
	    "sg13g2::nwell_draw" pdk1
   magic::add_toolkit_command $layoutframe "n-well region with guard ring (5.0V)" \
	    "sg13g2::hvnwell_draw" pdk1
   # magic::add_toolkit_command $layoutframe "via1" \
   #	    "sg13g2::via1_draw" pdk1
   # magic::add_toolkit_command $layoutframe "via2" \
   #	    "sg13g2::via2_draw" pdk1
   # magic::add_toolkit_command $layoutframe "via3" \
   #	    "sg13g2::via3_draw" pdk1
   # magic::add_toolkit_command $layoutframe "via4" \
   #	    "sg13g2::via4_draw" pdk1
   # magic::add_toolkit_command $layoutframe "via5" \
   #	    "sg13g2::via5_draw" pdk1
   # magic::add_toolkit_command $layoutframe "via6" \
   #	    "sg13g2::via6_draw" pdk1
   magic::add_toolkit_command $layoutframe "via stack" \
	    "magic::gencell sg13g2::via" pdk1
   magic::add_toolkit_separator	$layoutframe pdk1

   magic::add_toolkit_command $layoutframe "ESD diode" \
	    "magic::gencell sg13g2::diodevdd_2kv" pdk1
   magic::add_toolkit_command $layoutframe "nMOS clamp" \
	    "magic::gencell sg13g2::nmoscl_2" pdk1
   magic::add_toolkit_separator	$layoutframe pdk1

   magic::add_toolkit_command $layoutframe "Bond pad" \
	    "magic::gencell sg13g2::bondpad" pdk1

   # Additional DRC style for routing only---add this to the DRC menu
   ${layoutframe}.titlebar.mbuttons.drc.toolmenu add command -label "DRC Routing" -command {drc style drc(routing)}

   # Add SPICE import function to File menu
   ${layoutframe}.titlebar.mbuttons.file.toolmenu insert 4 command -label "Import SPICE" -command {sg13g2::importspice}
   ${layoutframe}.titlebar.mbuttons.file.toolmenu insert 4 separator

   # Add command entry window by default if enabled
   if {[info exists Opts(cmdentry)]} {
      set Winopts(${framename},cmdentry) $Opts(cmdentry)
   } else {
      set Winopts(${framename},cmdentry) 0
   }
   if {$Winopts(${framename},cmdentry) == 1} {
      addcommandentry $framename
   }
}

#----------------------------------------------------------------
# Menu callback function to read a SPICE netlist and generate an
# initial layout using the IHP sg13g2A gencells.
#----------------------------------------------------------------

proc sg13g2::importspice {} {
   global CAD_ROOT

   set Layoutfilename [ tk_getOpenFile -filetypes \
	    {{SPICE {.spice .spc .spi .ckt .cir .sp \
	    {.spice .spc .spi .ckt .cir .sp}}} {"All files" {*}}}]
   if {$Layoutfilename != ""} {
      magic::netlist_to_layout $Layoutfilename sg13g2
   }
}

#----------------------------------------------------------------
# Routines for generated vias
#----------------------------------------------------------------

proc sg13g2::via_defaults {} {
   return {metalbot metal1 metaltop metal2 nxcuts 1 nycuts 1 \
	orient default pattern none}
}

proc sg13g2::via_dialog {parameters} {
    # Editable fields:     all of them
    # Special handling:
    #	cuts can be lists if metaltop - metalbot > 1
    #	orientation can be a list if non-default

    set sellist {metal1 metal2 metal3 metal4 metal5 metal6}
    magic::add_selectlist metalbot "Bottom metal" $sellist $parameters metal1
    set sellist {metal2 metal3 metal4 metal5 metal6 metal7}
    magic::add_selectlist metaltop "Top metal" $sellist $parameters metal2

    # NOTE: Need to implement custom per-layer orientation
    set sellist {default inverse horizontal vertical}
    magic::add_selectlist orient "Orientation" $sellist $parameters default

    # NOTE: "none" just uses a square via area, filled by the algorithm.
    # To do:  Implement a "checker" pattern
    set sellist {none offset}
    magic::add_selectlist pattern "Pattern" $sellist $parameters none

    magic::add_entry nxcuts "Number cuts in X" $parameters
    magic::add_entry nycuts "Number cuts in Y" $parameters
}

proc sg13g2::via_convert {parameters} {
    # Vias do not import from SPICE;  nothing to do.
}

proc sg13g2::via_check {parameters} {
    # Numerical sanity checks

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # nxcuts and nycuts can be arrays of values.  If they are arrays,
    # then check that each entry in the array is integer, and that
    # the number of entries matches or exceeds the number of metal
    # layers selected.  If less, then pad out the list.

    for {set i 0} {$i < [llength $nxcuts]} {incr i} {
	set nxcut [lindex $nxcuts $i]
	if {[catch {expr abs($nxcut)}]} {
	    puts stderr "Number of cuts in X must be numeric!"
	    set nxcuts [lreplace $nxcuts $i $i 1]
            dict set parameters nxcuts $nxcuts
	} elseif {$nxcut < 1} {
	    puts stderr "Number of cuts in X must be at least 1!"
	    set nxcuts [lreplace $nxcuts $i $i 1]
            dict set parameters nxcuts $nxcuts
	} elseif {[floor $nxcut] != $nxcut} {
	    puts stderr "Number of cuts in X must be an integer!"
	    set nxcuts [lreplace $nxcuts $i $i [floor $nxcut]]
            dict set parameters nxcuts $nxcuts
	}
    }
    for {set i 0} {$i < [llength $nycuts]} {incr i} {
	set nycut [lindex $nycuts $i]
	if {[catch {expr abs($nycut)}]} {
	    puts stderr "Number of cuts in Y must be numeric!"
	    set nycuts [lreplace $nycuts $i $i 1]
            dict set parameters nycuts $nycuts
	} elseif {$nycut < 1} {
	    puts stderr "Number of cuts in Y must be at least 1!"
	    set nycuts [lreplace $nycuts $i $i 1]
            dict set parameters nycuts $nycuts
	} elseif {[floor $nycut] != $nycut} {
	    puts stderr "Number of cuts in Y must be an integer!"
	    set nycuts [lreplace $nycuts $i $i [floor $nycut]]
            dict set parameters nycuts $nycuts
	}
    }

    set metallist {metal1 metal2 metal3 metal4 metal5 metal6 metal7}
    set mbotidx [lsearch $metallist $metalbot]
    set mtopidx [lsearch $metallist $metaltop]

    if {$mbotidx < 0} {
	puts stderr "Invalid bottom metal selection!"
	set mbotidx 0
        dict set parameters metalbot metal1
    }
    if {$mtopidx < 0} {
	puts stderr "Invalid top metal selection!"
	if {$mtopidx == 6} {
	    set mbotidx 5
	} else {
	    set mtopidx [+ $mbotidx 1]
	}
        dict set parameters metaltop [lindex $metallist $mtopidx]
        dict set parameters metalbot [lindex $metallist $mbotidx]
    }
    if {$mtopidx <= $mbotidx} {
	puts stderr "Top metal must be higher than the bottom metal!"
	if {$mtopidx == 6} {
	    set mbotidx [- $mtopidx 1]
	} else {
	    set mtopidx [+ $mbotidx 1]
	}
        dict set parameters metaltop [lindex $metallist $mtopidx]
        dict set parameters metalbot [lindex $metallist $mbotidx]
    }

    set numvias [- $mtopidx $mbotidx]
    set numxcuts [llength $nxcuts]
    set numycuts [llength $nycuts]
    if {($numxcuts != 1) && ($numxcuts < $numvias)} {
	puts stderr "List of cuts in X padded to match metal layer stack."
	for {set i $numxcuts} {$i < $numvias} {incr i} {
	    lappend nxcuts [lindex $nxcuts end]
	}
	dict set parameters nxcuts $nxcuts
    }
    if {($numycuts != 1) && ($numycuts < $numvias)} {
	puts stderr "List of cuts in Y padded to match metal layer stack."
	for {set i $numycuts} {$i < $numvias} {incr i} {
	    lappend nycuts [lindex $nycuts end]
	}
	dict set parameters nycuts $nycuts
    }
    return $parameters
}

proc sg13g2::via_draw {parameters} {

    # suspendall

    # Set defaults if they are not in parameters
    set nxcuts 1
    set nycuts 2
    set pattern none
    set orient default

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Local to this routine:  width, spacing, and overlap rules
    # for each via layer, starting with via1.  Note that via rules
    # are magic's rules for via areas, not IHP rules for cut layers.

    set metalwidthrule {0.16  0.20  0.20  0.20  0.20  1.64 2.00}
    set metalarearule  {0.09  0.144 0.144 0.144 0.144 0.0  0.0}
    set viawidthrule   {0.20  0.20  0.20  0.20  0.62  1.90}
    set spacingrule    {0.21  0.21  0.21  0.21  0.22  0.06}
    set b0overlaprule  {0.005 0.0   0.0   0.0   0.0   0.0}
    set b1overlaprule  {0.045 0.045 0.045 0.045 0.0   0.0}
    set t0overlaprule  {0.0   0.0   0.0   0.0   0.32  0.0}
    set t1overlaprule  {0.045 0.045 0.045 0.045 0.32  0.0}

    # These are IHP cut rules and are needed to determine what area
    # is required for a specific number of cuts.
    set cutsizerule    {0.19  0.19  0.19  0.19  0.42  0.90}
    set cutspacerule   {0.22  0.22  0.22  0.22  0.42  1.06}
    set cutspace2rule  {0.29  0.29  0.29  0.29  0.42  1.06}
    set cutborderrule  {0.005 0.005 0.005 0.005 0.10  0.50}

    # Convert metalbot and metaltop to array indexes
    set metallist {metal1 metal2 metal3 metal4 metal5 metal6 metal7}
    set metalbot [lsearch $metallist $metalbot]
    set metaltop [lsearch $metallist $metaltop]

    # Expand cuts to cover all layers
    set numvias [- $metaltop $metalbot]
    set numlayers [+ $numvias 1]
    if {[llength $nxcuts] == 1} {
	set nxcuts [lrepeat 6 $nxcuts]
    } elseif {[llength $nxcuts] == $numvias} {
	set allnxcuts {}
	for {set i 0} {$i < $metalbot} {incr i} {
	    lappend allnxcuts 1
	}
	lappend allnxcuts {*}$nxcuts
	for {set i $metaltop} {$i < 7} {incr i} {
	    lappend allnxcuts 1
	}
	set nxcuts $allnxcuts
    }

    if {[llength $nycuts] == 1} {
	set nycuts [lrepeat 6 $nycuts]
    } elseif {[llength $nycuts] == $numvias} {
	set allnycuts {}
	for {set i 0} {$i < $metalbot} {incr i} {
	    lappend allnycuts 1
	}
	lappend allnycuts {*}$nycuts
	for {set i $metaltop} {$i < 7} {incr i} {
	    lappend allnycuts 1
	}
	set nycuts $allnycuts
    }

    if {[llength $orient] == 1} {
	if {$orient == "default"} {
	    set orient [lrepeat 4 horizontal vertical]
	} elseif {$orient == "inverse"} {
	    set orient [lrepeat 4 vertical horizontal]
	} else {
	    set orient [lrepeat 7 $orient]
	}
    } elseif {[llength $orient] == $numlayers} {
	# NOTE:  Custom per-layer orientation is not yet implemented in
	# the via dialog.
	set allorient {}
	for {set i 0} {$i < $metalbot} {incr i} {
	    lappend allorient default
	}
	lappend allorient {*}$orient
	for {set i $metaltop} {$i < 7} {incr i} {
	    lappend allorient default
	}
	set orient $allorient
    }

    # For offset patterns, reduce the counts of every other layer by 1
    # unless the count is only 1.
    if {$pattern == "offset"} {
	for {set i [+ $metalbot 1]} {$i <= $metaltop} {incr i 2} {
	    set lnxcuts [lindex $nxcuts $i]
	    set lnycuts [lindex $nycuts $i]
	    if {$lnxcuts > 1} {
		incr lnxcuts -1
		set nxcuts [lreplace $nxcuts $i $i $lnxcuts]
	    }
	    if {$lnycuts > 1} {
		incr lnycuts -1
		set nycuts [lreplace $nycuts $i $i $lnycuts]
	    }
	}
    }
    
    # Make sure all layers are represented so that indexing works.
    set metalwidth [lrepeat 7 0]
    set metalheight [lrepeat 7 0]
    set viawidth [lrepeat 6 0]
    set viaheight [lrepeat 6 0]

    # Compute the via layer area from the border/size/space rules
    for {set i $metalbot} {$i < $metaltop} {incr i} {
	set cutsize [lindex $cutsizerule $i]
	set cutborder [* [lindex $cutborderrule $i] 2]
	set lnxcuts [lindex $nxcuts $i]
	set lnycuts [lindex $nycuts $i]
	if {$lnxcuts > 3} {
	    set cutspacex [lindex $cutspace2rule $i]
	} else {
	    set cutspacex [lindex $cutspacerule $i]
	}
	if {$lnycuts > 3} {
	    set cutspacey [lindex $cutspace2rule $i]
	} else {
	    set cutspacey [lindex $cutspacerule $i]
	}

	set lviawidth [+ [* $lnxcuts $cutsize] [* [- $lnxcuts 1] $cutspacex] $cutborder]
	set lviaheight [+ [* $lnycuts $cutsize] [* [- $lnycuts 1] $cutspacey] $cutborder]

	set viawidth [lreplace $viawidth $i $i $lviawidth]
	set viaheight [lreplace $viaheight $i $i $lviaheight]
    }

    # Determine the surrounding metal area needed on each layer
    # Do this both for the metal as the bottom layer of the via on top, and
    # the metal as the top layer of the via underneath.

    for {set i $metalbot} {$i <= $metaltop} {incr i} {
	set lorient [lindex $orient $i]
	set twidth 0
	set theight 0
	set bwidth 0
	set bheight 0
	if {$i > $metalbot} {
	    # Calculate minimum dimensions of metal surrounding the via below
	    set narrowoverlap [lindex $t0overlaprule [- $i 1]]
	    set wideoverlap [lindex $t1overlaprule [- $i 1]]
	    if {$lorient == "horizontal"} {
		set toverlapx $wideoverlap
		set toverlapy $narrowoverlap
	    } else {
		set toverlapx $narrowoverlap
		set toverlapy $wideoverlap
	    }
	    # Find the total minimum X and Y extents of metal
	    set lviawidth [lindex $viawidth [- $i 1]]
	    set lviaheight [lindex $viaheight [- $i 1]]
	    set twidth [+ $lviawidth [* 2 $toverlapx]]
	    set theight [+ $lviaheight [* 2 $toverlapy]]
	}
	if {$i < $metaltop} {
	    # Calculate minimum dimensions of metal surrounding the via above
	    set narrowoverlap [lindex $b0overlaprule $i]
	    set wideoverlap [lindex $b1overlaprule $i]
	    if {$lorient == "horizontal"} {
		set boverlapx $wideoverlap
		set boverlapy $narrowoverlap
	    } else {
		set boverlapx $narrowoverlap
		set boverlapy $wideoverlap
	    }
	    # Find the total minimum X and Y extents of metal
	    set lviawidth [lindex $viawidth $i]
	    set lviaheight [lindex $viaheight $i]
	    set bwidth [+ $lviawidth [* 2 $boverlapx]]
	    set bheight [+ $lviaheight [* 2 $boverlapy]]
	}
	# Actual width and height are the greater of the two measurements
	if {$twidth > $bwidth} {set lmetalwidth $twidth} {set lmetalwidth $bwidth}
	if {$theight > $bheight} {set lmetalheight $theight} {set lmetalheight $bheight}
	
	set metalwidth [lreplace $metalwidth $i $i $lmetalwidth]
	set metalheight [lreplace $metalheight $i $i $lmetalheight]
    }

    puts stdout "Diagnostic 2:  metalwidth = $metalwidth   metalheight = $metalheight" 

    # For each internal metal, make sure that minimum metal area
    # rule is satisfied.  It is assumed that the top and bottom layers
    # of a stack will be routed to, and so do not need to meet the
    # minimum area requirement within this cell.

    for {set i [+ $metalbot 1]} {$i < $metaltop} {incr i} {
	set lmetalwidth [lindex $metalwidth $i]
	set lmetalheight [lindex $metalheight $i]
	set metalarea [* $lmetalwidth $lmetalheight]
	set minmetalarea [lindex $metalarearule $i]
	if {$metalarea < $minmetalarea} {
	    set lorient [lindex $orient $i]
	    if {$lorient == "horizontal"} {
		set lmetalwidth [/ $minmetalarea $lmetalheight]
		set metalwidth [lreplace $metalwidth $i $i $lmetalwidth]
	    } else {
		set lmetalheight [/ $minmetalarea $lmetalwidth]
		set metalheight [lreplace $metalheight $i $i $lmetalheight]
	    }
	}
    }

    puts stdout "Diagnostic 3:  metalwidth = $metalwidth   metalheight = $metalheight" 

    # For each metal layer, paint the metal and contact
    snap internal
    for {set i $metalbot} {$i <= $metaltop} {incr i} {
	set lmetalwidth [lindex $metalwidth $i]
	set lmetalheight [lindex $metalheight $i]
	set hmetalwidth [/ $lmetalwidth 2.0]
	set hmetalheight [/ $lmetalheight 2.0]
	box values -${hmetalwidth}um -${hmetalheight}um ${hmetalwidth}um ${hmetalheight}um
	paint metal[+ $i 1]

	if {$i < $metaltop} {
	    set lviawidth [lindex $viawidth $i]
	    set lviaheight [lindex $viaheight $i]
	    set hviawidth [/ $lviawidth 2.0]
	    set hviaheight [/ $lviaheight 2.0]
	    box values -${hviawidth}um -${hviaheight}um ${hviawidth}um ${hviaheight}um
	    paint via[+ $i 1]
	}
    }
    # resumeall
}

#----------------------------------------------------------------

proc sg13g2::via1_draw {{dir default}} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 0.2} {
      puts stderr "Via1 width must be at least 0.2um"
      return
   }
   if {$h < 0.2} {
      puts stderr "Via1 height must be at least 0.2um"
      return
   }
   suspendall
   paint via1
   if {($w < $h) || ($dir == "vert")} {
      pushbox
      box grow e 0.045um
      box grow w 0.045um
      paint m2
      popbox
      pushbox
      box grow c 0.005um
      box grow n 0.045um
      box grow s 0.045um
      paint m1
      popbox
   } else {
      pushbox
      box grow n 0.045um
      box grow s 0.045um
      paint m2
      popbox
      pushbox
      box grow c 0.005um
      box grow e 0.045um
      box grow w 0.045um
      paint m1
      popbox
   }
   resumeall
}

proc sg13g2::via2_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 0.2} {
      puts stderr "Via2 width must be at least 0.2um"
      return
   }
   if {$h < 0.2} {
      puts stderr "Via2 height must be at least 0.2um"
      return
   }
   suspendall
   pushbox
   paint via2
   box grow n 0.045um
   box grow s 0.045um
   paint m2
   popbox
   pushbox
   box grow e 0.045um
   box grow w 0.045um
   paint m3
   popbox
   resumeall
}

proc sg13g2::via3_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 0.2} {
      puts stderr "Via3 width must be at least 0.2um"
      return
   }
   if {$h < 0.2} {
      puts stderr "Via3 height must be at least 0.2um"
      return
   }
   suspendall
   pushbox
   paint via3
   box grow n 0.045um
   box grow s 0.045um
   paint m4
   popbox
   pushbox
   box grow e 0.045um
   box grow w 0.045um
   paint m3
   popbox
   resumeall
}

proc sg13g2::via4_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 0.2} {
      puts stderr "Via4 width must be at least 0.2um"
      return
   }
   if {$h < 0.2} {
      puts stderr "Via4 height must be at least 0.2um"
      return
   }
   suspendall
   pushbox
   paint via4
   box grow n 0.045um
   box grow s 0.045um
   paint m5
   popbox
   pushbox
   box grow e 0.045um
   box grow w 0.045um
   paint m4
   popbox
   resumeall
}

proc sg13g2::via5_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 0.62} {
      puts stderr "Via5 width must be at least 0.62um"
      return
   }
   if {$h < 0.62} {
      puts stderr "Via5 height must be at least 0.62um"
      return
   }
   suspendall
   paint via5
   pushbox
   box grow c 0.32um
   paint m6
   popbox
   resumeall
}

proc sg13g2::via6_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 1.90} {
      puts stderr "Via6 width must be at least 1.90um"
      return
   }
   if {$h < 1.90} {
      puts stderr "Via6 height must be at least 1.90um"
      return
   }
   suspendall
   paint via6
   paint m7
   resumeall
}

proc sg13g2::subconn_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 0.16} {
      puts stderr "Substrate tap width must be at least 0.16um"
      return
   }
   if {$h < 0.16} {
      puts stderr "Substrate tap height must be at least 0.16um"
      return
   }
   suspendall
   paint psc
   pushbox
   if {$w > $h} {
      box grow e 0.05um
      box grow w 0.05um
      paint m1
      box grow e 0.02um
      box grow w 0.02um
   } else {
      box grow n 0.05um
      box grow s 0.05um
      paint m1
      box grow n 0.02um
      box grow s 0.02um
   }
   paint psd
   popbox
   resumeall
}

#----------------------------------------------------------------

proc sg13g2::hvsubconn_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 0.16} {
      puts stderr "Substrate tap width must be at least 0.16um"
      return
   }
   if {$h < 0.16} {
      puts stderr "Substrate tap height must be at least 0.16um"
      return
   }
   suspendall
   paint hvpsc
   pushbox
   if {$w > $h} {
      box grow e 0.05um
      box grow w 0.05um
      paint m1
      box grow e 0.02um
      box grow w 0.02um
   } else {
      box grow n 0.05um
      box grow s 0.05um
      paint m1
      box grow n 0.02um
      box grow s 0.02um
   }
   paint hvpsd
   popbox
   resumeall
}

#----------------------------------------------------------------
# Helper function for drawing guard rings.
# Assumes that a box exists and defines the centerlines of the
# guard ring contacts.
# ctype = type to paint for contact
# dtype = type to paint for diffusion
#----------------------------------------------------------------

proc sg13g2::guard_ring_draw {ctype dtype} {
   pushbox
   box width 0
   box grow c 0.08um
   paint m1
   pushbox
   box grow n -0.3um
   box grow s -0.3um
   paint $ctype
   popbox
   box grow c 0.07um
   paint $dtype
   popbox

   pushbox
   box height 0
   box grow c 0.08um
   paint m1
   pushbox
   box grow e -0.3um
   box grow w -0.3um
   paint $ctype
   popbox
   box grow c 0.07um
   paint $dtype
   popbox

   pushbox
   box move n [box height]i
   box height 0
   box grow c 0.08um
   paint m1
   pushbox
   box grow e -0.3um
   box grow w -0.3um
   paint $ctype
   popbox
   box grow c 0.07um
   paint $dtype
   popbox

   pushbox
   box move e [box width]i
   box width 0
   box grow c 0.08um
   paint m1
   pushbox
   box grow n -0.3um
   box grow s -0.3um
   paint $ctype
   popbox
   box grow c 0.07um
   paint $dtype
   popbox
}

#----------------------------------------------------------------

proc sg13g2::subconn_guard_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   # NOTE:  Width and height are determined by the requirement for
   # a contact on each side.  There is not much that can be done
   # with an guarded nwell smaller than that, anyway.
   if {$w < 0.6} {
      puts stderr "Substrate guard ring width must be at least 0.6um"
      return
   }
   if {$h < 0.6} {
      puts stderr "Substrate guard ring height must be at least 0.6um"
      return
   }
   suspendall
   tech unlock *
   pushbox

   sg13g2::guard_ring_draw psc psd

   popbox
   tech revert
   resumeall
}

#----------------------------------------------------------------

proc sg13g2::hvsubconn_guard_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   # NOTE:  Width and height are determined by the requirement for
   # a contact on each side.  There is not much that can be done
   # with an guarded nwell smaller than that, anyway.
   if {$w < 0.6} {
      puts stderr "Substrate guard ring width must be at least 0.6um"
      return
   }
   if {$h < 0.6} {
      puts stderr "Substrate guard ring height must be at least 0.6um"
      return
   }
   suspendall
   tech unlock *
   pushbox

   sg13g2::guard_ring_draw hvpsc hvpsd

   popbox
   tech revert
   resumeall
}

#----------------------------------------------------------------

proc sg13g2::nwell_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   # NOTE:  Width and height are determined by the requirement for
   # a contact on each side.  There is not much that can be done
   # with an guarded nwell smaller than that, anyway.
   if {$w < 0.62} {
      puts stderr "N-well region width must be at least 0.62um"
      return
   }
   if {$h < 0.62} {
      puts stderr "N-well region height must be at least 0.62um"
      return
   }
   suspendall
   tech unlock *
   pushbox
   pushbox
   box grow c 0.265um
   paint nwell
   popbox

   sg13g2::guard_ring_draw nsc nsd

   popbox
   tech revert
   resumeall
}

#----------------------------------------------------------------

proc sg13g2::hvnwell_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   # NOTE:  Width and height are determined by the requirement for
   # a contact on each side.  There is not much that can be done
   # with an guarded nwell smaller than that, anyway.
   if {$w < 0.62} {
      puts stderr "MV N-well region width must be at least 0.62um"
      return
   }
   if {$h < 0.62} {
      puts stderr "MV N-well region height must be at least 0.26um"
      return
   }
   suspendall
   tech unlock *
   pushbox
   pushbox
   box grow c 0.415um
   paint nwell
   popbox

   sg13g2::guard_ring_draw hvnsc hvnsd

   popbox
   tech revert
   resumeall
}

#----------------------------------------------------------------

proc sg13g2::deep_nwell_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 3.0} {
      puts stderr "Deep-nwell region width must be at least 3.0um"
      return
   }
   if {$h < 3.0} {
      puts stderr "Deep-nwell region height must be at least 3.0um"
      return
   }
   suspendall
   tech unlock *
   paint dnwell
   pushbox
   pushbox
   box grow c 0.4um
   # Note:  Previous implementation was to draw nwell over the whole
   # area and then erase it from the center.  That can interact with
   # any layout already drawn in the center area.  Instead, draw four
   # separate rectangles.
   # -----------------
   # paint nwell
   # box grow c -1.05um
   # erase nwell
   # -----------------
   pushbox
   box width 1.05um
   paint nwell
   popbox
   pushbox
   box height 1.05um
   paint nwell
   popbox
   pushbox
   box move n ${h}um
   box move n 0.8um
   box move s 1.05um
   box height 1.05um
   paint nwell
   popbox
   pushbox
   box move e ${w}um
   box move e 0.8um
   box move w 1.05um
   box width 1.05um
   paint nwell
   popbox

   popbox
   box grow c 0.03um

   pushbox
   box width 0
   box grow c 0.085um
   paint m1
   pushbox
   box grow n -0.3um
   box grow s -0.3um
   paint nsc
   popbox
   box grow c 0.1um
   paint nsd
   popbox

   pushbox
   box height 0
   box grow c 0.085um
   paint m1
   pushbox
   box grow e -0.3um
   box grow w -0.3um
   paint nsc
   popbox
   box grow c 0.1um
   paint nsd
   popbox

   pushbox
   box move n [box height]i
   box height 0
   box grow c 0.085um
   paint m1
   pushbox
   box grow e -0.3um
   box grow w -0.3um
   paint nsc
   popbox
   box grow c 0.1um
   paint nsd
   popbox

   pushbox
   box move e [box width]i
   box width 0
   box grow c 0.085um
   paint m1
   pushbox
   box grow n -0.3um
   box grow s -0.3um
   paint nsc
   box grow c 0.1um
   paint nsd
   popbox

   popbox
   tech revert
   resumeall
}

#----------------------------------------------------------------

proc sg13g2::hvdeep_nwell_draw {} {
   set w [magic::i2u [box width]]
   set h [magic::i2u [box height]]
   if {$w < 3.0} {
      puts stderr "MV Deep-nwell region width must be at least 3.0um"
      return
   }
   if {$h < 3.0} {
      puts stderr "MV Deep-nwell region height must be at least 3.0um"
      return
   }
   suspendall
   tech unlock *
   paint dnwell
   pushbox
   pushbox
   box grow c 0.55um
   pushbox
   box width 1.58um
   paint nwell
   popbox
   pushbox
   box height 1.58um
   paint nwell
   popbox
   pushbox
   box move n ${h}um
   box move n 1.1um
   box move s 1.58um
   box height 1.58um
   paint nwell
   popbox
   pushbox
   box move e ${w}um
   box move e 1.1um
   box move w 1.58um
   box width 1.58um
   paint nwell
   popbox

   popbox
   box grow c 0.03um

   pushbox
   box width 0
   box grow c 0.085um
   paint m1
   pushbox
   box grow n -0.3um
   box grow s -0.3um
   paint hvnsc
   popbox
   box grow c 0.1um
   paint hvnsd
   popbox

   pushbox
   box height 0
   box grow c 0.085um
   paint m1
   pushbox
   box grow e -0.3um
   box grow w -0.3um
   paint hvnsc
   popbox
   box grow c 0.1um
   paint hvnsd
   popbox

   pushbox
   box move n [box height]i
   box height 0
   box grow c 0.085um
   paint m1
   pushbox
   box grow e -0.3um
   box grow w -0.3um
   paint hvnsc
   popbox
   box grow c 0.1um
   paint hvnsd
   popbox

   pushbox
   box move e [box width]i
   box width 0
   box grow c 0.085um
   paint m1
   pushbox
   box grow n -0.3um
   box grow s -0.3um
   paint hvnsc
   box grow c 0.1um
   paint hvnsd
   popbox

   popbox
   tech revert
   resumeall
}

#----------------------------------------------------------------

proc sg13g2::res_recalc {field parameters} {
    set snake 0
    set sterm 0.0
    set caplen 0
    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }
    set val [magic::spice2float $val]
    set l [magic::spice2float $l]
    set w [magic::spice2float $w]

    if {$snake == 0} {
	# Straight resistor calculation
	switch  $field {
	    val { set l [expr ($val * ($w - $dw) - (2 * $term)) / $rho]
		  set w [expr ((2 * $term + $l * $rho) / $val) + $dw]
		}
	    w   { set val [expr (2 * $term + $l * $rho) / ($w - $dw)]
		  set l [expr ($val * ($w - $dw) - (2 * $term)) / $rho]
		}
	    l   { set val [expr (2 * $term + $l * $rho) / ($w - $dw)]
		  set w [expr ((2 * $term + $l * $rho) / $val) + $dw]
		}
	}
    } else {
        set term [expr $term + $sterm]
	# Snake resistor calculation
	switch  $field {
	    val { set l [expr (($val - $rho * ($nx - 1)) * ($w - $dw) \
			- (2 * $term) - ($rho * $caplen * ($nx - 1))) \
			/ ($rho * $nx)]

		  set w [expr ((2 * $term + $l * $rho * $nx \
			+ $caplen * $rho * ($nx - 1)) \
			/ ($val - $rho * ($nx - 1))) + $dw]
		}
	    w   { set val [expr $rho * ($nx - 1) + ((2 * $term) \
			+ ($rho * $l * $nx) + ($rho * $caplen * ($nx - 1))) \
			/ ($w - $dw)]

		  set l [expr (($val - $rho * ($nx - 1)) * ($w - $dw) \
			- (2 * $term) - ($rho * $caplen * ($nx - 1))) \
			/ ($rho * $nx)]
		}
	    l   { set val [expr $rho * ($nx - 1) + ((2 * $term) \
			+ ($rho * $l * $nx) + ($rho * $caplen * ($nx - 1))) \
			/ ($w - $dw)]

		  set w [expr ((2 * $term + $l * $rho * $nx \
			+ $caplen * $rho * ($nx - 1)) \
			/ ($val - $rho * ($nx - 1))) + $dw]
		}
	}
    }

    set val [magic::3digitpastdecimal $val]
    set w [magic::3digitpastdecimal $w]
    set l [magic::3digitpastdecimal $l]

    dict set parameters val $val
    dict set parameters w $w
    dict set parameters l $l

    return $parameters
}

#----------------------------------------------------------------
# Drawn diode routines
#----------------------------------------------------------------

proc sg13g2::diode_recalc {field parameters} {
    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }
    switch  $field {
	area { puts stdout "area changed" }
	peri { puts stdout "perimeter changed" }
	w   { puts stdout "width changed" }
	l   { puts stdout "length changed" }
    }
    dict set parameters area $area
    dict set parameters peri $peri
    dict set parameters w $w
    dict set parameters l $l
}

#----------------------------------------------------------------
# diode: Conversion from SPICE netlist parameters to toolkit
#----------------------------------------------------------------

proc sg13g2::diode_convert {parameters} {
    set pdkparams [dict create]
    dict for {key value} $parameters {
	switch -nocase $key {
	    l -
	    w -
	    peri {
		# Length, width, and perimeter are converted to units of microns
		set value [magic::spice2float $value]
		# set value [expr $value * 1e6]
		set value [magic::3digitpastdecimal $value]
		dict set pdkparams [string tolower $key] $value
	    }
	    area {
		# area also converted to units of microns
		set value [magic::spice2float $value]
		# set value [expr $value * 1e12]
		set value [magic::3digitpastdecimal $value]
		dict set pdkparams [string tolower $key] $value
	    }
	    m {
                # Convert m to ny
		dict set pdkparams ny $value
	    }
	    default {
		# Allow unrecognized parameters to be passed unmodified
		dict set pdkparams $key $value
	    }
	}
    }
    return $pdkparams
}

#----------------------------------------------------------------
# diode: Interactively specifies the fixed layout parameters
#----------------------------------------------------------------

proc sg13g2::diode_dialog {device parameters} {
    # Editable fields:      w, l, area, perim, nx, ny

    magic::add_entry area "Area (um^2)" $parameters
    magic::add_entry peri "Perimeter (um)" $parameters
    sg13g2::compute_aptot $parameters
    magic::add_message atot "Total area (um^2)" $parameters
    magic::add_message ptot "Total perimeter (um)" $parameters
    magic::add_entry l "Length (um)" $parameters
    magic::add_entry w "Width (um)" $parameters
    magic::add_entry nx "X Repeat" $parameters
    magic::add_entry ny "Y Repeat" $parameters

    if {[dict exists $parameters compatible]} {
       set sellist [dict get $parameters compatible]
       magic::add_selectlist gencell "Device type" $sellist $parameters $device
    }

    if {[dict exists $parameters doverlap]} {
	magic::add_checkbox doverlap "Overlap at end contact" $parameters
    }
    if {[dict exists $parameters elc]} {
        magic::add_checkbox elc "Add left end contact" $parameters
    }
    if {[dict exists $parameters erc]} {
        magic::add_checkbox erc "Add right end contact" $parameters
    }
    if {[dict exists $parameters etc]} {
        magic::add_checkbox etc "Add top end contact" $parameters
    }
    if {[dict exists $parameters ebc]} {
        magic::add_checkbox ebc "Add bottom end contact" $parameters
    }

    if {[dict exists $parameters guard]} {
        magic::add_checkbox full_metal "Full metal guard ring" $parameters
    }
    if {[dict exists $parameters glc]} {
        magic::add_checkbox glc "Add left guard ring contact" $parameters
    }
    if {[dict exists $parameters grc]} {
        magic::add_checkbox grc "Add right guard ring contact" $parameters
    }
    if {[dict exists $parameters gtc]} {
        magic::add_checkbox gtc "Add top guard ring contact" $parameters
    }
    if {[dict exists $parameters gbc]} {
        magic::add_checkbox gbc "Add bottom guard ring contact" $parameters
    }
    if {[dict exists $parameters viagb]} {
	magic::add_entry viagb  "Bottom guard ring via coverage \[+/-\](%)" $parameters
    }
    if {[dict exists $parameters viagt]} {
	magic::add_entry viagt  "Top guard ring via coverage \[+/-\](%)" $parameters
    }
    if {[dict exists $parameters viagr]} {
	magic::add_entry viagr  "Right guard ring via coverage \[+/-\](%)" $parameters
    }
    if {[dict exists $parameters viagl]} {
	magic::add_entry viagl  "Left guard ring via coverage \[+/-\](%)" $parameters
    }

    if {[dict exists $parameters vias]} {
	magic::add_checkbox vias "Add vias over contacts" $parameters
    }

    magic::add_dependency sg13g2::diode_recalc $device sg13g2 l w area peri

    if {[dict exists $parameters addports]} {
	magic::add_checkbox doports "Add ports" $parameters
    }

    # magic::add_checkbox dummy "Add dummy" $parameters
}

#----------------------------------------------------------------
# Diode total area and perimeter computation
#----------------------------------------------------------------

proc sg13g2::compute_aptot {parameters} {
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }
    set area [magic::spice2float $area]
    set area [magic::3digitpastdecimal $area]
    set peri [magic::spice2float $peri]
    set peri [magic::3digitpastdecimal $peri]

    # Compute total area
    catch {set magic::atot_val [expr ($area * $nx * $ny)]}
    # Compute total perimeter
    catch {set magic::ptot_val [expr ($peri * $nx * $ny)]}
}

#----------------------------------------------------------------
# diode: Check device parameters for out-of-bounds values
#----------------------------------------------------------------

proc sg13g2::diode_check {parameters} {

    set guard 0
    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Normalize distance units to microns
    set l [magic::spice2float $l]
    set l [magic::3digitpastdecimal $l] 
    set w [magic::spice2float $w]
    set w [magic::3digitpastdecimal $w] 

    set area [magic::spice2float $area]
    set area [magic::3digitpastdecimal $area] 
    set peri [magic::spice2float $peri]
    set peri [magic::3digitpastdecimal $peri] 

    if {$l == 0} {
        # Calculate L from W and area
	set l [expr ($area / $w)]
	dict set parameters l [magic::float2spice $l]
    } elseif {$w == 0} {
        # Calculate W from L and area
	set w [expr ($area / $l)]
	dict set parameters w [magic::float2spice $w]
    }
    if {$w < $wmin} {
	puts stderr "Diode width must be >= $wmin"
	dict set parameters w $wmin
    } 
    if {$l < $lmin} {
	puts stderr "Diode length must be >= $lmin"
	dict set parameters l $lmin
    } 

    # Check via coverage for syntax
    if {$guard == 1} {
    	if {[catch {expr abs($viagb)}]} {
	    puts stderr "Guard ring bottom via coverage must be numeric!"
            dict set parameters viagb 0
    	} elseif {[expr abs($viagb)] > 100} {
	    puts stderr "Guard ring bottom via coverage can't be more than 100%"
            dict set parameters viagb 100
    	}
    	if {[catch {expr abs($viagt)}]} {
	    puts stderr "Guard ring top via coverage must be numeric!"
            dict set parameters viagt 0
	} elseif {[expr abs($viagt)] > 100} {
	    puts stderr "Guard ring top via coverage can't be more than 100%"
            dict set parameters viagt 100
	}
	if {[catch {expr abs($viagr)}]} {
	    puts stderr "Guard ring right via coverage must be numeric!"
            dict set parameters viagr 0
	} elseif {[expr abs($viagr)] > 100} {
	    puts stderr "Guard ring right via coverage can't be more than 100%"
            dict set parameters viagr 100
   	} 
        if {[catch {expr abs($viagl)}]} {
	    puts stderr "Guard ring left via coverage must be numeric!"
            dict set parameters viagl 0
	} elseif {[expr abs($viagl)] > 100} {
	   puts stderr "Guard ring left via coverage can't be more than 100%"
           dict set parameters viagl 100
	}
    }

    # Calculate area and perimeter from L and W
    set area [expr ($l * $w)]
    dict set parameters area [magic::float2spice $area]
    set peri [expr (2 * ($l + $w))]
    dict set parameters peri [magic::float2spice $peri]
    sg13g2::compute_aptot $parameters

    return $parameters
}

#------------------------------------------------------------------

proc sg13g2::dantenna_defaults {} {
    return {w 0.45 l 0.45 area 0.2025 peri 1.8 \
	nx 1 ny 1 dummy 0 lmin 0.45 wmin 0.45 class diode \
	elc 1 erc 1 etc 1 ebc 1 doverlap 0 doports 1 \
	full_metal 1 vias 1 viagb 0 viagt 0 viagl 0 viagr 0}
}

proc sg13g2::dpantenna_defaults {} {
    return {w 0.45 l 0.45 area 0.2025 peri 1.8 \
	nx 1 ny 1 dummy 0 lmin 0.45 wmin 0.45 class diode \
	elc 1 erc 1 etc 1 ebc 1 doverlap 0 doports 1 \
	full_metal 1 vias 1 viagb 0 viagt 0 viagl 0 viagr 0}
}

proc sg13g2::schottky_defaults {} {
    return {w 1.2 l 1.9 area 2.28 peri 6.2 \
	nx 1 ny 1 dummy 0 lmin 1.9 wmin 1.2 class diode \
	elc 1 erc 1 etc 1 ebc 1 doverlap 0 doports 1 \
	full_metal 1 vias 1 viagb 0 viagt 0 viagl 0 viagr 0}
}

#----------------------------------------------------------------

proc sg13g2::dantenna_convert {parameters} {
    return [sg13g2::diode_convert $parameters]
}

proc sg13g2::dpantenna_convert {parameters} {
    return [sg13g2::diode_convert $parameters]
}

proc sg13g2::schottky_convert {parameters} {
    return [sg13g2::diode_convert $parameters]
}

#----------------------------------------------------------------

proc sg13g2::dantenna_dialog {parameters} {
    sg13g2::diode_dialog dantenna $parameters
}

proc sg13g2::dpantenna_dialog {parameters} {
    sg13g2::diode_dialog dpantenna $parameters
}

proc sg13g2::schottky_dialog {parameters} {
    sg13g2::diode_dialog shottky $parameters
}

#----------------------------------------------------------------

proc sg13g2::dantenna_check {parameters} {
    sg13g2::diode_check $parameters
}

proc sg13g2::dpantenna_check {parameters} {
    sg13g2::diode_check $parameters
}

proc sg13g2::schottky_check {parameters} {
    sg13g2::diode_check $parameters
}

#----------------------------------------------------------------
# Diode: Draw a single device
#----------------------------------------------------------------

proc sg13g2::diode_device {parameters} {
    # Epsilon for avoiding round-off errors
    set eps  0.0005

    # Set local default values if they are not in parameters
    set doports 0	;# no port labels by default
    set dev_surround 0
    set dev_sub_type ""

    set term_d ""	;# diffusion
    set term_s ""	;# substrate/well

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # If there is no end_sub_surround, set it to sub_surround
    if {![dict exists $parameters end_sub_surround]} {
	set end_sub_surround $sub_surround
    }

    # Draw the device
    pushbox
    box size 0 0

    set hw [/ $w 2.0]
    set hl [/ $l 2.0]

    # Calculate ring size (measured to contact center)
    set gx [+ $w [* 2.0 [+ $dev_spacing $dev_surround]] $contact_size]
    set gy [+ $l [* 2.0 [+ $dev_spacing $dev_surround]] $contact_size]

    # Draw the ring first, because diode may occupy well/substrate plane
    set guardparams $parameters
    dict set guardparams plus_diff_type $end_type
    dict set guardparams plus_contact_type $end_contact_type
    dict set guardparams diff_surround $end_surround
    dict set guardparams sub_type $end_sub_type
    dict set guardparams sub_surround $sub_surround
    dict set guardparams guard_sub_surround $end_sub_surround
    dict set guardparams glc $elc
    dict set guardparams grc $erc
    dict set guardparams gtc $etc
    dict set guardparams gbc $ebc
    set cext [sg13g2::guard_ring $gx $gy $guardparams]

    pushbox
    box grow n ${hl}um
    box grow s ${hl}um
    box grow e ${hw}um
    box grow w ${hw}um
    paint ${dev_type}
    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]

    if {$dev_sub_type != ""} {
	box grow n ${sub_surround}um
	box grow s ${sub_surround}um
	box grow e ${sub_surround}um
	box grow w ${sub_surround}um
	paint ${dev_sub_type}
    }
    popbox

    # Diffusion port label
    if {$term_d != ""} {
	label $term_d c $dev_type
	select area label
	port make
    }

    if {${w} < ${l}} {
	set orient vert
    } else {
	set orient horz
    }

    # Reduce width by surround amount
    set w [- $w [* ${dev_surround} 2.0]]
    set l [- $l [* ${dev_surround} 2.0]]

    # Draw via over contact first
    if {$vias != 0} {
        pushbox
        set ch $l
    	if {$ch < $via_size} {set ch $via_size}
    	set cw $w
    	if {$cw < $via_size} {set cw $via_size}
	box grow n [/ $ch 2]um
	box grow s [/ $ch 2]um
	box grow w [/ $cw 2]um
	box grow e [/ $cw 2]um
        sg13g2::via1_draw
        popbox
    }
    set cext [sg13g2::unionbox $cext [sg13g2::draw_contact ${w} ${l} \
		${dev_surround} ${metal_surround} \
		${contact_size} ${dev_type} ${dev_contact_type} m1 ${orient}]]

    popbox
    return $cext
}

#----------------------------------------------------------------
# Diode: Draw the tiled device
#----------------------------------------------------------------

proc sg13g2::diode_draw {parameters} {
    tech unlock *

    # Set defaults if they are not in parameters
    set doverlap 0	;# overlap diodes at contacts
    set guard 0		;# draw a guard ring
    set prohibit_overlap false  ;# don't prohibit overlaps
    set doports 0

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Normalize distance units to microns
    set w [magic::spice2float $w]
    set l [magic::spice2float $l]

    pushbox
    box values 0 0 0 0

    # Determine the base device dimensions by drawing one device
    # while all layers are locked (nothing drawn).  This allows the
    # base drawing routine to do complicated geometry without having
    # to duplicate it here with calculations.

    tech lock *
    set bbox [sg13g2::diode_device $parameters]
    # puts stdout "Diagnostic: Device bounding box e $bbox (um)"
    tech unlock *

    set fw [- [lindex $bbox 2] [lindex $bbox 0]]
    set fh [- [lindex $bbox 3] [lindex $bbox 1]]
    set lw [+ [lindex $bbox 2] [lindex $bbox 0]]
    set lh [+ [lindex $bbox 3] [lindex $bbox 1]]

    # If prohibit_overlap is true, then end overlapping is prohibited when
    # nx or ny is > 1 to prevent DRC errors (typically from well spacing rule)
    if {$prohibit_overlap == true} {
        if {($nx > 1) || ($ny > 1)} {
	    set doverlap 0
	}
    }

    # Determine tile width and height (depends on overlap)

    if {$doverlap == 0} {
	set dx [+ $fw $end_spacing]
        set dy [+ $fh $end_spacing]
    } else {
        # overlap contact
        set dx [- $fw [+ [* 2.0 $sub_surround] [* 2.0 $end_surround] $contact_size]]
        set dy [- $fh [+ [* 2.0 $sub_surround] [* 2.0 $end_surround] $contact_size]]
    }

    # Determine core width and height
    set corex [+ [* [- $nx 1] $dx] $fw]
    set corey [+ [* [- $ny 1] $dy] $fh]
    set corellx [/ [+ [- $corex $fw] $lw] 2.0]
    set corelly [/ [+ [- $corey $fh] $lh] 2.0]

    if {$guard != 0} {
	# Calculate guard ring size (measured to contact center)
	set gx [+ $corex [* 2.0 [+ $diff_spacing $diff_surround]] $contact_size]
	set gy [+ $corey [* 2.0 [+ $diff_spacing $diff_surround]] $contact_size]

	# Draw the guard ring first, because diode may occupy well/substrate plane
	sg13g2::guard_ring $gx $gy $parameters
    }

    pushbox
    box move w ${corellx}um
    box move s ${corelly}um
    if {($nx > 1) || ($ny > 1)} {
	pushbox
	set hfw [/ $fw 2.0]
	set hfh [/ $fh 2.0]
	box move w ${hfw}um
	box move s ${hfh}um
	box size ${corex}um ${corey}um
	paint $end_sub_type
	popbox
    }
    for {set xp 0} {$xp < $nx} {incr xp} {
	pushbox
	for {set yp 0} {$yp < $ny} {incr yp} {
	    if {$doports} {
		if {($ny == 1) && ($nx == 1)} {
		    dict set parameters term_d D1
		} elseif {$ny == 1} {
		    dict set parameters term_d D1_$xp
		} elseif {$nx == 1} {
		    dict set parameters term_d D1_$yp
		} else {
		    dict set parameters term_d D1_${xp}_$yp
		}
		if {($xp == 0) && ($yp == 0)} {
		    dict set parameters term_s D2
		} else {
		    dict set parameters term_s ""
		}
	    }
	    sg13g2::diode_device $parameters
            box move n ${dy}um
        }
	popbox
        box move e ${dx}um
    }
    popbox
    popbox

    tech revert
}

#----------------------------------------------------------------
# Schottky: Draw a single device
#----------------------------------------------------------------

proc sg13g2::shottky_device {parameters} {

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Draw the device
    pushbox
    box size 0 0

    # Device has ntap fixed width of 0.41 x 0.41
    # Surrounded by nwell 0.84 x 0.84
    # Surrounded by deep nwell 3.0 x 3.0

    pushbox
    box grow c 0.205um
    paint nsd
    popbox
    pushbox
    box grow c 0.42um
    paint nwell
    popbox
    pushbox
    box grow c 1.5um
    paint photo

    set cext [sg13g2::getbox]

    popbox

    # Only enough space for one contact
    set w ${contact_size}
    set l ${contact_size}

    set cext [sg13g2::unionbox $cext [sg13g2::draw_contact ${w} ${l} \
		0 ${metal_surround} ${contact_size} \
		nsd nsc m1 horz]]

    popbox
    return $cext
}

#----------------------------------------------------------------

proc sg13g2::shottky_device {parameters} {

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    pushbox
    box values 0 0 0 0

    # Determine the base device dimensions by drawing one device
    # while all layers are locked (nothing drawn).  This allows the
    # base drawing routine to do complicated geometry without having
    # to duplicate it here with calculations.

    tech lock *
    set bbox [sg13g2::photodiode_device $parameters]
    # puts stdout "Diagnostic: Device bounding box e $bbox (um)"
    tech unlock *

    set fw [- [lindex $bbox 2] [lindex $bbox 0]]
    set fh [- [lindex $bbox 3] [lindex $bbox 1]]
    set lw [+ [lindex $bbox 2] [lindex $bbox 0]]
    set lh [+ [lindex $bbox 3] [lindex $bbox 1]]

    # Determine tile width and height

    set dx [+ $fw $end_spacing]
    set dy [+ $fh $end_spacing]

    # Determine core width and height
    set corex [+ [* [- $nx 1] $dx] $fw]
    set corey [+ [* [- $ny 1] $dy] $fh]
    set corellx [/ [+ [- $corex $fw] $lw] 2.0]
    set corelly [/ [+ [- $corey $fh] $lh] 2.0]

    # Calculate guard ring size (measured to contact center)
    # Spacing between photodiode (deep nwell) and deep nwell (other) is 5.3um
    set gx [+ $corex 15.965]
    set gy [+ $corey 15.965]

    pushbox

    # The deep nwell is offset 0.315 from the nwell ring center to get the
    # right overlap.  The deep nwell ring has a minimum width of 3um.
    set hgx [/ $gx 2.0]
    set hgy [/ $gy 2.0]
    set dwx [+ $hgx 0.315]
    set dwy [+ $hgy 0.315]
    box grow e ${dwx}um
    box grow w ${dwx}um
    box grow n ${dwy}um
    box grow s ${dwy}um
    paint dnwell
    box grow e -3.0um
    box grow w -3.0um
    box grow n -3.0um
    box grow s -3.0um
    erase dnwell

    popbox

    # Draw the guard ring first.  0.63 is the amount nwell surrounds contact;
    # 0.63 * 2 + 0.17 = total nwell width 1.43um, needed to cover dnwell edge.
    set newdict [dict create	 \
	sub_type    space	 \
	guard_sub_type	 nwell	 \
	guard_sub_surround  0.63 \
	plus_diff_type   nsd	 \
	plus_contact_type nsc	 \
    ]
    set guarddict [dict merge $parameters $newdict]
    sg13g2::guard_ring $gx $gy $guarddict

    # Draw outside P-ring and generated the 2nd ring
    set gx [+ $gx [* 2.0 [+ 0.56 $diff_spacing $diff_surround]] $contact_size]
    set gy [+ $gy [* 2.0 [+ 0.56 $diff_spacing $diff_surround]] $contact_size]
    sg13g2::guard_ring $gx $gy $parameters

    pushbox
    box move w ${corellx}um
    box move s ${corelly}um

    for {set xp 0} {$xp < $nx} {incr xp} {
	pushbox
	for {set yp 0} {$yp < $ny} {incr yp} {
	    sg13g2::photodiode_device $parameters
            box move n ${dy}um
        }
	popbox
        box move e ${dx}um
    }
    popbox
    popbox

    tech revert
}

#----------------------------------------------------------------

proc sg13g2::dantenna_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    dev_type		ndiode \
	    dev_contact_type	ndic \
	    end_type		psd \
	    end_contact_type	psc \
	    end_sub_type	psub \
	    dev_spacing		${diff_spacing} \
	    dev_surround	${diff_surround} \
	    end_spacing		${diff_spacing} \
	    end_surround	${diff_surround} \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::diode_draw $drawdict]
} 

#----------------------------------------------------------------

proc sg13g2::dpantenna_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    dev_type		pdiode \
	    dev_contact_type	pdic \
	    end_type		nsd \
	    end_contact_type	nsc \
	    end_sub_type	nwell \
	    dev_spacing		${diff_spacing} \
	    dev_surround	${diff_surround} \
	    end_spacing		${diff_spacing} \
	    end_surround	${diff_surround} \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::diode_draw $drawdict]
} 

#----------------------------------------------------------------
# Draw the Schottky device.  This is fixed layout style based
# around the L and W values.
#----------------------------------------------------------------

proc sg13g2::schottky_device {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set hl [/ $l 2.0]
    set hw [/ $w 2.0]

    box values 0 0 0 0
    pushbox
    box grow n ${hl}um
    box grow s ${hl}um
    box grow e ${hw}um
    box grow w ${hw}um
    pushbox
    box grow n 0.4um
    box grow s 0.4um
    box grow e 0.4um
    box grow w 0.4um
    pushbox
    box grow c 0.62um
    set cext [sg13g2::getbox]
    paint nwell
    popbox
    paint hvnsubdiff
    popbox
    paint schottky
    pushbox
    box grow e -0.25um
    box grow w -0.25um
    box grow n -0.38um
    box grow s -0.38um
    paint m1
    popbox
    pushbox
    box grow e 0.15um
    box grow w 0.15um
    box grow n ${hl}um
    box grow s ${hl}um
    box grow n -0.45um
    box grow s -0.45um
    paint schottkycont
    popbox
    
    # To do:  Add terminal contacts
}

#----------------------------------------------------------------

proc sg13g2::schottky_draw {parameters} {
    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		1	\
	    sub_type		space	\
	    end_spacing		5.0	\
	    end_surround	1.0	\
	    sub_spacing		5.3	\
	    guard_sub_type	pwell	\
	    guard_sub_surround  0.18	\
	    plus_diff_type	psd	\
	    plus_contact_type	psc	\
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::schottky_device $drawdict]

    # To do: Repeat device in X and Y and draw guard ring
}

#----------------------------------------------------------------
# Drawn capacitor routines
# NOTE:  Work in progress.  These values need to be corrected.
#----------------------------------------------------------------

proc sg13g2::cap_cmim_defaults {} {
    return {w 2.00 l 2.00 val 6.0 carea 1.50 cperi 0.19 class capacitor \
		compatible {cap_cmim cap_rfcmim} \
		nx 1 ny 1 dummy 0 square 0 lmin 2.00 wmin 2.00 \
		lmax 30.0 wmax 30.0 dc 0
		upcontact 0 bconnect 1 tconnect 1 \
		ccov 100 doports 1}
}

proc sg13g2::cap_rfcmim_defaults {} {
    return {w 7.00 l 7.00 val 73.5 carea 1.50 cperi 0.19 class capacitor \
		compatible {cap_cmim cap_rfcmim} \
		nx 1 ny 1 dummy 0 square 0 lmin 7.00 wmin 7.00 \
		lmax 30.0 wmax 30.0 dc 0
		upcontact 0 bconnect 1 tconnect 1 \
		ccov 100 doports 1}
}

#----------------------------------------------------------------
# Recalculate capacitor values from GUI entries.
# Recomputes W/L and Value as long as 2 of them are present
# (To be completed)
#----------------------------------------------------------------

proc sg13g2::cap_recalc {field parameters} {
    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }
    switch  $field {
	val { puts stdout "value changed" }
	w   { puts stdout "width changed" }
	l   { puts stdout "length changed" }
    }
    dict set parameters val $val
    dict set parameters w $w
    dict set parameters l $l
}

#----------------------------------------------------------------
# Capacitor defaults:
#----------------------------------------------------------------
#  w      Width of drawn cap
#  l      Length of drawn cap
#  nx     Number of devices in X
#  ny     Number of devices in Y
#  val    Default cap value
#  carea  Area
#  cperi  Perimeter
#  dummy  Add dummy cap
#  square Make square capacitor
#
#  (not user-editable)
#
#  wmin   Minimum allowed width
#  lmin   Minimum allowed length
#  dc     Area to remove to calculated area 
#----------------------------------------------------------------

#----------------------------------------------------------------
# capacitor: Conversion from SPICE netlist parameters to toolkit
#----------------------------------------------------------------

proc sg13g2::cap_convert {parameters} {
    set pdkparams [dict create]
    dict for {key value} $parameters {
	switch -nocase $key {
	    l -
	    w {
		# Length and width are converted to units of microns
		set value [magic::spice2float $value]
		# set value [expr $value * 1e6]
		set value [magic::3digitpastdecimal $value]
		dict set pdkparams [string tolower $key] $value
	    }
	    m {
                # Convert m to ny
		dict set pdkparams ny $value
	    }
	    default {
		# Allow unrecognized parameters to be passed unmodified
		dict set pdkparams $key $value
	    }
	}
    }
    return $pdkparams
}

proc sg13g2::cap_cmim_convert {parameters} {
    return [cap_convert $parameters]
}

proc sg13g2::cap_rfcmim_convert {parameters} {
    return [cap_convert $parameters]
}

#----------------------------------------------------------------
# capacitor: Interactively specifies the fixed layout parameters
#----------------------------------------------------------------

proc sg13g2::cap_dialog {device parameters} {
    # Editable fields:      w, l, nx, ny, val
    # Checked fields:  	    square, dummy

    magic::add_entry val "Value (fF)" $parameters
    sg13g2::compute_ctot $parameters
    magic::add_message ctot "Total capacitance (pF)" $parameters
    magic::add_entry l "Length (um)" $parameters
    magic::add_entry w "Width (um)" $parameters
    magic::add_entry nx "X Repeat" $parameters
    magic::add_entry ny "Y Repeat" $parameters

    if {[dict exists $parameters compatible]} {
	set sellist [dict get $parameters compatible]
	magic::add_selectlist gencell "Device type" $sellist $parameters $device
    }

    if {[dict exists $parameters square]} {
	magic::add_checkbox square "Square capacitor" $parameters
    }
    if {[dict exists $parameters upcontact]} {
	magic::add_checkbox upcontact "Create bottom plate contact" $parameters
    }
    if {[dict exists $parameters bconnect]} {
	magic::add_checkbox bconnect "Connect bottom plates in array" $parameters
    }
    if {[dict exists $parameters tconnect]} {
	magic::add_checkbox tconnect "Connect top plates in array" $parameters
    }
    if {[dict exists $parameters ccov]} {
    	magic::add_entry ccov "Capacitor contact coverage \[+/-\](%)" $parameters
    }
    if {[dict exists $parameters guard]} {
	magic::add_checkbox guard "Add guard ring" $parameters
    }

    magic::add_dependency sg13g2::cap_recalc $device sg13g2 l w val

    if {[dict exists $parameters addports]} {
	magic::add_checkbox doports "Add ports" $parameters
    }

    # magic::add_checkbox dummy "Add dummy" $parameters
}

proc sg13g2::cap_cmim_dialog {parameters} {
    sg13g2::cap_dialog cap_cmim $parameters
}

proc sg13g2::cap_rfcmim_dialog {parameters} {
    sg13g2::cap_dialog cap_rfcmim $parameters
}

#----------------------------------------------------------------
# Capacitor total capacitance computation
#----------------------------------------------------------------

proc sg13g2::compute_ctot {parameters} {
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }
    set val [magic::spice2float $val]
    set val [magic::3digitpastdecimal $val]

    # Compute total capacitance (and convert fF to pF)
    catch {set magic::ctot_val [expr (0.001 * $val * $nx * $ny)]}
}

#----------------------------------------------------------------
# Capacitor: Draw a single device
#----------------------------------------------------------------

proc sg13g2::cap_device {parameters} {
    # Epsilon for avoiding round-off errors
    set eps  0.0005

    # Set local default values if they are not in parameters
    set doports 0	;# no port labels by default
    set cap_surround 0
    set bot_surround 0
    set top_surround 0
    set top_spacing 0
    set upcontact 0	;# create bottom plate contact (disabled)
    set bconnect 0	;# bottom plates are connected in array (disabled)
    set cap_spacing 0	;# cap spacing in array
    set top_space 0   	;# top metal spacing (if larger than cap spacing)
    set top_width 0   	;# top metal minimum width
    set top_contact_size 0  ;# cap contact minimum size
    set ccov 100	    ;# amount of contact coverage

    set term_t ""
    set term_b ""

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    if {![dict exists $parameters top_space]} {
	set top_space $metal_spacing
    }

    # Draw the device
    pushbox
    box size 0 0

    pushbox
    set hw [/ $w 2.0]
    set hl [/ $l 2.0]
    box grow e ${hw}um
    box grow w ${hw}um
    box grow n ${hl}um
    box grow s ${hl}um
    paint ${cap_type}
    pushbox

    # Find contact width if ccov is other than 100
    set cmaxw [- $w [* $cap_surround 2]]
    set cw [* $cmaxw [/ [expr abs($ccov)] 100.0]]
    # Contact width must meet minimum
    if {$cw < $top_contact_size} {set cw $top_contact_size}
    # Max contact width must also meet minimum
    if {$cmaxw < $top_contact_size} {set cmaxw $top_contact_size}

    # Difference between maximum contact width and actual contact width
    set cdif [- $cmaxw $cw]

    # Reduce the box to the maximum contact area
    box grow n -${cap_surround}um
    box grow s -${cap_surround}um
    box grow e -${cap_surround}um
    box grow w -${cap_surround}um

    set anchor [string index $ccov 0]
    if {$anchor == "+"} {
	box grow e -${cdif}um
    } elseif {$anchor == "-"} {
	box grow w -${cdif}um
    } else {
        set cdif [/ ${cdif} 2]
	box grow w -${cdif}um
	box grow e -${cdif}um
    }
    paint ${cap_contact_type}

    pushbox
    box grow n ${top_surround}um
    box grow s ${top_surround}um
    box grow e ${top_surround}um
    box grow w ${top_surround}um

    # If the contact size does not meet the minimum top metal
    # width requirement, then add more top metal.
    set cext [sg13g2::getbox]
    set cwid [- [lindex $cext 2] [lindex $cext 0]]
    set chgt [- [lindex $cext 3] [lindex $cext 1]]
    if {$cwid < $top_width} {
	set cdif [- $top_width $cwid]
	set hcwid [/ $cdif 2.0]
	box grow e ${hcwid}um
	box grow w ${hcwid}um
    }
    if {$chgt < $top_width} {
	set cdif [- $top_width $chgt]
	set hchgt [/ $cdif 2.0]
	box grow n ${hchgt}um
	box grow s ${hchgt}um
    }

    paint ${top_type}
    set cext [sg13g2::getbox]
    puts stdout "Diagnostic:  cext is $cext"
    popbox
    popbox
    pushbox
    box grow n ${bot_surround}um
    box grow s ${bot_surround}um
    box grow e ${bot_surround}um
    box grow w ${bot_surround}um

    paint ${bot_type}
    # Create boundary using properties
    property FIXED_BBOX [box values]
    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]

    # Calculate the distance from the top metal on the cap contact
    # to the top metal on the end contact.
    set top_met_sep [+ $top_spacing [- $cdif $top_surround]]

    # Diagnostic!
    # puts stdout "cdif = $cdif"
    # puts stdout "top_met_sep = $top_met_sep"

    # Increase end spacing if top metal spacing rule is not met
    set loc_top_spacing $top_spacing
    if {$top_met_sep < $top_space} {
	set loc_top_spacing [+ $loc_top_spacing [- $top_space $top_met_sep]]
    }
    # Diagnostic!
    # puts stdout "loc_top_spacing = $loc_top_spacing"

    if {$upcontact == 1} {
	# Extend bottom metal under contact to right
	box grow e ${loc_top_spacing}um
	set chw [/ ${top_contact_size} 2.0]
	box grow e ${chw}um
	box grow e ${end_surround}um
	paint ${bot_type}
    }

    popbox
    popbox

    if {$upcontact == 1} {
	# Draw contact to right.  Reduce contact extent if devices are not
	# wired together and the top metal spacing rule limits the distance
	set lcont $l
	if {($bconnect == 0) && ($ny > 1)} {
	    if {$cap_spacing < $top_space} {
		set cspace [- $top_space $cap_spacing]
		set lcont [- $l $cspace]
	    }
	}

	pushbox
	box move e ${hw}um
	box move e ${bot_surround}um
	box move e ${loc_top_spacing}um
	set cl [- [+ ${lcont} [* ${bot_surround} 2.0]] [* ${end_surround} 2.0]]
	set cl [- ${cl} ${metal_surround}]  ;# see below
	set cext [sg13g2::unionbox $cext [sg13g2::draw_contact 0 ${cl} \
		${end_surround} ${metal_surround} \
		${top_contact_size} ${bot_type} ${top_contact_type} \
		${top_type} full]]
	# Bottom plate port label
	if {$term_b != ""} {
	    label $term_b c $bot_type
	    select area label
	    port make
	}
	popbox
    }
    popbox

    # Top plate port label
    if {$term_t != ""} {
	label $term_t c $top_type
	select area label
	port make
    }
    return $cext

    # cl shrinks top and bottom to accomodate larger bottom metal
    # surround rule for contacts near a MiM cap.  This should be its
    # own variable, but metal_surround is sufficient.
}

#----------------------------------------------------------------
# Capacitor: Draw the tiled device
#----------------------------------------------------------------

proc sg13g2::cap_draw {parameters} {
    tech unlock *
    set savesnap [snap]
    snap internal

    # Set defaults if they are not in parameters
    set coverlap 0	;# overlap capacitors at contacts
    set guard 0		;# draw a guard ring
    set sandwich 0	;# this is not a plate sandwich capacitor
    set cap_spacing 0	;# abutted caps if spacing is zero
    set cap_diff_spacing 0
    set wide_cap_spacing 0  ;# additional spacing for wide metal rule
    set wide_cap_width 0
    set top_spacing 0
    set end_surround 0
    set bot_surround 0
    set top_width 0
    set end_spacing 0
    set bconnect 0	;# connect bottom plates in array
    set tconnect 0	;# connect top plates in array
    set top_type ""
    set doports 0

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Normalize distance units to microns
    set w [magic::spice2float $w]
    set l [magic::spice2float $l]

    pushbox
    box values 0 0 0 0

    # Determine the base device dimensions by drawing one device
    # while all layers are locked (nothing drawn).  This allows the
    # base drawing routine to do complicated geometry without having
    # to duplicate it here with calculations.

    tech lock *
    if {$sandwich == 1} {
	set bbox [sg13g2::sandwich_cap_device $parameters]
    } else {
	set bbox [sg13g2::cap_device $parameters]
    }
    # puts stdout "Diagnostic: Device bounding box e $bbox (um)"
    tech unlock *

    set fw [- [lindex $bbox 2] [lindex $bbox 0]]
    set fh [- [lindex $bbox 3] [lindex $bbox 1]]
    set lw [+ [lindex $bbox 2] [lindex $bbox 0]]
    set lh [+ [lindex $bbox 3] [lindex $bbox 1]]

    set dwide 0
    if {($fw >= $wide_cap_width) && ($fh >= $wide_cap_width)} {
	set dwide $wide_cap_spacing
    }

    # Determine tile width and height (depends on overlap)
    if {$coverlap == 0} {
        set dy [+ $fh $cap_spacing $dwide]
    } else {
        # overlap at end contact
        set dy [- $fh [+ $end_surround $end_surround $contact_size]]
    }
    # Contact is placed on right so spacing is determined by top_spacing.
    set dx [+ $fw $end_spacing $dwide]

    # Determine core width and height
    set corex [+ [* [- $nx 1] $dx] $fw]
    set corey [+ [* [- $ny 1] $dy] $fh]
    set corellx [/ [+ [- $corex $fw] $lw] 2.0]
    set corelly [/ [+ [- $corey $fh] $lh] 2.0]

    if {$guard != 0} {
	# Calculate guard ring size (measured to contact center)
	set gx [+ $corex [* 2.0 [+ $cap_diff_spacing $diff_surround]] $contact_size]
	set gy [+ $corey [* 2.0 [+ $cap_diff_spacing $diff_surround]] $contact_size]

	# Draw the guard ring first.
	if {$doports} {dict set parameters bulk B}
	sg13g2::guard_ring $gx $gy $parameters

	# NOTE: A guard ring under a MiM cap implies an RF device.
	# The RF MiM cap uses PWELLBLK inside the guard ring area.
	# PWELLBLK is not a drawn layer but can be created with a
	# mask hint.  Put 0.6um space between PWELLBLK and the ring.
	set gedgex [+ $corex [* 2.0 [+ $cap_diff_spacing]]]
	set gedgey [+ $corey [* 2.0 [+ $cap_diff_spacing]]]
	set hgx [- [/ $gedgex 2.0] 0.6]
	set hgy [- [/ $gedgey 2.0] 0.6]
	set guardbox {}
	lappend guardbox [magic::u2i -$hgx]
	lappend guardbox [magic::u2i -$hgy]
	lappend guardbox [magic::u2i $hgx]
	lappend guardbox [magic::u2i $hgy]
	property MASKHINTS_PWELLBLK "$guardbox"
	# Erase pwell from this area
	pushbox
	box values {*}$guardbox
	erase pwell
	popbox
    }

    set twidth [+ ${contact_size} ${end_surround} ${end_surround}]
    if {${twidth} < ${top_width}} {
	set twidth ${top_width}
    }
    set hmw [/ $twidth 2.0]
    set hdy [/ $dy 2.0]
    set cdx [+ [/ ${w} 2.0] ${bot_surround} ${end_spacing}]

    pushbox
    box move w ${corellx}um
    box move s ${corelly}um
    for {set xp 0} {$xp < $nx} {incr xp} {
	pushbox
	for {set yp 0} {$yp < $ny} {incr yp} {
	    if {$doports} {
		if {($ny == 1) && ($nx == 1)} {
		    dict set parameters term_t C1
		    dict set parameters term_b C2
		} elseif {$ny == 1} {
		    dict set parameters term_t C1_$xp
		    dict set parameters term_b C2_$xp
		} elseif {$nx == 1} {
		    if {$tconnect} {
			dict set parameters term_t C1
		    } else {
			dict set parameters term_t C1_$yp
		    }
		    if {$bconnect} {
			dict set parameters term_b C2
		    } else {
			dict set parameters term_b C2_$yp
		    }
		} else {
		    if {$tconnect} {
			dict set parameters term_t C1_$xp
		    } else {
			dict set parameters term_t C1_${xp}_$yp
		    }
		    if {$bconnect} {
			dict set parameters term_b C2_$xp
		    } else {
			dict set parameters term_b C2_${xp}_$yp
		    }
		}
		if {$yp > 0} {
		    if {$tconnect} {dict set parameters term_t ""}
		    if {$bconnect} {dict set parameters term_b ""}
		}
	    }

	    sg13g2::cap_device $parameters
	    if {$ny > 1} {
		pushbox
		box grow e ${hmw}um
		box grow w ${hmw}um
		box grow n ${hdy}um
		box grow s ${hdy}um
		if {($top_type != "") && ($tconnect == 1)} {
		    paint ${top_type}
		}
		if {($top_type != "") && ($bconnect == 1)} {
		    box move e ${cdx}um
		    paint ${top_type}
		}
		popbox
	    }
            box move n ${dy}um
        }
	popbox
        box move e ${dx}um
    }
    popbox
    popbox

    snap $savesnap
    tech revert
}

#----------------------------------------------------------------

proc sg13g2::cap_cmim_draw {parameters} {
    set newdict [dict create \
	    top_type 		m6 \
	    top_contact_type	via5 \
	    cap_type 		mimcap \
	    cap_contact_type	mimcc \
	    bot_type 		m5 \
	    bot_surround	0.6 \
	    cap_spacing		1.2 \
	    cap_surround	0.2 \
	    top_surround	0.0 \
	    end_surround	0.1 \
	    metal_surround	0.32 \
	    top_contact_size    0.62 \
	    bot_width           0.20 \
	    bot_spacing         0.21 \
	    top_width           1.64 \
	    top_spacing         1.64 \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::cap_draw $drawdict]
}

proc sg13g2::cap_rfcmim_draw {parameters} {
    set newdict [dict create \
	    guard		1 \
	    top_type 		m6 \
	    top_contact_type	via5 \
	    cap_type 		mimcap \
	    cap_contact_type	mimcc \
	    bot_type 		m5 \
	    bot_surround	0.6 \
	    cap_spacing		1.2 \
	    cap_surround	0.2 \
	    top_surround	0.0 \
	    end_surround	0.1 \
	    metal_surround	0.32 \
	    top_contact_size    0.62 \
	    bot_width           0.20 \
	    bot_spacing         0.21 \
	    top_width           1.64 \
	    top_spacing         1.64 \
	    cap_diff_spacing	3.00 \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::cap_draw $drawdict]
}

#----------------------------------------------------------------
# capacitor: Check device parameters for out-of-bounds values
#----------------------------------------------------------------

proc sg13g2::cap_check {parameters} {
    # In case wmax and/or lmax are undefined
    set lmax 0
    set wmax 0
    set ccov 100

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Normalize distance units to microns
    set l [magic::spice2float $l]
    set l [magic::3digitpastdecimal $l] 
    set w [magic::spice2float $w]
    set w [magic::3digitpastdecimal $w] 

    set val   [magic::spice2float $val]
    set carea [magic::spice2float $carea]
    set cperi [magic::spice2float $cperi]
    set dc    [magic::spice2float $dc]

    if {$square == 1} {
        # Calculate L and W from value
	set a $carea
	set b [expr $cperi * 4]
	set c [expr -4 * $dc - $val]
	set l [expr ((-$b + sqrt($b * $b - (4 * $a * $c))) / (2 * $a))]
	dict set parameters l [magic::float2spice $l]
	set w $l
	dict set parameters w [magic::float2spice $w]
    } elseif {$l == 0} {
        # Calculate L from W and value
	set l [expr (($val + 4 * $dc - 2 * $w * $cperi) / ($w * $carea + 2 * $cperi))]
	dict set parameters l [magic::float2spice $l]
    } elseif {$w == 0} {
        # Calculate W from L and value
	set w [expr (($val + 4 * $dc - 2 * $l * $cperi) / ($l * $carea + 2 * $cperi))]
	dict set parameters w [magic::float2spice $w]
    }
    if {$w < $wmin} {
	puts stderr "Capacitor width must be >= $wmin"
	dict set parameters w $wmin
	set w $wmin
    } 
    if {$l < $lmin} {
	puts stderr "Capacitor length must be >= $lmin"
	dict set parameters l $lmin
	set l $lmin
    } 
    if {($wmax > 0) && ($w > $wmax)} {
	puts stderr "Capacitor width must be <= $wmax"
	dict set parameters w $wmax
	set w $wmax
    } 
    if {($lmax > 0) && ($l > $lmax)} {
	puts stderr "Capacitor length must be <= $lmax"
	dict set parameters l $lmax
	set l $lmax
    } 
    if {[catch {expr abs($ccov)}]} {
	puts stderr "Capacitor contact coverage must be numeric!"
        dict set parameters ccov 100
    } elseif {[expr abs($ccov)] > 100} {
	puts stderr "Capaitor contact coverage can't be more than 100%"
        dict set parameters ccov 100
    }

    # Calculate value from L and W
    set cval [expr ($l * $w * $carea + 2 * ($l + $w) * $cperi - 4 * $dc)]
    dict set parameters val [magic::float2spice $cval]
    sg13g2::compute_ctot $parameters

    return $parameters
}

proc sg13g2::cap_cmim_check {parameters} {
    return [sg13g2::cap_check $parameters]
}

proc sg13g2::cap_rfcmim_check {parameters} {
    return [sg13g2::cap_check $parameters]
}

#----------------------------------------------------------------
# Drawn resistors
#----------------------------------------------------------------

#----------------------------------------------------------------
# Resistor defaults:
#----------------------------------------------------------------
# User editable values:
#
#  val   Resistor value in ohms
#  w	 Width
#  l	 Length
#  t	 Number of turns
#  m	 Number devices in Y
#  nx	 Number devices in X
#  snake Use snake geometry (if not present, snake geometry not allowed)
#  dummy Flag to mark addition of dummy resistor
#
# Non-user editable values:
#
#  wmin  Minimum allowed width
#  lmin  Minimum allowed length
#  rho	 Resistance in ohms per square
#  dw    Delta width
#  term  Resistance per terminal
#  sterm Additional resistance per terminal for snake geometry
#----------------------------------------------------------------

#----------------------------------------------------------------
# rsil: Specify all user-editable default values and those
# needed by rp1_check
#----------------------------------------------------------------

proc sg13g2::rsil_defaults {} {
    return {w 0.50 l 2.00 m 1 nx 1 wmin 0.50 lmin 0.50 class resistor \
		rho 7.0 val 14.0 dummy 0 dw 0.0 term 0.0 \
		sterm 0.0 caplen 0 snake 0 guard 1 \
		glc 1 grc 1 gtc 1 gbc 1 roverlap 0 endcov 100 \
		full_metal 1 hv_guard 0 n_guard 0 vias 1 \
		viagb 0 viagt 0 viagl 0 viagr 0 doports 1}
}

proc sg13g2::rppd_defaults {} {
    return {w 0.50 l 2.00 m 1 nx 1 wmin 0.50 lmin 0.50 class resistor \
		rho 260.0 val 520.0 dummy 0 dw 0.0 term 0.0 \
		sterm 0.0 caplen 0 guard 1 glc 1 grc 1 gtc 1 gbc 1 \
		snake 0 full_metal 1 vias 1 n_guard 0 hv_guard 0 \
		viagb 0 viagt 0 viagl 0 viagr 0 doports 1}
}

proc sg13g2::rhigh_defaults {} {
    return {w 0.50 l 2.00 m 1 nx 1 wmin 0.50 lmin 0.50 class resistor \
		rho 1360 val 2720.0 dummy 0 dw 0.0 term 0.0 \
		sterm 0.0 caplen 0 \
		guard 1 glc 1 grc 1 gtc 1 gbc 1 \
		snake 0 full_metal 1 n_guard 0 hv_guard 0 vias 1 \
		viagb 0 viagt 0 viagl 0 viagr 0 doports 1}
}

#----------------------------------------------------------------
# metal resistors
#----------------------------------------------------------------
# rm*: Specify all user-editable default values and those needed
# by rm*_check
#----------------------------------------------------------------

proc sg13g2::rm1_defaults {} {
    return {w 0.160 l 0.100 m 1 nx 1 wmin 0.16 lmin 0.005 class resistor \
		compatible {rm1 rm2 rm3 rm4 rm5 rm6 rm7} \
		rho 0.110 val 0.069 dummy 0 dw 0.0 term 0.0 \
		roverlap 0 doports 1}
}

proc sg13g2::rm2_defaults {} {
    return {w 0.200 l 0.100 m 1 nx 1 wmin 0.20 lmin 0.005 class resistor \
		compatible {rm1 rm2 rm3 rm4 rm5 rm6 rm7} \
		rho 0.088 val 0.044 dummy 0 dw 0.0 term 0.0 \
		roverlap 0 doports 1}
}

proc sg13g2::rm3_defaults {} {
    return {w 0.200 l 0.100 m 1 nx 1 wmin 0.20 lmin 0.005 class resistor \
		compatible {rm1 rm2 rm3 rm4 rm5 rm6 rm7} \
		rho 0.088 val 0.044 dummy 0 dw 0.0 term 0.0 \
		roverlap 0 doports 1}
}

proc sg13g2::rm4_defaults {} {
    return {w 0.200 l 0.100 m 1 nx 1 wmin 0.20 lmin 0.005 class resistor \
		compatible {rm1 rm2 rm3 rm4 rm5 rm6 rm7} \
		rho 0.088 val 0.044 dummy 0 dw 0.0 term 0.0 \
		roverlap 0 doports 1}
}

proc sg13g2::rm5_defaults {} {
    return {w 0.200 l 0.100 m 1 nx 1 wmin 0.20 lmin 0.005 class resistor \
		compatible {rm1 rm2 rm3 rm4 rm5 rm6 rm7} \
		rho 0.088 val 0.044 dummy 0 dw 0.0 term 0.0 \
		roverlap 0 doports 1}
}

proc sg13g2::rm6_defaults {} {
    return {w 1.640 l 0.100 m 1 nx 1 wmin 1.64 lmin 0.005 class resistor \
		compatible {rm1 rm2 rm3 rm4 rm5 rm6 rm7} \
		rho 0.018 val 0.001098 dummy 0 dw 0.0 term 0.0 \
		roverlap 0 doports 1}
}

proc sg13g2::rm7_defaults {} {
    return {w 2.000 l 0.100 m 1 nx 1 wmin 2.00 lmin 0.005 class resistor \
		compatible {rm1 rm2 rm3 rm4 rm5 rm6 rm7} \
		rho 0.011 val 0.00055 dummy 0 dw 0.0 term 0.0 \
		roverlap 0 doports 1}
}

#----------------------------------------------------------------
# resistor: Conversion from SPICE netlist parameters to toolkit
#----------------------------------------------------------------

proc sg13g2::res_convert {parameters} {
    set pdkparams [dict create]
    dict for {key value} $parameters {
	switch -nocase $key {
	    l -
	    w {
		# Length and width are converted to units of microns
		set value [magic::spice2float $value]
		# set value [expr $value * 1e6]
		set value [magic::3digitpastdecimal $value]
		dict set pdkparams [string tolower $key] $value
	    }
	    default {
		# Allow unrecognized parameters to be passed unmodified
		dict set pdkparams $key $value
	    }
	}
    }
    return $pdkparams
}

#----------------------------------------------------------------

proc sg13g2::rsil_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rppd_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rhigh_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rm1_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rm2_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rm3_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rm4_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rm5_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rm6_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

proc sg13g2::rm7_convert {parameters} {
    return [sg13g2::res_convert $parameters]
}

#----------------------------------------------------------------
# resistor: Interactively specifies the fixed layout parameters
#----------------------------------------------------------------

proc sg13g2::res_dialog {device parameters} {
    # Editable fields:      w, l, t, nx, m, val
    # Checked fields:  

    magic::add_entry val "Value (ohms)" $parameters
    if {[dict exists $parameters snake]} {
	sg13g2::compute_ltot $parameters
	magic::add_message ltot "Total length (um)" $parameters
    }
    magic::add_entry l "Length (um)" $parameters
    magic::add_entry w "Width (um)" $parameters
    magic::add_entry nx "X Repeat" $parameters
    magic::add_entry m "Y Repeat" $parameters
    if {[dict exists $parameters endcov]} {
	magic::add_entry endcov "End contact coverage (%)" $parameters
    }

    if {[dict exists $parameters compatible]} {
	set sellist [dict get $parameters compatible]
	magic::add_selectlist gencell "Device type" $sellist $parameters $device
    }

    # magic::add_checkbox dummy "Add dummy" $parameters

    if {[dict exists $parameters snake]} {
	magic::add_checkbox snake "Use snake geometry" $parameters
    }
    if {[dict exists $parameters roverlap]} {
	if {[dict exists $parameters endcov]} {
            magic::add_checkbox roverlap "Overlap at end contact" $parameters
	} else {
            magic::add_checkbox roverlap "Overlap at ends" $parameters
	}
    }
    if {[dict exists $parameters guard]} {
	magic::add_checkbox guard "Add guard ring" $parameters

    	if {[dict exists $parameters hv_guard]} {
	    magic::add_checkbox hv_guard "High-voltage guard ring" $parameters
	}
	if {[dict exists $parameters n_guard]} {
	    magic::add_checkbox n_guard "N-well connected guard ring" $parameters
	}
	if {[dict exists $parameters full_metal]} {
	    magic::add_checkbox full_metal "Full metal guard ring" $parameters
	}
	if {[dict exists $parameters glc]} {
	    magic::add_checkbox glc "Add left guard ring contact" $parameters
	}
	if {[dict exists $parameters grc]} {
	    magic::add_checkbox grc "Add right guard ring contact" $parameters
	}
	if {[dict exists $parameters gtc]} {
	    magic::add_checkbox gtc "Add top guard ring contact" $parameters
	}
	if {[dict exists $parameters gbc]} {
	    magic::add_checkbox gbc "Add bottom guard ring contact" $parameters
	}


    	magic::add_entry viagb "Bottom guard ring via coverage \[+/-\](%)" $parameters
    	magic::add_entry viagt "Top guard ring via coverage \[+/-\](%)" $parameters
    	magic::add_entry viagr "Right guard ring via coverage \[+/-\](%)" $parameters
    	magic::add_entry viagl "Left guard ring via coverage \[+/-\](%)" $parameters
    }

    if {[dict exists $parameters vias]} {
	magic::add_checkbox vias "Add vias over contacts" $parameters
    }

    if {[dict exists $parameters snake]} {
       magic::add_dependency sg13g2::res_recalc $device sg13g2 l w val nx snake
    } else {
       magic::add_dependency sg13g2::res_recalc $device sg13g2 l w val nx
    }

    if {[dict exists $parameters addports]} {
	magic::add_checkbox doports "Add ports" $parameters
    }
}

#----------------------------------------------------------------

proc sg13g2::rsil_dialog {parameters} {
    sg13g2::res_dialog rsil $parameters
}

proc sg13g2::rppd_dialog {parameters} {
    sg13g2::res_dialog rppd $parameters
}

proc sg13g2::rhigh_dialog {parameters} {
    sg13g2::res_dialog rhigh $parameters
}

proc sg13g2::rm1_dialog {parameters} {
    sg13g2::res_dialog rm1 $parameters
}

proc sg13g2::rm2_dialog {parameters} {
    sg13g2::res_dialog rm2 $parameters
}

proc sg13g2::rm3_dialog {parameters} {
    sg13g2::res_dialog rm3 $parameters
}

proc sg13g2::rm4_dialog {parameters} {
    sg13g2::res_dialog rm4 $parameters
}

proc sg13g2::rm5_dialog {parameters} {
    sg13g2::res_dialog rm5 $parameters
}

proc sg13g2::rm6_dialog {parameters} {
    sg13g2::res_dialog rm6 $parameters
}

proc sg13g2::rm7_dialog {parameters} {
    sg13g2::res_dialog rm7 $parameters
}

#----------------------------------------------------------------
# Resistor: Draw a single device in straight geometry
#----------------------------------------------------------------

proc sg13g2::res_device {parameters} {
    # Epsilon for avoiding round-off errors
    set eps  0.0005

    # Set local default values if they are not in parameters
    set doports 0		;# no port labels by default
    set endcov 0	 	;# percent coverage of end contacts
    set roverlap 0		;# overlap resistors at end contacts
    set well_res_overlap 0 	;# not a well resistor
    set end_contact_type ""	;# no contacts for metal resistors
    set end_overlap_cont 0	;# additional end overlap on sides
    set vias 0			;# add vias over contacts
    set l_delta 0		;# delta between measured and drawn length
    set res_idtype none

    set term_t ""
    set term_b ""

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    if {![dict exists $parameters end_contact_size]} {
	set end_contact_size $contact_size
    }

    # Modify drawn length by the delta length
    set l [+ $l [* $l_delta 2.0]]

    # Draw the resistor and endcaps
    pushbox
    box size 0 0
    pushbox
    set hw [/ $w 2.0]
    set hl [/ $l 2.0]
    box grow n ${hl}um
    box grow s ${hl}um
    box grow e ${hw}um
    box grow w ${hw}um

    pushbox
    box grow n ${res_to_endcont}um
    box grow s ${res_to_endcont}um
    if {$well_res_overlap > 0} {
	set well_extend [+ ${well_res_overlap} [/ ${end_contact_size} 2.0] ${end_surround}]
	box grow n ${well_extend}um
	box grow s ${well_extend}um
	paint ${well_res_type}
    } else {
	paint ${end_type}
    }
    set cext [sg13g2::getbox]
    popbox

    if {$well_res_overlap > 0} {
	erase ${well_res_type}
    } else {
	erase ${end_type}
    }
    paint ${res_type}
    if {"$res_idtype" != "none"} {
	box grow c 2
	paint ${res_idtype}
    }
    popbox

    # Reduce contact sizes by (end type) surround so that
    # the contact area edges match the device type width.
    # (Minimum dimensions will be enforced by the contact drawing routine)
    set epl [- ${w} [* ${end_surround} 2]]     	    ;# end contact width

    # Reduce end material size for well resistor types
    if {$well_res_overlap > 0} {
	set epl [- ${epl} [* ${well_res_overlap} 2]]
    }

    # Reduce by coverage percentage unless overlapping at contacts
    if {(${roverlap} == 0) && (${endcov} > 0)} {
	set cpl [* ${epl} [/ ${endcov} 100.0]]
    } else {
	set cpl $epl
    }

    # Ensure additional overlap of diffusion contact if required
    set dov [* ${end_overlap_cont} 2]
    if {[- ${epl} ${cpl}] < $dov} {
	set cpl [- ${epl} $dov]    ;# additional end contact width
    }

    set hepl [+ [/ ${epl} 2.0] ${end_surround}]
    set hesz [/ ${end_contact_size} 2.0]

    # LV substrate diffusion types have a different surround requirement
    set lv_sub_types {"psd" "nsd"}
    if {[lsearch $lv_sub_types $end_type] < 0} {
	set hesz [+ ${hesz} ${end_surround}]
    }

    # Top end material & contact
    pushbox
    box move n ${hl}um
    box move n ${res_to_endcont}um

    pushbox
    box size 0 0
    box grow n ${hesz}um
    box grow s ${hesz}um
    box grow e ${hepl}um
    box grow w ${hepl}um
    paint ${end_type}
    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]
    popbox

    if {$term_t != ""} {
	label $term_t c $end_type
	select area label
	port make
    }
    if {${end_contact_type} != ""} {
	# Draw via over contact first
	if {$vias != 0} {
            pushbox
            set ch $res_to_endcont
    	    if {$ch < $via_size} {set ch $via_size}
    	    set cw $epl
    	    if {$cw < $via_size} {set cw $via_size}
	    box grow n [/ $via_size 2]um
	    box grow s [- $ch [/ $via_size 2]]um
	    box grow w [/ $cw 2]um
	    box grow e [/ $cw 2]um
            sg13g2::via1_draw
            popbox
    	}
	set cext [sg13g2::unionbox $cext [sg13g2::draw_contact ${cpl} 0 \
		${end_surround} ${metal_surround} \
		${end_contact_size} ${end_type} ${end_contact_type} m1 horz]]
    }
    popbox

    # Bottom end material & contact
    pushbox
    box move s ${hl}um
    box move s ${res_to_endcont}um

    pushbox
    box size 0 0
    box grow n ${hesz}um
    box grow s ${hesz}um
    box grow e ${hepl}um
    box grow w ${hepl}um
    paint ${end_type}
    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]
    popbox

    if {$term_b != ""} {
	label $term_b c $end_type
	select area label
	port make
    }
    if {${end_contact_type} != ""} {
	# Draw via over contact first
	if {$vias != 0} {
            pushbox
            set ch $res_to_endcont
    	    if {$ch < $via_size} {set ch $via_size}
    	    set cw $epl
    	    if {$cw < $via_size} {set cw $via_size}
	    box grow n [- $ch [/ $via_size 2]]um
	    box grow s [/ $via_size 2]um
	    box grow w [/ $cw 2]um
	    box grow e [/ $cw 2]um
            sg13g2::via1_draw
            popbox
    	}
	set cext [sg13g2::unionbox $cext [sg13g2::draw_contact ${cpl} 0 \
		${end_surround} ${metal_surround} \
		${end_contact_size} ${end_type} ${end_contact_type} m1 horz]]
    }
    popbox

    popbox
    return $cext
}

#----------------------------------------------------------------
# Resistor: Draw a single device in snake geometry
#----------------------------------------------------------------

proc sg13g2::res_snake_device {nf parameters} {
    # nf is the number of fingers of the snake geometry

    # Epsilon for avoiding round-off errors
    set eps  0.0005

    # Set local default values if they are not in parameters
    set doports 0		;# no port labels by default
    set endcov 100	 	;# percent coverage of end contacts
    set vias 0			;# add vias over terminal contacts
    set well_res_overlap 0 	;# not a well resistor
    set end_contact_type ""	;# no contacts for metal resistors
    set mask_clearance 0	;# additional length to clear mask

    set term_t ""
    set term_b ""

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    if {![dict exists $parameters end_contact_size]} {
	set end_contact_size $contact_size
    }

    # Compute half width and length
    set hw [/ $w 2.0]
    set hl [/ $l 2.0]

    # Reduce contact sizes by (end type) surround so that
    # the contact area edges match the device type width.
    # (Minimum dimensions will be enforced by the contact drawing routine)
    set epl [- ${w} [* ${end_surround} 2]]     	    ;# end contact width

    # Reduce contact size for well resistor types
    if {$well_res_overlap > 0} {
	set epl [- ${epl} [* ${well_res_overlap} 2]]
    }

    # Reduce contact part of end by coverage percentage
    if {${endcov} > 0} {
	set cpl [* ${epl} [/ ${endcov} 100.0]]
    } else {
	set cpl $epl
    }

    set hepl [+ [/ ${epl} 2.0] ${end_surround}]
    set hesz [+ [/ ${end_contact_size} 2.0] ${end_surround}]

    pushbox
    box size 0 0	;# Position is taken from caller

    # Front end contact (always bottom)
    pushbox
    box move s ${hl}um
    pushbox
    box move s ${mask_clearance}um
    box move s ${res_to_endcont}um

    pushbox
    box size 0 0
    box grow n ${hesz}um
    box grow s ${hesz}um
    box grow e ${hepl}um
    box grow w ${hepl}um
    paint ${end_type}
    set cext [sg13g2::getbox]
    popbox

    if {$term_t != ""} {
	label $term_t c $end_type
	select area label
	port make
    }
    if {${end_contact_type} != ""} {
        # Draw via over contact first
	if {$vias != 0} {
	    pushbox
	    set ch $res_to_endcont
	    if {$ch < $via_size} {set ch $via_size}
	    set cw $epl
	    if {$cw < $via_size} {set cw $via_size}
	    box grow n [- $ch [/ $via_size 2]]um
	    box grow s [/ $via_size 2]um
	    box grow w [/ $cw 2]um
	    box grow e [/ $cw 2]um
	    sg13g2::via1_draw
	    popbox
	}
	set cext [sg13g2::draw_contact ${cpl} 0 \
		${end_surround} ${metal_surround} \
		${end_contact_size} ${end_type} ${end_contact_type} m1 horz]
    }
    popbox

    # Draw portion between resistor end and contact.
    box grow e ${hw}um
    box grow w ${hw}um
    pushbox
    box grow s ${mask_clearance}um
    if {${mask_clearance} > 0} {
        paint ${res_type}
    }
    popbox
    box move s ${mask_clearance}um
    box grow s ${res_to_endcont}um
    if {$well_res_overlap > 0} {
	set well_extend [+ ${well_res_overlap} [/ ${end_contact_size} 2.0] ${end_surround}]
	box grow s ${well_extend}um
	paint ${well_res_type}
    } else {
	paint ${end_type}
    }
    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]
    popbox

    # Draw the resistor and endcaps
    pushbox
    box grow n ${hl}um
    box grow s ${hl}um
    box grow e ${hw}um
    box grow w ${hw}um

    # Capture these extents in the bounding box in case both contacts
    # are on one side.
    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]

    set deltax [+ ${res_spacing} ${w}]
    set deltay [- ${l} ${w}]
    for {set i 0} {$i < [- $nf 1]} {incr i} {
	# Really should be drawing endcaps last instead of working around 1st one
	if {($i == 0) && (${mask_clearance} < 0)} {
	    pushbox
	    box grow s ${mask_clearance}um
	    paint ${res_type}
	    popbox
	} else {
	    paint ${res_type}
	}
 	pushbox
	if {[% $i 2] == 0} {
	    box move n ${deltay}um
	}
	box height ${w}um
	box width ${deltax}um
	paint ${res_type}
 	popbox
	box move e ${deltax}um
    }
    paint ${res_type}
    # Capture these extents in the bounding box
    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]
    popbox

    # Move box to last finger
    set lastf [* [- $nf 1] $deltax]
    box move e ${lastf}um

    # Back-end contact (top or bottom, depending if odd or even turns)
    pushbox

    if {[% $nf 2] == 1} {
	set dir n
    } else {
	set dir s
    }
    box move $dir ${hl}um
    pushbox
    box move $dir ${mask_clearance}um
    box move $dir ${res_to_endcont}um

    pushbox
    box size 0 0
    box grow n ${hesz}um
    box grow s ${hesz}um
    box grow e ${hepl}um
    box grow w ${hepl}um
    paint ${end_type}
    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]
    popbox

    if {$term_b != ""} {
	label $term_b c $end_type
	select area label
	port make
    }
    if {${end_contact_type} != ""} {
	# Draw via over contact first
	if {$vias != 0} {
	    pushbox
	    set ch $res_to_endcont
	    if {$ch < $via_size} {set ch $via_size}
	    set cw $epl
	    if {$cw < $via_size} {set cw $via_size}
	    if {$dir == "n"} {
		box grow n [/ $via_size 2]um
		box grow s [- $ch [/ $via_size 2]]um
	    } else {
		box grow n [- $ch [/ $via_size 2]]um
		box grow s [/ $via_size 2]um
	    }
	    box grow w [/ $cw 2]um
	    box grow e [/ $cw 2]um
	    sg13g2::via1_draw
	    popbox
	}
	set cext [sg13g2::unionbox $cext [sg13g2::draw_contact ${cpl} 0 \
		${end_surround} ${metal_surround} \
		${end_contact_size} ${end_type} ${end_contact_type} m1 horz]]
    }
    popbox
    # Draw portion between resistor end and contact.
    box grow e ${hw}um
    box grow w ${hw}um
    pushbox
    box grow $dir ${mask_clearance}um
    if {${mask_clearance} > 0} {
        paint ${res_type}
    }
    popbox
    box move $dir ${mask_clearance}um
    box grow $dir ${res_to_endcont}um

    if {$well_res_overlap > 0} {
	set well_extend [+ ${well_res_overlap} [/ ${end_contact_size} 2.0] ${end_surround}]
	box grow $dir ${well_extend}um
	paint ${well_res_type}
    } else {
	paint ${end_type}
    }
    popbox

    popbox
    return $cext
}

#----------------------------------------------------------------
# Resistor: Draw the tiled device
#----------------------------------------------------------------

proc sg13g2::res_draw {parameters} {
    tech unlock *
    set savesnap [snap]
    snap internal

    # Set defaults if they are not in parameters
    set snake 0		;# some resistors don't allow snake geometry
    set roverlap 0	;# overlap resistors at contacts
    set guard 0		;# draw a guard ring
    set plus_diff_type   nsd	;# guard ring diffusion type
    set overlap_compress 0	;# special Y distance compression
    set well_res_overlap 0	;# additional well extension behind contact
    set res_diff_spacing 0	;# spacing from resistor to diffusion
    set res_idtype  none
    set doports 0

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # For devices where inter-device space is smaller than device-to-guard ring
    if {![dict exists $parameters end_to_end_space]} {
	set end_to_end_space $end_spacing
    }

    if {![dict exists $parameters end_contact_size]} {
	set end_contact_size $contact_size
    }

    # Normalize distance units to microns
    set w [magic::spice2float $w]
    set l [magic::spice2float $l]

    pushbox
    box values 0 0 0 0

    # Determine the base device dimensions by drawing one device
    # while all layers are locked (nothing drawn).  This allows the
    # base drawing routine to do complicated geometry without having
    # to duplicate it here with calculations.

    tech lock *
    set nf $nx
    if {($snake == 1) && ($nx == 1)} {set snake 0}
    if {$snake == 1} {
	set bbox [sg13g2::res_snake_device $nf $parameters]
	set nx 1
    } else {
	set bbox [sg13g2::res_device $parameters]
    }
    # puts stdout "Diagnostic: Device bounding box e $bbox (um)"
    tech unlock *

    set fw [- [lindex $bbox 2] [lindex $bbox 0]]
    set fh [- [lindex $bbox 3] [lindex $bbox 1]]
    set lw [+ [lindex $bbox 2] [lindex $bbox 0]]
    set lh [+ [lindex $bbox 3] [lindex $bbox 1]]

    # Determine tile width and height (depends on overlap)
    # Snake resistors cannot overlap.
    # However, snake resistors with an odd number of fingers can
    # compress the space if overlap_compress is defined

    if {($roverlap == 1) && ($snake == 1) && ([% $nf 2] == 1) && ($m > 1)} {
        set dy [- $fh $overlap_compress]
    } elseif {($roverlap == 0) || ($snake == 1)} {
        set dy [+ $fh $end_to_end_space]
    } else {
        # overlap poly
        set dy [- $fh [+ [* [+ $end_surround $well_res_overlap] 2.0] $end_contact_size]]
    }
    set dx [+ $fw $res_spacing]

    # Determine core width and height
    set corex [+ [* [- $nx 1] $dx] $fw]
    set corey [+ [* [- $m 1] $dy] $fh]
    set corellx [/ [+ [- $corex $fw] $lw] 2.0]
    set corelly [/ [+ [- $corey $fh] $lh] 2.0]

    set lv_sub_types {"psd" "nsd"}
    if {[lsearch $lv_sub_types $plus_diff_type] >= 0} {
	set guard_diff_surround 0
    } else {
	set guard_diff_surround ${diff_surround}
    }

    if {$guard != 0} {
	# Calculate guard ring size (measured to contact center)
	set gx [+ $corex [* 2.0 [+ $res_diff_spacing $guard_diff_surround]] $contact_size]
	set gy [+ $corey [* 2.0 [+ $end_spacing $guard_diff_surround]] $contact_size]

	# Draw the guard ring first, because well resistors are on the substrate plane
	if {$doports} {dict set parameters bulk B}
	sg13g2::guard_ring $gx $gy $parameters
    }

    pushbox
    box move w ${corellx}um
    box move s ${corelly}um
    # puts "Device position at = [sg13g2::getbox]"
    for {set xp 0} {$xp < $nx} {incr xp} {
	pushbox
	for {set yp 0} {$yp < $m} {incr yp} {
	    if {$doports} {
		if {($m == 1) && ($nx == 1)} {
		     dict set parameters term_t R1
		     dict set parameters term_b R2
		} elseif {$m == 1} {
		     dict set parameters term_t R1_$xp
		     dict set parameters term_b R2_$xp
		} elseif {$nx == 1} {
		     dict set parameters term_t R1_$yp
		     dict set parameters term_b R2_$yp
		} else {
		     dict set parameters term_t R1_${xp}_$yp
		     dict set parameters term_b R2_${xp}_$yp
		}
	    }
	    if {$snake == 1} {
		sg13g2::res_snake_device $nf $parameters
	    } else {
		sg13g2::res_device $parameters
	    }
            box move n ${dy}um
        }
	popbox
        box move e ${dx}um
    }
    popbox
    popbox

    snap $savesnap
    tech revert
}

#----------------------------------------------------------------

proc sg13g2::rsil_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    # Handle options related to guard ring type (high/low voltage, nwell/psub)
    if {[dict exists $parameters hv_guard]} {
	set use_hv_guard [dict get $parameters hv_guard]
    } else {
	set use_hv_guard 0
    }
    if {[dict exists $parameters n_guard]} {
	set use_n_guard [dict get $parameters n_guard]
    } else {
	set use_n_guard 0
    }

    if {$use_hv_guard == 1} {
	if {$use_n_guard == 1} {
	    set gdifftype hvnsd
	    set gdiffcont hvnsc
	} else {
	    set gdifftype hvpsd
	    set gdiffcont hvpsc
	}
	set gsurround 0.33
    } else {
	if {$use_n_guard == 1} {
	    set gdifftype nsd
	    set gdiffcont nsc
	} else {
	    set gdifftype psd
	    set gdiffcont psc
	}
	set gsurround $sub_surround
    }
    if {$use_n_guard == 1} {
	set gsubtype nwell
    } else {
	set gsubtype psub
    }

    set newdict [dict create \
	    res_type		nres \
	    end_type 		poly \
	    end_contact_type	pc \
	    plus_diff_type	$gdifftype \
	    plus_contact_type	$gdiffcont \
	    sub_type		$gsubtype \
	    guard_sub_surround	$gsurround \
	    end_surround	$poly_surround \
	    end_spacing		0.48 \
	    end_to_end_space	0.52 \
	    res_to_cont		0.575 \
	    res_to_endcont	0.2 \
	    res_spacing		$poly_spacing \
	    res_diff_spacing	0.48 \
	    mask_clearance	0.0 \
	    overlap_compress	0.36 \
    ]

    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rppd_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    # Handle options related to guard ring type (high/low voltage, nwell/psub)
    if {[dict exists $parameters hv_guard]} {
	set use_hv_guard [dict get $parameters hv_guard]
    } else {
	set use_hv_guard 0
    }
    if {[dict exists $parameters n_guard]} {
	set use_n_guard [dict get $parameters n_guard]
    } else {
	set use_n_guard 0
    }

    if {$use_hv_guard == 1} {
	if {$use_n_guard == 1} {
	    set gdifftype hvnsd
	    set gdiffcont hvnsc
	} else {
	    set gdifftype hvpsd
	    set gdiffcont hvpsc
	}
	set gsurround 0.33
    } else {
	if {$use_n_guard == 1} {
	    set gdifftype nsd
	    set gdiffcont nsc
	} else {
	    set gdifftype psd
	    set gdiffcont psc
	}
	set gsurround $sub_surround
    }
    if {$use_n_guard == 1} {
	set gsubtype nwell
	set gresdiff_spacing 0.785
	set gresdiff_end 0.525
    } else {
	set gsubtype psub
	set gresdiff_spacing 0.48
	set gresdiff_end 0.48
    }

    set newdict [dict create \
	    res_type		pres \
	    end_type 		poly \
	    end_contact_type	pc \
	    plus_diff_type	$gdifftype \
	    plus_contact_type	$gdiffcont \
	    sub_type		$gsubtype \
	    guard_sub_surround	$gsurround \
	    end_surround	$poly_surround \
	    end_spacing		$gresdiff_end \
	    end_to_end_space	0.52 \
	    res_to_cont		0.575 \
	    res_to_endcont	0.28 \
	    res_spacing		0.48 \
	    res_diff_spacing	$gresdiff_spacing \
	    mask_clearance	0.5 \
	    overlap_compress	0.36 \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rhigh_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    # Handle options related to guard ring type (high/low voltage, nwell/psub)
    if {[dict exists $parameters hv_guard]} {
	set use_hv_guard [dict get $parameters hv_guard]
    } else {
	set use_hv_guard 0
    }
    if {[dict exists $parameters n_guard]} {
	set use_n_guard [dict get $parameters n_guard]
    } else {
	set use_n_guard 0
    }

    if {$use_hv_guard == 1} {
	if {$use_n_guard == 1} {
	    set gdifftype hvnsd
	    set gdiffcont hvnsc
	} else {
	    set gdifftype hvpsd
	    set gdiffcont hvpsc
	}
	set gsurround 0.33
    } else {
	if {$use_n_guard == 1} {
	    set gdifftype nsd
	    set gdiffcont nsc
	} else {
	    set gdifftype psd
	    set gdiffcont psc
	}
	set gsurround $sub_surround
    }
    if {$use_n_guard == 1} {
	set gsubtype nwell
	set gresdiff_spacing 0.785
	set gresdiff_end 0.525
    } else {
	set gsubtype psub
	set gresdiff_spacing 0.48
	set gresdiff_end 0.48
    }

    set newdict [dict create \
	    res_type		xres \
	    end_type 		poly \
	    end_contact_type	pc \
	    plus_diff_type	$gdifftype \
	    plus_contact_type	$gdiffcont \
	    sub_type		$gsubtype \
	    guard_sub_surround	$gsurround \
	    end_surround	$poly_surround \
	    end_spacing		$gresdiff_end \
	    end_to_end_space	0.52 \
	    res_to_cont		0.575 \
	    res_to_endcont	0.28 \
	    res_spacing		0.48 \
	    res_diff_spacing	$gresdiff_spacing \
	    mask_clearance	0.52 \
	    overlap_compress	0.36 \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rm1_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		0 \
	    res_type		rm1 \
	    end_type 		m1 \
	    end_surround	0.0 \
	    end_spacing		0.0 \
	    end_to_end_space	0.28 \
	    res_to_endcont	0.2 \
	    res_spacing		$mmetal_spacing \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rm2_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		0 \
	    res_type		rm2 \
	    end_type 		m2 \
	    end_surround	0.0 \
	    end_spacing		0.0 \
	    end_to_end_space	0.28 \
	    res_to_endcont	0.2 \
	    res_spacing		$mmetal_spacing \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rm3_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		0 \
	    res_type		rm3 \
	    end_type 		m3 \
	    end_surround	0.0 \
	    end_spacing		0.0 \
	    end_to_end_space	0.28 \
	    res_to_endcont	0.2 \
	    res_spacing		$mmetal_spacing \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rm4_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		0 \
	    res_type		rm4 \
	    end_type 		m4 \
	    end_surround	0.0 \
	    end_spacing		0.0 \
	    end_to_end_space	0.28 \
	    res_to_endcont	0.2 \
	    res_spacing		$mmetal_spacing \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rm5_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		0 \
	    res_type		rm5 \
	    end_type 		m5 \
	    end_surround	0.0 \
	    end_spacing		0.0 \
	    end_to_end_space	0.28 \
	    res_to_endcont	0.2 \
	    res_spacing		$mmetal_spacing \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rm6_draw {parameters} {

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		0 \
	    res_type		rm6 \
	    end_type 		m6 \
	    end_surround	0.0 \
	    end_spacing		0.0 \
	    end_to_end_space	0.28 \
	    res_to_endcont	0.2 \
	    res_spacing		$mmetal_spacing \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------

proc sg13g2::rm7_draw {parameters} {
    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		0 \
	    res_type		rm7 \
	    end_type 		m7 \
	    end_surround	0.0 \
	    end_spacing		0.0 \
	    end_to_end_space	1.6 \
	    res_to_endcont	0.2 \
	    res_spacing		$mmetal_spacing \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::res_draw $drawdict]
}

#----------------------------------------------------------------
# Resistor total length computation
#----------------------------------------------------------------

proc sg13g2::compute_ltot {parameters} {
    # In case snake not defined
    set snake 0
    set caplen 0

    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    set l [magic::spice2float $l]
    set l [magic::3digitpastdecimal $l]

    # Compute total length.  Use catch to prevent error in batch/scripted mode.
    if {$snake == 1} {
	catch {set magic::ltot_val [expr ($caplen * ($nx - 1)) + ($l * $nx) + ($nx - 1)]}
    } else {
	catch {set magic::ltot_val $l}
    }
}

#----------------------------------------------------------------
# resistor: Check device parameters for out-of-bounds values
#----------------------------------------------------------------

proc sg13g2::res_check {device parameters} {

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    set snake 0
    set guard 0
    set sterm 0.0
    set caplen 0
    set wmax 0

    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Normalize distance units to microns
    set w [magic::spice2float $w]
    set w [magic::3digitpastdecimal $w]
    set l [magic::spice2float $l]
    set l [magic::3digitpastdecimal $l]

    set val  [magic::spice2float $val]
    set rho  [magic::spice2float $rho]

    # nf, m must be integer
    if {![string is int $nx]} {
	puts stderr "X repeat must be an integer!"
        dict set parameters nx 1
    }
    if {![string is int $m]} {
	puts stderr "Y repeat must be an integer!"
        dict set parameters m 1
    }

    # Width always needs to be specified
    if {$w < $wmin} {
	puts stderr "Resistor width must be >= $wmin um"
	dict set parameters w $wmin
    } 
    if {$wmax > 0 && $w > $wmax} {
	puts stderr "Resistor width must be <= $wmax um"
	dict set parameters w $wmax
    }
    
    # Val and W specified - no L
    if {$l == 0}  {
   	set l [expr ($w - $dw) * $val / $rho]
        set l [magic::3digitpastdecimal $l]
        set stringval [magic::float2spice $val]
	dict set parameters l [magic::float2spice [expr $l * 1e-6]]
	# L and W specified - ignore Val if specified
    } else {
	if {$snake == 0} {
	    set val [expr (2 * $term + $l * $rho) / ($w - $dw)]
	} else {
	    set val [expr $rho * ($nx - 1) + ((2 * ($term + $sterm)) \
			+ ($rho * $l * $nx) + ($rho * $caplen * ($nx - 1))) \
			/ ($w - $dw)]
	}
	set val [magic::float2spice $val]
        dict set parameters val $val
    }
    if {$l < $lmin} {
	puts stderr "Resistor length must be >= $lmin um"
	dict set parameters l $lmin
    } 
    if {$nx < 1} {
	puts stderr "X repeat must be >= 1"
	dict set parameters nx 1
    } 
    if {$m < 1} {
	puts stderr "Y repeat must be >= 1"
	dict set parameters m 1
    } 

    # Snake resistors cannot have width greater than length
    if {$snake == 1} {
	if {$w > $l} {
	    puts stderr "Snake resistor width must be < length"
	    dict set parameters w $l
	}
    }

    # Check via coverage for syntax
    if {$guard == 1} {
    	if {[catch {expr abs($viagb)}]} {
	    puts stderr "Guard ring bottom via coverage must be numeric!"
            dict set parameters viagb 0
    	} elseif {[expr abs($viagb)] > 100} {
	    puts stderr "Guard ring bottom via coverage can't be more than 100%"
            dict set parameters viagb 100
    	}
    	if {[catch {expr abs($viagt)}]} {
	    puts stderr "Guard ring top via coverage must be numeric!"
            dict set parameters viagt 0
	} elseif {[expr abs($viagt)] > 100} {
	    puts stderr "Guard ring top via coverage can't be more than 100%"
            dict set parameters viagt 100
	}
	if {[catch {expr abs($viagr)}]} {
	    puts stderr "Guard ring right via coverage must be numeric!"
            dict set parameters viagr 0
	} elseif {[expr abs($viagr)] > 100} {
	    puts stderr "Guard ring right via coverage can't be more than 100%"
            dict set parameters viagr 100
   	} 
        if {[catch {expr abs($viagl)}]} {
	    puts stderr "Guard ring left via coverage must be numeric!"
            dict set parameters viagl 0
	} elseif {[expr abs($viagl)] > 100} {
	   puts stderr "Guard ring left via coverage can't be more than 100%"
           dict set parameters viagl 100
	}
    }

    sg13g2::compute_ltot $parameters
    return $parameters
}

#----------------------------------------------------------------

proc sg13g2::rsil_check {parameters} {
    return [sg13g2::res_check rsil $parameters]
}

proc sg13g2::rppd_check {parameters} {
    return [sg13g2::res_check rppd $parameters]
}

proc sg13g2::rhigh_check {parameters} {
    return [sg13g2::res_check rhigh $parameters]
}

proc sg13g2::rm1_check {parameters} {
    return [sg13g2::res_check rm1 $parameters]
}

proc sg13g2::rm2_check {parameters} {
    return [sg13g2::res_check rm2 $parameters]
}

proc sg13g2::rm3_check {parameters} {
    return [sg13g2::res_check rm3 $parameters]
}

proc sg13g2::rm4_check {parameters} {
    return [sg13g2::res_check rm4 $parameters]
}
proc sg13g2::rm5_check {parameters} {
    return [sg13g2::res_check rm5 $parameters]
}

#----------------------------------------------------------------
# MOS defaults:
#----------------------------------------------------------------
#    w       = Gate width
#    l       = Gate length
#    m	     = Multiplier
#    nf	     = Number of fingers
#    diffcov = Diffusion contact coverage
#    polycov = Poly contact coverage
#    topc    = Top gate contact
#    botc    = Bottom gate contact
#    guard   = Guard ring
#
# (not user-editable)
#
#    lmin    = Gate minimum length
#    wmin    = Gate minimum width
#----------------------------------------------------------------

#----------------------------------------------------------------
# pmos: Specify all user-editable default values and those
# needed by mos_check
#----------------------------------------------------------------

proc sg13g2::sg13_lv_pmos_defaults {} {
    return {w 0.15 l 0.13 m 1 nf 1 diffcov 100 polycov 100 \
		guard 1 glc 1 grc 1 gtc 1 gbc 1 tbcov 100 rlcov 100 \
		topc 1 botc 1 poverlap 0 doverlap 1 lmin 0.13 wmin 0.15 \
		class mosfet compatible {sh13_lv_pmos sh13_hv_pmos} full_metal 1 \
		viasrc 100 viadrn 100 viagate 100 \
		viagb 0 viagr 0 viagl 0 viagt 0 doports 1}
}

proc sg13g2::sg13_hv_pmos_defaults {} {
    return {w 0.15 l 0.4 m 1 nf 1 diffcov 100 polycov 100 \
		guard 1 glc 1 grc 1 gtc 1 gbc 1 tbcov 100 rlcov 100 \
		topc 1 botc 1 poverlap 0 doverlap 1 lmin 0.4 wmin 0.15 \
		class mosfet compatible {sg13_lv_pmos sg13_hv_pmos} full_metal 1 \
		viasrc 100 viadrn 100 viagate 100 \
		viagb 0 viagr 0 viagl 0 viagt 0 doports 1}
}

#----------------------------------------------------------------
# nmos: Specify all user-editable default values and those
# needed by mos_check
#----------------------------------------------------------------

proc sg13g2::sg13_lv_nmos_defaults {} {
    return {w 0.15 l 0.13 m 1 nf 1 diffcov 100 polycov 100 \
		guard 1 glc 1 grc 1 gtc 1 gbc 1 tbcov 100 rlcov 100 \
		topc 1 botc 1 poverlap 0 doverlap 1 lmin 0.13 wmin 0.15 \
		class mosfet compatible {sg13_lv_nmos sg13_hv_nmos} \
		full_metal 1 viasrc 100 viadrn 100 viagate 100 \
		viagb 0 viagr 0 viagl 0 viagt 0 doports 1}
}

proc sg13g2::sg13_hv_nmos_defaults {} {
    return {w 0.15 l 0.45 m 1 nf 1 diffcov 100 polycov 100 \
		guard 1 glc 1 grc 1 gtc 1 gbc 1 tbcov 100 rlcov 100 \
		topc 1 botc 1 poverlap 0 doverlap 1 lmin 0.45 wmin 0.15 \
		class mosfet compatible {sg13_lv_nmos sg13_hv_nmos} \
		full_metal 1 viasrc 100 viadrn 100 viagate 100 \
		viagb 0 viagr 0 viagl 0 viagt 0 doports 1}
}

#----------------------------------------------------------------
# mos: Conversion from SPICE netlist parameters to toolkit
#----------------------------------------------------------------

proc sg13g2::mos_convert {parameters} {
    set pdkparams [dict create]
    dict for {key value} $parameters {
	switch -nocase $key {
	    l -
	    w {
		# Length and width are converted to units of microns
		set value [magic::spice2float $value]
		# set value [expr $value * 1e6]
		set value [magic::3digitpastdecimal $value]
		dict set pdkparams [string tolower $key] $value
	    }
	    m {
		dict set pdkparams [string tolower $key] $value
	    }
	    nf {
		# Adjustment ot W will be handled below
		dict set pdkparams [string tolower $key] $value
	    }
	    default {
		# Allow unrecognized parameters to be passed unmodified
		dict set pdkparams $key $value
	    }
	}
    }

    # Magic does not understand "nf" as a parameter, but expands to
    # "nf" number of devices connected horizontally.  The "w" value
    # must be divided down accordingly, as the "nf" parameter implies
    # that the total width "w" is divided into "nf" fingers.

    catch {
	set w [dict get $pdkparams w]
	set nf [dict get $pdkparams nf]
	if {$nf > 1} {
	    dict set pdkparams w [expr $w / $nf]
	}
    }

    return $pdkparams
}

#----------------------------------------------------------------

proc sg13g2::sg13_hv_nmos_convert {parameters} {
    return [sg13g2::mos_convert $parameters]
}

proc sg13g2::sg13_lv_nmos_convert {parameters} {
    return [sg13g2::mos_convert $parameters]
}

proc sg13g2::sg13_hv_pmos_convert {parameters} {
    return [sg13g2::mos_convert $parameters]
}

proc sg13g2::sg13_lv_pmos_convert {parameters} {
    return [sg13g2::mos_convert $parameters]
}

#----------------------------------------------------------------
# mos: Interactively specifies the fixed layout parameters
#----------------------------------------------------------------

proc sg13g2::mos_dialog {device parameters} {
    # Editable fields:      w, l, nf, m, diffcov, polycov
    # Checked fields:  topc, botc
    # For specific devices, gate type is a selection list

    magic::add_entry w "Width (um)" $parameters
    magic::add_entry l "Length (um)" $parameters
    magic::add_entry nf "Fingers" $parameters
    magic::add_entry m "M" $parameters

    if {[dict exists $parameters compatible]} {
       set sellist [dict get $parameters compatible]
       magic::add_selectlist gencell "Device type" $sellist $parameters $device
    }

    # Default-empty message area, used by the varactor dialog.
    magic::add_message minfo "" $parameters brown

    magic::add_entry diffcov "Diffusion contact coverage (%)" $parameters
    magic::add_entry polycov "Poly contact coverage (%)" $parameters
    magic::add_entry rlcov "Guard ring contact coverage (%)" $parameters
    if {[dict exists $parameters gbc]} {
	magic::add_entry tbcov "Guard ring top/bottom contact coverage (%)" $parameters
    }

    magic::add_checkbox poverlap "Overlap at poly contact" $parameters
    magic::add_checkbox doverlap "Overlap at diffusion contact" $parameters
    magic::add_checkbox topc "Add top gate contact" $parameters
    magic::add_checkbox botc "Add bottom gate contact" $parameters

    magic::add_checkbox guard "Add guard ring" $parameters
    magic::add_checkbox full_metal "Full metal guard ring" $parameters
    magic::add_checkbox glc "Add left guard ring contact" $parameters
    magic::add_checkbox grc "Add right guard ring contact" $parameters
    if {[dict exists $parameters gbc]} {
	magic::add_checkbox gbc "Add bottom guard ring contact" $parameters
    }
    if {[dict exists $parameters gtc]} {
	magic::add_checkbox gtc "Add top guard ring contact" $parameters
    }

    if {[string first "cap_var" $device] != -1} {
	magic::add_checkbox gshield "Metal shield over gate" $parameters
    }

    magic::add_entry viasrc "Source via coverage \[+/-\](%)" $parameters
    magic::add_entry viadrn "Drain via coverage \[+/-\](%)" $parameters
    magic::add_entry viagate "Gate via coverage \[+/-\](%)" $parameters
    magic::add_entry viagb "Bottom guard ring via coverage \[+/-\](%)" $parameters
    magic::add_entry viagt "Top guard ring via coverage \[+/-\](%)" $parameters
    magic::add_entry viagr "Right guard ring via coverage \[+/-\](%)" $parameters
    magic::add_entry viagl "Left guard ring via coverage \[+/-\](%)" $parameters

    if {[dict exists $parameters addports]} {
	magic::add_checkbox doports "Add ports" $parameters
    }

    # magic::add_checkbox dummy "Add dummy" $parameters
}

#----------------------------------------------------------------

proc sg13g2::sg13_lv_nmos_dialog {parameters} {
    sg13g2::mos_dialog sg13_lv_nmos $parameters
}

proc sg13g2::sg13_lv_pmos_dialog {parameters} {
    sg13g2::mos_dialog sg13_lv_pmos $parameters
}

proc sg13g2::sg13_hv_nmos_dialog {parameters} {
    sg13g2::mos_dialog sg13_hv_nmos $parameters
}

#----------------------------------------------------------------
# getbox:  Get the current cursor box, in microns
#----------------------------------------------------------------

proc sg13g2::getbox {} {
    set curbox [box values]
    set newbox []
    set oscale [cif scale out]
    for {set i 0} {$i < 4} {incr i} {
        set v [* [lindex $curbox $i] $oscale]
        lappend newbox $v
    }
    return $newbox
}

#----------------------------------------------------------------
# unionbox:  Get the union bounding box of box1 and box2
#----------------------------------------------------------------

proc sg13g2::unionbox {box1 box2} {
    set newbox []
    for {set i 0} {$i < 2} {incr i} {
        set v [lindex $box1 $i]
        set o [lindex $box2 $i]
        if {$v < $o} {
            lappend newbox $v
        } else {
            lappend newbox $o
        }
    }
    for {set i 2} {$i < 4} {incr i} {
        set v [lindex $box1 $i]
        set o [lindex $box2 $i]
        if {$v > $o} {
            lappend newbox $v
        } else {
            lappend newbox $o
        }
    }
    return $newbox
}

#----------------------------------------------------------------
# Draw a contact
#----------------------------------------------------------------

proc sg13g2::draw_contact {w h s o x atype ctype mtype {orient vert}} {

    # Draw a minimum-size diff contact centered at current position
    # w is width, h is height.  Minimum size ensured.
    # x is contact size
    # s is contact diffusion (or poly) surround
    # o is contact metal surround
    # atype is active (e.g., ndiff) or bottom metal if a via
    # ctype is contact (e.g., ndc)
    # mtype is metal (e.g., m1) or top metal if a via
    # orient is the orientation of the contact

    # Set orientations for the bottom material based on material type.
    # All diffusions overlap on all sides.  Metal1 overlaps on all
    # sides but in different amounts depending on the direction.

    set aorient "full"

    pushbox
    box size 0 0
    if {$w < $x} {set w $x}
    if {$h < $x} {set h $x}
    set hw [/ $w 2.0]
    set hh [/ $h 2.0]
    box grow n ${hh}um
    box grow s ${hh}um
    box grow e ${hw}um
    box grow w ${hw}um
    paint ${ctype}
    pushbox
    # Bottom layer surrounded on sides as declared by aorient
    if {($aorient == "vert") || ($aorient == "full")} {
	box grow n ${s}um
	box grow s ${s}um
    }
    if {($aorient == "horz") || ($aorient == "full")} {
	box grow e ${s}um
	box grow w ${s}um
    }
    paint ${atype}
    set extents [sg13g2::getbox]
    popbox
    # Top layer surrounded on sides as declared by orient
    if {($orient == "vert") || ($orient == "full")} {
        box grow n ${o}um
        box grow s ${o}um
    }
    if {($orient == "horz") || ($orient == "full")} {
        box grow e ${o}um
        box grow w ${o}um
    }
    paint ${mtype}
    popbox
    return $extents
}

#----------------------------------------------------------------
# Draw a guard ring
#----------------------------------------------------------------

proc sg13g2::guard_ring {gw gh parameters} {

    # Set local default values if they are not in parameters
    set rlcov 100	;# Right-left contact coverage percentage
    set tbcov 100	;# Top-bottom contact coverage percentage
    set grc 1		;# Draw right side contact
    set glc 1		;# Draw left side contact
    set gtc 1		;# Draw right side contact
    set gbc 1		;# Draw left side contact
    set viagb 0		;# Draw bottom side via
    set viagt 0		;# Draw top side via
    set viagr 0		;# Draw right side via
    set viagl 0		;# Draw left side via
    set full_metal 1	;# Draw full (continuous) metal ring
    set guard_sub_type	 pwell	;# substrate type under guard ring
    set guard_sub_surround  0	;# substrate type surrounds guard ring
    set plus_diff_type   nsd	;# guard ring diffusion type
    set plus_contact_type nsc	;# guard ring diffusion contact type
    set sub_type	 pwell	;# substrate type
    set bulk ""		;# Default no port label

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Set guard_sub_type to sub_type if it is not defined
    if {![dict exists $parameters guard_sub_type]} {
	set guard_sub_type $sub_type
    }

    set hx [/ $contact_size 2.0]
    set hw [/ $gw 2.0]
    set hh [/ $gh 2.0]

    # Compute diffusion width
    set difft [+ $contact_size $diff_surround $diff_surround]
    set hdifft [/ $difft 2.0]
    # Compute guard ring diffusion width and height
    set hdiffw [/ [+ $gw $difft] 2.0]
    set hdiffh [/ [+ $gh $difft] 2.0]

    pushbox
    box size 0 0

    pushbox
    box move n ${hh}um
    box grow n ${hdifft}um
    box grow s ${hdifft}um
    box grow e ${hdiffw}um
    box grow w ${hdiffw}um
    paint $plus_diff_type
    if {$guard_sub_surround > 0} {
	box grow c ${guard_sub_surround}um
	paint $guard_sub_type
    }
    popbox
    pushbox
    box move s ${hh}um
    pushbox
    box grow n ${hdifft}um
    box grow s ${hdifft}um
    box grow e ${hdiffw}um
    box grow w ${hdiffw}um
    paint $plus_diff_type
    if {$guard_sub_surround > 0} {
	box grow c ${guard_sub_surround}um
	paint $guard_sub_type
    }
    popbox
    # At guard ring bottom center, place a port if requested
    if {$bulk != ""} {
	label $bulk c $plus_diff_type
	select area label
	port make
    }
    popbox
    pushbox
    box move e ${hw}um
    box grow e ${hdifft}um
    box grow w ${hdifft}um
    box grow n ${hdiffh}um
    box grow s ${hdiffh}um
    paint $plus_diff_type
    if {$guard_sub_surround > 0} {
	box grow c ${guard_sub_surround}um
	paint $guard_sub_type
    }
    popbox
    pushbox
    box move w ${hw}um
    box grow e ${hdifft}um
    box grow w ${hdifft}um
    box grow n ${hdiffh}um
    box grow s ${hdiffh}um
    paint $plus_diff_type
    if {$guard_sub_surround > 0} {
	box grow c ${guard_sub_surround}um
	paint $guard_sub_type
    }
    popbox

    if {$full_metal} {
	set hmetw [/ [+ $gw $contact_size] 2.0]
	set hmeth [/ [+ $gh $contact_size] 2.0]
	pushbox
	box move n ${hh}um
	box grow n ${hx}um
	box grow s ${hx}um
	box grow e ${hmetw}um
	box grow w ${hmetw}um
	paint m1
	popbox
	pushbox
	box move s ${hh}um
	box grow n ${hx}um
	box grow s ${hx}um
	box grow e ${hmetw}um
	box grow w ${hmetw}um
	paint m1
	popbox
	pushbox
	box move e ${hw}um
	box grow e ${hx}um
	box grow w ${hx}um
	box grow n ${hmeth}um
	box grow s ${hmeth}um
	paint m1
	popbox
	pushbox
	box move w ${hw}um
	box grow e ${hx}um
	box grow w ${hx}um
	box grow n ${hmeth}um
	box grow s ${hmeth}um
	paint m1
	popbox
    }

    # Set guard ring height so that contact metal reaches to end, scale by $per
    # set ch [* [+ $gh $contact_size [* $metal_surround -2.0]] [/ $rlcov 100.0]]
    set ch [* [- $gh $contact_size [* [+ $metal_surround $metal_spacing] \
		2.0]] [/ $rlcov 100.0]]
    if {$ch < $contact_size} {set ch $contact_size}

    # Set guard ring width so that contact metal reaches to side contacts
    set cw [* [- $gw $contact_size [* [+ $metal_surround $metal_spacing] \
		2.0]] [/ $tbcov 100.0]]
    if {$cw < $contact_size} {set cw $contact_size}

    if {$tbcov > 0.0} {
        if {$gtc == 1} {
            pushbox
            box move n ${hh}um
            sg13g2::draw_contact $cw 0 $diff_surround $metal_surround \
		$contact_size $plus_diff_type \
		$plus_contact_type m1 horz
            popbox
	}
	if {$gbc == 1} {
	    pushbox
	    box move s ${hh}um
	    sg13g2::draw_contact $cw 0 $diff_surround $metal_surround \
		$contact_size $plus_diff_type \
		$plus_contact_type m1 horz
	    popbox
	}
    }
    if {$rlcov > 0.0} {
        if {$grc == 1} {
            pushbox
            box move e ${hw}um
            sg13g2::draw_contact 0 $ch $diff_surround $metal_surround \
		$contact_size $plus_diff_type \
		$plus_contact_type m1 vert
            popbox
        }
        if {$glc == 1} {
            pushbox
            box move w ${hw}um
            sg13g2::draw_contact 0 $ch $diff_surround $metal_surround \
		$contact_size $plus_diff_type \
		$plus_contact_type m1 vert
            popbox
        }
    }

    # Vias
    if {$viagb != 0} {
        pushbox
    	set ch $via_size
    	set cw [* [- $gw $via_size] [/ [expr abs($viagb)] 100.0]]
    	if {$cw < $via_size} {set cw $via_size}
        box move s ${hh}um
	box grow n [/ $ch 2]um
	box grow s [/ $ch 2]um
        set anchor [string index $viagb 0]
	if {$anchor == "+"} {
            box move w [/ [- $gw $via_size] 2]um
	    box grow e ${cw}um
	} elseif {$anchor == "-"} {
            box move e [/ [- $gw $via_size] 2]um
	    box grow w ${cw}um
	} else {
	    box grow e [/ $cw 2]um
	    box grow w [/ $cw 2]um
	}
        sg13g2::via1_draw horz
        popbox
    }
    if {$viagt != 0} {
        pushbox
    	set ch $via_size
    	set cw [* [- $gw $via_size] [/ [expr abs($viagt)] 100.0]]
    	if {$cw < $via_size} {set cw $via_size}
        box move n ${hh}um
	box grow n [/ $ch 2]um
	box grow s [/ $ch 2]um
        set anchor [string index $viagt 0]
	if {$anchor == "+"} {
            box move w [/ [- $gw $via_size] 2]um
	    box grow e ${cw}um
	} elseif {$anchor == "-"} {
            box move e [/ [- $gw $via_size] 2]um
	    box grow w ${cw}um
	} else {
	    box grow e [/ $cw 2]um
	    box grow w [/ $cw 2]um
	}
        sg13g2::via1_draw horz
        popbox
    }
    if {$viagr != 0} {
        pushbox
    	set ch [* [- $gh $via_size] [/ [expr abs($viagr)] 100.0]]
    	if {$ch < $via_size} {set ch $via_size}
    	set cw $via_size
        box move e ${hw}um
	box grow e [/ $cw 2]um
	box grow w [/ $cw 2]um
        set anchor [string index $viagr 0]
	if {$anchor == "+"} {
            box move s [/ [- $gh $via_size] 2]um
	    box grow n ${ch}um
	} elseif {$anchor == "-"} {
            box move n [/ [- $gh $via_size] 2]um
	    box grow s ${ch}um
	} else {
	    box grow n [/ $ch 2]um
	    box grow s [/ $ch 2]um
	}
        sg13g2::via1_draw vert
        popbox
    }
    if {$viagl != 0} {
        pushbox
    	set ch [* [- $gh $via_size] [/ [expr abs($viagl)] 100.0]]
    	if {$ch < $via_size} {set ch $via_size}
    	set cw $via_size
        box move w ${hw}um
	box grow e [/ $cw 2]um
	box grow w [/ $cw 2]um
        set anchor [string index $viagl 0]
	if {$anchor == "+"} {
            box move s [/ [- $gh $via_size] 2]um
	    box grow n ${ch}um
	} elseif {$anchor == "-"} {
            box move n [/ [- $gh $via_size] 2]um
	    box grow s ${ch}um
	} else {
	    box grow n [/ $ch 2]um
	    box grow s [/ $ch 2]um
	}
        sg13g2::via1_draw vert
        popbox
    }

    pushbox
    box grow e ${hw}um
    box grow w ${hw}um
    box grow n ${hh}um
    box grow s ${hh}um
    # Create boundary using properties
    property FIXED_BBOX [box values]
    box grow c ${hx}um  ;# to edge of contact
    box grow c ${diff_surround}um  ;# to edge of diffusion
    box grow c ${sub_surround}um  ;# sub/well overlap of diff (NOT guard_sub)
    paint $sub_type
    set cext [sg13g2::getbox]
    popbox
    popbox

    return $cext
}

#----------------------------------------------------------------
# MOSFET: Draw a single device
#----------------------------------------------------------------

proc sg13g2::mos_device {parameters} {

    # Epsilon for avoiding round-off errors
    set eps  0.0005

    # Set local default values if they are not in parameters
    set doports 0	;# no port labels by default
    set diffcov 100	;# percent coverage of diffusion contact
    set polycov 100	;# percent coverage of poly contact
    set topc 1		;# draw top poly contact
    set botc 1		;# draw bottom poly contact
    set viasrc 100	;# draw source vias
    set viadrn 100	;# draw drain vias
    set viagate 100	;# draw gate vias
    set evens 1		;# even or odd (evens = 1 means drain is on the left)
    set dev_sub_type ""	;# device substrate type (if different from guard ring)
    set dev_sub_dist 0	;# device substrate distance (if nondefault dev_sub_type)
    set min_effl 0	;# gate length below which finger pitch must be stretched
    set diff_overlap_cont 0	;# extra overlap of end contact by diffusion
    set gshield 0	;# no metal shield over gate (used for varactors)
    set drain_proc {}	;# no special procedure to draw the drain

    set drain ""
    set source ""
    set gate ""

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # "topc" and "botc" may be modified for alternating top-bottom gate
    # contacts.  If so, original values are in "oldtopc" and "oldbotc".
    if {![dict exists $parameters oldtopc]} {set oldtopc $topc}
    if {![dict exists $parameters oldbotc]} {set oldbotc $botc}

    # Draw the diffusion and poly
    pushbox
    box size 0 0
    pushbox
    set hw [/ $w 2.0]
    set hl [/ $l 2.0]
    set he [/ $min_effl 2.0]
    if {$nf == 1 || $he < $hl} {set he $hl}
    box grow n ${hw}um
    box grow s ${hw}um
    box grow e ${hl}um
    box grow w ${hl}um

    # Set drain and source sides based on "evens".
    if {$evens == 0} {
	set dside e
	set sside w
    } else {
	set dside w
	set sside e
    }

    pushbox
    if {${diff_extension} > ${gate_to_diffcont}} {
        box grow $dside ${diff_extension}um
        box grow $sside ${diff_extension}um
    } else {
        box grow $dside ${gate_to_diffcont}um
        box grow $sside ${gate_to_diffcont}um
    }
    paint ${diff_type}
    popbox
    pushbox
    if {${gate_extension} > ${gate_to_polycont}} {
	box grow n ${gate_extension}um
	box grow s ${gate_extension}um
    } else {
	if {$topc} {
	    box grow n ${gate_to_polycont}um
	} else {
	    box grow n ${gate_extension}um
	}
	if {$botc} {
	    box grow s ${gate_to_polycont}um
	} else {
	    box grow s ${gate_extension}um
	}
    }
    paint ${poly_type}
    set cext [sg13g2::getbox]
    popbox
    # save gate area now and paint later, so that diffusion surrounding the
    # contact does not paint over the gate area, in case the gate type is
    # not part of a "compose" entry in the techfile.
    set gaterect [box values]
    popbox

    # Adjust position of contacts for dogbone geometry
    # Rule 1: Minimize diffusion length.  Contacts only move out
    # if width <  contact diffusion height.  They move out enough
    # that the diffusion-to-poly spacing is satisfied.

    set ddover 0
    set cdwmin [+ ${contact_size} [* ${diff_surround} 2]]
    set cstem [- ${gate_to_diffcont} [/ ${cdwmin} 2.0]]
    set cgrow [- ${diff_poly_space} ${cstem}]
    if {[+ ${w} ${eps}] < ${cdwmin}} {
        if {${cgrow} > 0} {
            set gate_to_diffcont [+ ${gate_to_diffcont} ${cgrow}]
        }
	set ddover [/ [- ${cdwmin} ${w}] 2.0]
    }

    # Rule 2: Minimum poly width.  Poly contacts only move out
    # if length < contact poly width.  They move out enough
    # that the diffusion-to-poly spacing is satisfied.

    set gporig ${gate_to_polycont}
    set cplmin [+ ${contact_size} [* ${poly_surround} 2]]
    set cstem [- ${gate_to_polycont} [/ ${cplmin} 2.0]]
    set cgrow [- ${diff_poly_space} ${cstem}]
    if {[+ ${l} ${eps}] < ${cplmin}} {
        if {${cgrow} > 0} {
            set gate_to_polycont [+ ${gate_to_polycont} ${cgrow}]
        }
    }

    # Rule 3: If both poly and diffusion are dogboned, then move
    # poly out further to clear spacing to the diffusion contact

    if {[+ ${w} ${eps}] < ${cdwmin}} {
        if {[+ ${l} ${eps}] < ${cplmin}} {
            set cgrow [/ [- ${cplmin} ${w}] 2.0]
            set gate_to_polycont [+ ${gate_to_polycont} ${cgrow}]
        }
    }

    # Rule 4: If M > 1 and poly contacts overlap, then increase the
    # transistor-to-poly-contact distance by the amount of any
    # diffusion dogbone overhang.

    if {($poverlap == 1) && ($m > 1)} {
	if {${gate_to_polycont} - $gporig < $ddover} {
	    set gate_to_polycont [+ ${gporig} ${ddover}]
	}
    }

    # Reduce contact sizes by poly or diffusion surround so that
    # the contact area edges match the device diffusion or poly.
    # (Minimum dimensions will be enforced by the contact drawing routine)
    set tsurround [+ ${diff_surround} ${diff_overlap_cont}]
    set cdw [- ${w} [* ${tsurround} 2]]		;# diff contact height
    set cpl [- ${l} [* ${poly_surround} 2]]     ;# poly contact width

    # Save the full diffusion (source/drain) and poly (gate) lengths
    set cdwfull $cdw
    set cplfull $cpl

    # Reduce by coverage percentage.  NOTE:  If overlapping multiple devices,
    # keep maximum poly contact coverage.

    set cdw [* ${cdw} [/ ${diffcov} 100.0]]
    if {($poverlap == 0) || ($m == 1)} {
	set cpl [* ${cpl} [/ ${polycov} 100.0]]
    }

    if {$drain_proc != {}} {
	# Add variables to the parameters dictionary that we'll need
	dict set parameters cdw $cdw
	dict set parameters cdwfull $cdwfull
	dict set parameters dside $dside
	dict set parameters sside $sside
	dict set parameters doports $doports
	set cext [sg13g2::unionbox $cext [eval $drain_proc {$parameters}]]
    } else {
	# Drain diffusion contact
	pushbox
	box move $dside ${he}um
	box move $dside ${gate_to_diffcont}um

	# Drain via on top of contact
	set viatype $viadrn
	if {$viatype != 0} {
            pushbox
            set cw $via_size
    	    set ch [* $cdwfull [/ [expr abs($viatype)] 100.0]]
    	    if {$ch < $via_size} {set ch $via_size}
	    box grow $dside [/ $cw 2]um
	    box grow $sside [/ $cw 2]um
            set anchor [string index $viatype 0]
	    if {$anchor == "+"} {
        	box move s [/ [- $cdwfull $via_size] 2]um
		box grow n ${ch}um
	    } elseif {$anchor == "-"} {
        	box move n [/ [- $cdwfull $via_size] 2]um
		box grow s ${ch}um
	    } else {
		box grow n [/ $ch 2]um
		box grow s [/ $ch 2]um
	    }
	    sg13g2::via1_draw vert
	    popbox
	}
	set cext [sg13g2::unionbox $cext [sg13g2::draw_contact 0 ${cdw} \
		${diff_surround} ${metal_surround} \
		${contact_size} ${diff_type} ${diff_contact_type} m1 vert]]
	if {$drain != ""} {
            label $drain c $idff_contact_type
            select area label
            port make
	}
	popbox
    }

    # Source diffusion contact
    pushbox
    box move $sside ${he}um
    box move $sside ${gate_to_diffcont}um

    # Source via on top of contact
    set viatype $viasrc
    if {$viatype != 0} {
        pushbox
        set cw $via_size
    	set ch [* $cdwfull [/ [expr abs($viatype)] 100.0]]
    	if {$ch < $via_size} {set ch $via_size}
	box grow $sside [/ $cw 2]um
	box grow $dside [/ $cw 2]um
        set anchor [string index $viatype 0]
	if {$anchor == "+"} {
            box move s [/ [- $cdwfull $via_size] 2]um
	    box grow n ${ch}um
	} elseif {$anchor == "-"} {
            box move n [/ [- $cdwfull $via_size] 2]um
	    box grow s ${ch}um
	} else {
	    box grow n [/ $ch 2]um
	    box grow s [/ $ch 2]um
	}
        sg13g2::via1_draw vert
        popbox
    }
    set cext [sg13g2::unionbox $cext [sg13g2::draw_contact 0 ${cdw} \
		${diff_surround} ${metal_surround} \
		${contact_size} ${diff_type} ${diff_contact_type} m1 vert]]
    if {$source != ""} {
        label $source c $diff_contact_type
        select area label
        port make
    }
    set diffarea $cext
    popbox
    # Gate shield (only on varactors)
    if {$gshield == 1} {
 	pushbox
	box move w ${he}um
	box move w ${gate_to_diffcont}um
	box width [* 2 [+ ${he} ${gate_to_diffcont}]]um
	box grow n [/ $cdwfull 2]um
	box grow s [/ $cdwfull 2]um
	paint m2
	# Enforce slotting of large metal
	set gsh [magic::i2u [box height]]
	if {$gsh > 25} {
	   box move n [/ $gsh 2]um
	   box move s 1.15um
	   box height 2.3um	;# Minimum m1 slot width
	   box grow w -${via_size}um
	   box grow e -${via_size}um
	   erase m2
	}
	popbox
    }
    # Top poly contact
    if {$topc && $oldtopc} {
       pushbox
       box move n ${hw}um
       box move n ${gate_to_polycont}um

       # Gate via on top of contact
       if {$viagate != 0} {
           pushbox
    	   set ch $via_size
    	   set cw [* $cplfull [/ [expr abs($viagate)] 100.0]]
    	   if {$cw < $via_size} {set cw $via_size}
	   box grow n [/ $ch 2]um
	   box grow s [/ $ch 2]um
           set anchor [string index $viagate 0]
	   if {$anchor == "+"} {
               box move w [/ [- $cplfull $via_size] 2]um
	       box grow e ${cw}um
	   } elseif {$anchor == "-"} {
               box move e [/ [- $cplfull $via_size] 2]um
	       box grow w ${cw}um
	   } else {
	       box grow e [/ $cw 2]um
	       box grow w [/ $cw 2]um
	   }
           sg13g2::via1_draw horz
           popbox
       }
       set cext [sg13g2::unionbox $cext [sg13g2::draw_contact ${cpl} 0 \
		${poly_surround} ${metal_surround} \
		${contact_size} ${poly_type} ${poly_contact_type} m1 horz]]
       if {$gate != ""} {
	   label $gate c $poly_contact_type
	   select area label
	   port make
       }
       popbox
    }
    # Bottom poly contact
    if {$botc && $oldbotc} {
       pushbox
       box move s ${hw}um
       box move s ${gate_to_polycont}um

       # Gate via on top of contact
       if {$viagate != 0} {
           pushbox
    	   set ch $via_size
    	   set cw [* $cplfull [/ [expr abs($viagate)] 100.0]]
    	   if {$cw < $via_size} {set cw $via_size}
	   box grow n [/ $ch 2]um
	   box grow s [/ $ch 2]um
           set anchor [string index $viagate 0]
	   if {$anchor == "+"} {
               box move w [/ [- $cplfull $via_size] 2]um
	       box grow e ${cw}um
	   } elseif {$anchor == "-"} {
               box move e [/ [- $cplfull $via_size] 2]um
	       box grow w ${cw}um
	   } else {
	       box grow e [/ $cw 2]um
	       box grow w [/ $cw 2]um
	   }
           sg13g2::via1_draw horz
           popbox
       }
       set cext [sg13g2::unionbox $cext [sg13g2::draw_contact ${cpl} 0 \
		${poly_surround} ${metal_surround} \
		${contact_size} ${poly_type} ${poly_contact_type} m1 horz]]
       if {($gate != "") && ($topc == 0)} {
	   label $gate c $poly_contact_type
	   select area label
	   port make
       }
       popbox
    }

    # Now draw the gate, after contacts have been drawn
    pushbox
    box values {*}${gaterect}
    # gate_type need not be defined if poly over diff paints the right type.
    catch {paint ${gate_type}}
    # sub_surround_dev, if defined, may create a larger area around the gate
    # than sub_surround creates around the diffusion/poly area.
    if [dict exists $parameters sub_surround_dev] {
	box grow n ${sub_surround_dev}um
	box grow s ${sub_surround_dev}um
	box grow e ${sub_surround_dev}um
	box grow w ${sub_surround_dev}um
	paint ${dev_sub_type}
	set cext [sg13g2::unionbox $cext [sg13g2::getbox]]
    }
    popbox

    if {$dev_sub_type != ""} {
	box values [lindex $diffarea 0]um [lindex $diffarea 1]um \
	    [lindex $diffarea 2]um [lindex $diffarea 3]um
	box grow n ${sub_surround}um
	box grow s ${sub_surround}um
	box grow e ${sub_surround}um
	box grow w ${sub_surround}um
	paint ${dev_sub_type}
	if {$dev_sub_dist > 0} {
	    set cext [sg13g2::unionbox $cext [sg13g2::getbox]]
	}
        # puts stdout "Diagnostic:  bounding box is $cext"
    }

    popbox
    return $cext
}

#----------------------------------------------------------------
# MOSFET: Draw the tiled device
#----------------------------------------------------------------

proc sg13g2::mos_draw {parameters} {
    tech unlock *
    set savesnap [snap]
    snap internal

    # Set defaults if they are not in parameters
    set poverlap 0	;# overlap poly contacts when tiling
    set doverlap 1	;# overlap diffusion contacts when tiling
    set dev_sub_dist 0	;# substrate to guard ring, if dev_sub_type defined
    set dev_sub_space 0	;# distance between substrate areas for arrayed devices
    set min_allc 0	;# gate length below which poly contacts must be interleaved
    set id_type ""	;# additional type covering everything
    set id_surround 0	;# amount of surround on above type
    set id2_type ""	;# additional type covering everything
    set id2_surround 0	;# amount of surround on above type
    set doports 0	;# no port labels unless requested

    set set_x_to_guard ""	;# override x distance to guard ring
    set set_y_to_guard ""	;# override y distance to guard ring

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Diff surround on drain is by default the same as diff surround
    if {![dict exist $parameters drain_diff_surround]} {
	set drain_diff_surround $diff_surround
    }

    # Diff-to-tap spacing is by default the same as diff spacing
    if {![dict exist $parameters diff_tap_space]} {
	set diff_tap_space $diff_spacing
    }

    # If poverlap is 1 then both poly contacts must be present
    if {$poverlap == 1} {
	set topc 1
	set botc 1
	dict set parameters topc 1
	dict set parameters botc 1
    }

    # Normalize distance units to microns
    set w [magic::spice2float $w]
    set l [magic::spice2float $l]

    pushbox
    box values 0 0 0 0

    # If dx < (poly contact space + poly contact width), then there is not
    # enough room for a row of contacts, so force alternating contacts

    set evens 1
    if {$nf > 1 && $l < $min_allc} {
	set intc 1
	set evenodd 1
	set topc 1
	set botc 1
	dict set parameters topc 1
	dict set parameters botc 1
	set poverlap 0
    } else {
	set intc 0
    }

    # Determine the base device dimensions by drawing one device
    # while all layers are locked (nothing drawn).  This allows the
    # base drawing routine to do complicated geometry without having
    # to duplicate it here with calculations.

    tech lock *
    set bbox [sg13g2::mos_device $parameters]
    puts stdout "Diagnostic: Device bounding box e $bbox (um)"
    tech unlock *

    set fw [- [lindex $bbox 2] [lindex $bbox 0]]
    set fh [- [lindex $bbox 3] [lindex $bbox 1]]
    set lw [+ [lindex $bbox 2] [lindex $bbox 0]]
    set lh [+ [lindex $bbox 3] [lindex $bbox 1]]

    # If the bounding box is not symmetric about x=0, then find the
    # offset.  Assumed to be needed only for X (asymmetric drain)
    set xoffset [+ [lindex $bbox 0] [lindex $bbox 2]]
    
    # If dev_sub_dist > 0 then each device must be in its own substrate
    # (well) area, and overlaps are disallowed.  dev_sub_space determines
    # the distance between individual devices in an array.

    if {$dev_sub_dist > 0} {
        set poverlap 0
        set doverlap 0

	if {$dev_sub_space > $poly_spacing} {
	    set dx [+ $fw $dev_sub_space]
	    set dy [+ $fh $dev_sub_space]
	} else {
	    set dx [+ $fw $poly_spacing]
	    set dy [+ $fh $poly_spacing]
	}

    } else {

	# Determine tile width and height (depends on overlap)
	if {$poverlap == 0} {
	    set dy [+ $fh $poly_spacing]
	} else {
	    # overlap poly
	    set dy [- $fh [+ $poly_surround $poly_surround $contact_size]]
	}

	if {$doverlap == 0} {
	    set dx [+ $fw $diff_spacing]
	} else {
	    # overlap diffusions
	    set dx [- $fw [+ $drain_diff_surround $drain_diff_surround $contact_size]]
	}
    }

    # Determine core width and height
    set corex [+ [* [- $nf 1] $dx] $fw]
    set corey [+ [* [- $m 1] $dy] $fh]
    set corellx [/ [+ [- $corex $fw] $lw] 2.0]
    set corelly [/ [+ [- $corey $fh] $lh] 2.0]

    # If there is a diffusion dogbone, and no top poly contact, then
    # increase the core height by the amount of the dogbone overhang.

    if {$topc == 0} {
	set cdwmin [+ ${contact_size} [* ${diff_surround} 2]]
	if {${w} < ${cdwmin}} {
	    set corey [+ $corey [/ [- ${cdwmin} ${w}] 2.0]]
	}
    }

    # Calculate guard ring size (measured to contact center)
    if {($guard != 0) || (${id_type} != "")} {
	if {($dev_sub_dist > 0) && ([+ $dev_sub_dist $sub_surround] > $diff_tap_space)} {
	    set gx [+ $corex [* 2.0 [+ $dev_sub_dist $diff_surround]] $contact_size]
	} else {
	    set gx [+ $corex [* 2.0 [+ $diff_tap_space $diff_surround]] $contact_size]
	}
	if {($dev_sub_dist > 0) && ([+ $dev_sub_dist $sub_surround] > $diff_gate_space)} {
	    set gy [+ $corey [* 2.0 [+ $dev_sub_dist $diff_surround]] $contact_size]
	} else {
	    set gy [+ $corey [* 2.0 [+ $diff_gate_space $diff_surround]] $contact_size]
	}

	# Somewhat tricky. . . if the width is small and the diffusion is 
	# a dogbone, and the top or bottom poly contact is missing, then
	# the spacing to the guard ring may be limited by diffusion spacing, not
	# poly to diffusion.

	set inset [/ [+ $contact_size [* 2.0 $diff_surround] -$w] 2.0]
	set sdiff [- [+ $inset $diff_tap_space] [+ $gate_extension $diff_gate_space]]

	if {$sdiff > 0} {
	    if {$topc == 0} {
		set gy [+ $gy $sdiff]
		set corelly [+ $corelly [/ $sdiff 2.0]]
	    }
	    if {$botc == 0} {
		set gy [+ $gy $sdiff]
		set corelly [- $corelly [/ $sdiff 2.0]]
	    }
	}

	# set_x|y_to_guard overrides the above calculations if present.
	if {$set_x_to_guard != ""} {
	    set gx [+ $corex [* 2.0 $set_x_to_guard]]
	}
	if {$set_y_to_guard != ""} {
	    set gy [+ $corey [* 2.0 $set_y_to_guard]]
	}
    }
    if {$guard != 0} {
	if {$doports} {dict set parameters bulk B}
	# Draw the guard ring first, as MOS well may interact with guard ring substrate
	sg13g2::guard_ring $gx $gy $parameters
    }

    pushbox
    # If any surrounding identifier type is defined, draw it
    if {${id_type} != ""} {
	set hw [/ $gx 2]
	set hh [/ $gy 2]
	box grow e ${hw}um
	box grow w ${hw}um
	box grow n ${hh}um
	box grow s ${hh}um
	box grow c ${id_surround}um
	paint ${id_type}
    }
    popbox
    pushbox
    box move w ${corellx}um
    box move s ${corelly}um
    for {set xp 0} {$xp < $nf} {incr xp} {
	dict set parameters evens $evens
	set evens [- 1 $evens]
        pushbox
	if {$intc == 1} {
	    set evenodd [- 1 $evenodd]
	    if {$evenodd == 1} {
		dict set parameters topc 1
		dict set parameters botc 0
	    } else {
		dict set parameters topc 0
		dict set parameters botc 1
	    }
	    set saveeo $evenodd
	}
        for {set yp 0} {$yp < $m} {incr yp} {
	    # Apply rules for source/drain/gate port labeling
	    if {$doports && ($m == 1)} {
		if {$nf == 1} {
		    dict set parameters drain D
		    dict set parameters source S
		    dict set parameters gate G
		} else {
		    if {$doverlap && $evens && ($xp > 0)} {
			dict set parameters drain ""
		    } else {
			dict set parameters drain D$xp
		    }
		    if {$doverlap  && ($evens == 0) && ($xp < $nf-1)} {
			dict set parameters source ""
		    } else {
			dict set parameters source S$xp
		    }
		    dict set parameters gate G$xp
		}
	    } elseif {$doports} {
		if {$nf == 1} {
		    dict set parameters drain D$yp
		    dict set parameters source S$yp
		    if {$poverlap && ($yp == 0)} {
			dict set parameters gate G
		    } elseif {$poverlap} {
			dict set parameters gate ""
		    } else {
			dict set parameters gate G$yp
		    }
		} else {
		    if {$doverlap && $evens && ($xp > 0)} {
			dict set parameters drain ""
		    } else {
			dict set parameters drain D${xp}_$yp
		    }
		    if {$doverlap && ($evens == 0) && ($xp < $nf-1)} {
			dict set parameters source ""
		    } else {
			dict set parameters source S${xp}_$yp
		    }
		    if {$poverlap && ($yp == 0)} {
			dict set parameters gate G$xp
		    } elseif {$poverlap} {
			dict set parameters gate ""
		    } else {
			dict set parameters gate G${xp}_$yp
		    }
		}
	    }
            if {$evens != 0} {box move e ${xoffset}um}
            sg13g2::mos_device $parameters
            if {$evens != 0} {box move w ${xoffset}um}
            box move n ${dy}um
	    if {$intc == 1} {
		set evenodd [- 1 $evenodd]
		if {$evenodd == 1} {
		    dict set parameters topc 1
		    dict set parameters botc 0
		} else {
		    dict set parameters topc 0
		    dict set parameters botc 1
		}
	    }
        }
	if {$intc == 1} {
	    set evenodd $saveeo
	}
        popbox
        box move e ${dx}um
    }
    popbox
    popbox

    snap $savesnap
    tech revert
}

#-------------------
# nMOS 1.8V
#-------------------

proc sg13g2::sg13_lv_nmos_draw {parameters} {
    set newdict [dict create \
	    gate_type		nfet \
	    diff_type 		ndiff \
	    diff_contact_type	ndc \
	    plus_diff_type	psd \
	    plus_contact_type	psc \
	    poly_type		poly \
	    poly_contact_type	pc \
	    sub_type		psub \
	    min_effl		0.185 \
	    min_allc		0.26 \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::mos_draw $drawdict]
}

#-------------------
# pMOS 1.8V
#-------------------

proc sg13g2::sg13_lv_pmos_draw {parameters} {
    set newdict [dict create \
	    gate_type		pfet \
	    diff_type 		pdiff \
	    diff_contact_type	pdc \
	    plus_diff_type	nsd \
	    plus_contact_type	nsc \
	    poly_type		poly \
	    poly_contact_type	pc \
	    sub_type		nwell \
	    dev_sub_type	nwell \
	    gate_to_polycont	0.32 \
	    min_effl		0.185 \
	    min_allc		0.26 \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::mos_draw $drawdict]
}

#-------------------
# pMOS 5.0V
#-------------------

proc sg13g2::sg13_hv_pmos_draw {parameters} {
    set newdict [dict create \
	    gate_type		hvpfet \
	    diff_type 		hvpdiff \
	    diff_contact_type	hvpdc \
	    plus_diff_type	hvnsd \
	    plus_contact_type	hvnsc \
	    poly_type		poly \
	    poly_contact_type	pc \
	    sub_type		nwell \
	    dev_sub_type	nwell \
	    guard_sub_surround	0.33 \
	    gate_to_polycont	0.32 \
	    diff_spacing	0.31 \
	    diff_tap_space	0.38 \
	    diff_gate_space	0.38 \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::mos_draw $drawdict]
}

#-------------------
# nMOS 5.0V
#-------------------

proc sg13g2::sg13_hv_nmos_draw {parameters} {
    set newdict [dict create \
	    gate_type		hvnfet \
	    diff_type 		hvndiff \
	    diff_contact_type	hvndc \
	    plus_diff_type	hvpsd \
	    plus_contact_type	hvpsc \
	    poly_type		poly \
	    poly_contact_type	pc \
	    sub_type		psub \
	    diff_spacing	0.31 \
	    diff_tap_space	0.38 \
	    diff_gate_space	0.38 \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::mos_draw $drawdict]
}

#----------------------------------------------------------------
# MOSFET: Check device parameters for out-of-bounds values
#----------------------------------------------------------------

proc sg13g2::mos_check {device parameters} {

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Normalize distance units to microns
    set l [magic::spice2float $l] 
    set l [magic::3digitpastdecimal $l]
    set w [magic::spice2float $w] 
    set w [magic::3digitpastdecimal $w]

    # nf, m must be integer
    if {![string is int $nf]} {
	puts stderr "NF must be an integer!"
        dict set parameters nf 1
    }
    if {![string is int $m]} {
	puts stderr "M must be an integer!"
        dict set parameters m 1
    }
    # diffcov, polycov must be numeric
    if {[catch {expr abs($diffcov)}]} {
	puts stderr "diffcov must be numeric!"
	set diffcov 100
	dict set parameters diffcov $diffcov
    }
    if {[catch {expr abs($polycov)}]} {
	puts stderr "polycov must be numeric!"
	set polycov 100
	dict set parameters polycov $polycov
    }

    if {$l < $lmin} {
	puts stderr "Mos length must be >= $lmin um"
        dict set parameters l $lmin
    } 
    if {$w < $wmin} {
	puts stderr "Mos width must be >= $wmin um"
        dict set parameters w $wmin
    } 
    if {$nf < 1} {
	puts stderr "NF must be >= 1"
        dict set parameters nf 1
    } 
    if {$m < 1} {
	puts stderr "M must be >= 1"
        dict set parameters m 1
    } 
    if {$diffcov < 20 } {
	puts stderr "Diffusion contact coverage must be at least 20%"
        dict set parameters diffcov 20
    } elseif {$diffcov > 100 } {
	puts stderr "Diffusion contact coverage can't be more than 100%"
        dict set parameters diffcov 100
    }
    if {$polycov < 20 } {
	puts stderr "Poly contact coverage must be at least 20%"
        dict set parameters polycov 20
    } elseif {$polycov > 100 } {
	puts stderr "Poly contact coverage can't be more than 100%"
        dict set parameters polycov 100
    }

    if {[catch {expr abs($viasrc)}]} {
	puts stderr "Source via coverage must be numeric!"
        dict set parameters viasrc 100
    } elseif {[expr abs($viasrc)] > 100} {
	puts stderr "Source via coverage can't be more than 100%"
        dict set parameters viasrc 100
    }
    if {[catch {expr abs($viadrn)}]} {
	puts stderr "Drain via coverage must be numeric!"
        dict set parameters viadrn 100
    } elseif {[expr abs($viadrn)] > 100} {
	puts stderr "Drain via coverage can't be more than 100%"
        dict set parameters viadrn 100
    }
    if {[catch {expr abs($viagate)}]} {
	puts stderr "Gate via coverage must be numeric!"
        dict set parameters viagate 100
    } elseif {[expr abs($viagate)] > 100} {
	puts stderr "Gate via coverage can't be more than 100%"
        dict set parameters viagate 100
    }
    if {[catch {expr abs($viagb)}]} {
	puts stderr "Guard ring bottom via coverage must be numeric!"
        dict set parameters viagb 0
    } elseif {[expr abs($viagb)] > 100} {
	puts stderr "Guard ring bottom via coverage can't be more than 100%"
        dict set parameters viagb 100
    }
    if {[catch {expr abs($viagt)}]} {
	puts stderr "Guard ring top via coverage must be numeric!"
        dict set parameters viagt 0
    } elseif {[expr abs($viagt)] > 100} {
	puts stderr "Guard ring top via coverage can't be more than 100%"
        dict set parameters viagt 100
    }
    if {[catch {expr abs($viagr)}]} {
	puts stderr "Guard ring right via coverage must be numeric!"
        dict set parameters viagr 0
    } elseif {[expr abs($viagr)] > 100} {
	puts stderr "Guard ring right via coverage can't be more than 100%"
        dict set parameters viagr 100
    }
    if {[catch {expr abs($viagl)}]} {
	puts stderr "Guard ring left via coverage must be numeric!"
        dict set parameters viagl 0
    } elseif {[expr abs($viagl)] > 100} {
	puts stderr "Guard ring left via coverage can't be more than 100%"
        dict set parameters viagl 100
    }

    # Values must satisfy diffusion-to-tap spacing of 15um.
    # Therefore the maximum of guard ring width or height cannot exceed 30um.
    # This requires detailed knowledge of the layout, so can only be estimated
    # here.  Since the estimate may be off, do not enforce the rule but just
    # generate a warning.

    # "clearance" is an estimation of the amount of space taken up by the
    # gate or source/drain contacts.
    set clearance 1.0

    set origm $m
    set orignf $nf
    while true {
       set yext [expr ($w + $clearance) * $m + $clearance]
       set xext [expr ($l + $clearance) * $nf + $clearance]
       if {[expr min($xext, $yext)] > 30.0} {
          if {$yext > 30.0 && $m > 1} {
	     incr m -1
	  } elseif {$xext > 30.0 && $nf > 1} {
	     incr nf -1
	  } elseif {$yext > 30.0} {
	     set w 29
	     puts -nonewline stderr "Transistor width must be < 29 um"
	     puts stderr " to avoid tap spacing violation."
	     dict set parameters w $w
	  } elseif {$xext > 30.0} {
	     set l 29
	     puts -nonewline stderr "Transistor length must be < 29 um"
	     puts stderr " to avoid tap spacing violation."
	     dict set parameters l $l
	  }
       } else {
	  break
       }
    }
    if {$m != $origm} {
       puts stderr "Warning: M may need to be reduced to prevent tap distance violation"
       # dict set parameters m $m
    }
    if {$nf != $orignf} {
       puts stderr "Warning: Fingers may need to be reduced to prevent tap distance violation"
       # dict set parameters nf $nf
    }

    catch {set magic::minfo_val ""}

    return $parameters
}

#----------------------------------------------------------------

proc sg13g2::sg13_lv_nmos_check {parameters} {
   return [sg13g2::mos_check sg13_lv_nmos $parameters]
}

proc sg13g2::sg13_hv_nmos_check {parameters} {
   return [sg13g2::mos_check sg13_hv_nmos $parameters]
}

proc sg13g2::sg13_lv_pmos_check {parameters} {
   return [sg13g2::mos_check sg13_lv_pmos $parameters]
}

proc sg13g2::sg13_hv_pmos_check {parameters} {
   return [sg13g2::mos_check sg13_hv_pmos $parameters]
}

#----------------------------------------------------------------
# Bipolar transistors:
# Starting by copying "fixed device" routines and picking the
# example file from sg13g2_pr.  To be done:  Create callback
# routines for the bipolar devices.  Given the difference in
# layout, each one will likely have its own set of drawing
# routines, although they may share the other callbacks.
#
# npn13g2
# npn13g2l
# npn13g2v
# pnpMPA
#----------------------------------------------------------------

proc sg13g2::npn13g2_defaults {} {
    return {w 0.07 l 0.9 area 0.063 peri 1.94 class bjt \
	lmin 0.9 lmax 0.9 nx 1 nxmax 10 \
	compatible {npn13g2 npn13g2l npn13g2v} \
	elc 1 erc 1 etc 1 ebc 1 doports 1}
}
proc sg13g2::npn13g2l_defaults {} {
    return {w 0.07 l 1.0 area 0.07 peri 2.14 class bjt \
	lmin 1.0 lmax 2.5 nx 1 nxmax 4 \
	compatible {npn13g2 npn13g2l npn13g2v} \
	elc 1 erc 1 etc 1 ebc 1 doports 1}
}

proc sg13g2::npn13g2v_defaults {} {
    return {w 0.12 l 1.0 area 0.12 peri 2.24 class bjt \
	lmin 1.0 lmax 5.0 nx 1 nxmax 8 \
	compatible {npn13g2 npn13g2l npn13g2v} \
	elc 1 erc 1 etc 1 ebc 1 doports 1}
}

proc sg13g2::pnpMPA_defaults {} {
    return {w 2.0 l 0.7 area 1.4 peri 5.4 \
	nx 1 ny 1 dummy 0 lmin 0.7 wmin 2.0 class bjt \
	elc 1 erc 1 etc 1 ebc 1 doverlap 0 doports 1 \
	full_metal 1 vias 1 viagb 0 viagt 0 viagl 0 viagr 0}
}

#----------------------------------------------------------------
# Bipolar device: Conversion from SPICE netlist parameters to toolkit
#----------------------------------------------------------------

proc sg13g2::bipolar_convert {parameters} {
    set pdkparams [dict create]
    dict for {key value} $parameters {
	switch -nocase $key {
	    m {
		 dict set pdkparams nx $value
	    }
	    we {
		 dict set pdkparams w $value
	    }
	    le {
		 dict set pdkparams l $value
	    }
	    default {
		# Allow unrecognized parameters to be passed unmodified
		dict set pdkparams $key $value
	    }
	}
    }
    return $pdkparams
}

#----------------------------------------------------------------

proc sg13g2::npn13g2_convert {parameters} {
    return [sg13g2::bipolar_convert $parameters]
}

proc sg13g2::npn13g2l_convert {parameters} {
    return [sg13g2::bipolar_convert $parameters]
}

proc sg13g2::npn13g2v_convert {parameters} {
    return [sg13g2::bipolar_convert $parameters]
}

proc sg13g2::pnpMPA_convert {parameters} {
    return [sg13g2::bipolar_convert $parameters]
}

#----------------------------------------------------------------
# Bipolar device: Interactively specifies the fixed layout parameters
#----------------------------------------------------------------

proc sg13g2::bipolar_dialog {device parameters} {
    # Editable fields:      l, area, perim, nx

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    magic::add_entry area "Area (um^2)" $parameters
    magic::add_entry peri "Perimeter (um)" $parameters
    sg13g2::compute_aptot $parameters
    magic::add_message atot "Total area (um^2)" $parameters
    magic::add_message ptot "Total perimeter (um)" $parameters
    magic::add_entry l "Emitter length (um)" $parameters
    magic::add_message w "Emitter width (um)" $parameters
    magic::add_entry nx "Number of emitters" $parameters

    if {[dict exists $parameters compatible]} {
       set sellist [dict get $parameters compatible]
       magic::add_selectlist gencell "Device type" $sellist $parameters $device
    }

    if {[dict exists $parameters guard]} {
        magic::add_checkbox full_metal "Full metal guard ring" $parameters
    }
    if {[dict exists $parameters glc]} {
        magic::add_checkbox glc "Add left guard ring contact" $parameters
    }
    if {[dict exists $parameters grc]} {
        magic::add_checkbox grc "Add right guard ring contact" $parameters
    }
    if {[dict exists $parameters gtc]} {
        magic::add_checkbox gtc "Add top guard ring contact" $parameters
    }
    if {[dict exists $parameters gbc]} {
        magic::add_checkbox gbc "Add bottom guard ring contact" $parameters
    }
    if {[dict exists $parameters viagb]} {
	magic::add_entry viagb  "Bottom guard ring via coverage \[+/-\](%)" $parameters
    }
    if {[dict exists $parameters viagt]} {
	magic::add_entry viagt  "Top guard ring via coverage \[+/-\](%)" $parameters
    }
    if {[dict exists $parameters viagr]} {
	magic::add_entry viagr  "Right guard ring via coverage \[+/-\](%)" $parameters
    }
    if {[dict exists $parameters viagl]} {
	magic::add_entry viagl  "Left guard ring via coverage \[+/-\](%)" $parameters
    }

    magic::add_dependency sg13g2::diode_recalc $device sg13g2 l w area peri

    if {[dict exists $parameters addports]} {
	magic::add_checkbox doports "Add ports" $parameters
    }
}

proc sg13g2::npn13g2_dialog {parameters} {
    sg13g2::bipolar_dialog npn13g2 $parameters
}

proc sg13g2::npn13g2l_dialog {parameters} {
    sg13g2::bipolar_dialog npn13g2l $parameters
}

proc sg13g2::npn13g2v_dialog {parameters} {
    sg13g2::bipolar_dialog npn13g2v $parameters
}

proc sg13g2::pnpMPA_dialog {parameters} {
    sg13g2::diode_dialog pnpMPA $parameters
}

#----------------------------------------------------------------
# SiGe bipolar base device:  This layout is fixed and cannot be
# modified.  If it exists already, then do not regenerate.
#----------------------------------------------------------------

proc sg13g2::npn13g2_base_generate {} {
    if {[cellname list exists npn13g2_base]} {return}
    suspendall

    # Save critical values before creating and editing a new cell 
    set curcell [cellname list self]
    set curbox [box values]
    # Stop the tag method from messing with this procedure
    set ltag [tag load]
    tag load {}
    load npn13g2_base -silent
    tech unlock *
    set savesnap [snap]
    snap internal

    # Everything here is fixed geometry
    box values -0.975um 1.02um 0.975um 1.26um
    paint m1
    box values -0.35um -0.77um 0.35um 0.785um
    paint m1
    box values -0.925um -1.25um 0.925um -1.01um
    paint m1
    
    box values -0.925um -0.77um 0.925um 0.785um
    paint m2

    box values -0.305um -0.705um 0.305um 0.725um
    paint via

    box values -1.055um -1.41um 1.055um -0.85um
    paint pwell

    box values -0.925um -1.28um 0.925um -0.98um
    paint ndiff

    box values -0.825um -1.21um 0.825um -1.05um
    paint ndc

    box values -0.76um 1.06um 0.76um 1.22um
    paint pbasec

    polygon nemitter -0.925um -0.98um 0.925um -0.98um 0.925um 0.62um \
		0.445um 0.62um 0.235um 0.83um -0.235um 0.83um \
		-0.445um 0.62um -0.925um 0.62um

    polygon pbase -1.015um 2.47um -1.015um 0.86um -0.595um 0.44um \
		-0.595um -0.73um -0.345um -0.98um 0.345um -0.98um \
		0.595um -0.73um 0.595um 0.44um 1.015um 0.86um \
		1.015um 2.47um

    box values -0.035um -0.45um 0.035um 0.45um
    paint gemitterc

    # Cell has a text label at the bottom;  force it to attach to "comment"
    box position 0 -2.3um
    box size 0 0
    label npn13G2 c space
    select area label
    setlabel sticky 1
    setlabel layer comment
    
    # Return to our regularly scheduled program
    load $curcell
    snap $savesnap
    tag load $ltag
    tech revert
    box values {*}$curbox
    resumeall
}

#----------------------------------------------------------------
# Bipolar devices: Draw the device.  Each bipolar type has its
# own unique drawing routine.
#----------------------------------------------------------------

proc sg13g2::npn13g2_draw {parameters} {

    # To be done:  Loop over number of emitters

    tech unlock *
    set savesnap [snap]
    snap internal

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Emitter width is exactly 0.07um and cannot be changed
    set w 0.07

    box values 0 0 0 0
    pushbox

    # The guard ring is not centered on the device
    box move n 0.225um

    # Draw the guard ring first, then remove any pwell from inside
    set gx [+ 5.73 $w]
    set gy [+ 5.31 $l]
    dict set parameters contact_size ${contact_size}
    dict set parameters metal_surround ${metal_surround}
    dict set parameters metal_spacing ${metal_spacing}
    dict set parameters diff_surround ${diff_surround}
    dict set parameters sub_surround 0.05
    dict set parameters plus_diff_type    psd
    dict set parameters plus_contact_type psc
    set cext [sg13g2::guard_ring $gx $gy $parameters]
    select top cell
    box grow c -0.4um
    erase pwell

    popbox
    pushbox
    if {![cellname list exists npn13G2_base]} {sg13g2::npn13g2_base_generate}
    getcell npn13g2_base child 0 0
    popbox
    pushbox

    box grow c 0.745um
    box grow n 0.03um
    box grow s 0.03um
    paint m2
    if {$doports} {
	label E c m2
	port make
    }

    popbox
    pushbox

    box move n 1.135um
    box grow n 0.12um
    box grow s 0.12um
    box grow e 0.92um
    box grow w 0.92um
    paint m1
    if {$doports} {
	label C c m1
	port make
    }

    popbox
    pushbox

    box move s 1.135um
    box grow n 0.12um
    box grow s 0.12um
    box grow e 0.97um
    box grow w 0.97um
    paint m1
    if {$doports} {
	label B c m1
	port make
    }
    popbox

    snap $savesnap
    tech revert
}

#----------------------------------------------------------------

proc sg13g2::npn13g2l_draw {parameters} {

    # To be done:  Loop over number of emitters

    tech unlock *
    set savesnap [snap]
    snap internal

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Emitter width is exactly 0.07um and cannot be changed
    set w 0.07

    box values 0 0 0 0
    pushbox

    # Draw the guard ring first, otherwise it overwrites pbase
    set gx [+ 6.82 $w]
    set gy [+ 5.31 $l]
    dict set parameters contact_size ${contact_size}
    dict set parameters metal_surround ${metal_surround}
    dict set parameters metal_spacing ${metal_spacing}
    dict set parameters diff_surround ${diff_surround}
    dict set parameters sub_surround 0.05
    dict set parameters plus_diff_type    psd
    dict set parameters plus_contact_type psc
    set cext [sg13g2::guard_ring $gx $gy $parameters]
 
    # Length and width of the device are the length and width of
    # the emitter window/contact.  All other dimensions are
    # derived from this.

    set hl [/ $l 2.0]
    set hw [/ $w 2.0]

    box grow w ${hw}um
    box grow e ${hw}um
    box grow n ${hl}um
    box grow s ${hl}um
    pushbox

    # Draw the substrate underneath
    pushbox
    box grow n 0.41um
    box grow s 0.41um
    box grow e 1.495um
    box grow w 1.495um
    paint pwell
    popbox

    # Draw the via over the emitter
    box grow e 0.065um
    box grow w 0.065um
    box grow n 0.105um
    box grow s 0.105um
    pushbox

    # Draw the full emitter diffusion area
    box grow e 0.03um
    box grow w 0.03um
    box grow n 0.175um
    box grow s 0.175um
    pushbox

    # Draw the base area
    box grow e 0.61um
    box grow w 0.61um
    pushbox

    # Draw the collector right side
    box move e [box width]
    box width 0.66um
    paint ndiff

    popbox
    pushbox

    # Draw the collector left side
    box move w 0.66um
    box width 0.66um
    paint ndiff

    popbox
    paint pbase

    popbox
    paint nemitter

    popbox
    paint via

    popbox
    pushbox

    # Draw the emitter metal1 area
    box grow n 0.2um
    box grow s 0.2um
    box grow e 0.095um
    box grow w 0.095um
    paint m1

    # Draw the emitter metal2 area (extends over the whole device)
    box grow e 1.27um
    box grow w 1.27um
    paint m2

    popbox
    pushbox

    # Draw the right side base contact
    box move e [box width]
    box move e 0.32um
    box width 0.16um
    box grow n 0.2um
    box grow s 0.2um
    paint pbasec
    box grow n 0.08um
    box grow s 1.45um
    paint m1

    popbox
    pushbox

    # Draw the left side base contact
    box move w 0.48um
    box width 0.16um
    box grow n 0.2um
    box grow s 0.2um
    paint pbasec
    box grow n 0.08um
    box grow s 1.45um
    paint m1

    # Connect the two base sides across the bottom
    box height 0.65um
    box width 0.96um
    box grow e ${w}um
    paint m1
    if {$doports} {
	label B c m1
	port make
    }

    popbox
    pushbox

    # Paint the right side collector contact
    box move e [box width]
    box move e 1.025um
    box width 0.16um
    box grow s 0.15um
    box grow n 0.15um
    paint ndiffc
    box grow c 0.05um
    box grow s 0.08um
    box grow n 1.45um
    box grow e 0.13um
    paint m1

    popbox
    pushbox

    # Paint the left side collector contact
    box move w 1.185um
    box width 0.16um
    box grow s 0.15um
    box grow n 0.15um
    paint ndiffc
    box grow c 0.05um
    box grow s 0.08um
    box grow n 1.45um
    box grow w 0.13um
    paint m1

    # Connect the two collector sides across the top
    box move n 1.28um
    box move n ${l}um
    box height 0.65um
    box width 2.73um
    box grow e ${w}um
    paint m1
    if {$doports} {
	label C c m1
	port make
    }

    popbox
    paint nemitterc
    if {$doports} {
	label E c nemitterc
	port make
    }

    popbox
    snap $savesnap
    tech revert
}

#----------------------------------------------------------------
# Draw the high-voltage NPN
#----------------------------------------------------------------

proc sg13g2::npn13g2v_draw {parameters} {

    # To be done:  Loop over number of emitters

    tech unlock *
    set savesnap [snap]
    snap internal

    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Emitter width is exactly 0.12um and cannot be changed
    set w 0.12

    box values 0 0 0 0
    pushbox

    # Draw the guard ring first, otherwise it overwrites pbase
    set gx [+ 6.72 $w]
    set gy [+ 5.3 $l]
    dict set parameters contact_size ${contact_size}
    dict set parameters metal_surround ${metal_surround}
    dict set parameters metal_spacing ${metal_spacing}
    dict set parameters diff_surround ${diff_surround}
    dict set parameters sub_surround 0.05
    dict set parameters plus_diff_type    psd
    dict set parameters plus_contact_type psc
    set cext [sg13g2::guard_ring $gx $gy $parameters]
 
    # Length and width of the device are the length and width of
    # the emitter window/contact.  All other dimensions are
    # derived from this.

    set hl [/ $l 2.0]
    set hw [/ $w 2.0]
    box grow w ${hw}um
    box grow e ${hw}um
    box grow n ${hl}um
    box grow s ${hl}um
    pushbox

    # Draw the substrate
    pushbox
    box grow n 0.41um
    box grow s 0.41um
    box grow e 1.24um
    box grow w 1.24um
    paint pwell
    popbox

    # Draw the via over the emitter
    box grow e 0.04um
    box grow w 0.04um
    box grow n 0.235um
    box grow s 0.235um
    pushbox

    # Draw the full emitter diffusion area
    box grow e 0.03um
    box grow w 0.03um
    box grow n 0.045um
    box grow s 0.045um
    pushbox

    # Draw the base area
    box grow e 0.635um
    box grow w 0.635um
    pushbox

    # Draw the collector right side
    box move e [box width]
    box width 0.405um
    paint ndiff

    popbox
    pushbox

    # Draw the collector left side
    box move w 0.405um
    box width 0.405um
    paint ndiff

    popbox
    paint pbase

    popbox
    paint hvnemitter

    popbox
    paint via

    popbox
    pushbox

    # Draw the emitter metal1 area
    box grow n 0.28um
    box grow s 0.28um
    box grow e 0.07um
    box grow w 0.07um
    paint m1

    # Draw the emitter metal2 area (extends over the whole device)
    box grow e 1.04um
    box grow w 1.04um
    paint m2

    popbox
    pushbox

    # Draw the right side base contact
    box move e [box width]
    box move e 0.295um
    box width 0.17um
    box grow n 0.28um
    box grow s 0.28um
    paint pbasec
    box grow s 1.37um
    paint m1

    popbox
    pushbox

    # Draw the left side base contact
    box move w 0.465um
    box width 0.17um
    box grow n 0.28um
    box grow s 0.28um
    paint pbasec
    box grow s 1.37um
    paint m1

    # Connect the two base sides across the bottom
    box height 0.65um
    box width 0.93um
    box grow e ${w}um
    paint m1
    if {$doports} {
	label B c m1
	port make
    }

    popbox
    pushbox

    # Paint the right side collector contact
    box move e [box width]
    box move e 0.85um
    box width 0.16um
    box grow s 0.21um
    box grow n 0.21um
    paint ndiffc
    box grow c 0.06um
    box grow s 0.01um
    box grow n 1.38um
    box grow e 0.04um
    paint m1

    popbox
    pushbox

    # Paint the left side collector contact
    box move w 1.01um
    box width 0.16um
    box grow s 0.21um
    box grow n 0.21um
    paint ndiffc
    box grow c 0.06um
    box grow s 0.01um
    box grow n 1.38um
    box grow w 0.04um
    paint m1

    # Connect the two collector sides across the top
    box move n 1.28um
    box move n ${l}um
    box height 0.65um
    box width 2.22um
    box grow e ${w}um
    paint m1
    if {$doports} {
	label C c m1
	port make
    }

    popbox
    paint hvnemitterc
    if {$doports} {
	label E c hvnemitterc
	port make
    }

    popbox
    snap $savesnap
    tech revert
}

#----------------------------------------------------------------
# The PNP is drawn like a diode (base, emitter)
# with a guard ring (collector)
#----------------------------------------------------------------

proc sg13g2::pnpMPA_draw {parameters} {
    # Set a local variable for each rule in ruleset
    foreach key [dict keys $sg13g2::ruleset] {
        set $key [dict get $sg13g2::ruleset $key]
    }

    set newdict [dict create \
	    guard		1 \
	    dev_type		pdiff \
	    dev_contact_type	pdc \
	    end_type		nsd \
	    end_contact_type	nsc \
	    end_sub_type	nbase \
	    dev_spacing		${diff_spacing} \
	    dev_surround	${diff_surround} \
	    end_spacing		${diff_spacing} \
	    end_surround	${diff_surround} \
    ]
    set drawdict [dict merge $sg13g2::ruleset $newdict $parameters]
    return [sg13g2::diode_draw $drawdict]
}

#----------------------------------------------------------------
# Bipolar device: Check device parameters for out-of-bounds values
#----------------------------------------------------------------

proc sg13g2::bipolar_check {parameters} {

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # nx must be integer and less that maximum
    if {![string is int $nx]} {
	puts stderr "Number of emitters must be an integer!"
        dict set parameters nx 1
    }
    if {$nxmax > 0 && $nx > $nxmax} {
	puts stderr "Number of emitters must be <= $nxmax"
	dict set parameters nx $nxmax
    }

    # Length muxt be within limits
    if {$l < $lmin} {
	puts stderr "Emitter length must be >= $lmin um"
	dict set parameters l $lmin
    } 
    if {$lmax > 0 && $l > $lmax} {
	puts stderr "Emitter length must be <= $lmax um"
	dict set parameters l $lmax
    }
    

    return $parameters
}

#----------------------------------------------------------------

proc sg13g2::npn13g2_check {parameters} {
    return [sg13g2::bipolar_check $parameters]
}

proc sg13g2::npn13g2l_check {parameters} {
    return [sg13g2::bipolar_check $parameters]
}

proc sg13g2::npn13g2v_check {parameters} {
    return [sg13g2::bipolar_check $parameters]
}

proc sg13g2::pnpMPA_check {parameters} {
    return [sg13g2::diode_check $parameters]
}

#----------------------------------------------------------------
# Bond pad device (special drawing routine)
#----------------------------------------------------------------

proc sg13g2::bondpad_defaults {} {
    return {height 80 width 80 depth 5 class special shape octagon}
}

proc sg13g2::bondpad_convert {parameters} {
    set pdkparams [dict create]
    dict for {key value} $parameters {
	switch -nocase $key {
	    size {
		dict set pdkparams width $value
		dict set pdkparams height $value
	    }
	    default {
		# Allow unrecognized parameters to be passed unmodified
		dict set pdkparams $key $value
	    }
	}
    }
    return $pdkparams
}

proc sg13g2::bondpad_dialog {parameters} {
    # Instance fields:	    width, height, depth, shape
    # Editable fields:	    width, height, depth, shape
    # Non-editable fields:  

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    magic::add_entry width "W" $parameters
    magic::add_entry height "H" $parameters
    magic::add_entry depth "Number metals" $parameters
    set sellist {octagon square circle}
    magic::add_selectlist shape "Shape" $sellist $parameters octagon
}

# To be completed:  Needs to resolve round-off errors to preserve 45
# degree angles, and needs contacts to lower metals.

proc sg13g2::bondpad_draw {parameters} {
    tech unlock *
    set savesnap [snap]
    snap internal

    # Set defaults if they are not in parameters
    set shape	octagon	;# Draw an octagon shape
    set depth	3	;# Use three metal layers for the pad

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    pushbox
    box values 0 0 0 0

    # Calculate pad polygons by drawing a circle of N points, where
    # N is 4 for a square, 8 for an octagon, and we will arbitrarily
    # set N to 24 for a circle.  The polygon will assume that W = L,
    # and points will be adjusted to make up the difference.

    if {$shape == "octagon"} {
	set npoints 8
    } elseif {$shape == "circle"} {
	# Note that this will violate DRC rules.
	set npoints 24
    } else {
	set npoints 4
    }

    if {$width < $height} {set minside $width} else {set minside $height}
    set pwidth [- $minside 4.2]		;# passivation cut layer width
    set mcenter [- $minside 4.0]	;# metal (< 7) centerline

    set angle [/ 180.0 $npoints]
    set adelta [/ 360.0 $npoints]
    set pi 3.1415926
    set rad [/ [* $angle $pi] 180]
    set rdelta [/ [* $adelta $pi] 180]

    # Expand width so that the flats of the polygon reach the value of "minside"
    set rwidth [/ [/ $minside 2] [cos $rad]]
    set pwidth [/ [/ $pwidth 2] [cos $rad]]
    set mcenter [/ [/ $mcenter 2] [cos $rad]]

    set metal_edge {}	;# outside edge of pad metal
    set pad_edge {}	;# outside edge of passivation cut
    set metal_center {}	;# centerline of lower metals

    # Compute point at angle 0 for the wire endpoints, so that the
    # wire starts and ends on a straight edge.
    set x [- [/ $minside 2.0] 2.0]
    set y 0.0
    set ux [magic::i2u [magic::magceil [magic::u2i $x]]]
    set uy [magic::i2u [magic::magceil [magic::u2i $y]]]
    if {$width > $minside} {
	if {$ux > 0} {
	    set ux [+ $ux [/ [- $width $minside] 2.0]]
	} else {
	    set ux [- $ux [/ [- $width $minside] 2.0]]
	}
    }
    lappend metal_center ${ux}um ${uy}um

    for {set i 0} {$i < $npoints} {incr i} {
        set x [* $rwidth [cos $rad]]
        set y [* $rwidth [sin $rad]]
        set ux [magic::i2u [magic::magceil [magic::u2i $x]]]
        set uy [magic::i2u [magic::magceil [magic::u2i $y]]]
        if {$width > $minside} {
	    if {$ux > 0} {
		set ux [+ $ux [/ [- $width $minside] 2.0]]
	    } else {
		set ux [- $ux [/ [- $width $minside] 2.0]]
	    }
	}
	if {$height > $minside} {
	    if {$uy > 0} {
		set uy [+ $uy [/ [- $height $minside] 2.0]]
	    } else {
		set uy [- $uy [/ [- $height $minside] 2.0]]
	    }
	}
	lappend metal_edge ${ux}um ${uy}um

	set x [* $pwidth [cos $rad]]
	set y [* $pwidth [sin $rad]]
	set ux [magic::i2u [magic::magceil [magic::u2i $x]]]
	set uy [magic::i2u [magic::magceil [magic::u2i $y]]]
        if {$width > $minside} {
	    if {$ux > 0} {
		set ux [+ $ux [/ [- $width $minside] 2.0]]
	    } else {
		set ux [- $ux [/ [- $width $minside] 2.0]]
	    }
	}
	if {$height > $minside} {
	    if {$uy > 0} {
		set uy [+ $uy [/ [- $height $minside] 2.0]]
	    } else {
		set uy [- $uy [/ [- $height $minside] 2.0]]
	    }
	}
	lappend pad_edge ${ux}um ${uy}um

	set x [* $mcenter [cos $rad]]
	set y [* $mcenter [sin $rad]]
	set ux [magic::i2u [magic::magfloor [magic::u2i $x]]]
	set uy [magic::i2u [magic::magfloor [magic::u2i $y]]]
        if {$width > $minside} {
	    if {$ux > 0} {
		set ux [+ $ux [/ [- $width $minside] 2.0]]
	    } else {
		set ux [- $ux [/ [- $width $minside] 2.0]]
	    }
	}
	if {$height > $minside} {
	    if {$uy > 0} {
		set uy [+ $uy [/ [- $height $minside] 2.0]]
	    } else {
		set uy [- $uy [/ [- $height $minside] 2.0]]
	    }
	}
	lappend metal_center ${ux}um ${uy}um

	set angle [+ $angle $adelta]
	set rad [+ $rad $rdelta]
    }

    # Repeat the first coordinate pair at the end of metal_center
    lappend metal_center {*}[lrange $metal_center 0 1]

    # Diagnostic
    puts stdout "metal_edge = $metal_edge"
    puts stdout "pad_edge = $pad_edge"
    puts stdout "metal_center = $metal_center"

    # Create polygons
    polygon metal7 {*}$metal_edge
    polygon pad {*}$pad_edge

    # Create lower metals as wires 4um wide, matching the outer
    # edge of the pad metal.
    if {$depth > 1} {wire segment metal6 4um {*}$metal_center}
    if {$depth > 2} {wire segment metal5 4um {*}$metal_center}
    if {$depth > 3} {wire segment metal4 4um {*}$metal_center}
    if {$depth > 4} {wire segment metal3 4um {*}$metal_center}
    if {$depth > 5} {wire segment metal2 4um {*}$metal_center}
    if {$depth > 6} {wire segment metal1 4um {*}$metal_center}

    popbox

    snap $savesnap
    tech revert
}

#----------------------------------------------------------------
# Bondpad device: Check device parameters for out-of-bounds values
#----------------------------------------------------------------

proc sg13g2::bondpad_check {parameters} {

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Metal depth must be 1 to 7
    if {$depth < 1} {
	puts stderr "depth must be >= 1"
        dict set parameters depth 1
    }
    if {$depth > 7} {
	puts stderr "depth must be <= 7"
        dict set parameters depth 7
    }

    # Min/Max passivation layer recommended dimensions are 30 and 150.
    # The pad is measured according to the metal layer dimension, where
    # the metal overlaps passivation cut by 2.1um, so add 4.2 to these.
    if {$width < 34.2} {
	puts stderr "width must be >= 34.2"
        dict set parameters width 34.2
    }
    if {$width > 154.2} {
	puts stderr "width must be <= 154.2"
        dict set parameters width 154.2
    }
    if {$height < 34.2} {
	puts stderr "height must be >= 34.2"
        dict set parameters height 34.2
    }
    if {$height > 154.2} {
	puts stderr "height must be <= 154.2"
        dict set parameters height 154.2
    }

    return $parameters
}

#----------------------------------------------------------------

proc sg13g2::diodevdd_2kv_check {parameters} {
    return [sg13g2::fixed_check $parameters]
}

#----------------------------------------------------------------
# Fixed device: Specify all user-editable default values
#
# deltax --- Additional horizontal space between devices
# deltay --- Additional vertical space between devices
# nx     --- Number of arrayed devices in X
# ny     --- Number of arrayed devices in Y
#
# Note that these values, specifically nx, ny, deltax,
# and deltay, are properties of the instance, not the cell.
# They translate to the instance array x and y counts;  while
# deltax is the x pitch less the cell width, and deltay is the
# y pitch less the cell height.
#
# non-user-editable
#
# nocell --- Indicates that this cell has a predefined layout
#	     and therefore there is no cell to draw.
# xstep  --- Width of the cell (nominal array pitch in X)
# ystep  --- Height of the cell (nominal array pitch in Y)
#----------------------------------------------------------------

# Fixed-layout devices (from sg13g2_fd_pr library)
#
# diodevdd_2kv
# diodevdd_4kv
# diodevss_2kv
# diodevss_4kv
# nmoscl_2
# nmoscl_4

proc sg13g2::diodevdd_2kv_defaults {} {
    return {nx 1 ny 1 deltax 0 deltay 0 nocell 1 \
    compatible {diodevdd_2kv diodevdd_4kv diodevss_2kv diodevss_4kv} \
    xstep 7.03 ystep 7.03 class diode}
}
proc sg13g2::diodevdd_4kv_defaults {} {
    return {nx 1 ny 1 deltax 0 deltay 0 nocell 1 \
    compatible {diodevdd_2kv diodevdd_4kv diodevss_2kv diodevss_4kv} \
    xstep 7.03 ystep 8.03 class diode}
}

proc sg13g2::diodevss_2kv_defaults {} {
    return {nx 1 ny 1 deltax 0 deltay 0 nocell 1 \
    compatible {diodevdd_2kv diodevdd_4kv diodevss_2kv diodevss_4kv} \
    xstep 7.03 ystep 7.03 class diode}
}

proc sg13g2::diodevss_4kv_defaults {} {
    return {nx 1 ny 1 deltax 0 deltay 0 nocell 1 \
    compatible {diodevdd_2kv diodevdd_4kv diodevss_2kv diodevss_4kv} \
    xstep 7.03 ystep 8.03 class diode}
}

proc sg13g2::nmoscl_2_defaults {} {
    return {nx 1 ny 1 deltax 0 deltay 0 nocell 1 \
    compatible {nmoscl_2 nmoscl_4} \
    xstep 3.72 ystep 3.72 class mosfet}
}

proc sg13g2::nmoscl_4_defaults {} {
    return {nx 1 ny 1 deltax 0 deltay 0 nocell 1 \
    compatible {nmoscl_2 nmoscl_4} \
    xstep 6.44 ystep 6.44 class mosfet}
}

#----------------------------------------------------------------
# Bipolar device: Conversion from SPICE netlist parameters to toolkit
#----------------------------------------------------------------

proc sg13g2::fixed_convert {parameters} {
    set pdkparams [dict create]
    dict for {key value} $parameters {
	switch -nocase $key {
	    m {
		 dict set pdkparams nx $value
	    }
	    default {
		# Allow unrecognized parameters to be passed unmodified
		dict set pdkparams $key $value
	    }
	}
    }
    return $pdkparams
}

#----------------------------------------------------------------
# To be reworked:  Bipolars are not fixed devices.  Each has
# a unique layout and needs its own drawing routine.
#----------------------------------------------------------------

proc sg13g2::diodevdd_2kv_convert {parameters} {
    return [sg13g2::fixed_convert $parameters]
}

proc sg13g2::diodevdd_4kv_convert {parameters} {
    return [sg13g2::fixed_convert $parameters]
}

proc sg13g2::diodevss_2kv_convert {parameters} {
    return [sg13g2::fixed_convert $parameters]
}

proc sg13g2::diodevss_4kv_convert {parameters} {
    return [sg13g2::fixed_convert $parameters]
}

proc sg13g2::nmoscl_2_convert {parameters} {
    return [sg13g2::fixed_convert $parameters]
}

proc sg13g2::nmoscl_4_convert {parameters} {
    return [sg13g2::fixed_convert $parameters]
}

#----------------------------------------------------------------
# Fixed device: Interactively specifies the fixed layout parameters
#----------------------------------------------------------------

proc sg13g2::fixed_dialog {device parameters} {
    # Instance fields:	    nx, ny, pitchx, pitchy
    # Editable fields:	    nx, ny, deltax, deltay
    # Non-editable fields:  nocell, xstep, ystep

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # "nocell" field causes nx and ny to be dropped in from
    # "array count".  Also "pitchx" and "pitchy" are passed
    # in internal units.  Convert these to microns and generate
    # If there is no pitchx and pitchy, then the device has not
    # yet been created, so keep the deltax and deltay defaults.

    if [dict exists $parameters pitchx] {
	set pitchux [magic::i2u $pitchx]
	set stepux [magic::spice2float $xstep]
        set deltax [magic::3digitpastdecimal [expr $pitchux - $stepux]] 
        # An array size 1 should not cause deltax to go negative
	if {$deltax < 0.0} {set deltax 0.0}
	dict set parameters deltax $deltax
    }
    if [dict exists $parameters pitchy] {
	set pitchuy [magic::i2u $pitchy]
	set stepuy [magic::spice2float $ystep]
        set deltay [magic::3digitpastdecimal [expr $pitchuy - $stepuy]] 
        # An array size 1 should not cause deltay to go negative
	if {$deltay < 0.0} {set deltay 0.0}
	dict set parameters deltay $deltay
    }

    magic::add_entry nx "NX" $parameters
    magic::add_entry ny "NY" $parameters
    magic::add_entry deltax "X step (um)" $parameters
    magic::add_entry deltay "Y step (um)" $parameters

    if {[dict exists $parameters compatible]} {
       set sellist [dict get $parameters compatible]
       magic::add_selectlist gencell "Device type" $sellist $parameters $device
    }
}

proc sg13g2::diodevdd_2kv_dialog {parameters} {
    sg13g2::fixed_dialog diodevdd_2kv $parameters
}

proc sg13g2::diodevdd_4kv_dialog {parameters} {
    sg13g2::fixed_dialog diodevdd_4kv $parameters
}

proc sg13g2::diodevss_2kv_dialog {parameters} {
    sg13g2::fixed_dialog diodevss_2kv $parameters
}

proc sg13g2::diodevss_4kv_dialog {parameters} {
    sg13g2::fixed_dialog diodevss_4kv $parameters
}

proc sg13g2::nmoscl_2_dialog {parameters} {
    sg13g2::fixed_dialog nmoscl_2 $parameters
}

proc sg13g2::nmoscl_4_dialog {parameters} {
    sg13g2::fixed_dialog nmoscl_4 $parameters
}

#----------------------------------------------------------------
# Fixed device: Draw the device
#----------------------------------------------------------------

proc sg13g2::fixed_draw {devname parameters} {

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # This cell declares "nocell" in parameters, so it needs to
    # instance the cell and set properties.

    # Instantiate the cell.  The name corresponds to the cell in the sg13g2_fd_pr_* directory.
    set instname [getcell ${devname}]

    set deltax [magic::spice2float $deltax] 
    set deltay [magic::spice2float $deltay] 
    set xstep [magic::spice2float $xstep] 
    set ystep [magic::spice2float $ystep] 

    # Array stepping
    if {$nx > 1 || $ny > 1} {
        set xstep [expr $xstep + $deltax]
        set ystep [expr $ystep + $deltay]
        box size ${xstep}um ${ystep}um
	array $nx $ny
    }
    select cell $instname
    expand
    return $instname
}

#----------------------------------------------------------------
# No additional parameters declared for drawing
#----------------------------------------------------------------

proc sg13g2::diodevdd_2kv_draw {parameters} {
    return [sg13g2::fixed_draw diodevdd_2kv $parameters]
}

proc sg13g2::diodevdd_4kv_draw {parameters} {
    return [sg13g2::fixed_draw diodevdd_4kv $parameters]
}

proc sg13g2::diodevss_2kv_draw {parameters} {
    return [sg13g2::fixed_draw diodevss_2kv $parameters]
}

proc sg13g2::diodevss_4kv_draw {parameters} {
    return [sg13g2::fixed_draw diodevss_4kv $parameters]
}

proc sg13g2::nmoscl_2_draw {parameters} {
    return [sg13g2::fixed_draw nmoscl_2 $parameters]
}

proc sg13g2::nmoscl_4_draw {parameters} {
    return [sg13g2::fixed_draw nmoscl_4 $parameters]
}

#----------------------------------------------------------------
# Fixed device: Check device parameters for out-of-bounds values
#----------------------------------------------------------------

proc sg13g2::fixed_check {parameters} {

    # Set a local variable for each parameter (e.g., $l, $w, etc.)
    foreach key [dict keys $parameters] {
        set $key [dict get $parameters $key]
    }

    # Normalize distance units to microns
    set deltax [magic::spice2float $deltax -1] 
    set deltax [magic::3digitpastdecimal $deltax]
    set deltay [magic::spice2float $deltay -1] 
    set deltay [magic::3digitpastdecimal $deltay]

    # nx, ny must be integer
    if {![string is int $nx]} {
	puts stderr "NX must be an integer!"
        dict set parameters nx 1
    }
    if {![string is int $ny]} {
	puts stderr "NY must be an integer!"
        dict set parameters nx 1
    }

    # Number of devices in X and Y must be at least 1
    if {$nx < 1} {
	puts stderr "NX must be >= 1"
        dict set parameters nx 1
    }
    if {$ny < 1} {
	puts stderr "NY must be >= 1"
        dict set parameters nx 1
    }
    # Step less than zero violates DRC
    if {$deltax < 0} {
	puts stderr "X step must be >= 0"
        dict set parameters deltax 0
    }
    if {$deltay < 0} {
	puts stderr "Y step must be >= 0"
        dict set parameters deltay 0
    }
    return $parameters
}

#----------------------------------------------------------------

proc sg13g2::diodevdd_2kv_check {parameters} {
    return [sg13g2::fixed_check $parameters]
}

proc sg13g2::diodevdd_4kv_check {parameters} {
    return [sg13g2::fixed_check $parameters]
}

proc sg13g2::diodevss_2kv_check {parameters} {
    return [sg13g2::fixed_check $parameters]
}

proc sg13g2::diodevss_4kv_check {parameters} {
    return [sg13g2::fixed_check $parameters]
}

proc sg13g2::nmoscl_2_check {parameters} {
    return [sg13g2::fixed_check $parameters]
}

proc sg13g2::nmoscl_4_check {parameters} {
    return [sg13g2::fixed_check $parameters]
}

#-------------------------------------------------------------------
# General-purpose routines for the PDK script in all technologies
#-------------------------------------------------------------------
# 
#----------------------------------------
# Number Conversion Functions
#----------------------------------------

#---------------------
# Microns to Lambda
#---------------------
proc magic::u2l {micron} {
    set techlambda [magic::tech lambda]
    set tech1 [lindex $techlambda 1]
    set tech0 [lindex $techlambda 0]
    set tscale [expr {$tech1 / $tech0}]
    set lambdaout [expr {((round([magic::cif scale output] * 10000)) / 10000.0)}]
    return [expr $micron / ($lambdaout*$tscale) ]
}

#---------------------
# Lambda to Microns
#---------------------
proc magic::l2u {lambda} {
    set techlambda [magic::tech lambda]
    set tech1 [lindex $techlambda 1] ; set tech0 [lindex $techlambda 0]
    set tscale [expr {$tech1 / $tech0}]
    set lambdaout [expr {((round([magic::cif scale output] * 10000)) / 10000.0)}]
    return [expr $lambda * $lambdaout * $tscale ]
}

#---------------------
# Internal to Microns
#---------------------
proc magic::i2u { value } {
    set value [expr {((round([magic::cif scale output] * 10000)) / 10000.0) * $value}]
    return [format "%.3f" $value]
}

#---------------------
# Microns to Internal
#---------------------
proc magic::u2i {value} {
    return [expr {round($value / ((round([magic::cif scale output] * 10000)) / 10000.0))}]
}

#--------------------------------------------------------------------------
# Find the smallest integer value greater in magnitude than the given value
#--------------------------------------------------------------------------
proc magic::magceil {value} {
    if {$value < 0} {
	return [::tcl::mathfunc::floor $value]
    } else {
	return [::tcl::mathfunc::ceil $value]
    }
}

#--------------------------------------------------------------------------
# Find the largest integer value less in magnitude than the given value
#--------------------------------------------------------------------------
proc magic::magfloor {value} {
    if {$value < 0} {
	return [::tcl::mathfunc::ceil $value]
    } else {
	return [::tcl::mathfunc::floor $value]
    }
}

#---------------------
# Float to Spice 
#---------------------
proc magic::float2spice {value} { 
    if {$value >= 1.0e+6} { 
	set exponent 1e+6
	set unit "meg"
    } elseif {$value >= 1.0e+3} { 
	set exponent 1e+3
	set unit "k"
    } elseif { $value >= 1} { 
	set exponent 1
	set unit ""
    } elseif {$value >= 1.0e-3} { 
	set exponent 1e-3
	set unit "m"
    } elseif {$value >= 1.0e-6} { 
	set exponent 1e-6
	set unit "u"
    } elseif {$value >= 1.0e-9} { 
	set exponent 1e-9
	set unit "n"
    } elseif {$value >= 1.0e-12} { 
	set exponent 1e-12
	set unit "p"
    } elseif {$value >= 1.0e-15} { 
	set exponent 1e-15
	set unit "f"
    } else {
	set exponent 1e-18
	set unit "a"
    }
    set val [expr $value / $exponent]
    set val [expr int($val * 1000) / 1000.0]
    if {$val == 0} {set unit ""}
    return $val$unit
}

#---------------------
# Spice to Float
#---------------------
proc magic::spice2float {value {faultval 0.0}} { 
    # Remove trailing units, at least for some common combinations
    set value [string tolower $value]
    set value [string map {um u nm n uF n nF n pF p aF a} $value]
    set value [string map {meg "* 1.0e6" k "* 1.0e3" m "* 1.0e-3" u "* 1.0e-6" \
		 n "* 1.0 e-9" p "* 1.0e-12" f "* 1.0e-15" a "* 1.0e-15"} $value]
    if {[catch {set rval [expr $value]}]} {
	puts stderr "Value is not numeric!"
	set rval $faultval
    }
    return $rval
}

#---------------------
# Numeric Precision
#---------------------
proc magic::3digitpastdecimal {value} {
    set new [expr int([expr $value * 1000 + 0.5 ]) / 1000.0]
    return $new
}

#-------------------------------------------------------------------
# File Access Functions
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# Ensures that a cell name does not already exist, either in
# memory or on disk. Modifies the name until it does.
#-------------------------------------------------------------------
proc magic:cellnameunique {cellname} {
    set i 0
    set newname $cellname
    while {[cellname list exists $newname] != 0 || [magic::searchcellondisk $newname] != 0} {
	incr i
	set newname ${cellname}_$i
    }
    return $newname
}

#-------------------------------------------------------------------
# Looks to see if a cell exists on disk
#-------------------------------------------------------------------
proc magic::searchcellondisk {name} {
    set rlist {}
    foreach dir [path search] {
	set ftry [file join $dir ${name}.mag]
	if [file exists $ftry] {
	    return 1
	}
    }
    return 0
} 

#-------------------------------------------------------------------
# Checks to see if a cell already exists on disk or in memory
#-------------------------------------------------------------------
proc magic::iscellnameunique {cellname} {
    if {[cellname list exists $cellname] == 0 && [magic::searchcellondisk $cellname] == 0} { 
	return 1
    } else {
	return 0
    }
}

#----------------------------------------------------------------
