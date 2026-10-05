# Place and route tt_um_relwire on a 6x4 Tiny Tapeout footprint (about
# 1200 x 600 um), IHP sg13g2, slow corner (1.08 V, 125 C), 50 MHz.
# Floorplan -> macro -> place -> resize -> CTS -> timing repair -> global
# route -> reports. No power grid: these are area/timing numbers, not a
# tapeout-ready layout.
set plat $::env(PLAT)
set out out
read_lef $plat/lef/sg13g2_tech.lef
read_lef $plat/lef/sg13g2_stdcell.lef
read_lef $plat/lef/RM_IHPSG13_1P_256x48_c2_bm_bist.lef
read_liberty $plat/lib/sg13g2_stdcell_slow_1p08V_125C.lib
read_liberty $plat/lib/RM_IHPSG13_1P_256x48_c2_bm_bist_slow_1p08V_125C.lib
read_verilog $out/tt_um_relwire.v
link_design tt_um_relwire
create_clock -name clk -period 20.0 [get_ports clk]
set_input_delay 4.0 -clock clk [delete_from_list [all_inputs] [get_ports clk]]
set_output_delay 4.0 -clock clk [all_outputs]
set_wire_rc -signal -layer Metal3
set_wire_rc -clock -layer Metal4

initialize_floorplan -die_area "0 0 1200 600" -core_area "6 6 1194 594" -site CoreSite
source $plat/make_tracks.tcl
place_pins -hor_layers Metal3 -ver_layers Metal2
set m [[ord::get_db_block] findInst imem]
$m setOrigin [expr int(20 / 0.001)] [expr int(460 / 0.001)]
$m setPlacementStatus FIRM
global_placement -density 0.55 -skip_io
estimate_parasitics -placement
repair_design
detailed_placement
clock_tree_synthesis -buf_list {sg13g2_buf_8 sg13g2_buf_4} -root_buf sg13g2_buf_16
set_propagated_clock [all_clocks]
estimate_parasitics -placement
repair_timing -setup
repair_timing -hold -hold_margin 0.1
detailed_placement
check_placement
set_routing_layers -signal Metal2-Metal5 -clock Metal2-Metal5
global_route -congestion_report_file $out/congestion.rpt
estimate_parasitics -global_routing
report_checks -path_delay max -digits 3 > $out/timing_setup.txt
report_checks -path_delay min -digits 3 > $out/timing_hold.txt
report_wns > $out/summary.txt
report_tns >> $out/summary.txt
report_worst_slack -max >> $out/summary.txt
report_worst_slack -min >> $out/summary.txt
report_design_area >> $out/summary.txt
report_power >> $out/summary.txt
write_def $out/tt_um_relwire.def
puts "PNR_DONE"
exit
