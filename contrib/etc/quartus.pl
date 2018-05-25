#!/usr/bin/perl -w
use strict;
use warnings;
use Term::ANSIColor;
use Cwd;

#############################################
#LE SCRIPT DOIT ETRE PLACE DANS LE DOSSIER DU CODE VHDL GENERE PAR CAPH
#############################################

# recuperer nom du top level vhdl
my %h;
#my $vhd_rep = getcwd;
my $vhd_rep = "Z:/vhdl/caph/scale";
my $project;
my $toplevel;
my $toplevelwoext; # top level without extension
my $synth_rep="$vhd_rep/quartus";
(-e $synth_rep) or mkdir ($synth_rep);


opendir(REP,$vhd_rep) or die "E/S:$!\n";
while(defined(my $fic=readdir REP)){
  my $f="${vhd_rep}/$fic";
  if($fic=~/[a-z].vhd$/){ # on recuperer la liste des fichiers vhd
    $h{$f}=$fic;
  }
}
#printf "%s \n",$_ for keys %h;
closedir(REP);

#on recherche le top level
foreach my $k (keys(%h)) {
	if ($h{$k}=~m/_net.vhd/)
	{
		$toplevel = $h{$k};
		$toplevelwoext = $toplevel;
		$toplevelwoext =~ s/.vhd//;
		my @tmp = split("_",$toplevel);
		$project = $tmp[0];
		print("---------------------------------\n");
		print "TOPLEVEL VHDL:$toplevel\n";
		print color("blue"), "TOPLEVEL:$toplevelwoext\n", color("reset");
		print "PROJECT:$project\n";
		print("---------------------------------\n");

	}
}

#############################################
# READING INPUT ARGUMENTS A REVOIR 
#################################################
my $sdcfile="$project.sdc";
my $clockname="clock";
my $argnum=0;
my $numArgs = $#ARGV + 1;
my $waveform = 0.5;

my $SDC_PERIOD=20;
my $SDC_INPUT_MIN_DELAY=0.2;
my $SDC_OUTPUT_MIN_DELAY=0.2;
my $SDC_INPUT_MAX_DELAY=0.5;
my $SDC_OUTPUT_MAX_DELAY=0.5;

my $FPGA_fam="CYCLONE III";
#my $FPGA_fam="CYCLONE V";
my $FPGA_dev="EP3C120F780C7";
#my $FPGA_dev="5CSXFC6D6F31C6";
my $quartus_ver="13.1";
my $eda_simu="ModelSim-Altera (VHDL)";
my $caphvhdllib = "Z:/vhdl/caph/lib";

# si ./sdc.pl -h => Display Help
if ( $numArgs==1 and  $ARGV[0] eq "-h" ){ 
	print ("Usage: ./synth.pl  [OPTIONS] ...\n\n");
	print ("   \nSDC OPTIONS:\n\n");
	print ("   -period t (ns)\n\t clock period :default = 20 ns\n");
	print ("   -input_min_delay t(ns) \n\t Hold constraints for inputs\n"); 
	print ("   -input_max_delay t(ns) \n\t Setup time for inputs \n");
	print ("   -output_min_delay t(ns) \n\t Hold time for outputs pins\n"); 
	print ("   -output_max_delay t(ns) \n\t Setup tiime for outputs pins\n");
	print ("   -clock_name \n\t Specify the clock's name in VHDL entity if different from clock\n");

	print ("   \nFPGA OPTIONS:\n\n");
	print ("   -family=xx specify FPGA family\n");
	print ("   -device=xx specify FPGA device\n");
	print ("   -quartus_ver=12.0 specify quartus version\n"); 
	print ("   -caphvhdllib=path_to_caph_vhdl_library \n"); 

	die "\n";
}

foreach $argnum (1 .. $#ARGV) {

  if ($ARGV[$argnum] eq "-period"){$SDC_PERIOD = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-input_min_delay") {$SDC_INPUT_MIN_DELAY = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-input_max_delay") {$SDC_INPUT_MAX_DELAY = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-output_min_delay"){$SDC_OUTPUT_MIN_DELAY = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-output_max_delay"){$SDC_OUTPUT_MAX_DELAY = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-clock_name"){$clockname = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-family"){$FPGA_fam = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-device"){$FPGA_dev = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-quartus_ver"){$FPGA_dev = $ARGV[$argnum+1]; }
  if ($ARGV[$argnum] eq "-caphvhdllib"){$caphvhdllib = $ARGV[$argnum+1]; }

}


#############################################
# GENERE LE MAKEFILE POUR SYNTHESE QUARTUS
#############################################
open(FIC,"+> $synth_rep/Makefile") or die"open: $!";

print(FIC "PROJECT=$project.qpf\n");
print(FIC "SDC_FILE=$project.sdc\n");
print(FIC "TOP_LEVEL_VHD=$toplevel\n");
print(FIC "Modelsim_path=/home/cedric/altera/13.1/modelsim_ase/linuxaloem\n");
#print(FIC "\n##SDC FILE PARAMETER##\n");
#print(FIC "SDC_PERIOD= $SDC_PERIOD\n");
#print(FIC "SDC_INPUT_MIN_DELAY=$SDC_INPUT_MIN_DELAY\n");
#print(FIC "SDC_OUTPUT_MIN_DELAY=$SDC_OUTPUT_MIN_DELAY\n");
#print(FIC "SDC_INPUT_MAX_DELAY=$SDC_INPUT_MAX_DELAY\n");
#print(FIC "SDC_OUTPUT_MAX_DELAY=$SDC_OUTPUT_MAX_DELAY\n\n");
print(FIC "all: project map fit sta\n\n");
print(FIC "project:\n");
print(FIC "	quartus_sh -t $project.tcl\n");
#print(FIC "sdc:\n");
#print(FIC "	./sdc.pl -i \$(TOP_LEVEL_VHD) -o \$(SDC_FILE) -period \$(SDC_PERIOD) -input_min_delay \$(SDC_INPUT_MIN_DELAY) -output_min_delay \$(SDC_OUTPUT_MIN_DELAY) -input_max_delay \$(SDC_INPUT_MAX_DELAY) -output_max_delay \$SDC_OUTPUT_MAX_DELAY\n");
print(FIC "map:\n");
print(FIC "	quartus_map \$(PROJECT)\n");
print(FIC "fit:\n");
print(FIC "	quartus_fit \$(PROJECT)\n");
print(FIC "sta:\n");
print(FIC "	quartus_sta -sdc \$(SDC_FILE) \$(PROJECT)\n");
print(FIC "rpt_c3:\n");
print(FIC "	quartus_sh -t report_c3.tcl\n");
print(FIC "rpt_c5:\n");
print(FIC "	quartus_sh -t report_c5.tcl\n");
print(FIC "rtl_sim:\n");
print(FIC "	quartus_eda $project --simulation --tool=modelsim_oem --format=vhdl\n");
print(FIC "	cp rtl_simu.do simulation/modelsim\n");
print(FIC "	cd simulation/modelsim && \$(Modelsim_path)/vsim -do rtl_simu.do\n");
print(FIC "gate_sim:\n");
print(FIC "	quartus_eda $project --simulation --tool=modelsim_oem --format=vhdl\n");
print(FIC "	cp gate_simu.do simulation/modelsim\n");
print(FIC "	cd simulation/modelsim && \$(Modelsim_path)/vsim -do gate_simu.do\n");

print(FIC ".PHONY:clean\n");
print(FIC "clean:\n");
print(FIC "	rm -rf *.rpt *.chg smart.log *.htm *.eqn *.pin *.sof *.pof db \n");
print(FIC "	rm -rf incremental_db\n");
print(FIC "	rm $project.fit.*\n");
print(FIC "	rm $project.map.*\n");
print(FIC "	rm $project.sta.*\n");
print(FIC "	rm $project.qws\n");
print(FIC "	rm $project.jdi\n");
close( FIC );

print color("green"), "Makefile Generated in $synth_rep/makefile\n", color("reset");

#################################################
#### CREATION DU FICHIER DE CONSTRAINTES SDC ####
#################################################
open(FIC,"$toplevel") or die"open: $!";
open(FIC2,"+> $synth_rep/$sdcfile") or die"open: $!";

my @t = <FIC>; #lecture tout le fichier VHD
my @input;
my @output;
my @in_bus;
my @out_bus;

my @tmp;
my $i=0;
my $incpt=0;
my $outcpt=0;
my $inbus_cpt=0;
my $outbus_cpt=0;

# We are looking for entity IN/OUT (declared before architecture keyword)
while ($t[$i] !~ m/architecture/)
{
 # print "$t[$i]\n";

  #SEARCH FOR INPUT pins
  if ($t[$i] =~ m/in/){
	 @tmp = split(/:/,$t[$i]);
	 $tmp[0]=~ s/^\s+//; #delete space at the start of string

	if ($tmp[1] =~ m/vector/) { #if input is a bus
		$in_bus[$inbus_cpt++]= join("",$tmp[0],"[*]"); # ADD [*] at the end of the name to specify input bus
	} #put in the the input bus array
	else {
		# remove clock from input list timing 
		 my $test = $tmp[0] cmp $clockname;
		 if ($tmp[0] ne $clockname ){
			 $input[$incpt++] = $tmp[0];
	 	}
	}
  
 } 
  #SEARCH FOR OUTPUT pins
  if ($t[$i] =~ m/out/){
	 @tmp = split(/:/,$t[$i]);
	 $tmp[0]=~ s/^\s+//; #delete space at the start of string

	if ($tmp[1] =~ m/vector/) { #if output is a bus
		$out_bus[$outbus_cpt++]= join('',$tmp[0],"[*]");
	} #put in the the output bus array
	else {
	 $output[$outcpt++] = $tmp[0];
	}
  }

 #SEARCH FOR INOUT PINS

$i++;
}
print("---------------------------------\n");
print( "SDC  Informations \n");
print("---------------------------------\n");
print "clockname         : $clockname\n";
print "period            : $SDC_PERIOD ns\n";
print "Setup time Input  : $SDC_INPUT_MAX_DELAY ns\n";
print "Hold time Input   : $SDC_INPUT_MIN_DELAY ns\n";
print "Setup time Output : $SDC_OUTPUT_MAX_DELAY ns\n";
print "Hold time Output  : $SDC_OUTPUT_MIN_DELAY ns\n";
print("---------------------------------\n");

print( FIC2 "#**************************************************************\n");
print( FIC2 "#    Generated SDC file hog.sdc by Perl Script\n\n" );
print( FIC2 "#    Author: Cedric Bourrasset \n\n" );
print( FIC2 "#**************************************************************\n");
print( FIC2 "# Time Information\n");
print( FIC2 "#**************************************************************\n\n");
print( FIC2 "set_time_format -unit ns -decimal_places 3\n" );
print( FIC2 "#**************************************************************\n");
print( FIC2 "# Create Clock\n");
print( FIC2 "#**************************************************************\n");

#Calculate duty cycle of clock 
my $dc = $SDC_PERIOD * $waveform;
print(FIC2 "create_clock -name {$clockname} -period $SDC_PERIOD -waveform { 0 $dc } [get_ports {$clockname}]\n");

print( FIC2 "#**************************************************************\n");
print( FIC2 "# Create Generated Clock\n");
print( FIC2 "#**************************************************************\n");
print( FIC2 "derive_clocks -period $SDC_PERIOD \n");
print( FIC2 "#**************************************************************\n");
print( FIC2 "# Set Clock Latency\n");
print( FIC2 "#**************************************************************\n");
print( FIC2 "#**************************************************************\n");
print( FIC2 "# Set Clock Uncertainty\n");
print( FIC2 "#**************************************************************\n");
print( FIC2 "derive_clock_uncertainty\n");
print( FIC2 "#**************************************************************\n");
print( FIC2 "# Set Input Delay\n");
print( FIC2 "#**************************************************************\n");

foreach my $inn (@in_bus){
print (FIC2 "set_input_delay -clock $clockname -min $SDC_INPUT_MIN_DELAY [get_ports {$inn}]\n"); # bus specification
print (FIC2 "set_input_delay -clock $clockname -max $SDC_INPUT_MAX_DELAY [get_ports {$inn}]\n");
}   
foreach my $inn (@input){
print (FIC2 "set_input_delay -clock $clockname -min $SDC_INPUT_MIN_DELAY [get_ports {$inn}]\n");
print (FIC2 "set_input_delay -clock $clockname -max $SDC_INPUT_MAX_DELAY [get_ports {$inn}]\n");
}   

print( FIC2 "#**************************************************************\n");
print( FIC2 "# Set Output Delay\n");
print( FIC2 "#**************************************************************\n");
foreach my $out (@out_bus){
print (FIC2 "set_output_delay -clock $clockname -min $SDC_OUTPUT_MIN_DELAY [get_ports {$out}]\n");
print (FIC2 "set_output_delay -clock $clockname -max $SDC_OUTPUT_MAX_DELAY [get_ports {$out}]\n");
}   

foreach my $out (@output){
print (FIC2 "set_output_delay -clock $clockname -min $SDC_OUTPUT_MIN_DELAY [get_ports {$out}]\n");
print (FIC2 "set_output_delay -clock $clockname -max $SDC_OUTPUT_MAX_DELAY [get_ports {$out}]\n");
}   
close( FIC );
close( FIC2 );

print color("green"), "SDC File $sdcfile Generated in $synth_rep/$sdcfile\n", color("reset");
print( "#########################################\n");



#################################################
#### GENERATION DU SCRIPT TCL POUR CREATION PROJET QUARTUS
#################################################

open(FIC,"+> $synth_rep/$project.tcl") or die"open: $!";
##open(FIC2,"+> $synth_rep/$sdcfile") or die"open: $!";

## Lire QSF
## Lire Synthesis options File

my ($sec, $min, $heure, $jour, $mois,$annee, undef, undef, undef) = localtime();
$mois += 1 and $annee += 1900;
$jour = sprintf("%02d",$jour);
$mois = sprintf("%02d",$mois);
my $d0 = "$heure:$min:$sec $annee-$mois-$jour";

print("---------------------------------\n");
print("Quartus Project  Informations \n");
print("---------------------------------\n");
print "FPGA Family : $FPGA_fam\n";
print "FPGA Device : $FPGA_dev\n";
print "Original Quartus Version : $quartus_ver\n";
print "CAPH VHDL LIB:$caphvhdllib\n";
print("---------------------------------\n");
print( FIC "## TCL Script for quartus project generated by Synth.pl script\n");
print( FIC "# Load Quartus II Tcl Project package\n");
print( FIC "package require ::quartus::project\n");
print( FIC "package require ::quartus::flow\n\n");
print( FIC "set need_to_close_project 0\n");
print( FIC "set make_assignments 1\n\n");
print( FIC "# Check that the right project is open\n");
print( FIC "if {[is_project_open]} {\n");
print( FIC "	if {[string compare \$quartus(project) \"$project\"]} {\n");
print( FIC "		puts \"Project $project is not open\"\n");
print( FIC "		set make_assignments 0\n");
print( FIC "	}\n");
print( FIC "} else {");
print( FIC "	# Only open if not already open\n");
print( FIC "	if {[project_exists $project]} {\n");
print( FIC "		project_open -revision $project $project\n");
print( FIC "	} else {");
print( FIC "		project_new -revision $project $project\n");
print( FIC "	}\n");
print( FIC "	set need_to_close_project 1\n");
print( FIC "}\n\n");
print( FIC "remove_all_global_assignments -name *\n");
print( FIC "remove_all_parameters -name *\n");
print( FIC "remove_all_instance_assignments -name *_REQUIREMENT\n");
print( FIC "# Make assignments\n");
print( FIC "if {\$make_assignments} {\n");
print( FIC "	set_global_assignment -name FAMILY \"$FPGA_fam\"\n");
print( FIC "	set_global_assignment -name DEVICE $FPGA_dev\n");
print( FIC "	set_global_assignment -name TOP_LEVEL_ENTITY $toplevelwoext\n");
print( FIC "	set_global_assignment -name ORIGINAL_QUARTUS_VERSION $quartus_ver\n");
print( FIC "	set_global_assignment -name PROJECT_CREATION_TIME_DATE \"$d0\"\n");
#print( FIC "	set_global_assignment -name LAST_QUARTUS_VERSION 12.0\n");

print( FIC "	set_global_assignment -name EDA_SIMULATION_TOOL \"$eda_simu\"\n");
print( FIC "	set_global_assignment -name EDA_OUTPUT_DATA_FORMAT VHDL -section_id eda_simulation\n");
print( FIC "	set_global_assignment -name EDA_TEST_BENCH_ENABLE_STATUS TEST_BENCH_MODE -section_id eda_simulation\n");
print( FIC "	set_global_assignment -name EDA_NATIVELINK_SIMULATION_TEST_BENCH $project -section_id eda_simulation\n");
print( FIC "	set_global_assignment -name EDA_TEST_BENCH_NAME $project -section_id eda_simulation\n");
print( FIC "	set_global_assignment -name EDA_DESIGN_INSTANCE_NAME NA -section_id $project\n");
print( FIC "	set_global_assignment -name EDA_TEST_BENCH_MODULE_NAME $project\_tb -section_id $project\n");
print( FIC "	set_global_assignment -name EDA_NATIVELINK_SIMULATION_SETUP_SCRIPT rtl_simu.do -section_id eda_simulation\n");
print( FIC "	set_global_assignment -name EDA_TEST_BENCH_FILE $vhd_rep/$project\_tb.vhd -section_id test\n");

print( FIC "	set_global_assignment -name COMMAND_MACRO_FILE rtl_simu.do\n");

print( FIC "	set_global_assignment -name MIN_CORE_JUNCTION_TEMP 0\n");
print( FIC "	set_global_assignment -name MAX_CORE_JUNCTION_TEMP 85\n");
print( FIC "	set_global_assignment -name PARTITION_NETLIST_TYPE SOURCE -section_id Top\n");
print( FIC "	set_global_assignment -name PARTITION_FITTER_PRESERVATION_LEVEL PLACEMENT_AND_ROUTING -section_id Top\n");
print( FIC "	set_global_assignment -name PARTITION_COLOR 16764057 -section_id Top\n");
#print( FIC "	set_global_assignment -name SEARCH_PATH \"/home/cedric/Documents/caph/v2/dist/caph-unix/lib/vhdl\"\n");
print( FIC "	set_global_assignment -name POWER_PRESET_COOLING_SOLUTION \"23 MM HEAT SINK WITH 200 LFPM AIRFLOW\"\n");
print( FIC "	set_global_assignment -name POWER_BOARD_THERMAL_MODEL \"NONE (CONSERVATIVE)\"\n");
print( FIC "	set_global_assignment -name USE_TIMEQUEST_TIMING_ANALYZER ON\n");
print( FIC "	set_global_assignment -name SDC_FILE $sdcfile\n");
#print( FIC "	set_global_assignment -name DSP_BLOCK_BALANCING \"DSP BLOCKS\"\n\n");

print("VHDL FILES Included in Quartus Project\n");
print("---------------------------------\n");

print(FIC "# VHDL files\n");
foreach my $k (keys(%h)) {
print(FIC "	set_global_assignment -name VHDL_FILE  $vhd_rep/$h{$k}\n");
print("$h{$k}\n");
}

print("---------------------------------\n");
my %hh;
# search caph lib vhdl files...
print("CAPH FILES Included in Quartus Project\n");
print("---------------------------------\n");
opendir(REP,$caphvhdllib) or die "E/S:$!\n";
while(defined(my $fic=readdir REP)){
  my $f="$fic";
# on recupere la liste des fichiers vhd ss caph_fp fifo_small_bis et fifo_small_bis_t qui genere des erreurs
  if( ($fic!~ m/^\./) && ($fic !~ m/_fp/ ) && ($fic =~ m/.vhd$/) && ($fic !~ m/_bis/) && ($fic !~ m/_t/)){
    $hh{$f}=$fic;
	printf "$hh{$f}\n" ;
  }
}
#printf "%s \n",$_ for keys %hh;
closedir(REP);

# CAPH FILE
print(FIC "# CAPH files\n");

foreach my $k (keys(%hh)) {
print(FIC "	set_global_assignment -name VHDL_FILE  $caphvhdllib/$hh{$k}\n");
#print("$hh{$k}\n");
}

print( FIC "	set_instance_assignment -name PARTITION_HIERARCHY root_partition -to | -section_id Top\n\n");
print( FIC "	# Commit assignments\n");
print( FIC "	export_assignments\n\n");
print( FIC "  	if {\$need_to_close_project} {\n");
print( FIC "	    project_close\n");
print( FIC "  }\n");
print( FIC "}\n");
close( FIC );

print color("green"), "Quartus Project $project.tcl Generated in $synth_rep/$project.tcl\n", color("reset");


#################################################################
## MODELSIM SIMULATON
#################################################################

open(FIC,"+> $synth_rep/rtl_simu.do") or die"open: $!";

print(FIC "transcript on\n");
print(FIC "if {[file exists rtl_work]} {\n");
print(FIC "	vdel -lib rtl_work -all\n");
print(FIC "}\n");
print(FIC "vlib caph\n");
print(FIC "vmap lib caph\n\n");
print(FIC "vlib rtl_work\n");
print(FIC "vmap work rtl_work\n\n");

# caph file
foreach my $k (keys(%hh)) {
print(FIC "vcom -O0 -work lib {$caphvhdllib/$hh{$k}}\n");
}

#vhdl files
foreach my $k (keys(%h)) {
	if ($h{$k}!~ m/_net/){
		print(FIC "vcom -O0 -work work {$k}\n");
	}
}
# le toplevel doit etre en dernier dans la liste
print(FIC "vcom -O0 -work work {$vhd_rep/$toplevel}\n");
print(FIC "vsim -novopt  $project\_tb\n");
print(FIC "add wave /* \n");
#print(FIC "run -all\n");
print(FIC "run 1 us\n");
print(FIC "wave zoomfull\n");

close( FIC );
print color("green"), "Modelsim RTL_LEVEL script rtl_simu.do Generated in $synth_rep/rtl_simu.do\n", color("reset");

open(FIC,"+> $synth_rep/gate_simu.do") or die"open: $!";

print(FIC "transcript on\n");
print(FIC "if {[file exists gate_work]} {\n");
print(FIC "	vdel -lib gate_work -all\n");
print(FIC "}\n");
print(FIC "vlib caph\n");
print(FIC "vmap lib caph\n\n");
print(FIC "vlib gate_work\n");
print(FIC "vmap work gate_work\n\n");

# caph file
foreach my $k (keys(%hh)) {
print(FIC "vcom -O0 -work lib {$caphvhdllib/$hh{$k}}\n");
}

#vhdl files
foreach my $k (keys(%h)) {
	if ($h{$k}!~ m/_net/){
		print(FIC "vcom -O0 -work work {$k}\n");
	}
}
# le toplevel doit etre en dernier dans la liste
print(FIC "vcom -O0 -work work {$vhd_rep/$toplevel}\n");
print(FIC "vsim -voptargs=+acc  gate_work.$project\_tb\n");
print(FIC "add wave /* \n");
print(FIC "run 1 us\n");
print(FIC "wave zoomfull\n");

close( FIC );
print color("green"), "Modelsim GATE LEVEL script gate_simu.do Generated in $synth_rep/gate_simu.do\n", color("reset");


#### TMP: PATCH TOP LEVEL FOR INCLUDING MAGIC FIFO (required by modelsim) #####
##################################################################################"
my $l;
open(FIC,"< $toplevel") or die"open: $!"; #on ouvre le toplevel
@t = <FIC>;
my $test_patch=0;
$i=0;
while( defined( $t[$i]))
{
	if($t[$i]=~ m/component Magic_FIFO/) #on a deja patche le code 
	{
		$test_patch=1; #on passe le flag à 1
		close FIC; # on ferme le fichier
		last; #on sort
	}
	$i=$i+1;
}
close FIC; # on referme le toplevel

if (!$test_patch)
{
	my $new = "$toplevel.tmp";
	print "Insert Magic_FIFO component declaration in top_level\n";
	open(FIC,"< $toplevel") or die"open: $!"; #on ouvre le toplevel
	open(FIC2,"> $new" ) or die"open: $!";    #on ouvre un fichier temporaire

	while ( ($l=<FIC>) !~ m/architecture/) # tant qu'on a trouvé le mot cle architecture
	{ 
		if ( $l =~ m/caph.core.all/) {next;}
		print FIC2 $l; # on copie
	}
	# on insere la magic fifo 
	print FIC2 "$l\n";
	print(FIC2 "component Magic_FIFO is generic(\n");
	print(FIC2 "       depth    : integer  := 50;\n");
	print(FIC2 "       size     : integer  := 10;\n");
	print(FIC2 "       DEPT_TH  : integer  := 30);\n");
	print(FIC2 "port(\n");
	print(FIC2 "         full : out std_logic;\n");
	print(FIC2 "         datain : in std_logic_vector (size-1 downto 0);\n");
	print(FIC2 "         enw : in std_logic;\n");
	print(FIC2 "         empty : out std_logic;\n");
	print(FIC2 "         dataout : out std_logic_vector(size-1 downto 0);\n");
	print(FIC2 "         enr : in std_logic;\n");
	print(FIC2 "         clk : in std_logic;\n");
	print(FIC2 "         rst: in std_logic\n");
	print(FIC2 "         );\n");
	print(FIC2 "end component;\n\n");
	# on finit de recopier le fichier
	while ( defined ($l=<FIC>))
	{
		print FIC2 $l;
	}
	close (FIC);
	close (FIC2);
	rename($new,$toplevel) # on met à jour le toplevel avec le fichier temporaire
}

##################################################
## Generate perf.tcl for reporting cyclone III
open(FIC,"+> $synth_rep/report_c3.tcl") or die"open: $!";

print(FIC "package require ::quartus::project\n");
print(FIC "load_package report \n");
print(FIC "load_package flow\n\n");
print(FIC "set need_to_close_project 0\n");
print(FIC "if {[is_project_open]} {\n");
print(FIC "if {[string compare \$quartus($project) \"$project\"]} {\n");
print(FIC "	puts \" $project main is not open\"\n");
print(FIC " set make_assignments 0 \n");

print(FIC " 	}\n");
print(FIC " } else {	# Only open if not already open\n");
print(FIC " 	if {[project_exists $project]} {\n");
print(FIC " 		project_open -revision $project $project\n");
print(FIC " 	} else {		project_new -revision $project $project\n");
print(FIC " 	}\n");
print(FIC " 	set need_to_close_project 1\n");
print(FIC " }\n");
print(FIC "load_report\n ");


#print(FIC "set num_rows_Space_report [get_number_of_rows -name \$Space_report]\n ");
#print(FIC "set num_rows_Time_report [get_number_of_rows -name \$Time_report]\n ");

########################
##  Logic Elements    ##
########################

print(FIC "set Space_report_top \"Fitter||Resource Section||Fitter Resource Usage Summary\"\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 1]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 2]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 3]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 4]\n ");
print(FIC "set Space_report \"Fitter||Fitter Summary\"\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report -row 8]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report -row 9]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report -row 10]\n ");

# On prend la 4ieme ligne
print(FIC "set Time_report \"TimeQuest Timing Analyzer||Slow 1200mV 85C Model||Slow 1200mV 85C Model Fmax Summary\"\n ");
print(FIC "set Freq_report [get_report_panel_row -name \$Time_report -row 1]\n ");
print(FIC "puts \$Freq_report\n ");



print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 27]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 28]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 29]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 30]\n ");

#set entity_report "Fitter||Resource Section||Fitter Resource Utilization by Entity"
#set num_rows_entity_report [get_number_of_rows -name $entity_report]
#puts $num_rows_entity_report
#for {set i 0} {$i< $num_rows_entity_report} {incr i} {
#    puts [get_report_panel_row -name $entity_report -row $i]
#}
	
print(FIC "if {\$need_to_close_project} {\n ");
print(FIC "project_close\n ");
print(FIC "}\n ");

##################################################
## Generate perf.tcl for reporting cyclone V
open(FIC,"+> $synth_rep/report_c5.tcl") or die"open: $!";

print(FIC "package require ::quartus::project\n");
print(FIC "load_package report \n");
print(FIC "load_package flow\n\n");
print(FIC "set need_to_close_project 0\n");
print(FIC "if {[is_project_open]} {\n");
print(FIC "if {[string compare \$quartus($project) \"$project\"]} {\n");
print(FIC "	puts \" $project main is not open\"\n");
print(FIC " set make_assignments 0 \n");

print(FIC " 	}\n");
print(FIC " } else {	# Only open if not already open\n");
print(FIC " 	if {[project_exists $project]} {\n");
print(FIC " 		project_open -revision $project $project\n");
print(FIC " 	} else {		project_new -revision $project $project\n");
print(FIC " 	}\n");
print(FIC " 	set need_to_close_project 1\n");
print(FIC " }\n");
print(FIC "load_report\n ");


#print(FIC "set num_rows_Space_report [get_number_of_rows -name \$Space_report]\n ");
#print(FIC "set num_rows_Time_report [get_number_of_rows -name \$Time_report]\n ");

########################
##  Logic Elements    ##
########################

print(FIC "set Space_report_top \"Fitter||Resource Section||Fitter Resource Usage Summary\"\n ");
print(FIC "for {set i 0} {\$i<14} {incr i} {\n");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row \$i]\n ");
print(FIC "}\n");

print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 40]\n ");


# On prend la 4ieme ligne
print(FIC "set Time_report \"TimeQuest Timing Analyzer||Slow 1100mV 85C Model||Slow 1100mV 85C Model Fmax Summary\"\n ");
print(FIC "set Freq_report [get_report_panel_row -name \$Time_report -row 1]\n ");
print(FIC "puts \$Freq_report\n ");


print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 69]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 70]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 71]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 72]\n ");
print(FIC "puts [get_report_panel_row -name \$Space_report_top -row 74]\n ");
	
print(FIC "if {\$need_to_close_project} {\n ");
print(FIC "project_close\n ");
print(FIC "}\n ");

#puts "----------------------------"
#set ALM [string trim [get_report_panel_row -name $Space_report_top -row 2] "{ALMs needed \[=A-B+C\]} {"]
#set tmp [string trim [get_report_panel_row -name $Space_report_top -row 3] "{    \[A\] ALMs used in final placement \[=a+b+c+d\]} {"]
#set ALMA [split " "]

#puts $ALM
#puts $tmp
#puts $ALMA
  
close( FIC );
print color("blue"), "Perf.tcl for reporting result after compilation\n", color("reset");

