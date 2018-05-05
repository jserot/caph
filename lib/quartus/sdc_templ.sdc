#**************************************************************
#    Sample SDC File for Quartus II
#    Author: Cedric Bourrasset 
#**************************************************************
# Time Information
#**************************************************************

set_time_format -unit ns -decimal_places 3
#**************************************************************
# Create Clock
#**************************************************************
create_clock -name {clock} -period 20 -waveform { 0 10 } [get_ports {clock}]
#**************************************************************
# Create Generated Clock
#**************************************************************
derive_clocks -period 20 
#**************************************************************
# Set Clock Latency
#**************************************************************
#**************************************************************
# Set Clock Uncertainty
#**************************************************************
derive_clock_uncertainty
#**************************************************************
# Set Input Delay
#**************************************************************
set_input_delay -clock clock -min 0.2 [get_ports {w669[*]}]
set_input_delay -clock clock -max 0.5 [get_ports {w669[*]}]
set_input_delay -clock clock -min 0.2 [get_ports {w669_wr}]
set_input_delay -clock clock -max 0.5 [get_ports {w669_wr}]
set_input_delay -clock clock -min 0.2 [get_ports {w808_rd}]
set_input_delay -clock clock -max 0.5 [get_ports {w808_rd}]
set_input_delay -clock clock -min 0.2 [get_ports {reset}]
set_input_delay -clock clock -max 0.5 [get_ports {reset}]
#**************************************************************
# Set Output Delay
#**************************************************************
set_output_delay -clock clock -min 0.2 [get_ports {w808[*]}]
set_output_delay -clock clock -max 0.5 [get_ports {w808[*]}]
set_output_delay -clock clock -min 0.2 [get_ports {w669_f}]
set_output_delay -clock clock -max 0.5 [get_ports {w669_f}]
set_output_delay -clock clock -min 0.2 [get_ports {w808_e}]
set_output_delay -clock clock -max 0.5 [get_ports {w808_e}]
