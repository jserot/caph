-----------------------------------------------------------------------------------------
--                                                                                     --
--                This file is part of the CAPH Compiler distribution                  --
--                            http://caph.univ-bpclermont.fr                           --
--                                                                                     --
--                                  Jocelyn SEROT                                      --
--                         Jocelyn.Serot@univ-bpclermont.fr                            --
--                                                                                     --
--         Copyright 2011-2018 Jocelyn SEROT.  All rights reserved.                    --
--  This file is distributed under the terms of the GNU Library General Public License --
--      with the special exception on linking described in file ../LICENSE.            --
--                                                                                     --
-----------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;	   
use std.textio.all;

entity port_in is
  generic ( filename: string := ""; size: integer := 10; ival: bit_vector; max_events: integer := 16);
  port (  full : in std_logic; 
          dout : out std_logic_vector(size-1 downto 0);
          wr : out std_logic;   -- write (push) signal, active 1 on clk^
          clk : in std_logic;
          rst : in std_logic
          );
end port_in;

architecture beh of port_in is
begin
  process

  -- Note 2014-11-22, JS
  -- Types and fns for reading event files.
 --  This should really be in a separate package,
  -- but this requires generic packages, which are not always supported  :(

  type port_event is record
      date: natural;
      value : bit_vector(size-1 downto 0);
  end record;

  type event_array is array (0 to max_events-1) of port_event;

  type port_events is record
      length: natural;
      data: event_array;
  end record;

  file f: text;
  variable l,log: line;
  variable i: integer;
  variable r: port_events;

  variable current_value: bit_vector(size-1 downto 0);
  variable current_time: integer; -- in ns
  variable next_index: integer;
  variable next_change: integer;

  begin
    wr <= '0';

    i := 0;   -- Reading the event file
    if ( filename /= "" ) then 
      file_open(f,filename,READ_MODE);
      while not endfile(f) loop
        readline (f,l);
        read(l,r.data(i).date);
        readline (f,l);
        read(l,r.data(i).value);
        i := i+1;
      end loop;
      file_close(f);
      r.length := i;
      report "port_in.vhd: read " & integer'image(r.length) & " event(s) from file " & filename;
      for j in 0 to r.length-1 loop
        write(log,r.data(j).value);
        write(log,string'(" @ "));
        write(log,r.data(j).date);
        writeline(output,log);
      end loop;
      next_change := r.data(0).date;
    else
      r.length := 0;
      next_change := -1;
    end if;
    
    current_value := ival;
    next_index := 0;
    while true loop
      wait until rising_edge(clk);
      current_time := time'pos(now) / 1000000;  -- now() returns time in fs
      if ( next_index < r.length and current_time >= next_change ) then
        report "port_in.vhd: changing value at t= " & integer'image(current_time);
        current_value := r.data(next_index).value;
        next_change := r.data(next_index).date;
        next_index := next_index+1;
      end if;
      assert (full='0')
          report "port_in: cannot write: downstream fifo is full !"
          severity warning;
      dout <= to_stdlogicvector(current_value); 
      wr <= '1';
      wait until rising_edge(clk);
      wr <= '0';
    end loop;
    wait;
end process;
end;
