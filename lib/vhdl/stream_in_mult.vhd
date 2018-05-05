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
use work.data_types.all;

entity stream_in_mult is
  generic (
    filename: string_array;
    nfiles: integer;
    size: integer := 10;
    period: integer := 1;
    blanking: boolean := false;
    skew: time := 0 ns
    );
  port (
    full : in std_logic; 
    dout : out std_logic_vector(size-1 downto 0);
    wr : out std_logic;
    clk : in std_logic;
    rst : in std_logic
    );
end stream_in_mult;

architecture beh of stream_in_mult is
begin
  process
  file input_file: text;
  variable file_line: line;
  variable line_out: line;
  variable data: bit_vector(size-1 downto 0);
  begin
    if ( period < 1 ) then
      report "stream_in(" & filename(0) & ") : period < 1 !"
      severity error;
    end if;
    for i in 0 to nfiles-1 loop
      wr <= '0';
      if ( nfiles > 1 ) then report "stream_in: opening " & str_rtrim(filename(i)); end if;
      file_open(input_file,str_rtrim(filename(i)),READ_MODE);
      while not endfile(input_file) loop
        readline (input_file,file_line);
        read (file_line,data) ;
        wait until rising_edge(clk);
        for i in 0 to period-2 loop
          wr <= '0';
          wait until rising_edge(clk);
        end loop;
        assert (full='0')
          report "stream_in(" & str_rtrim(filename(i)) & ") : cannot write: downstream fifo is full !"
          severity warning;
        if ( blanking = false or data(size-1 downto size-2) /= "00" ) then 
          -- Note 2014-10-08, JS
          -- This conditionnal is a hack for implementing blanking :
          -- Special tokens 00xxxxx are supposed to mean "NO DATA"
          -- These tokens will be generated in the input .bin file by the [txt2bin] utility
          wait for skew;
          dout <= to_stdlogicvector(data); 
          wr <= '1';
        else
          wr <= '0';
        end if;
      end loop;
      wait until rising_edge(clk);
      wr <= '0';
      if ( nfiles > 1 ) then report "stream_in: closing " & str_rtrim(filename(i)); end if;
      file_close(input_file);
    end loop;
    wait;
end process;
end;

