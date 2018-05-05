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
use ieee.numeric_std.all;
use work.data_types.all;

entity stream_out_mult is
  generic (
    filename: string_array;
    nfiles: integer;
    size: integer := 10;
    split_output_frames: boolean := false;
    period: integer := 1
    );
  port (  empty : in std_logic;
          din : in std_logic_vector(size-1 downto 0);
          rd : out std_logic; 
          clk : in std_logic;
          rst : in std_logic
          );
end stream_out_mult;

architecture beh of stream_out_mult is
begin
  process
  file output_file: text;
  variable file_line: line;
  variable token: integer;
  variable i: integer;
  variable d, dd : std_logic_vector(size-1 downto 0);
  variable eof: boolean;
  begin
    if ( period < 1 ) then
      report "stream_out(" & filename(0) & ") : period < 1 !"
      severity error;
    end if;
    i := 0;
    while ( i < nfiles ) loop
      if ( nfiles > 1 ) then report "stream_out: opening " & str_rtrim(filename(i)); end if;
      file_open(output_file,str_rtrim(filename(i)),WRITE_MODE);
      eof := false;   
      while not eof loop
        wait until rising_edge(clk);
        dd := d;
        d := din;
        if ( empty = '0' ) then 
          write (file_line,to_bitvector(d));
          writeline (output_file,file_line);
        end if;
        if ( split_output_frames and is_eos(d,size) and is_eos(dd,size) ) then -- End of frame
          if ( nfiles > 1 ) then report "stream_out: closing " & str_rtrim(filename(i)); end if;
          file_close(output_file);
          i := i+1;
          eof := true;
          end if;
        end loop;
      end loop;
      wait;
    end process;
    rd <= not(empty);
end;
