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

entity stream_out is
  generic (
    filename: string;
    size: integer;
    period: integer := 1
    );
  port (  empty : in std_logic;
          din : in std_logic_vector(size-1 downto 0);
          rd : out std_logic; 
          clk : in std_logic;
          rst : in std_logic
          );
end stream_out;

architecture beh of stream_out is
begin
  process
  file output_file: text;
  variable file_line: line;
  variable token: integer;
  variable eof: boolean;
  begin
    if ( period < 1 ) then
      report "stream_out(" & filename & ") : period < 1 !"
      severity error;
    end if;
    eof := false;
    file_open(output_file,filename,WRITE_MODE);
    while not eof loop
      wait until rising_edge(clk);
      if ( empty = '0' ) then 
        write (file_line,to_bitvector(din));
        writeline (output_file,file_line);
      end if;
    end loop;          -- TODO: set eof when run is completed ?
    file_close(output_file);
    wait;
end process;
rd <= not(empty);
end;
