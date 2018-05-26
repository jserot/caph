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

entity cstream_out is
  generic (
    size: integer;
    period: integer := 1
    );
  port (  empty : in std_logic;
          din : in std_logic_vector(size-1 downto 0);
          rd : out std_logic; 
          clk : in std_logic;
          rst : in std_logic;
          dout : out std_logic_vector(size-1 downto 0);
          cnt : out natural   -- Numbers of tokens read so far
          );
end cstream_out;

architecture beh of cstream_out is
begin
  process
    variable ctr: natural;
  begin
    ctr := 0;
    while true loop
      wait until rising_edge(clk);
      if ( empty = '0' ) then 
        dout <= din;
        ctr := ctr+1;
      end if;
      cnt <= ctr;
    end loop;      
    wait;
end process;
rd <= not(empty);
end;
