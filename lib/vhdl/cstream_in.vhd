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

library ieee,caph;
use ieee.std_logic_1164.all;	   
use ieee.numeric_std.all;	   
use caph.data_types.all;

entity cstream_in is
  generic (
    tokens: slv32_array;
    size: integer := 10;
    period: integer := 1;          -- Unused (to fix)
    blanking: boolean := false;    -- Unused (to fix)
    skew: time := 0 ns             -- Unused
    );
  port (
    full : in std_logic; 
    dout : out std_logic_vector(size-1 downto 0);
    wr : out std_logic;
    clk : in std_logic;
    rst : in std_logic;
    err : out std_logic;   -- Asserted when downstream FIFO is full
    cnt : out natural      -- Number of tokens produced so far
    );
end cstream_in;

architecture beh of cstream_in is
begin
  process (rst, clk)
    variable data: std_logic_vector(31 downto 0);
    variable i: natural;
  begin
    if ( rst='0' ) then
      i := 0;
      wr <= '0';
      err <= '0';
    elsif ( rising_edge(clk) ) then
      if ( i < tokens'length ) then
        data := tokens(i);
        -- TO FIX : should wait for [period-2] extra clock ticks here..
        if ( full='1' ) then err <= '1'; end if;
        -- TO FIX : should wait if blancking is true here
        dout <= data(size-1 downto 0); 
        wr <= '1';
        i := i+1;
        cnt <= i;
      else
	    wr <= '0';
      end if;
    end if;
end process;
end;

