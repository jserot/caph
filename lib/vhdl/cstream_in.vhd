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
    tokens: natural_array;
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
    rst : in std_logic;
    err : out std_logic;
    cnt : out natural
    );
end cstream_in;

architecture beh of cstream_in is
begin
  process
  variable data: std_logic_vector(size-1 downto 0);
  variable ctr: natural;
  begin
    ctr := 0;
    wr <= '0';
    err <= '0';
    for i in tokens'range loop
      data := std_logic_vector(to_unsigned(tokens(i),size));
      wait until rising_edge(clk);
      for j in 0 to period-2 loop
        wr <= '0';
        wait until rising_edge(clk);
      end loop;
      if ( full='1' ) then err <= '1'; end if;
      if ( blanking = false or data(size-1 downto size-2) /= "00" ) then 
        wait for skew;
        dout <= data; 
        wr <= '1';
        ctr := ctr+1;
      else
        wr <= '0';
      end if;
      cnt <= ctr;
    end loop;
	wait until rising_edge(clk);
	wr <= '0';
    wait;
end process;
end;

