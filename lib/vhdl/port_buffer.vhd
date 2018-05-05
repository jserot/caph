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
use ieee.std_logic_1164.ALL;

--               +--------------+
--         w_f   |              |   w_e
--      <--------|              |-------->
--          wi   |              |   wo
--      -------->| PORT_BUFFER  |-------->
--         w_wr  |              |   w_rd
--      -------->|              |<--------
--               |              |
--               +--------------+ 

entity port_buffer is
  generic ( size: integer := 10);
  port ( w_f:    out std_logic;
         wi :     in std_logic_vector (size-1 downto 0);
         w_wr :  in std_logic;
         w_e :   out std_logic; 
         wo :    out std_logic_vector(size-1 downto 0);
         w_rd :  in std_logic;
         clk:    in std_logic;
         rst: in std_logic
         );
end port_buffer;

architecture rtl of port_buffer is
   signal buf : std_logic_vector(size-1 downto 0);
begin
  process (clk) is
  begin
    if ( rst='0' ) then  
       buf <= (others => '0');
    elsif ( rising_edge(clk) ) then
      if ( w_wr='1' ) then    -- write
        buf <= wi;
      end if;
    end if;
   end process;
   w_f <= '0';
   w_e <= '0';
   wo <= buf;
end rtl;
