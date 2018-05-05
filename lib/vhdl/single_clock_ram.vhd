-----------------------------------------------------------------------------------------
--                                                                                     --
--                This file is part of the CAPH Compiler distribution                  --
--                            http://caph.univ-bpclermont.fr                           --
--                                                                                     --
--                           Jocelyn SEROT, Francois BERRY                      --
--                   {Jocelyn.Serot,Francois.Berry}@univ-bpclermont.fr           --
--                                                                                     --
--         Copyright 2011-2018 Jocelyn SEROT.  All rights reserved.                    --
--  This file is distributed under the terms of the GNU Library General Public License --
--      with the special exception on linking described in file ../LICENSE.            --
--                                                                                     --
-----------------------------------------------------------------------------------------

library ieee,caph;
use ieee.std_logic_1164.all;

ENTITY single_clock_ram IS
  GENERIC (
    size: integer := 64;
    width: integer := 8
    );
  PORT (
	clock: IN STD_LOGIC;
	data: IN std_logic_vector (width-1 DOWNTO 0);
	write_address: IN INTEGER RANGE 0 to size-1 ;
	read_address: IN INTEGER RANGE 0 to size-1 ;
	we: IN STD_LOGIC;
	q: OUT std_logic_vector (width-1 DOWNTO 0)
	);
END single_clock_ram;

ARCHITECTURE rtl OF single_clock_ram IS

  TYPE MEM IS ARRAY(0 TO size-1) OF std_logic_vector (width-1 DOWNTO 0);

  signal ram_block: MEM;

  BEGIN
		PROCESS (clock)
		BEGIN
		  IF (clock'event AND clock = '1') THEN
            assert(read_address < size) report integer'image(read_address);
            IF (we = '1') THEN
              assert(write_address < size) report integer'image(write_address);
              ram_block(write_address) <= data;
            END IF;
            q <= ram_block(read_address);
            -- Will return the OLD data at the address, when reading during a write to the same address
          END IF;
		END PROCESS;
END rtl;
