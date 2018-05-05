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

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY sync_ram IS
 generic ( 	depth: integer := 10; 
			size: integer := 10
		);
	PORT (
	clock: IN STD_LOGIC;
	data: IN STD_LOGIC_VECTOR (size-1 DOWNTO 0);
	write_address: IN INTEGER RANGE 0 to depth-1;
	read_address: IN INTEGER RANGE 0 to depth-1;
	we: IN STD_LOGIC;
	q: OUT STD_LOGIC_VECTOR (size-1 DOWNTO 0)
	);
END sync_ram;

ARCHITECTURE rtl OF sync_ram IS
	TYPE MEM IS ARRAY(0 TO depth) OF STD_LOGIC_VECTOR(size-1 DOWNTO 0);
	signal ram_block: MEM;
	BEGIN
		PROCESS (clock)
		BEGIN
			IF (clock'event AND clock = '1') THEN
				IF (we = '1') THEN
				ram_block(write_address) <= data;
				END IF;
			END IF;
		END PROCESS;
		q <= ram_block(read_address);  -- This assignation deliberately moved here
END rtl;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo_big is
generic
   (
       depth     : integer  := 10;  -- FIFO depth (number of cells)
       size      : integer  := 8  -- FIFO width (size in bits of each cell)
  );

 port (
         full : out std_logic;
         datain : in std_logic_vector (size-1 downto 0);
         enw : in std_logic;
         empty : out std_logic;
         dataout : out std_logic_vector(size-1 downto 0);
         enr : in std_logic;
         clk : in std_logic;
         rst: in std_logic
         );
end fifo_big;


architecture archi of fifo_big is

 signal we_a,enr_c,enw_c:std_logic;
 signal readaddr : natural range 0 to depth-1;
 signal writeaddr : natural range 0 to depth-1;

 signal cnt, cnt_c : integer  range 0 to depth-1:=0;
 signal inputD,outputD,inputR,outputR: STD_LOGIC_VECTOR (size-1 DOWNTO 0); 

 component sync_ram is
 generic ( depth: integer := 10; size: integer := 10);
	PORT (
	clock: IN STD_LOGIC;
	data: IN STD_LOGIC_VECTOR (size-1 DOWNTO 0);
	write_address: IN INTEGER RANGE 0 to depth-1;
	read_address: IN INTEGER RANGE 0 to depth-1;
	we: IN STD_LOGIC;
	q: OUT STD_LOGIC_VECTOR (size-1 DOWNTO 0)
	);
 end component;

 begin

-----------------------------
-- Mapping of SRAM Block
-----------------------------
    MEM :sync_ram generic map (depth,size) port map (clk,inputR,writeaddr, readaddr, enw, outputR );
  
--------------------------------------
-- Synchronization of control signals
--------------------------------------

	process(clk)
	begin
      if ( clk'event and clk='1' ) then
        enw_c<=enw;
        enr_c<=enr;
        cnt_c<=cnt;
      end if;
	end process ;

----------------------------------------------------------------------
-- Bypass when the FIFO is empty and we write and read simulaneoulsy
----------------------------------------------------------------------
	MUX: process(datain, outputD,outputR,enw_c,enr_c,cnt_c)
      
	begin
	
	if (cnt_c=0 and enr_c='1' and enw_c='1') then
		inputD<= datain;
		inputR<= (others => 'X');
		dataout<= outputD;
	else
		inputR<= datain;
		inputD<= (others => 'X');
		dataout<= outputR;
	end if;

	end process MUX;

-----------------------------
-- empty/full flag generation
-----------------------------

    flags: process(cnt)
	begin
      if (  cnt  = 0 ) then empty <= '1'; else empty <='0'; end if;
      if ( cnt = depth-1 ) then full<= '1'; else full <='0';end if;
	end process flags;

----------------------------------------
-- read and write pointers computations
----------------------------------------

    process(clk,rst)
    begin
      if ( rst='0' ) then         
        readaddr <= 0;
        writeaddr <= 0;
        cnt <= 0;
      elsif ( clk'event and clk='1' ) then 
	 
        outputD<= inputD;
      
        if ( enr = '1'  ) then  -- Read
          if ( readaddr = depth-1 ) then
            readaddr <=  0; -- circular buffer
          else
            readaddr <= readaddr + 1; 
          end if; 
        end if;

        if ( enw = '1' and cnt < depth ) then  -- Write
          if ( writeaddr = depth-1 ) then
            writeaddr <=  0; -- circular buffer
          else
            writeaddr <= writeaddr + 1;   
          end if;
        end if;

        if ( enw = '1' and enr = '0' and cnt < depth ) then
          cnt <= cnt + 1;
        elsif ( enw = '0' and enr = '1' and cnt > 0) then
          cnt <= cnt - 1;
        end if;
		
      end if;
    end process;

end archi;
