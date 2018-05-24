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
use ieee.numeric_std.all;

package core is

  function cond(e1: boolean; e2: signed; e3: signed) return signed;
  function cond(e1: boolean; e2: unsigned; e3: unsigned) return unsigned;
  function cond(e1: boolean; e2: std_logic_vector; e3: std_logic_vector) return std_logic_vector;
  function eq(e1: signed; e2: signed) return unsigned;
  function eq(e1: unsigned; e2: unsigned) return unsigned;
  function mul(e1: signed; e2: signed) return signed;       -- signed<m> * signed<n> -> signed<max(m,n)>
  function mul(e1: unsigned; e2: unsigned) return unsigned; -- unsigned<m> * unsigned<n> -> unsigned<max(m,n)>
  -- IEEE "*" operator has type : (un)signed<m> * (un)signed<n> -> (un)signed<m+n>>
  function ext_add (e1: unsigned; e2: unsigned) return unsigned; -- unsigned<m> + unsigned<n> -> unsigned<max(m,n)+1>
  function ext_add (e1: signed; e2: signed) return signed;       -- signed<m> + signed<n> -> signed<max(m,n)+1>
  -- IEEE "+" operator has type : (un)signed<m> * (un)signed<n> -> (un)signed<max(m,n)>
  function to_std_logic_vector(e: unsigned; s: natural) return std_logic_vector;
  function to_std_logic_vector(e: signed; s: natural) return std_logic_vector;
  function to_std_logic_vector(e: boolean; s: natural) return std_logic_vector;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return unsigned;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return signed;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return boolean;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return std_logic_vector;
  function to_bool(e: unsigned) return boolean;
  function to_bool(e: signed) return boolean;
  function to_bool(e: std_logic_vector) return boolean;
  function conv_unsigned(e: unsigned; s: natural) return unsigned;
  function conv_unsigned(e: signed; s: natural) return unsigned;
  function conv_unsigned(e: boolean; s: natural) return unsigned;
  function conv_unsigned(e: integer; s: natural) return unsigned;
  function conv_signed(e: signed; s: natural) return signed;
  function conv_signed(e: unsigned; s: natural) return signed;
  function conv_signed(e: boolean; s: natural) return signed;
  function conv_signed(e: integer; s: natural) return signed;
  function to_integer(e: integer) return integer;
  function to_string(v : std_logic_vector) return string;  -- for debug only
  procedure dump_slv(name: string; v: std_logic_vector); -- for debug only

component fifo is
generic (depth: integer; width: integer);
port ( full : out std_logic;
       datain : in std_logic_vector (width-1 downto 0);
       enw : in std_logic;
       empty : out std_logic;
       dataout : out std_logic_vector(width-1 downto 0);
       enr : in std_logic;
       clk : in std_logic;
       rst: in std_logic );
end component;

use work.data_types.all;

component stream_in is
  generic (
    filename: string;
    size: integer;
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
end component;

component stream_out is
generic (
  filename: string;
  size: integer;
  period: integer := 1
  );
port (  empty : in std_logic; 
        din : in std_logic_vector(size-1 downto 0);
        rd : out std_logic;   -- read (pop) signal
        clk : in std_logic;
        rst : in std_logic
        );
end component;

component stream_in_mult is
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
end component;

component stream_out_mult is
  generic (
    filename: string_array;
    nfiles: integer;
    size: integer := 10;
    split_output_frames: boolean := false;
    period: integer := 1
    );
port (  empty : in std_logic; 
        din : in std_logic_vector(size-1 downto 0);
        rd : out std_logic;   -- read (pop) signal
        clk : in std_logic;
        rst : in std_logic
        );
end component;

component port_in is
generic ( filename: string := ""; size: integer := 10; ival: bit_vector);
port (  full : in std_logic;
        dout : out std_logic_vector(size-1 downto 0);
        wr : out std_logic;   -- write (push) signal, active 1 on clk^
        clk : in std_logic;
        rst : in std_logic
        );
end component;

component port_out is
generic ( filename: string := "result.bin"; size: integer := 10 );
port (  empty : in std_logic; 
        din : in std_logic_vector(size-1 downto 0);
        rd : out std_logic;   -- read (pop) signal, active 1 on clk^
        clk : in std_logic;
        rst : in std_logic
        );
end component;

component port_buffer is
    generic ( size: integer := 10);
    port ( w_f:    out std_logic;
           wi :     in std_logic_vector (size-1 downto 0);
           w_wr :  in std_logic;
           w_e :   out std_logic; 
           wo :    out std_logic_vector(size-1 downto 0);
           w_rd :  in std_logic;
           clk:    in std_logic;
           rst:    in std_logic );
end component;

--component decoder is
--    generic ( size: integer := 8 )
--    port (
--      sel: in integer;
--      outp: std_logic_vector ( size-1 downto 0)
--      );
--end component;

end core;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


-- use STD.textio.all;   -- FOR DEBUG ONLY

package body core is

  function cond(e1: boolean; e2: signed; e3: signed) return signed is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function cond(e1: boolean; e2: unsigned; e3: unsigned) return unsigned is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function cond(e1: boolean; e2: std_logic_vector; e3: std_logic_vector) return std_logic_vector is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function eq(e1: signed; e2: signed) return unsigned is
  begin
     if ( e1 = e2 ) then
       return "1";
     else
       return "0";
     end if;
  end;

  function eq(e1: unsigned; e2: unsigned) return unsigned is
  begin
     if ( e1 = e2 ) then
       return "1";
     else
       return "0";
     end if;
  end;

  function maximum(L, R: INTEGER) return INTEGER is
    begin
        if L > R then
            return L;
        else
            return R;
        end if;
  end;

  function mul (e1: signed; e2: signed) return signed is
        constant length: INTEGER := maximum(e1'length, e2'length);
    begin
        return resize(e1*e2, length);
    end;

  function mul (e1: unsigned; e2: unsigned) return unsigned is
        constant length: INTEGER := maximum(e1'length, e2'length);
    begin
        return resize(e1*e2, length);
    end;

  function ext_add (e1: unsigned; e2: unsigned) return unsigned is
        constant length: INTEGER := maximum(e1'length,e2'length)+1;
    begin
        return resize(e1,length) + resize(e2,length);
    end;

  function ext_add (e1: signed; e2: signed) return signed is
        constant length: INTEGER := maximum(e1'length,e2'length)+1;
    begin
        return resize(e1,length) + resize(e2,length);
    end;

  function to_std_logic_vector(e: unsigned; s:natural) return std_logic_vector is
  begin
    return STD_LOGIC_VECTOR(resize(e,s));
  end;

  function to_std_logic_vector(e: signed; s:natural) return std_logic_vector is
  begin
    return STD_LOGIC_VECTOR(resize(e,s));
  end;

  function to_std_logic_vector(e: boolean; s:natural) return std_logic_vector is
  begin
    if e then
      return STD_LOGIC_VECTOR(TO_UNSIGNED(0,s-1)) & "1";
    else
      return STD_LOGIC_VECTOR(TO_UNSIGNED(0,s-1)) & "0";
    end if;
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return unsigned is
  begin
    -- return UNSIGNED(e(s-1 downto 0));
    -- return UNSIGNED(e(e'high downto e'high-s+1));
    return UNSIGNED(e(s-1 downto 0));  -- Changed in v2.6.2: in variants, fields are RIGHT justified
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return signed is
  begin
    -- return SIGNED(e(s-1 downto 0));
    -- return SIGNED(e(e'high downto e'high-s+1));
    return SIGNED(e(s-1 downto 0));  -- Changed in v2.6.2: in variants, fields are RIGHT justified
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return boolean is
  begin
    -- if e(e'high downto e'high) = "1" then
    if e(0 downto 0) = "1" then  -- Changed in v2.6.2: in variants, fields are RIGHT justified
      return true;
    else
      return false;
    end if;
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return std_logic_vector is
  begin
    return (e(s-1 downto 0));  -- Changed in v2.6.2: in variants, fields are RIGHT justified
    -- return e(e'high downto e'high-s+1);
  end;

  function to_bool(e: unsigned) return boolean is
  begin
    if e = (e'range=>'0') then return false; else return true; end if;
  end;
                                      
  function to_bool(e: signed) return boolean is
  begin
    if e = (e'range=>'0') then return false; else return true; end if;
  end;

  function to_bool(e: std_logic_vector) return boolean is
  begin
    if e = (e'range=>'0') then return false; else return true; end if;
  end;
                                      
  function conv_unsigned(e: unsigned; s: natural) return unsigned is
  begin
    return resize(e, s);
  end;
  
  function conv_unsigned(e: signed; s: natural) return unsigned is
  begin
    return resize(unsigned(e), s);
  end;
  
  function conv_unsigned(e: boolean; s: natural) return unsigned is
  begin
    if ( e ) then return to_unsigned(1,s); else return to_unsigned(0,s); end if;
  end;
  
  function conv_unsigned(e: integer; s: natural) return unsigned is
  begin
    return to_unsigned(e,s);
  end;
  
  function conv_signed(e: signed; s: natural) return signed is
  begin
    return resize(e, s);
  end;
  
  function conv_signed(e: unsigned; s: natural) return signed is
  begin
    return resize(signed('0' & e), s);
  end;
  
  function conv_signed(e: boolean; s: natural) return signed is
  begin
    if ( e ) then return to_signed(1,s); else return to_signed(0,s); end if;
  end;
  
  function conv_signed(e: integer; s: natural) return signed is
  begin
    return to_signed(e,s);
  end;

  function to_integer(e: integer) return integer is
  begin
    return e;
  end;

  -- Debug aux fns 

  function to_string(v : std_logic_vector) return string is
    variable s : string(1 to v'length) := (others => 'x');
    variable c : string(1 to 3);
    variable j : integer := 1;
  begin
      for i in v'high downto v'low loop
        c := std_logic'image(v(i));
        s(j to j) := c(2 to 2);  -- c is a 3 character string (with quotes) ! 
        j := j+1;
      end loop;
    return s;
  end to_string;

  procedure dump_slv(name: string; v: std_logic_vector) is
  begin
    report name & "[" & integer'image(v'high) & ":" & integer'image(v'low) & "]=" & to_string(v);
  end;

end package body core;

library ieee;
use ieee.std_logic_1164.all;
  
package optimized is

component d1l_opt is  -- Optimized implementation for infering RAM blocks with
                      -- Altera Quartus II synthetizer
   generic (
    linewidth: integer;
    bitwidth: integer
    -- Warning : no padding value specified here; will always be 0..
    );
  port (
    a_empty: in std_logic;
    a: in std_logic_vector(bitwidth+1 downto 0); -- +2 bits for tag
    a_rd: out std_logic;
    c_full: in std_logic;
    c: out std_logic_vector(bitwidth+1 downto 0); -- +2 bits for tag
    c_wr: out std_logic;
    clock: in std_logic;
    reset: in std_logic
    );
end component;
  
end optimized;
