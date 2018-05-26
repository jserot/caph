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

package data_types is
  -- Note: VHDL strings, which are arrays of [character]s  must have a fixed length.
  -- We therefore have to define a pair of [pad]/[trim] fns to accomodate variable length strings :(
  constant string_length: natural := 64;
  type string_array is array (natural range <>) of string(1 to string_length);
  type natural_array is array (natural range <>) of natural;
  type slv32_array is array (natural range <>) of std_logic_vector(31 downto 0);
  function str_pad(s: string; c: character := '_') return string;
  function str_rtrim(s: string; c: character := '_') return string;
  --function slv_to_string(v : std_logic_vector) return string;
  function is_eos(t: std_logic_vector; size: natural) return boolean;
end package;

package body data_types is

  function str_pad(s: string; c: character := '_') return string is
    variable ret : string(1 to string_length);
  begin
    assert string_length >= s'length report "Data_types.str_pad: length of arg string exceeds maximum" severity failure;
    for i in 1 to s'high loop
      ret(i) := s(i);
    end loop;
    for i in s'high+1 to string_length loop
      ret(i) := c;
    end loop;
    return ret;
  end function str_pad;

  function str_rtrim(s: string; c: character := '_') return string is
  begin
    for i in s'reverse_range loop
      if s(i) /= c then
        return s(s'low to i);
      end if;
    end loop;
    return "";
  end function;

--  function slv_to_string(v : std_logic_vector) return string is
--    variable s : string(1 to v'length) := (others => 'x');
--    variable c : string(1 to 3);
--    variable j : integer := 1;
--  begin
--      for i in v'high downto v'low loop
--        c := std_logic'image(v(i));
--        s(j to j) := c(2 to 2);  -- c is a 3 character string (with quotes) ! 
--        j := j+1;
--      end loop;
--    return s;
--  end slv_to_string;

  function is_eos(t: std_logic_vector; size: natural) return boolean is
  begin
    return t(size-1 downto size-2) = "10";  -- WARNING : TO BE CHANGED ACCORDING to  DC TYPE REPRESENTATION
  end;

end package body;
