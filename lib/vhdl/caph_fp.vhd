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

library ieee, ieee_proposed;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use ieee_proposed.fixed_float_types.all;
use ieee_proposed.fixed_pkg.all;
use ieee_proposed.float_pkg.all;

package core_fp is

  function cond(e1: boolean; e2: float32; e3: float32) return float32;
  function eq(e1: float32; e2: float32) return unsigned;
  function mul(e1: float32; e2: float32) return float32;
  function to_std_logic_vector(e: float32; s: natural) return std_logic_vector;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return float32;
  function to_bool(e: float32) return boolean;
  function conv_unsigned(e: float32; s: natural) return unsigned;
  function conv_signed(e: float32; s: natural) return signed;

end core_fp;

library ieee, ieee_proposed;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package body core_fp is

  function cond(e1: boolean; e2: float32; e3: float32) return float32 is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function eq(e1: float32; e2: float32) return unsigned is
  begin
     if ( e1 = e2 ) then
       return "1";
     else
       return "0";
     end if;
  end;

  function mul (e1: float32; e2: float32) return float32 is
  begin
    return e1 * e2;  -- the provided operator should do the job..
  end;

  function to_std_logic_vector(e: float32; s:natural) return std_logic_vector is
  begin
    return to_slv(e);
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return float32 is
  begin
    return to_float(e(s-1 downto 0));
  end;

  function to_bool(e: float32) return boolean is
  begin
    if e = to_float(0) then return false; else return true; end if;
  end;

  function conv_unsigned(e: float32; s: natural) return unsigned is
  begin
    if ( e /= 0 ) then return to_unsigned(e,s); else return to_unsigned(0,s); end if;
  end;
  
  function conv_signed(e: float32; s: natural) return signed is
  begin
    if ( e /= 0 ) then return to_signed(e,s); else return to_signed(0,s); end if;
  end;
  
end package body core_fp;
  
