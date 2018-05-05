library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package extfns is
  function mult(a, b: signed(7 downto 0)) return signed;
end extfns;

package body extfns is

  function mult(a, b: signed(7 downto 0)) return signed is
    variable res : signed(15 downto 0);
  begin
     res := a * b;
     return res(7 downto 0);
  end;
  
end package body extfns;
  
  
