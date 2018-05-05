library ieee,caph,work;
use ieee.std_logic_1164.all;
use caph.core.all;
use ieee.numeric_std.all;
use work.all;
use work.main_globals.all;

entity histo_act is
   port (
    a_empty: in std_logic;
    a: in std_logic_vector(0 downto 0);
    a_rd: out std_logic;
    o_full: in std_logic;
    o: out std_logic_vector(3 downto 0);
    o_wr: out std_logic;
    clock: in std_logic;
    reset: in std_logic
    );
end histo_act;

architecture FSM of histo_act is
    type t_h is array (0 to 1) of unsigned(3 downto 0);
    signal h : t_h;
    signal n_h : t_h;
    signal en_h : std_logic_vector (0 to 1);
begin
  comb: process(a, a_empty, o_full, h)
    variable p_i : unsigned(0 downto 0);
    variable tmp : std_logic_vector (0 to 1);
  begin
    -- a.rdy, o.rdy / p_i=a, h[p_i]:=h[p_i] + 1, wr(o,h[0])
    if a_empty='0' and o_full='0' then
      p_i := from_std_logic_vector(a,1);
      a_rd <= '1';
      n_h(to_integer(p_i)) <= (h(to_integer(p_i))) + (to_unsigned(1,4));
      -- en_h <= ( to_integer(p_i) => '1', others => '0'); -- NOT ALLOWED 
      -- en_h <= decode(to_integer(p_i)); -- MAY BE TRIED
      tmp := (others => '0');
      tmp(to_integer(p_i)) := '1';
      en_h <= tmp;
      o <= std_logic_vector(h(0));
      o_wr <= '1';
    else
      a_rd <= '0';
      o_wr <= '0';
      o <= (others => 'X');
      en_h <= (others => '0');
      n_h <= h;
    end if;
  end process;
  seq: process(clock, reset)
  begin
    if (reset='0') then
      h(0) <= "0000";
      h(1) <= "0000";
    elsif rising_edge(clock) then
      for j in 0 to en_h'high loop
        if ( en_h(j)='1' ) then
          h(j) <= n_h(j);
        end if;
      end loop;
    end if;
  end process;
end FSM;
