library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.STD_LOGIC_SIGNED.ALL;

Entity Registers is port(
	Clk: in std_logic;
    Reset: in std_logic;
    Wr: in std_logic;
    Data_wr: in std_logic_vector(31 downto 0);
    Reg1_rd: in std_logic_vector(4 downto 0);
    Reg2_rd: in std_logic_vector(4 downto 0);
    Reg_wr: in std_logic_vector(4 downto 0);
    Data1_rd: out std_logic_vector(31 downto 0);
    Data2_rd: out std_logic_vector(31 downto 0)
);
end Registers;

Architecture reg_arq of Registers is
	Type MEM is array(0 to 31) of std_logic_vector(31 downto 0);
	signal Regs: MEM;
begin
	process(Clk,Reset) begin
		if(Reset='1') then
			Regs<=(others=>x"00000000");
		elsif (Falling_Edge(clk) and wr = '1' and not (reg_wr="00000")) then
				Regs(to_integer(unsigned(Reg_wr))) <= Data_wr;
		end if;
	end process;

	process (reg1_rd, reg2_rd, Regs)
	begin
		Data1_rd <= Regs(to_integer(unsigned(reg1_rd)));
		Data2_rd <= Regs(to_integer(unsigned(reg2_rd)));
	end process;

end reg_arq;
