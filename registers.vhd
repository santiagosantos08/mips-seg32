LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

ENTITY Registers IS PORT (
	Clk : IN STD_LOGIC;
	Reset : IN STD_LOGIC;
	Wr : IN STD_LOGIC;
	Data_wr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	Reg1_rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
	Reg2_rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
	Reg_wr : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
	Data1_rd : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	Data2_rd : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
);
END Registers;

ARCHITECTURE reg_arq OF Registers IS
	TYPE MEM IS ARRAY(0 TO 31) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL Regs : MEM;
BEGIN
	PROCESS (Clk, Reset) BEGIN
		IF (Reset = '1') THEN
			Regs <= (OTHERS => x"00000000");
		ELSIF (Falling_Edge(clk) AND wr = '1' AND NOT (reg_wr = "00000")) THEN
			Regs(to_integer(unsigned(Reg_wr))) <= Data_wr;
		END IF;
	END PROCESS;

	PROCESS (reg1_rd, reg2_rd, Regs)
	BEGIN
		Data1_rd <= Regs(to_integer(unsigned(reg1_rd)));
		Data2_rd <= Regs(to_integer(unsigned(reg2_rd)));
	END PROCESS;

END reg_arq;