library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.STD_LOGIC_SIGNED.ALL;
entity Unidad_Control is port(
	ID_Instruct   : in std_logic_vector(5 downto 0); --viene de I_DataOut son sus 6bits mas significativos
	ID_RegWrite : out std_logic;
	ID_MemToReg : out std_logic;
	ID_BranchEquals : out std_logic;
	ID_MemRead  : out std_logic;
	ID_MemWrite : out std_logic;
	ID_RegDst   : out std_logic;
	ID_AluOp    : out std_logic_vector(2 downto 0);
	ID_AluSrc   : out std_logic
);
end Unidad_Control;

architecture control_arq of Unidad_Control is
begin
	Control: process(ID_Instruct)
	begin
		case (Id_Instruct) is
			when ("000000") => -- R
				ID_AluOp <= "001";
				ID_AluSrc <= '0';
				ID_RegDst <= '1';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			when ("100011") => -- LW
				ID_AluOp <= "010";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '1';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '1';
				ID_RegWrite <= '1';
			when ("101011") => -- SW
				ID_AluOp <= "010";
				ID_AluSrc <= '1';
				ID_RegDst <= '0'; -- en teoria es X
				ID_MemWrite <= '1';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0'; --X
				ID_RegWrite <= '0';
			when ("001111") => -- LUI
				ID_AluOp <= "110";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			when ("000100") => -- BEQ
				ID_AluOp <= "011";
				ID_AluSrc <= '0';
				ID_RegDst <= '0'; --X
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '1';
				ID_MemToReg <= '0'; --X
				ID_RegWrite <= '0';
			when ("001000") => -- ADDI
				ID_AluOp <= "111";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			when ("001100") => -- ANDI
				ID_AluOp <= "101";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			when ("001101") => -- Ori
				ID_AluOp <= "100";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			when others =>
				ID_AluOp <= "000";
				ID_AluSrc <= '0';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '0';
		end case;
	end process;
end control_arq;
