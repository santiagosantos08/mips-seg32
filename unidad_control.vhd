LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;
ENTITY Unidad_Control IS PORT (
	ID_Instruct : IN STD_LOGIC_VECTOR(5 DOWNTO 0); --viene de I_DataOut son sus 6bits mas significativos
	ID_RegWrite : OUT STD_LOGIC;
	ID_MemToReg : OUT STD_LOGIC;
	ID_BranchEquals : OUT STD_LOGIC;
	ID_MemRead : OUT STD_LOGIC;
	ID_MemWrite : OUT STD_LOGIC;
	ID_RegDst : OUT STD_LOGIC;
	ID_AluOp : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
	ID_AluSrc : OUT STD_LOGIC
);
END Unidad_Control;

ARCHITECTURE control_arq OF Unidad_Control IS
BEGIN
	Control : PROCESS (ID_Instruct)
	BEGIN
		CASE (Id_Instruct) IS
			WHEN ("000000") => -- R
				ID_AluOp <= "001";
				ID_AluSrc <= '0';
				ID_RegDst <= '1';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			WHEN ("100011") => -- LW
				ID_AluOp <= "010";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '1';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '1';
				ID_RegWrite <= '1';
			WHEN ("101011") => -- SW
				ID_AluOp <= "010";
				ID_AluSrc <= '1';
				ID_RegDst <= '0'; -- en teoria es X
				ID_MemWrite <= '1';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0'; --X
				ID_RegWrite <= '0';
			WHEN ("001111") => -- LUI
				ID_AluOp <= "110";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			WHEN ("000100") => -- BEQ
				ID_AluOp <= "011";
				ID_AluSrc <= '0';
				ID_RegDst <= '0'; --X
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '1';
				ID_MemToReg <= '0'; --X
				ID_RegWrite <= '0';
			WHEN ("001000") => -- ADDI
				ID_AluOp <= "111";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			WHEN ("001100") => -- ANDI
				ID_AluOp <= "101";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			WHEN ("001101") => -- Ori
				ID_AluOp <= "100";
				ID_AluSrc <= '1';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '1';
			WHEN OTHERS =>
				ID_AluOp <= "000";
				ID_AluSrc <= '0';
				ID_RegDst <= '0';
				ID_MemWrite <= '0';
				ID_MemRead <= '0';
				ID_BranchEquals <= '0';
				ID_MemToReg <= '0';
				ID_RegWrite <= '0';
		END CASE;
	END PROCESS;
END control_arq;