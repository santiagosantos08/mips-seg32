LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY processor IS PORT (
	Clk : IN STD_LOGIC;
	Reset : IN STD_LOGIC;
	-- Instruction memory
	I_Addr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	I_RdStb : OUT STD_LOGIC;
	I_WrStb : OUT STD_LOGIC;
	I_DataOut : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	I_DataIn : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	-- Data memory
	D_Addr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	D_RdStb : OUT STD_LOGIC;
	D_WrStb : OUT STD_LOGIC;
	D_DataOut : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	D_DataIn : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
);
END processor;

ARCHITECTURE processor_arq OF processor IS
	--------------------------------------------------------------------------------
	-----------------Declaración de componentes y señales---------------------------
	COMPONENT Registers PORT (
		clk : IN STD_LOGIC;
		reset : IN STD_LOGIC;
		Wr : IN STD_LOGIC;
		Data_wr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
		Reg1_rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
		Reg2_rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
		Reg_wr : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
		Data1_rd : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		Data2_rd : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		);
	END COMPONENT;

	COMPONENT Alu PORT (
		a : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		b : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		control : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
		result : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		zero : OUT STD_LOGIC;
		);
	END COMPONENT;

	COMPONENT Unidad_Control PORT (
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
	END COMPONENT;

	SIGNAL clock : STD_LOGIC;
	SIGNAL rst : STD_LOGIC := '0';
	--------------------------------------------------------------------------------
	--Señales de la Etapa IF
	SIGNAL IF_PCplus4 : STD_LOGIC_VECTOR(31 DOWNTO 0);-- Salida del sumador
	SIGNAL IF_PCIN : STD_LOGIC_VECTOR(31 DOWNTO 0); --pc input
	SIGNAL IF_PCOut : STD_LOGIC_VECTOR(31 DOWNTO 0); --pc output
	SIGNAL IF_DataOut : STD_LOGIC_VECTOR(31 DOWNTO 0); --Entrada al instruction reg
	--------------------------------------------------------------------------------
	--Señales de segmentacion IFID
	SIGNAL IFID_Next : STD_LOGIC_VECTOR(31 DOWNTO 0); --Salida instruccion memory
	SIGNAL IFID_Inst : STD_LOGIC_VECTOR(31 DOWNTO 0); --Salida sumador que conecta con el IFID
	--------------------------------------------------------------------------------
	--Señales de la Etapa ID
	SIGNAL ID_Instruction : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL ID_SignExt : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL ID_DataReg1 : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL ID_DataReg2 : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL ID_Next : STD_LOGIC_VECTOR(31 DOWNTO 0);
	--------------------------------------------------------------------------------
	--Señales salida UC
	SIGNAL ID_RegWrite : STD_LOGIC;
	SIGNAL ID_MemToReg : STD_LOGIC;
	SIGNAL ID_BranchEquals : STD_LOGIC;
	SIGNAL ID_MemRead : STD_LOGIC;
	SIGNAL ID_MemWrite : STD_LOGIC;
	SIGNAL ID_RegDst : STD_LOGIC;
	SIGNAL ID_AluOp : STD_LOGIC_VECTOR(2 DOWNTO 0);
	SIGNAL ID_AluSrc : STD_LOGIC;
	--------------------------------------------------------------------------------
	--Señales ID/EXE
	SIGNAL IDEXE_RegWrite : STD_LOGIC;
	SIGNAL IDEXE_MemToReg : STD_LOGIC;
	SIGNAL IDEXE_BranchEquals : STD_LOGIC;
	SIGNAL IDEXE_MemRead : STD_LOGIC;
	SIGNAL IDEXE_MemWrite : STD_LOGIC;
	SIGNAL IDEXE_RegDst : STD_LOGIC;
	SIGNAL IDEXE_AluOp : STD_LOGIC_VECTOR(2 DOWNTO 0);
	SIGNAL IDEXE_AluSrc : STD_LOGIC;
	SIGNAL IDEXE_PcNext : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL IDEXE_DataReg1 : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL IDEXE_DataReg2 : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL IDEXE_SignExt : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL IDEXE_Rt : STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL IDEXE_Rd : STD_LOGIC_VECTOR(4 DOWNTO 0);
	--------------------------------------------------------------------------------
	--Señales EXE
	SIGNAL EXE_AluControl : STD_LOGIC_VECTOR(2 DOWNTO 0);
	SIGNAL EXE_SignExt : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXE_AluOp : STD_LOGIC_VECTOR(2 DOWNTO 0);
	SIGNAL EXE_RegDst : STD_LOGIC;
	SIGNAL EXE_Rt : STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL EXE_Rd : STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL EXE_BranchAddress : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXE_Result : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXE_zero : STD_LOGIC;
	SIGNAL EXE_AluMux : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXE_DataReg1 : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXE_DataReg2 : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXE_AluSrc : STD_LOGIC;
	SIGNAL EXE_PcNext : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXE_WriteDest : STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL EXE_BranchEquals : STD_LOGIC;
	SIGNAL EXE_MemWrite : STD_LOGIC;
	SIGNAL EXE_MemRead : STD_LOGIC;
	SIGNAL EXE_MemToReg : STD_LOGIC;
	SIGNAL EXE_RegWrite : STD_LOGIC;
	--------------------------------------------------------------------------------
	--Segmentacion EXE/MEM
	SIGNAL EXEMEM_BranchAddress : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXEMEM_Zero : STD_LOGIC;
	SIGNAL EXEMEM_BranchEquals : STD_LOGIC;
	SIGNAL EXEMEM_MemWrite : STD_LOGIC;
	SIGNAL EXEMEM_MemRead : STD_LOGIC;
	SIGNAL EXEMEM_MemToReg : STD_LOGIC;
	SIGNAL EXEMEM_RegWrite : STD_LOGIC;
	SIGNAL EXEMEM_Result : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EXEMEM_WriteDest : STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL EXEMEM_DataReg2 : STD_LOGIC_VECTOR(31 DOWNTO 0);

	--------------------------------------------------------------------------------
	--Señales MEM
	SIGNAL MEM_WriteDest : STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL MEM_BranchEquals : STD_LOGIC;
	SIGNAL MEM_MemWrite : STD_LOGIC;
	SIGNAL MEM_MemRead : STD_LOGIC;
	SIGNAL MEM_MemToReg : STD_LOGIC;
	SIGNAL MEM_RegWrite : STD_LOGIC;
	SIGNAL MEM_Zero : STD_LOGIC;
	SIGNAL MEM_Result : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL MEM_DataReg2 : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL MEM_PcSrc : STD_LOGIC; --selector de salto
	SIGNAL MEM_BranchAddress : STD_LOGIC_VECTOR(31 DOWNTO 0); --direccion de salto
	SIGNAL MEM_DataOut : STD_LOGIC_VECTOR(31 DOWNTO 0);

	--------------------------------------------------------------------------------
	--Señales MEM/WB
	SIGNAL MEMWB_RegWrite : STD_LOGIC;
	SIGNAL MEMWB_MemToReg : STD_LOGIC;
	SIGNAL MEMWB_Result : STD_LOGIC_VECTOR (31 DOWNTO 0);
	SIGNAL MEMWB_WriteDest : STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL MEMWB_DataOut : STD_LOGIC_VECTOR(31 DOWNTO 0);
	--------------------------------------------------------------------------------
	--Señales de WriteBack WB
	SIGNAL WB_MemToReg : STD_LOGIC;
	SIGNAL WB_DataOut : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL WB_Result : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL WB_RegWrite : STD_LOGIC;
	SIGNAL WB_MuxWbResult : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL WB_WriteDest : STD_LOGIC_VECTOR(4 DOWNTO 0);
	--------------------------------------------------------------------------------
	--------------Etapas y registros de seg-----------------------------------------
BEGIN
	--------------------------------------------------------------------------------
	--Etapa IF
	clock <= Clk;
	rst <= Reset;
	I_Addr <= IF_PCOut;
	I_RdStb <= '1';
	I_WrStb <= '0';
	I_DataOut <= x"00000000";
	IF_DataOut <= I_DataIn;

	PC_reg : PROCESS (clock, rst) -- Carga de la direccion si bien no hay un pcReg era el nombre facil
	BEGIN
		IF (Reset = '1') THEN
			IF_PCOut <= (OTHERS => '0');
		ELSIF (rising_edge(clock)) THEN
			IF_PCOut <= IF_PCIn;
		END IF;
	END PROCESS;

	IF_PCplus4 <= IF_PCOut + x"00000004";

	IF_PCIN <= MEM_BranchAddress WHEN MEM_PcSrc = '1' ELSE
		IF_PCplus4; --mux para saltos de instruccion
	--------------------------------------------------------------------------------
	--Segmentacion IFID
	SegmentacionIFID : PROCESS (Clock, Reset)
	BEGIN
		IF (Reset = '1') THEN
			IFID_Next <= x"00000000";
			IFID_Inst <= x"00000000";
		ELSIF (rising_edge(Clock)) THEN
			IFID_Next <= IF_PCplus4;
			IFID_Inst <= IF_DataOut;
		END IF;
	END PROCESS;
	--------------------------------------------------------------------------------
	--Etapa ID
	--mapeo banco de regs
	Regs : Registers PORT MAP(
		clk => Clock,
		Reset => Reset,
		Wr => WB_RegWrite,
		reg1_rd => ID_Instruction(25 DOWNTO 21),
		reg2_rd => ID_Instruction(20 DOWNTO 16),
		reg_wr => WB_WriteDest,
		data_wr => WB_MuxWbResult,
		data1_rd => ID_DataReg1,
		data2_rd => ID_DataReg2
	);

	--mapeo unidad de control
	UC : Unidad_Control PORT MAP(
		ID_Instruct => ID_Instruction(31 DOWNTO 26),
		ID_RegWrite => ID_RegWrite,
		ID_MemToReg => ID_MemToReg,
		ID_BranchEquals => ID_BranchEquals,
		ID_MemRead => ID_MemRead,
		ID_MemWrite => ID_MemWrite,
		ID_RegDst => ID_RegDst,
		ID_AluOp => ID_AluOp,
		ID_AluSrc => ID_AluSrc
	);

	ID_Next <= IFID_Next;
	ID_Instruction <= IFID_Inst;

	--if inst(15) == 1 completa con 1 else completa con 0
	ID_SignExt <= x"0000" & ID_Instruction(15 DOWNTO 0) WHEN (ID_Instruction(15) = '0') ELSE
		(x"FFFF" & ID_Instruction(15 DOWNTO 0));
	--------------------------------------------------------------------------------
	--Segmentacion ID/EXE
	SegmentacionIDEX : PROCESS (clock, rst)
	BEGIN
		IF (Reset = '1') THEN
			IDEXE_PcNext <= (OTHERS => '0');
			IDEXE_DataReg1 <= (OTHERS => '0');
			IDEXE_DataReg2 <= (OTHERS => '0');
			IDEXE_SignExt <= (OTHERS => '0');
			IDEXE_RegDst <= '0';
			IDEXE_AluSrc <= '0';
			IDEXE_AluOp <= "000";
			IDEXE_BranchEquals <= '0';
			IDEXE_MemWrite <= '0';
			IDEXE_MemRead <= '0';
			IDEXE_MemToReg <= '0';
			IDEXE_RegWrite <= '0';
		ELSIF (rising_edge(clock)) THEN
			IDEXE_PcNext <= ID_Next;
			IDEXE_DataReg1 <= ID_DataReg1;
			IDEXE_DataReg2 <= ID_DataReg2;
			IDEXE_SignExt <= ID_SignExt;
			IDEXE_RegDst <= ID_RegDst;
			IDEXE_AluSrc <= ID_AluSrc;
			IDEXE_AluOp <= ID_AluOp;
			IDEXE_Rt <= ID_Instruction(20 DOWNTO 16);
			IDEXE_Rd <= ID_Instruction(15 DOWNTO 11);
			IDEXE_BranchEquals <= ID_BranchEquals;
			IDEXE_MemWrite <= ID_MemWrite;
			IDEXE_MemRead <= ID_MemRead;
			IDEXE_MemToReg <= ID_MemToReg;
			IDEXE_RegWrite <= ID_RegWrite;
		END IF;
	END PROCESS;
	--------------------------------------------------------------------------------
	--Etapa EXE
	--mapeo alu
	AluInst : Alu PORT MAP(
		a => EXE_DataReg1,
		b => EXE_AluMux,
		control => EXE_AluControl,
		result => EXE_Result,
		zero => EXE_Zero
	);
	EXE_DataReg1 <= IDEXE_DataReg1;
	EXE_Rt <= IDEXE_Rt;
	EXE_Rd <= IDEXE_Rd;
	EXE_SignExt <= IDEXE_SignExt;
	EXE_AluOp <= IDEXE_AluOp;
	EXE_DataReg2 <= IDEXE_DataReg2;
	EXE_AluSrc <= IDEXE_AluSrc;
	EXE_PcNext <= IDEXE_PcNext;
	EXE_RegDst <= IDEXE_RegDst;
	EXE_BranchEquals <= IDEXE_BranchEquals;
	EXE_MemWrite <= IDEXE_MemWrite;
	EXE_MemRead <= IDEXE_MemRead;
	EXE_MemToReg <= IDEXE_MemToReg;
	EXE_RegWrite <= IDEXE_RegWrite;
	EXE_BranchAddress <= EXE_PcNext + (EXE_SignExt(29 DOWNTO 0) & "00");
	EXE_AluMux <= EXE_DataReg2 WHEN (EXE_AluSrc = '0') ELSE
		EXE_SignExt;
	EXE_WriteDest <= EXE_Rt WHEN (EXE_RegDst = '0') ELSE
		EXE_Rd;
	--proceso de alu control
	AluControl : PROCESS (EXE_SignExt(5 DOWNTO 0), EXE_AluOp)
	BEGIN
		CASE(EXE_AluOp) IS
			WHEN "001" => --Tipo R
			CASE (EXE_SignExt(5 DOWNTO 0)) IS
				WHEN "100000" => --SUM
					EXE_AluControl <= "010";
				WHEN "100010" => --SUB
					EXE_AluControl <= "110";
				WHEN "100100" => -- AND
					EXE_AluControl <= "011";
				WHEN "100101" => -- OR
					EXE_AluControl <= "001";
				WHEN "101010" => -- SL
					EXE_AluControl <= "100";
				WHEN OTHERS =>
					EXE_AluControl <= "000";
			END CASE;
			WHEN "011" => --BEQ 110
			EXE_AluControl <= "110";
			WHEN "010" => -- MEM 010
			EXE_AluControl <= "010";
			WHEN OTHERS =>
			EXE_AluControl <= "000";
		END CASE;
	END PROCESS;
	--------------------------------------------------------------------------------
	--Segmentacion EXE/MEM
	SegmentacionEXMEM : PROCESS (clock, rst)
	BEGIN
		IF (Reset = '1') THEN
			EXEMEM_BranchAddress <= (OTHERS => '0');
			EXEMEM_WriteDest <= (OTHERS => '0');
			EXEMEM_BranchEquals <= '0';
			EXEMEM_MemWrite <= '0';
			EXEMEM_MemRead <= '0';
			EXEMEM_MemToReg <= '0';
			EXEMEM_RegWrite <= '0';
			EXEMEM_Zero <= '0';
			EXEMEM_Result <= (OTHERS => '0');
			EXEMEM_DataReg2 <= (OTHERS => '0');
		ELSIF (rising_edge(clock)) THEN
			EXEMEM_BranchAddress <= EXE_BranchAddress;
			EXEMEM_WriteDest <= EXE_WriteDest;
			EXEMEM_BranchEquals <= EXE_BranchEquals;
			EXEMEM_MemWrite <= EXE_MemWrite;
			EXEMEM_MemRead <= EXE_MemRead;
			EXEMEM_MemToReg <= EXE_MemToReg;
			EXEMEM_RegWrite <= EXE_RegWrite;
			EXEMEM_Zero <= EXE_Zero;
			EXEMEM_Result <= EXE_Result;
			EXEMEM_DataReg2 <= EXE_DataReg2;
		END IF;
	END PROCESS;
	--------------------------------------------------------------------------------
	--Etapa MEM
	MEM_WriteDest <= EXEMEM_WriteDest;
	MEM_BranchEquals <= EXEMEM_BranchEquals;
	MEM_MemWrite <= EXEMEM_MemWrite;
	MEM_MemRead <= EXEMEM_MemRead;
	MEM_MemToReg <= EXEMEM_MemToReg;
	MEM_RegWrite <= EXEMEM_RegWrite;
	MEM_Zero <= EXEMEM_Zero;
	MEM_Result <= EXEMEM_Result;
	MEM_DataReg2 <= EXEMEM_DataReg2;
	MEM_BranchAddress <= EXEMEM_BranchAddress;
	D_Addr <= MEM_Result;
	D_RdStb <= MEM_MemRead;
	D_WrStb <= MEM_MemWrite;
	D_DataOut <= MEM_DataReg2;
	MEM_DataOut <= D_DataIn;
	MEM_PcSrc <= (MEM_BranchEquals AND MEM_Zero);
	--------------------------------------------------------------------------------
	--Segmentacion MEM/WB
	SegmentacionMEMWB : PROCESS (clock, rst)
	BEGIN
		IF (Reset = '1') THEN
			MEMWB_RegWrite <= '0';
			MEMWB_MemToReg <= '0';
			MEMWB_DataOut <= (OTHERS => '0');
			MEMWB_Result <= (OTHERS => '0');
			MEMWB_WriteDest <= (OTHERS => '0');
		ELSIF (rising_edge(clock)) THEN
			MEMWB_RegWrite <= MEM_RegWrite;
			MEMWB_MemToReg <= MEM_MemToReg;
			MEMWB_DataOut <= MEM_DataOut;
			MEMWB_Result <= MEM_Result;
			MEMWB_WriteDest <= MEM_WriteDest;
		END IF;
	END PROCESS;
	--------------------------------------------------------------------------------
	--Etapa WB
	WB_RegWrite <= MEMWB_RegWrite;
	WB_MemToReg <= MEMWB_MemToReg;
	WB_DataOut <= MEMWB_DataOut;
	WB_Result <= MEMWB_Result;
	WB_WriteDest <= MEMWB_WriteDest;
	WB_MuxWbResult <= WB_DataOut WHEN (WB_MemToReg = '0') ELSE
		WB_Result;
END processor_arq;