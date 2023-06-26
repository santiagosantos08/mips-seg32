library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity processor is port(
	Clk         : in  std_logic;
	Reset       : in  std_logic;
	-- Instruction memory
	I_Addr      : out std_logic_vector(31 downto 0);
	I_RdStb     : out std_logic;
	I_WrStb     : out std_logic;
	I_DataOut   : out std_logic_vector(31 downto 0);
	I_DataIn    : in  std_logic_vector(31 downto 0);
	-- Data memory
	D_Addr      : out std_logic_vector(31 downto 0);
	D_RdStb     : out std_logic;
	D_WrStb     : out std_logic;
	D_DataOut   : out std_logic_vector(31 downto 0);
	D_DataIn    : in  std_logic_vector(31 downto 0)
);
end processor;

architecture processor_arq of processor is
--------------------------------------------------------------------------------
-----------------Declaración de componentes y señales---------------------------
	Component Registers port(
		clk: in std_logic;
		reset: in std_logic;
		Wr: in std_logic;
		Data_wr: in std_logic_vector(31 downto 0);
		Reg1_rd: in std_logic_vector(4 downto 0);
		Reg2_rd: in std_logic_vector(4 downto 0);
		Reg_wr: in std_logic_vector(4 downto 0);
		Data1_rd: out std_logic_vector(31 downto 0);
		Data2_rd: out std_logic_vector(31 downto 0);
	);
	end component;

	Component Alu Port(
		a: in std_logic_vector (31 downto 0);
		b: in std_logic_vector (31 downto 0);
		control: in std_logic_vector(2 downto  0);
		result: out std_logic_vector(31 downto 0);
		zero: out std_logic;
	);
	end component;

	Component Unidad_Control port (
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
	end component;

	signal clock: std_logic;
	signal rst: std_logic := '0';
--------------------------------------------------------------------------------
--Señales de la Etapa IF
	signal IF_PCplus4: std_logic_vector(31 downto 0);-- Salida del sumador
	signal IF_PCIN: std_logic_vector(31 downto 0);	--pc input
	signal IF_PCOut: std_logic_vector(31 downto 0);	--pc output
	signal IF_DataOut: std_logic_vector(31 downto 0); --Entrada al instruction reg
--------------------------------------------------------------------------------
--Señales de segmentacion IFID
	signal IFID_Next: std_logic_vector(31 downto 0); --Salida instruccion memory
	signal IFID_Inst: std_logic_vector(31 downto 0); --Salida sumador que conecta con el IFID
--------------------------------------------------------------------------------
--Señales de la Etapa ID
	signal ID_Instruction: std_logic_vector(31 downto 0);
	signal ID_SignExt: std_logic_vector(31 downto 0);
	signal ID_DataReg1: std_logic_vector(31 downto 0);
	signal ID_DataReg2: std_logic_vector(31 downto 0);
	signal ID_Next: std_logic_vector(31 downto 0);
--------------------------------------------------------------------------------
--Señales salida UC
	signal ID_RegWrite : std_logic;
	signal ID_MemToReg :  std_logic;
	signal ID_BranchEquals : std_logic;
	signal ID_MemRead  : std_logic;
	signal ID_MemWrite : std_logic;
	signal ID_RegDst   : std_logic;
	signal ID_AluOp    : std_logic_vector(2 downto 0);
	signal ID_AluSrc   : std_logic;
--------------------------------------------------------------------------------
--Señales ID/EXE
	signal IDEXE_RegWrite: std_logic;
	signal IDEXE_MemToReg: std_logic;
	signal IDEXE_BranchEquals: std_logic;
	signal IDEXE_MemRead: std_logic;
	signal IDEXE_MemWrite: std_logic;
	signal IDEXE_RegDst: std_logic;
	signal IDEXE_AluOp: std_logic_vector(2 downto 0);
	signal IDEXE_AluSrc: std_logic;
	signal IDEXE_PcNext: std_logic_vector(31 downto 0);
	signal IDEXE_DataReg1: std_logic_vector(31 downto 0);
	signal IDEXE_DataReg2: std_logic_vector(31 downto 0);
	signal IDEXE_SignExt: std_logic_vector(31 downto 0);
	signal IDEXE_Rt: std_logic_vector(4 downto 0);
	signal IDEXE_Rd: std_logic_vector(4 downto 0);
--------------------------------------------------------------------------------
--Señales EXE
	signal EXE_AluControl: std_logic_vector(2 downto 0);
	signal EXE_SignExt: std_logic_vector(31 downto 0);
	signal EXE_AluOp: std_logic_vector(2 downto 0);
	signal EXE_RegDst: std_logic;
	signal EXE_Rt: std_logic_vector(4 downto 0);
	signal EXE_Rd: std_logic_vector(4 downto 0);
	signal EXE_BranchAddress: std_logic_vector(31 downto 0);
	signal EXE_Result: std_logic_vector(31 downto 0);
	signal EXE_zero: STD_LOGIC;
	signal EXE_AluMux: std_logic_vector(31 downto 0);
	signal EXE_DataReg1: std_logic_vector(31 downto 0);
	signal EXE_DataReg2: std_logic_vector(31 downto 0);
	signal EXE_AluSrc: std_logic;
	signal EXE_PcNext: std_logic_vector(31 downto 0);
	signal EXE_WriteDest: std_logic_vector(4 downto 0);
	signal EXE_BranchEquals: std_logic;
	signal EXE_MemWrite: std_logic;
	signal EXE_MemRead: std_logic;
	signal EXE_MemToReg: std_logic;
	signal EXE_RegWrite: std_logic;
--------------------------------------------------------------------------------
--Segmentacion EXE/MEM
	signal EXEMEM_BranchAddress: std_logic_vector(31 downto 0);
	signal EXEMEM_Zero: std_logic;
	signal EXEMEM_BranchEquals: std_logic;
	signal EXEMEM_MemWrite: std_logic;
	signal EXEMEM_MemRead: std_logic;
	signal EXEMEM_MemToReg: std_logic;
	signal EXEMEM_RegWrite: std_logic;
	signal EXEMEM_Result: std_logic_vector(31 downto 0);
	signal EXEMEM_WriteDest: std_logic_vector(4 downto 0);
	signal EXEMEM_DataReg2: std_logic_vector(31 downto 0);
--------------------------------------------------------------------------------
--Señales MEM
	signal MEM_WriteDest: std_logic_vector(4 downto 0);
	signal MEM_BranchEquals: std_logic;
	signal MEM_MemWrite: std_logic;
	signal MEM_MemRead: std_logic;
	signal MEM_MemToReg: std_logic;
	signal MEM_RegWrite: std_logic;
	signal MEM_Zero: std_logic;
	signal MEM_Result: std_logic_vector(31 downto 0);
	signal MEM_DataReg2: std_logic_vector(31 downto 0);
	signal MEM_PcSrc: std_logic; --selector de salto
	signal MEM_BranchAddress: std_logic_vector(31 downto 0); --direccion de salto
	signal MEM_DataOut: std_logic_vector(31 downto 0);
--------------------------------------------------------------------------------
--Señales MEM/WB
	signal MEMWB_RegWrite: std_logic;
	signal MEMWB_MemToReg: std_logic;
	signal MEMWB_Result: std_logic_vector (31 downto 0);
	signal MEMWB_WriteDest: std_logic_vector(4 downto 0);
	signal MEMWB_DataOut: std_logic_vector(31 downto 0);
--------------------------------------------------------------------------------
--Señales de WriteBack WB
	signal WB_MemToReg: std_logic;
	signal WB_DataOut: std_logic_vector(31 downto 0);
	signal WB_Result: std_logic_vector(31 downto 0);
	signal WB_RegWrite: std_logic;
	signal WB_MuxWbResult: std_logic_vector(31 downto 0);
	signal WB_WriteDest: std_logic_vector(4 downto 0);
--------------------------------------------------------------------------------
--------------Etapas y registros de seg-----------------------------------------
begin
--------------------------------------------------------------------------------
--Etapa IF
	clock <= Clk;
	rst <= Reset;
	I_Addr <= IF_PCOut;
	I_RdStb <= '1';
	I_WrStb <= '0';
	I_DataOut <= x"00000000";
	IF_DataOut <= I_DataIn;

	PC_reg: process(clock,rst) -- Carga de la direccion si bien no hay un pcReg era el nombre facil
	begin
		if (Reset = '1') then
			IF_PCOut <= (others => '0');
		elsif (rising_edge(clock)) then
			IF_PCOut <= IF_PCIn;
		end if;
	end process;

	IF_PCplus4 <= IF_PCOut + x"00000004";

	IF_PCIN <= MEM_BranchAddress when MEM_PcSrc = '1' else IF_PCplus4;  --mux para saltos de instruccion
--------------------------------------------------------------------------------
--Segmentacion IFID
	SegmentacionIFID: process(Clock,Reset)
	begin
		if (Reset = '1') then
			IFID_Next <= x"00000000";
			IFID_Inst <= x"00000000";
		elsif (rising_edge(Clock)) then
			IFID_Next <= IF_PCplus4;
			IFID_Inst <= IF_DataOut;
		end if;
	end process;
--------------------------------------------------------------------------------
--Etapa ID
	--mapeo banco de regs
	Regs: Registers PORT MAP(
		clk => Clock,
		Reset => Reset,
		Wr => WB_RegWrite,
		reg1_rd => ID_Instruction(25 downto 21),
		reg2_rd => ID_Instruction(20 downto 16),
		reg_wr => WB_WriteDest,
		data_wr => WB_MuxWbResult,
		data1_rd => ID_DataReg1,
		data2_rd => ID_DataReg2
	);

	--mapeo unidad de control
	UC: Unidad_Control PORT MAP(
	    ID_Instruct => ID_Instruction(31 downto 26),
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
	ID_SignExt <= x"0000" & ID_Instruction(15 downto 0) when (ID_Instruction(15) = '0') else  (x"FFFF" & ID_Instruction(15 downto 0));
--------------------------------------------------------------------------------
--Segmentacion ID/EXE
	SegmentacionIDEX:process (clock,rst)
	begin
		if (Reset = '1') then
			IDEXE_PcNext <= (others => '0');
			IDEXE_DataReg1 <=(others => '0');
			IDEXE_DataReg2 <=(others => '0');
			IDEXE_SignExt <=(others => '0');
			IDEXE_RegDst <= '0';
			IDEXE_AluSrc <= '0';
			IDEXE_AluOp <= "000";
			IDEXE_BranchEquals <= '0';
			IDEXE_MemWrite <='0';
			IDEXE_MemRead <= '0';
			IDEXE_MemToReg <= '0';
			IDEXE_RegWrite <='0';
		elsif (rising_edge(clock)) then
			IDEXE_PcNext <= ID_Next;
			IDEXE_DataReg1 <= ID_DataReg1;
			IDEXE_DataReg2 <= ID_DataReg2;
			IDEXE_SignExt <= ID_SignExt;
			IDEXE_RegDst <= ID_RegDst;
			IDEXE_AluSrc <= ID_AluSrc;
			IDEXE_AluOp <= ID_AluOp;
			IDEXE_Rt <= ID_Instruction(20 downto 16);
			IDEXE_Rd <= ID_Instruction(15 downto 11);
			IDEXE_BranchEquals <= ID_BranchEquals;
			IDEXE_MemWrite <= ID_MemWrite;
			IDEXE_MemRead <= ID_MemRead;
			IDEXE_MemToReg <= ID_MemToReg;
			IDEXE_RegWrite <= ID_RegWrite;
		end if;
	end process;
--------------------------------------------------------------------------------
--Etapa EXE
	--mapeo alu
    	AluInst: Alu PORT MAP(
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
	EXE_BranchAddress <= EXE_PcNext + (EXE_SignExt(29 downto 0) & "00");
	EXE_AluMux <= EXE_DataReg2 when (EXE_AluSrc = '0') else EXE_SignExt;
	EXE_WriteDest <= EXE_Rt when (EXE_RegDst = '0') else EXE_Rd;
	--proceso de alu control
	AluControl: process(EXE_SignExt(5 downto 0), EXE_AluOp)
	begin
	case(EXE_AluOp) is
		when "001" => --Tipo R
			case (EXE_SignExt(5 downto 0)) is
				when "100000"=>  	--SUM
					EXE_AluControl <= "010";
				when "100010" => 	--SUB
					EXE_AluControl <= "110";
				when "100100" =>	 -- AND
					EXE_AluControl <= "011";
				when "100101" =>	 -- OR
					EXE_AluControl <= "001";
				when "101010" =>	 -- SLT
					EXE_AluControl <= "111";
				when others =>
					EXE_AluControl <= "000";
			end case;
		when "011" =>  --BEQ 110
			EXE_AluControl <= "110"; --resta para ver si da 0
		when "010" =>  --MEM 010
			EXE_AluControl <= "010"; --suma para la direcc
		when "110" =>  --LUI
			EXE_AluControl <= "100"; --hace shift para cargar esa mitad
		when "111" =>  --ADDIMMEDIATE
			EXE_AluControl <= "010";
		when "101" =>  --ANDIMMEDIATE
			EXE_AluControl <= "011";
		when "100" =>  --ORIMMEDIATE
			EXE_AluControl <= "001";
		when others =>
			EXE_AluControl <= "000";
	end case;
	end process;
--------------------------------------------------------------------------------
--Segmentacion EXE/MEM
	SegmentacionEXMEM:process (clock,rst)
	begin
		if (Reset = '1') then
			EXEMEM_BranchAddress <= (others => '0');
			EXEMEM_WriteDest <= (others => '0');
			EXEMEM_BranchEquals <= '0';
			EXEMEM_MemWrite <= '0';
			EXEMEM_MemRead <= '0';
			EXEMEM_MemToReg <= '0';
			EXEMEM_RegWrite <= '0';
			EXEMEM_Zero <= '0';
			EXEMEM_Result <= (others => '0');
			EXEMEM_DataReg2 <= (others => '0');
		elsif (rising_edge(clock)) then
			EXEMEM_BranchAddress <= EXE_BranchAddress;
			EXEMEM_WriteDest <= EXE_WriteDest;
			EXEMEM_BranchEquals <= EXE_BranchEquals;
			EXEMEM_MemWrite <= EXE_MemWrite;
			EXEMEM_MemRead <= EXE_MemRead;
			EXEMEM_MemToReg <= EXE_MemToReg;
			EXEMEM_RegWrite <= EXE_RegWrite;
			EXEMEM_Zero <= EXE_Zero;
			EXEMEM_Result <=EXE_Result;
			EXEMEM_DataReg2 <= EXE_DataReg2;
		end if;
	end process;
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
	MEM_PcSrc <= (MEM_BranchEquals and MEM_Zero);
--------------------------------------------------------------------------------
--Segmentacion MEM/WB
    SegmentacionMEMWB:process (clock,rst)
	begin
		if (Reset = '1') then
			MEMWB_RegWrite <= '0';
			MEMWB_MemToReg <= '0';
			MEMWB_DataOut <= (others => '0');
			MEMWB_Result <= (others => '0');
			MEMWB_WriteDest <= (others => '0');
		elsif (rising_edge(clock)) then
			MEMWB_RegWrite <= MEM_RegWrite;
			MEMWB_MemToReg <= MEM_MemToReg;
			MEMWB_DataOut <= MEM_DataOut;
			MEMWB_Result <= MEM_Result;
			MEMWB_WriteDest <= MEM_WriteDest;
		end if;
	end process;
--------------------------------------------------------------------------------
--Etapa WB
	WB_RegWrite <= MEMWB_RegWrite;
	WB_MemToReg <= MEMWB_MemToReg;
	WB_DataOut <= MEMWB_DataOut;
	WB_Result <= MEMWB_Result;
	WB_WriteDest <= MEMWB_WriteDest;
	WB_MuxWbResult <= WB_DataOut when (WB_MemToReg = '0') else WB_Result;
end processor_arq;
