 library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.STD_LOGIC_SIGNED.ALL;

Entity ALU is port(
	control: IN std_logic_vector(2 downto 0);
   	A:		 IN std_logic_vector(31 downto 0);
    B:		 IN std_logic_vector(31 downto 0);
    result:  OUT std_logic_vector(31 downto 0);
    zero:    OUT std_logic);
End ALU;

Architecture ALU32 of ALU is
begin
	process(control)
    begin
    	case control is
        	when "011" => -- and
            	result <= A and B;
            when "001" => -- or
            	result <= A or B;
            when "010" => -- sum
            	result <= A + B;
                --std_logic_vector(signed(A) + signed(B));
            when "110" => -- sub
            	result <=  A - B;
                --std_logic_vector(signed(A) - signed(B));
            when "100" => -- sll 16
            	result <= B(15 downto 0) & x"0000";
            when "111" => -- a<b
            	if (A < B) then
                  	result <= x"00000001";
                else
                	result <= x"00000000";
                end if;
            when others => -- etc
            	result <= x"00000000";
       	end case;
    end process;

    process(result)
    begin
    	if(result = "00000000000000000000000000000000") then
        	zero <= '1';
        else
        	zero <= '0';
        end if;
    end process;
end;
