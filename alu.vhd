LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

ENTITY ALU IS PORT (
    control : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
    A : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    result : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    zero : OUT STD_LOGIC);
END ALU;

ARCHITECTURE ALU32 OF ALU IS
BEGIN
    PROCESS (control)
    BEGIN
        CASE control IS
            WHEN "011" => -- and
                result <= A AND B;
            WHEN "001" => -- or
                result <= A OR B;
            WHEN "010" => -- sum
                result <= A + B;
                --std_logic_vector(signed(A) + signed(B));
            WHEN "110" => -- sub
                result <= A - B;
                --std_logic_vector(signed(A) - signed(B));
            WHEN "100" => -- sll 16
                result <= B(15 DOWNTO 0) & x"0000";
            WHEN "111" => -- a<b
                IF (A < B) THEN
                    result <= x"00000001";
                ELSE
                    result <= x"00000000";
                END IF;
            WHEN OTHERS => -- etc
                result <= x"00000000";
        END CASE;
    END PROCESS;

    PROCESS (result)
    BEGIN
        IF (result = "00000000000000000000000000000000") THEN
            zero <= '1';
        ELSE
            zero <= '0';
        END IF;
    END PROCESS;
END;