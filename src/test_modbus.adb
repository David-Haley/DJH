-- This program test the functionality of DJH.Modbus
-- holding registers only.

-- Author    : David Haley
-- Created   : 15/03/2021
-- Last Edit : 21/03/2021

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;
with GNAT.Sockets; use GNAT.Sockets;
with DJH.Modbus; use DJH.Modbus;

procedure Test_Modbus is

   Unit_Id : Unit_Ids := 1;
   Server_Port : Port_Type := Default_Modbus_Port;
   Result : Registers;
   All_Registers : Register_Arrays (30000 .. 30201);
   -- lower and higher than the range supported by the simulator
   Pass_Test : Boolean;

begin -- Test_Modbus
   if Argument_Count < 1 then
      raise Modbus_Error with
        "Usage: test_modbus Server_Name Unit_Id Server_Port";
   end if; -- Argument_Count < 1
   if Argument_Count > 1 then
      Unit_Id := Unit_Ids'Value (Argument (1));
      if Argument_Count > 2 then
         Server_Port := Port_Type'Value (Argument (2));
      end if; -- Argument_Count > 2
   end if; -- Argument_Count > 1
   Connect (Argument (1), Server_Port);
   Put ("Reading one register at a time, ");
   Pass_Test := True;
   for R in Register_Addresses range 30001 .. 30186 loop
      Result := Read_Register (Unit_Id, R);
      Pass_Test := Pass_Test and R = Result;
   end loop; -- R in Register_Addresses range 30003 .. 30105
   if Pass_Test then
      Put_Line ("test passed");
   else
      Put_Line ("test failed");
   end if; -- Pass_Test
   Put_Line ("Reading registers 30001 .. 30125");
   Read_Registers (Unit_Id, All_Registers (30001 .. 30125));
   -- Maximum allowed in Modbus RTU
   Put_Line ("Reading registers 30126 .. 30200");
   Read_Registers (Unit_Id, All_Registers (30126 .. 30200));
   -- Assumed less than 125 remaining
   Pass_Test := True;
   for R in Register_Addresses range All_Registers'First .. 30186 loop
      Pass_Test := Pass_Test and R = All_Registers (R);
   end loop; -- R in Register_Addresses range All_Registers'First ...
   Put ("Comparison, ");
   if Pass_Test then
      Put_Line ("test passed");
   else
      Put_Line ("test failed");
   end if; -- Pass_Test
   Put_Line ("Conversions Tests");
   Put_Line ("U32:" & To_U32 (All_Registers (30187 .. 30188))'Img);
   Put_Line ("U64:" & To_U64 (All_Registers (30189 .. 30192))'Img);
   Put_Line ("String_1: """ & To_String (All_Registers (30193 .. 30197)) &
               """");
   Put_Line ("String_2: """ & To_String (All_Registers (30198 .. 30200)) &
               """");
   Put_Line ("Exception Tests");
   begin -- Read register 30000
      Put_Line ("Read register 30000");
      Result := Read_Register (1, 30000);
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
   end; -- Read register 30000
   begin -- Read register 30201
      Put_Line ("Read register 30201");
      Result := Read_Register (1, 30201);
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
   end; -- Read register 30201
   begin -- Read registers 30001 .. 30126
      Put_Line ("Read registers 30001 .. 30126");
      Read_Registers (Unit_Id, All_Registers (30001 .. 30126));
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
   end; -- Read registers 30001 .. 30126
   begin -- Read registers 30000 .. 30124
      Put_Line ("Read registers 30000 .. 30124");
      Read_Registers (Unit_Id, All_Registers (30000 .. 30124));
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
   end; -- Read registers 30000 .. 30124
   begin -- Read registers 30077 .. 30201
      Put_Line ("Read registers 30077 .. 30201");
      Read_Registers (Unit_Id, All_Registers (30077 .. 30201));
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
   end; -- Read registers 30077 .. 30201
   Put_Line ("Close_Connection");
   Close_Connection;
end Test_Modbus;
