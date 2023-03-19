-- This package provides a minimal Modbus TCP functionality, specifically to
-- provide read access to SMA Inverters for data loggong purposes.

-- Author    : David Haley
-- Created   : 17/02/2021
-- Last Edit : 20/08/2022
-- 20220820 : Exception messages correcred.

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

package body DJH.Modbus is

   Header_Length : constant Stream_Element_Offset := 7;

   subtype Stream_Pairs is Stream_Element_Array (1 .. 2);

   subtype PAU_Request_3 is Stream_Element_Array (1 .. 5);

   subtype Function_Codes is Stream_Element range 1 .. 127;

   Read_Holding_Registers : constant Function_Codes := 3;
   Exception_Length : constant Stream_Element_Offset := Header_Length + 2;
   -- This is the length of a response when an the RTU raises an exception
   Byte_Shift : constant Natural := 8;
   Register_Shift : constant Natural := 16;
   Low_Mask : constant Registers := 2#0000000011111111#;

   Request_Socket : Socket_Type;
   Transaction_Id : Registers := 0;

   procedure Connect (Server_Name : in String;
                      Server_Port : in Port_Type := Default_Modbus_Port) is

      SS_No_Delay : constant Option_Type :=
        (Name => No_Delay, Enabled => True);

      Server_Entry : Host_Entry_Type := Get_Host_By_Name (Server_Name);
      Request_Socket_Address : Sock_Addr_Type;

   begin -- Connect
      Request_Socket_Address :=
        Network_Socket_Address (Addresses (Server_Entry),
                                Server_Port);
      Create_Socket (Request_Socket,
                     Family_Inet,
                     Socket_Stream,
                     IP_Protocol_For_TCP_Level);
      Set_Socket_Option (Request_Socket,
                         IP_Protocol_For_TCP_Level,
                         SS_No_Delay);
      Bind_Socket (Request_Socket,
                   Network_Socket_Address (No_Inet_Addr, Any_Port));
      Connect_Socket (Request_Socket, Request_Socket_Address);
   exception
      when E : others =>
         raise Modbus_Error with "Connect (Name) - " & Exception_Message (E);
   end Connect;

   procedure Connect (Server_IP : in Inet_Addr_Type;
                      Server_Port : in Port_Type := Default_Modbus_Port) is

      -- Use IPV4 only (GNAT.Sockets.Family_Inet)

      SS_No_Delay : constant Option_Type :=
        (Name => No_Delay, Enabled => True);

      Server_Entry : Host_Entry_Type :=  Get_Host_By_Address (Server_IP);
      Request_Socket_Address : Sock_Addr_Type;

   begin -- Connect
      Request_Socket_Address :=
        Network_Socket_Address (Addresses (Server_Entry),
                                Server_Port);
      Create_Socket (Request_Socket,
                     Family_Inet,
                     Socket_Stream,
                     IP_Protocol_For_TCP_Level);
      Set_Socket_Option (Request_Socket,
                         IP_Protocol_For_TCP_Level,
                         SS_No_Delay);
      Bind_Socket (Request_Socket,
                   Network_Socket_Address (No_Inet_Addr, Any_Port));
      Connect_Socket (Request_Socket, Request_Socket_Address);
   exception
      when E : others =>
         raise Modbus_Error with "Connect (address) - " & Exception_Message (E);
   end Connect;

   function Split_Register (Register : in Registers) return Stream_Pairs is

      Temp : Registers := Register;
      Result : Stream_Pairs;

   begin -- Split_Register
      Result (1) := Stream_Element (Shift_Right (Temp, Byte_Shift));
      Result (2) := Stream_Element (Register and Low_Mask);
      return Result;
   end Split_Register;

   function Assemble_Register (Stream_Pair : in Stream_Pairs)
                                return Registers is

      High : Registers := Shift_Left (Registers (Stream_Pair (1)), Byte_Shift);
      Low : Registers := Registers (Stream_Pair (2));

   begin -- Assemble_Register
      return High or Low;
   end Assemble_Register;

   function Assemble_Offset (Stream_Pair : in Stream_Pairs)
                                return Stream_Element_Offset is

   begin -- Assemble_Offset
      return Stream_Element_Offset (Assemble_Register (Stream_Pair));
   end  Assemble_Offset;


   function Encapuslate (Transaction_Id : in Registers;
                         Unit_Id : in Unit_Ids;
                         PAU : in Stream_Element_Array)
                         return Stream_Element_Array is

      Result : Stream_Element_Array (0 .. Header_Length + PAU'Last - Pau'First);

   begin -- Encapuslate
      Result (0 .. 1) := Split_Register (Transaction_Id);
      Result (2 .. 3) := Split_Register (0);
      -- Protocol identifier always zero for Modbus
      Result (4 .. 5) := Split_Register (Registers (PAU'Last - PAU'First + 2));
      -- Bytes including Unit_Id to end
      Result (6) := Stream_Element (Unit_Id);
      Result (Header_Length .. Result'Last) := PAU;
      return Result;
   end Encapuslate;

   procedure Strip_Encapulation (Transaction_Id : in Registers;
                                 Unit_Id : in Unit_Ids;
                                 Number_of_Registers : in Registers;
                                 Response : in Stream_Element_Array;
                                 Register_Start : out Stream_Element_Offset) is

      -- Response'First assumed to be 1

   begin -- Strip_Encapulation
      if Assemble_Register (Response (1 .. 2)) /= Transaction_Id then
         raise Modbus_Error with "Mismatched Transaction_Id, expected" &
           Transaction_Id'Img & " and received" &
           Assemble_Register (Response (0 .. 1))'img;
      end if; -- Assemble_Register (Response (1 .. 2)) /= Transaction_Id
      if Assemble_Register (Response (3 .. 4)) /= 0 then
         raise Modbus_Error with "Unknown protocol" &
           Assemble_Register (Response (3 .. 4))'Img;
      end if; -- Assemble_Register (Response (3 .. 4)) /= 0
      if Response'Last /=
        Header_Length - 1 + Assemble_Offset (Response (5 .. 6)) then
         raise Modbus_Error with "Incorrect Message length, expected" &
           Stream_Element_Offset'Image (Header_Length - 1 +
                                          Assemble_Offset (Response (5 .. 6))) &
           " and received" & Response'Last'Img;
      end if; -- Response'Last /= ...
      if Response (Header_Length) /= Unit_Id then
         raise Modbus_Error with "Mismatched Unit_Id, expected" & Unit_Id'Img &
           " and received" & Response (Header_Length)'img;
      end if; -- Response (Header_Length) /= Unit_Id
      if Response (Header_Length + 1) /= Read_Holding_Registers then
         raise Modbus_Error with "Mismatched function, expected " &
           Read_Holding_Registers'Img & "and received" &
           Response (Header_Length + 1)'Img;
      end if; -- Response (Header_Length + 1) /= Read_Holding_Registers
      if Registers (Response (Header_Length + 2)) /=
        Number_of_Registers * 2 then
         raise Modbus_Error with "Mismatched number of registers, expected" &
           Number_of_Registers'Img & " and received" &
           Response (Header_Length + 2)'Img & " bytes";
      end if; -- Registers (Response (Header_Length + 2)) /= ...
      Register_Start := Header_Length + 3;
   end Strip_Encapulation;

   function Read_Register (Unit_Id : in Unit_Ids;
                           Register_Address : in Register_Addresses)
                           return Registers is

      -- uses Modbus function Read_Holding_Registers

      PAU_Request : PAU_Request_3;
      Last : Stream_Element_Offset;
      Rx_Buffer : Stream_Element_Array (1 .. 1024); -- oversized buffer
      Register_Start : Stream_Element_Offset;

   begin -- Read_Register
      Transaction_Id := Transaction_Id + 1;
      PAU_Request (1) := Read_Holding_Registers; -- Function Code
      PAU_Request (2 .. 3) := Split_Register (Registers (Register_Address));
      -- Start Address
      PAU_Request (4 .. 5) := Split_Register (1); -- Quantity of registers
      declare -- Send Request
         Request : Stream_Element_Array :=
           Encapuslate (Transaction_Id, Unit_Id, PAU_Request);
      begin
         Send_Socket (Request_Socket, Request, Last);
         if Last /= Request'Last then
            raise Modbus_Error with "Read_Register incomplete request sent";
         end if; -- Last /= Request'Last
      exception
         when Modbus_Error =>
            raise;
         when E : others =>
            raise Modbus_Error with "Read_Register send request - " &
              Exception_Message (E);
      end; -- Send Request
      begin -- Receive
         Receive_Socket (Request_Socket, Rx_Buffer, Last,
                         Peek_At_Incoming_Data);
      exception
         when E : others =>
            raise Modbus_Error with "Read_Register receive error - " &
              Exception_Message (E);
      end; -- Receive
      declare -- Parse Response
         Response : Stream_Element_Array (1 .. Last);
      begin
         Receive_Socket (Request_Socket, Response, Last);
         if Last = Exception_Length then
            raise Modbus_Error with "Read_Register error code" &
              Response (Exception_Length -1)'img & " exception code" &
              Response (Exception_Length)'Img;
         else
            Strip_Encapulation (Transaction_Id, Unit_Id, 1, Response,
                                Register_Start);
            return Assemble_Register (Response (Register_Start ..
                                        Register_Start + 1));
         end if; -- Last = Exception_Length
      end; -- Parse Response
   end Read_Register;

   procedure Read_Registers (Unit_Id : in Unit_Ids;
                             Register_Array : out Register_Arrays) is

      -- uses Modbus function 3

      PAU_Request : PAU_Request_3;
      Last : Stream_Element_Offset;
      Rx_Buffer : Stream_Element_Array (1 .. 1024); -- oversized buffer
      Register_Start : Stream_Element_Offset;
      Number_of_Registers : Registers :=
        Registers (Register_Array'Last - Register_Array'First + 1);

   begin -- Read_Registers
      Transaction_Id := Transaction_Id + 1;
      PAU_Request (1) := Read_Holding_Registers; -- Function Code
      PAU_Request (2 .. 3) := Split_Register (Registers (Register_Array'First));
      -- Start Address
      PAU_Request (4 .. 5) := Split_Register (Number_of_Registers);
      -- Quantity of registers
      declare -- Send Request
         Request : Stream_Element_Array :=
           Encapuslate (Transaction_Id, Unit_Id, PAU_Request);
      begin
         Send_Socket (Request_Socket, Request, Last);
         if Last /= Request'Last then
            raise Modbus_Error with "Read_Registers incomplete request sent";
         end if; -- Last /= Request'Last
      exception
         when Modbus_Error =>
            raise;
         when E : others =>
            raise Modbus_Error with "Read_Registers send request - " &
              Exception_Message (E);
      end; -- Send Request
      begin -- Receive
         Receive_Socket (Request_Socket, Rx_Buffer, Last,
                         Peek_At_Incoming_Data);
      exception
         when E : others =>
            raise Modbus_Error with "Read_Registers receive error - " &
              Exception_Message (E);
      end; -- Receive
      declare -- Parse Response
         Response : Stream_Element_Array (1 .. Last);
      begin
         Receive_Socket (Request_Socket, Response, Last);
         if Last = Exception_Length then
            raise Modbus_Error with "Error Code" &
              Response (Exception_Length -1)'img & " exception code" &
              Response (Exception_Length)'Img;
         else
            Strip_Encapulation (Transaction_Id, Unit_Id, Number_of_Registers,
                                Response, Register_Start);
            for R in Register_Addresses range Register_Array'First ..
              Register_Array'Last loop
               Register_Array (R) :=
                 Assemble_Register (Response (Register_Start ..
                                        Register_Start + 1));
               Register_Start := Register_Start + 2;
            end loop; --
         end if; -- Last = Exception_Length
      end; -- Parse Response
   end Read_Registers;

   procedure Close_Connection is

   -- Closes connection

   begin -- Close_Connection
      Shutdown_Socket (Request_Socket, Shut_Read_Write);
      Close_Socket (Request_Socket);
   end Close_Connection;

   -- Type Conversions from 16 bit registers to SMA data types

   function To_U32 (Register_Array : in Register_Arrays) return Unsigned_32 is

      -- Returns the Unsigned_32 representation of the first two Registers in
      -- Register Array, The first is treated as most significant than the
      -- second.

      First : Register_Addresses := Register_Array'First;
      Second : Register_Addresses := First + 1;

   begin -- To_U32
      return Shift_Left (Unsigned_32 (Register_Array (First)), Register_Shift)
        or Unsigned_32 (Register_Array (Second));
   end To_U32;

   function To_U64 (Register_Array : in Register_Arrays) return Unsigned_64 is

      -- Returns the Unsigned_64 representation of the first four Registers in
      -- Register Array, The first is treated as most significant.

      First : Register_Addresses := Register_Array'First;
      Result : Unsigned_64 := Unsigned_64 (Register_Array (First));

   begin -- To_U64
      for R in Register_Addresses range First .. First + 2 loop
         Result := Shift_Left (Result, Register_Shift) or
           Unsigned_64 (Register_Array (R + 1));
      end loop; -- R in Register_Addresses range First .. First + 2
      return Result;
   end To_U64;

   function To_String (Register_Array : in Register_Arrays) return String is

      -- Returns string representation of Register_Array, the length is the
      -- lesser of twice the number of registers or the length up to the first
      -- zero byte.

      Result : String (1 .. Positive (Register_Array'Last - Register_Array'First
                       + 1) * 2);

      S : Positive := Result'First;

   begin -- To_String
      for R in Register_Array'Range loop
         Result (S) :=
           Character'Val (Shift_Right (Register_Array (R), Byte_Shift));
         Result (S + 1) := Character'Val (Register_Array (R) and Low_Mask);
         S := S + 2;
      end loop; -- R in Register_Array'Range
      S := 1;
      while S in Result'Range and then Result (S) /= NUL loop
         S := S + 1;
      end loop; -- S in Result'Range and then Result (S) /= NUL
      if S <= Result'Last then
         return Delete (Result, S, Result'Last);
      else
         return Result;
      end if; -- S <= Result'Last
   end To_String;

end DJH.Modbus;
