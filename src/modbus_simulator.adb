-- This program provides a limited simulation of a sensor supporting reading of
-- holding registers 30003 .. 30105 only.

-- Author    : David Haley
-- Created   : 14/03/2021
-- Last Edit : 21/03/2021

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;
with GNAT.Sockets; use GNAT.Sockets;

procedure Modbus_Simulator is

   subtype Registers is Unsigned_16;
   subtype Register_Addresses is Unsigned_16;
   subtype Device_Addresses is Register_Addresses range 30001 .. 30200;
   type Holding_Registers is array (Register_Addresses range <>) of Registers;
   subtype Unit_Identifiers is Stream_Element;
   subtype Function_Numbers is Stream_Element;

   type MBAP_Headers is record
      Transaction_Identifier : Registers;
      Protocol_Identifier : Registers := 0; -- Modbus
      Length : Registers;
      Unit_Identifier : Unit_Identifiers;
   end record; -- MBAP_Headers

   Function_Offset : constant Stream_Element_Offset := 8; -- First byte of PDU

   subtype MBAP_Streams is Stream_Element_Array (1 .. 7);
   subtype Two_Bytes is Stream_Element_Array (1 .. 2);

   Byte_Shift : constant Natural := 8;

   function Combine (Two_Byte : in Two_Bytes) return Registers is

      Result : Registers;

   begin -- Combine
      Result := Unsigned_16 (Two_Byte (1));
      Result := Shift_Right (Result, Byte_Shift) or Unsigned_16 (Two_Byte (2));
      return Result;
   end Combine;

   function Split (Register : in Registers) return Two_Bytes is

      Low_Byte : constant Unsigned_16 := 2#0000000011111111#;
      Result : Two_Bytes;

   begin -- Split
      Result (1) := Stream_Element (Shift_Right (Register, Byte_Shift));
      Result (2) := Stream_Element (Register and Low_Byte);
      return Result;
   end Split;

   procedure Parse_MBAP (MBAP_Stream : in MBAP_Streams;
                         This_Unit : in Unit_Identifiers;
                         MBAP_Header : out MBAP_Headers;
                         Valid : out Boolean) is

   begin -- Parse_MBAP
      Valid := True;
      MBAP_Header.Transaction_Identifier := Combine (MBAP_Stream (1 .. 2));
      if Combine (MBAP_Stream (3 .. 4)) /= 0 then
         Put_Line (MBAP_Header.Transaction_Identifier'Img &
                     ": MBAP Protocol Identifier was" &
                     Registers'Image (Combine (MBAP_Stream (3 .. 4))) &
                     " expected 0");
         Valid := False;
      end if; -- Combine (MBAP_Stream (3 .. 4)) /= 0
      MBAP_Header.Length := Combine (MBAP_Stream (5 .. 6));
      MBAP_Header.Unit_Identifier := MBAP_Stream (7);
      if MBAP_Header.Unit_Identifier /= This_Unit then
         Put_Line (MBAP_Header.Transaction_Identifier'Img &
                     ": Expected Unit Identifier" & This_Unit'Img &
                     " and received" & MBAP_Header.Unit_Identifier'Img);
         Valid := False;
      end if; -- MBAP_Header.Unit_Identifier /= This_Unit
   end Parse_MBAP;

   procedure Exception_Response (Responder : in Socket_Type;
                                 Exception_Code : in Stream_Element;
                                 MBAP_Header : in MBAP_Headers;
                                 Function_Number : in Function_Numbers) is

      Tx_Buffer : Stream_Element_Array (1 .. 9);
      Exception_Mask : constant Function_Numbers := 2#10000000#;
      Last : Stream_Element_Offset;

   begin -- Exception_Response
      Tx_Buffer (1 .. 2) := Split (MBAP_Header.Transaction_Identifier);
      Tx_Buffer (3 .. 4) := Split (0); -- Ptptocol (Modbus)
      Tx_Buffer (5 .. 6) := Split (3); -- length of message
      Tx_Buffer (7) := MBAP_Header.Unit_Identifier;
      Tx_Buffer (Function_Offset) := Exception_Mask or Function_Number;
      Tx_Buffer (9) := Exception_Code;
      Send_Socket (Responder, Tx_Buffer, Last);
   end Exception_Response;

   procedure Valid_Response (Responder : in Socket_Type;
                             MBAP_Header : in MBAP_Headers;
                             Function_Number : in Function_Numbers;
                             Holding_Register : in Holding_Registers;
                             Start_Address : in Register_Addresses;
                             Number_of_Registers : in Unsigned_16) is

      Tx_Buffer : Stream_Element_Array
        (1 .. 7 + 2 + 2 * Stream_Element_Offset (Number_of_Registers));
      Data_Offset : Stream_Element_Offset := 10; -- start of returned registers
      Last : Stream_Element_Offset;

   begin -- Valid_Response
      Tx_Buffer (1 .. 2) := Split (MBAP_Header.Transaction_Identifier);
      Tx_Buffer (3 .. 4) := Split (MBAP_Header.Protocol_Identifier);
      Tx_Buffer (5 .. 6) := Split (Registers (Tx_Buffer'Last) - 6);
      Tx_Buffer (7) := MBAP_Header.Unit_Identifier;
      Tx_Buffer (Function_Offset) := Function_Number;
      Tx_Buffer (9) := Stream_Element (Number_of_Registers * 2);
      for I in Device_Addresses range Start_Address ..
        Start_Address + Number_of_Registers - 1 loop
         Tx_Buffer (Data_Offset .. Data_Offset + 1) :=
           Split (Holding_Register (I));
         Data_Offset := Data_Offset + 2;
      end loop; -- I in Device_Addresses range Start_Address ...
      Send_Socket (Responder, Tx_Buffer, Last);
   end Valid_Response;

   procedure Create_Registers (Holding_Register : out Holding_Registers) is

      Test_String_1 : constant String := "SMA Modbus"; -- exact fit 5 registers
      Test_String_2 : constant String := "Short";
      -- requires NULL pad to fit in 3 registers

      procedure Copy_String (Text : in String;
                             Holding_Register : out Holding_Registers) is

         function To_U16 (Text : in String;
                          S : Positive) return Registers is

         begin -- To_U16
            if S in Text'Range then
               return Registers (Character'Pos (Text (S)));
            else
               return 0;
            end if; -- S in Text'Range
         end To_U16;

         S : Positive := Text'First;

      begin -- Copy_String
         for R in Holding_Register'Range loop
            Holding_Register (R) := Shift_Left (To_U16 (Text, S), Byte_Shift) or
              To_U16 (Text, S + 1);
            S := S + 2;
         end loop; -- R in Holding_Register'Range
      end Copy_String;

      U64 : constant Unsigned_64 := 123456789012345678;
      U32 : constant Unsigned_32 := 123456789;
      U64_16 : constant Unsigned_64 := 16#FFFF#;
      U32_16 : constant Unsigned_32 := 16#FFFF#;
      Word_Shift : constant Natural := 16;

   begin -- Create_Registers
      Copy_String (Test_String_2, Holding_Register (30198 .. 30200));
      -- 30198: String_2
      Copy_String (Test_String_1, Holding_Register (30193 .. 30197));
      -- 30193: String_1
      Holding_Register (30192) := Registers (U64 and U64_16);
      Holding_Register (30191) :=
        Registers (Shift_Right (U64, Word_Shift) and U64_16);
      Holding_Register (30190) :=
        Registers (Shift_Right (U64, Word_Shift * 2) and U64_16);
      Holding_Register (30189) := Registers (Shift_Right (U64, Word_Shift * 3));
      -- 30189: U64
      Holding_Register (30188) := Registers (U32 and U32_16);
      Holding_Register (30187) := Registers (Shift_Right (U32, Word_Shift));
      -- 30187: U32
      for I in Device_Addresses range Device_Addresses'First .. 30186 loop
         Holding_Register (I) := I;
      end loop; -- I in Device_Addresses range Device_Addresses'First ...
   end Create_Registers;

   This_Unit : Unit_Identifiers := 1;
   Modbus_Port : Port_Type := 502;
   Holding_Register : Holding_Registers (Device_Addresses);
   MBAP_Header : MBAP_Headers;
   Function_Number : Stream_Element;
   Start_Address : Register_Addresses;
   Number_of_Registers : Unsigned_16;
   Server, Responder  : Socket_Type;
   RS_No_Delay : constant Option_Type := (Name => No_Delay, Enabled => True);
   Address, Client_Address : Sock_Addr_Type;
   Buffer : Stream_Element_Array (1 .. 1024);
   Last : Stream_Element_Offset;
   Error : Boolean;
   Request_Count : Natural := 0;

begin -- Modbus_Simulator
   if Argument_Count > 0 then
      This_Unit := Unit_Identifiers'Value (Argument (1));
      if Argument_Count > 1 then
         Modbus_Port := Port_Type'Value (Argument (2));
      end if; -- Argument_Count > 0
   end if; -- Argument_Count > 1
   Put_Line ("Unit Identifier:" & This_Unit'Img & " Modbus Port:" &
               Modbus_Port'Img);
   Create_Registers (Holding_Register);
   Create_Socket (Server);
   Address.Port := 502;
   Address.Addr := Any_Inet_Addr;
   Bind_Socket (Server, Address);
   Listen_Socket (Server);
   Accept_Socket (Server, Responder, Client_Address);
   Set_Socket_Option (Responder, IP_Protocol_For_TCP_Level, RS_No_Delay);
   loop -- one request
      Receive_Socket (Responder, Buffer, Last, Peek_At_Incoming_Data);
      exit when Last = 0;
      declare -- Process received data
         Rx_Data : Stream_Element_Array (1 .. Last);
      begin
         Receive_Socket (Responder, Rx_Data, Last);
         Parse_MBAP (Rx_Data (1 .. 7), This_Unit, MBAP_Header, Error);
         if Last /= Stream_Element_Offset (MBAP_Header.Length + 6) then
            Put_Line (MBAP_Header.Transaction_Identifier'Img &
                        ": Incorrect message length, read" & Last'Img &
                        " expected" &
                        Unsigned_16'Image (MBAP_Header.Length + 6));
         end if; -- Last /= Stream_Element_Offset (MBAP_Header.Length + 6)
         Function_Number := Rx_Data (Function_Offset);
         if Function_Number = 3 then
            Start_Address := Shift_Left (Unsigned_16 (Rx_Data (9)), 8);
            Start_Address := Start_Address or Unsigned_16 (Rx_Data (10));
            Number_of_Registers := Shift_Left (Unsigned_16 (Rx_Data (11)), 8);
            Number_of_Registers := Number_of_Registers or
              Unsigned_16 (Rx_Data (12));
            if Number_of_Registers >= 0 and Number_of_Registers <= 16#7D# then
               if Start_Address in Device_Addresses and then
                 Start_Address + Number_of_Registers - 1 in Device_Addresses
               then
                  Valid_Response (Responder, MBAP_Header, Function_Number,
                                  Holding_Register, Start_Address,
                                  Number_of_Registers);
               else
                  Put_Line (MBAP_Header.Transaction_Identifier'Img &
                              ": Bad start address" & Start_Address'Img &
                              " or Number of registers" &
                              Number_of_Registers'Img);
                  Exception_Response (Responder, 2, MBAP_Header,
                                      Function_Number);
               end if; -- Start_Address in Device_Addresses ..
            else
               Put_Line (MBAP_Header.Transaction_Identifier'Img &
                           ": Bad Number of registers" &
                           Number_of_Registers'Img);
               Exception_Response (Responder, 3, MBAP_Header, Function_Number);
            end if; -- Number_of_Registers >= 0 and Number_of_Registers ...
         else
            Put_Line (MBAP_Header.Transaction_Identifier'Img &
                        ": Unsupported function" &
                        Stream_Element'Image (Function_Number));
            Exception_Response (Responder, 1, MBAP_Header, Function_Number);
         end if; -- Function_Number = 3
      end; -- Process received data
      Request_Count := Request_Count + 1;
   end loop; -- one request
   Put_Line ("Normal Termination," & Request_Count'Img & " requests processed");
   Close_Socket (Server);
   Close_Socket (Responder);
exception
   when E: others =>
      Put_Line (Exception_Message (E));
      Close_Socket (Server);
      Close_Socket (Responder);
end Modbus_Simulator;
