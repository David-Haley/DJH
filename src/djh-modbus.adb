-- This package provides a minimal Modbus TCP functionality, specifically to
-- provide read access to SMA Inverters for data loggong purposes.

-- Author    : David Haley
-- Created   : 17/02/2021
-- Last Edit : 30/10/2026

--  20261030 : Compiler warnings removed
--  20261026 : Improved reliability, connect and receive time outs, TCP keep
-- alive, complete responses received, peer closure detected, any failure
-- closes the connection and is reported as Modbus_Error.
-- 20220820 : Exception messages correcred.

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;

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

   Connect_Timeout : constant Selector_Duration := 10.0;
   Receive_Timeout_Duration : constant Timeval_Duration := 10.0;
   -- Bounds the time spent waiting for the server, a lost connection is then
   -- reported as Modbus_Error rather than blocking indefinitely.
   Keep_Alive_Idle_Time : constant Natural := 60;
   Keep_Alive_Probe_Interval : constant Natural := 10;
   Keep_Alive_Probes : constant Natural := 3;

   Request_Socket : Socket_Type := No_Socket;
   Connected : Boolean := False;
   Transaction_Id : Registers := 0;

   Server_Exception : exception;
   -- Raised internally when the server returns a Modbus exception response,
   -- the connection remains usable.

   procedure Connect_Address (Request_Socket_Address : in Sock_Addr_Type) is

      -- Use IPV4 only (GNAT.Sockets.Family_Inet)

      Status : Selector_Status;

   begin -- Connect_Address
      Close_Connection;
      -- ensures any previous socket is released
      Create_Socket (Request_Socket,
                     Family_Inet,
                     Socket_Stream,
                     IP_Protocol_For_TCP_Level);
      Set_Socket_Option (Request_Socket,
                         IP_Protocol_For_TCP_Level,
                         (Name => No_Delay, Enabled => True));
      Set_Socket_Option (Request_Socket,
                         Socket_Level,
                         (Name => Keep_Alive, Enabled => True));
      Set_Socket_Option (Request_Socket,
                         IP_Protocol_For_TCP_Level,
                         (Name => Keep_Alive_Idle,
                          Idle_Seconds => Keep_Alive_Idle_Time));
      Set_Socket_Option (Request_Socket,
                         IP_Protocol_For_TCP_Level,
                         (Name => Keep_Alive_Interval,
                          Interval_Seconds => Keep_Alive_Probe_Interval));
      Set_Socket_Option (Request_Socket,
                         IP_Protocol_For_TCP_Level,
                         (Name => Keep_Alive_Count,
                          Count => Keep_Alive_Probes));
      Set_Socket_Option (Request_Socket,
                         Socket_Level,
                         (Name => Receive_Timeout,
                          Timeout => Receive_Timeout_Duration));
      Connect_Socket (Request_Socket, Request_Socket_Address, Connect_Timeout,
                      Status => Status);
      if Status /= Completed then
         raise Modbus_Error with "connection timed out";
      end if; -- Status /= Completed
      Connected := True;
   exception
      when others =>
         Close_Connection;
         raise;
   end Connect_Address;

   procedure Connect (Server_Name : in String;
                      Server_Port : in Port_Type := Default_Modbus_Port) is

   begin -- Connect
      Connect_Address
        (Network_Socket_Address (Addresses (Get_Host_By_Name (Server_Name)),
                                 Server_Port));
   exception
      when E : others =>
         raise Modbus_Error with "Connect (Name) - " & Exception_Message (E);
   end Connect;

   procedure Connect (Server_IP : in Inet_Addr_Type;
                      Server_Port : in Port_Type := Default_Modbus_Port) is

      -- Use IPV4 only (GNAT.Sockets.Family_Inet)

   begin -- Connect
      Connect_Address (Network_Socket_Address (Server_IP, Server_Port));
   exception
      when E : others =>
         raise Modbus_Error with "Connect (address) - " & Exception_Message (E);
   end Connect;

   function Is_Connected return Boolean is

   begin -- Is_Connected
      return Connected;
   end Is_Connected;

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

      High : constant Registers :=
        Shift_Left (Registers (Stream_Pair (1)), Byte_Shift);
      Low : constant Registers := Registers (Stream_Pair (2));

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

      Result : Stream_Element_Array (0 .. Header_Length + PAU'Last - PAU'First);

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
           Assemble_Register (Response (1 .. 2))'Img;
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
           " and received" & Response (Header_Length)'Img;
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

   procedure Receive_Exact (Item : out Stream_Element_Array) is

      -- Receives exactly Item'Length elements, TCP may deliver a response in
      -- more than one segment.

      First : Stream_Element_Offset := Item'First;
      Last : Stream_Element_Offset;

   begin -- Receive_Exact
      while First <= Item'Last loop
         Receive_Socket (Request_Socket, Item (First .. Item'Last), Last);
         if Last < First then
            raise Modbus_Error with "connection closed by server";
         end if; -- Last < First
         First := Last + 1;
      end loop; -- First <= Item'Last
   exception
      when E : Socket_Error =>
         if Resolve_Exception (E) = Resource_Temporarily_Unavailable then
            raise Modbus_Error with "receive timed out";
         else
            raise;
         end if; -- Resolve_Exception (E) = Resource_Temporarily_Unavailable
   end Receive_Exact;

   function Read_Register (Unit_Id : in Unit_Ids;
                           Register_Address : in Register_Addresses)
                           return Registers is

      -- uses Modbus function Read_Holding_Registers

      Register_Array : Register_Arrays (Register_Address .. Register_Address);

   begin -- Read_Register
      Read_Registers (Unit_Id, Register_Array);
      return Register_Array (Register_Address);
   end Read_Register;

   procedure Read_Registers (Unit_Id : in Unit_Ids;
                             Register_Array : out Register_Arrays) is

      -- uses Modbus function 3

      Exception_Function : constant Stream_Element :=
        Read_Holding_Registers + 16#80#;
      PAU_Request : PAU_Request_3;
      Last : Stream_Element_Offset;
      Header : Stream_Element_Array (1 .. Header_Length);
      Message_Length : Stream_Element_Offset;
      Register_Start : Stream_Element_Offset;
      Number_of_Registers : constant Registers :=
        Registers (Register_Array'Last - Register_Array'First + 1);

   begin -- Read_Registers
      if not Connected then
         raise Modbus_Error with "not connected";
      end if; -- not Connected
      Transaction_Id := Transaction_Id + 1;
      PAU_Request (1) := Read_Holding_Registers; -- Function Code
      PAU_Request (2 .. 3) := Split_Register (Registers (Register_Array'First));
      -- Start Address
      PAU_Request (4 .. 5) := Split_Register (Number_of_Registers);
      -- Quantity of registers
      declare -- Send Request
         Request : constant Stream_Element_Array :=
           Encapuslate (Transaction_Id, Unit_Id, PAU_Request);
      begin
         Send_Socket (Request_Socket, Request, Last);
         if Last /= Request'Last then
            raise Modbus_Error with "incomplete request sent";
         end if; -- Last /= Request'Last
      end; -- Send Request
      Receive_Exact (Header);
      Message_Length := Assemble_Offset (Header (5 .. 6));
      -- Bytes including Unit_Id to end
      if Message_Length not in Exception_Length - Header_Length + 1 .. 254 then
         raise Modbus_Error with "invalid message length" &
           Message_Length'Img;
      end if; -- Message_Length not in ...
      declare -- Parse Response
         Response : Stream_Element_Array (1 .. Header_Length - 1 +
                                            Message_Length);
      begin
         Response (Header'Range) := Header;
         Receive_Exact (Response (Header_Length + 1 .. Response'Last));
         if Response (Header_Length + 1) = Exception_Function and
           Assemble_Register (Response (1 .. 2)) = Transaction_Id then
            raise Server_Exception with "Error Code" &
              Response (Header_Length + 1)'Img & " exception code" &
              Response (Header_Length + 2)'Img;
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
         end if; -- Response (Header_Length + 1) = Exception_Function ...
      end; -- Parse Response
   exception
      when E : Server_Exception =>
         -- Valid response from the server, connection remains usable
         raise Modbus_Error with "Read_Registers - " & Exception_Message (E);
      when E : others =>
         -- Socket error, time out or a response which cannot be trusted,
         -- the connection must be reestablished.
         Close_Connection;
         raise Modbus_Error with "Read_Registers - " & Exception_Message (E);
   end Read_Registers;

   procedure Close_Connection is

   -- Closes connection, any errors are ignored since the connection may
   -- already be broken.

   begin -- Close_Connection
      Connected := False;
      if Request_Socket /= No_Socket then
         begin -- Shutdown
            Shutdown_Socket (Request_Socket, Shut_Read_Write);
         exception
            when others =>
               null;
         end; -- Shutdown
         begin -- Close
            Close_Socket (Request_Socket);
         exception
            when others =>
               null;
         end; -- Close
         Request_Socket := No_Socket;
      end if; -- Request_Socket /= No_Socket
   end Close_Connection;

   -- Type Conversions from 16 bit registers to SMA data types

   function To_U32 (Register_Array : in Register_Arrays) return Unsigned_32 is

      -- Returns the Unsigned_32 representation of the first two Registers in
      -- Register Array, The first is treated as most significant than the
      -- second.

      First : constant Register_Addresses := Register_Array'First;
      Second : constant Register_Addresses := First + 1;

   begin -- To_U32
      return Shift_Left (Unsigned_32 (Register_Array (First)), Register_Shift)
        or Unsigned_32 (Register_Array (Second));
   end To_U32;

   function To_U64 (Register_Array : in Register_Arrays) return Unsigned_64 is

      -- Returns the Unsigned_64 representation of the first four Registers in
      -- Register Array, The first is treated as most significant.

      First : constant Register_Addresses := Register_Array'First;
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
