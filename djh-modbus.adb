-- This package provides a minimal Modbus TCP functionality, specifically to
-- provide read access to SMA Inverters for data loggong purposes.

-- Author    : David Haley
-- Created   : 17/02/2021
-- Last Edit : 25/02/2021

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;use Ada.Streams;

package body DJH.Modbus is

   Header_Length : constant Stream_Element_Offset := 7;

   subtype Stream_Pairs is Stream_Element_Array (0 .. 1);

   subtype PAU_Request_3 is Stream_Element_Array (0 .. 4);

   subtype Byte_Counts is Registers range 0 .. 260;

   subtype Function_Codes is Stream_Element range 1 .. 127;

   Read_Holding_Registers : constant Function_Codes := 3;

   Request_Socket, Listen_Socket : Socket_Type;
   Transaction_Id : Registers := 0;

   procedure Connect (Server_Name : in String;
                      Client_Port : in Port_Type;
                      Server_Port : in Port_Type := Default_Modbus_Port) is

      SS_No_Delay : constant Option_Type :=
        (Name => No_Delay, Enabled => True);

      Server_Entry : Host_Entry_Type := Get_Host_By_Name (Server_Name);
      Request_Socket_Address, Listen_Socket_Address : Sock_Addr_Type;

   begin -- Connect
      begin -- server socket exception
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
                      Network_Socket_Address (No_Inet_Addr, Client_Port));
         Connect_Socket (Request_Socket, Request_Socket_Address);
      exception
         when E : others =>
            raise Modbus_Error with "Server socket - " & Exception_Message (E);
      end; -- server socket exception
      begin -- client socket exception
         Listen_Socket_Address :=
           Network_Socket_Address (No_Inet_Addr, Server_Port);
         Create_Socket (Listen_Socket);
         Bind_Socket (Listen_Socket, Listen_Socket_Address);
      exception
         when E : others =>
            raise Modbus_Error with "Client socket - " & Exception_Message (E);
      end; --  client socket exception
   end Connect;

   procedure Connect (Server_IP : in Inet_Addr_Type;
                      Client_Port : in Port_Type;
                      Server_Port : in Port_Type := Default_Modbus_Port) is

      -- Use IPV4 only (GNAT.Sockets.Family_Inet)

      SS_No_Delay : constant Option_Type :=
        (Name => No_Delay, Enabled => True);

      Server_Entry : Host_Entry_Type :=  Get_Host_By_Address (Server_IP);
      Request_Socket_Address, Listen_Socket_Address : Sock_Addr_Type;

   begin -- Connect
      begin -- server socket exception
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
                      Network_Socket_Address (No_Inet_Addr, Client_Port));
         Connect_Socket (Request_Socket, Request_Socket_Address);
      exception
         when E : others =>
            raise Modbus_Error with "Server socket - " & Exception_Message (E);
      end; -- server socket exception
      begin -- client socket exception
         Listen_Socket_Address :=
           Network_Socket_Address (No_Inet_Addr, Server_Port);
         Create_Socket (Listen_Socket);
         Bind_Socket (Listen_Socket, Listen_Socket_Address);
         GNAT.Sockets.Listen_Socket (Listen_Socket);
      exception
         when E : others =>
            raise Modbus_Error with "Client socket - " & Exception_Message (E);
      end; --  client socket exception
   end Connect;

   function Split_Register (Register : in Registers) return Stream_Pairs is

      Low_Mask : constant Registers := 2#0000000011111111#;
      Temp : Registers := Register;
      Result : Stream_Pairs;

   begin -- Split_Register
      Result (0) := Stream_Element (Shift_Right (Temp, 8) and Low_Mask);
      Result (1) := Stream_Element (Register and Low_Mask);
      return Result;
   end Split_Register;

   function Assemble_Register (Stream_Pair : in Stream_Pairs)
                                return Registers is

      High : Registers := Registers (Stream_Pair (0));
      Low : Registers := Shift_Left (Registers (Stream_Pair (1)), 8);

   begin -- Assemble_Register
      return High or Low;
   end Assemble_Register;

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

   function Strip_Encapulation (Response : in Stream_Element_Array)
                                return Stream_Element_Array is

      -- Response'First assumed to be 0, caution the returned stream will have
      -- a first element offset of Header Length.

      Exception_Base : constant Stream_Element := 16#80#;
      PAU_End : Stream_Element_Offset;

   begin -- Strip_Encapulation
      if Assemble_Register (Response (0 .. 1)) /= Transaction_Id then
         raise Modbus_Error with "mismatched Transaction_Id, expected" &
           Transaction_Id'Img & " and received" &
           Assemble_Register (Response (0 .. 1))'img;
      end if; -- Assemble_Register (Resopnse (0 .. 1)) /= Transaction_Id
      if Assemble_Register (Response (2 .. 3)) /= 0 then
         raise Modbus_Error with "Unknown protocol" &
           Assemble_Register (Response (2 .. 3))'Img;
      end if; -- Assemble_Register (Response (2 .. 3)) /= 0
      if Response (Header_Length) > Exception_Base then
         raise Modbus_Error with "Error response to function" &
           Stream_Element'Image (Response (Header_Length) - Exception_Base) &
           ", exception code" & Response (Header_Length + 1)'Img;
      end if; -- Response (Header_Length) > Exception_Base
      -- Unit_Id is not verified, if transaction_Id matches and no exception was
      -- raised it is assumed that the requested unit responded!
      PAU_End := Stream_Element_Offset (Assemble_Register (Response (4 .. 5)))
        + Header_Length - 1;
      return Response (Header_Length .. PAU_End);
   end Strip_Encapulation;

   function Read_Register (Unit_Id : in Unit_Ids;
                           Register_Address : in Register_Addresses)
                           return Registers is

      -- uses Modbus function Read_Holding_Registers

      PAU_Request : PAU_Request_3;
      Last : Stream_Element_Offset;
      Response : Stream_Element_Array (0 .. Header_Length + 3);

   begin -- Read_Register
      PAU_Request (0) := Read_Holding_Registers; -- Function Code
      PAU_Request (1 .. 2) := Split_Register (Registers (Register_Address));
      -- Start Address
      PAU_Request (3 .. 4) := Split_Register (1); -- Quantity of registers
      declare -- Send Request
         Request : Stream_Element_Array :=
           Encapuslate (Transaction_Id, Unit_Id, PAU_Request);
      begin
         Send_Socket (Request_Socket, Request, Last, Process_Out_Of_Band_Data);
         if Last /= Request'Last then
            raise Modbus_Error with "incomplete request sent";
         end if; -- Last /= Request'Last
      exception
         when Modbus_Error =>
            raise;
         when E : others =>
            raise Modbus_Error with "send request - " & Exception_Message (E);
      end; -- Send Request
      begin -- Receive
         Receive_Socket (Listen_Socket, Response, Last,
                         Process_Out_Of_Band_Data);
      exception
         when E : others =>
            raise Modbus_Error with "receive error - " & Exception_Message (E);
      end; -- Receive
      Transaction_Id := Transaction_Id + 1;
      declare -- Parse PAU
         PAU : Stream_Element_Array := Strip_Encapulation (Response);
      begin
         if PAU (PAU'First) /= Read_Holding_Registers then
            raise Modbus_Error with "Bad response function code expected" &
              Read_Holding_Registers'Img &
              " and received" & PAU (PAU'First)'Img;
         end if; -- PAU (PAU'First) /= Read_Holding_Registers
         if Assemble_Register (PAU (PAU'First + 1 .. PAU'First + 2)) /= 1 then
            raise Modbus_Error with
              "Incorrect quantity of registers expected 1 and received " &
              Assemble_Register (PAU (PAU'First + 1 .. PAU'First + 2))'Img;
         end if; -- Assemble_Register (PAU (PAU'First + 1 .. PAU'First + 2)) ...
         return Assemble_Register (PAU (PAU'First + 3 .. PAU'First + 4));
      end; -- Parse PAU
   end Read_Register;

   procedure Read_Registers (Unit_Id : in Unit_Ids;
                             Register_Array : out Register_Arrays) is

   -- uses Modbus function 3

   begin -- Read_Registers
      null;
   end Read_Registers;

   procedure Close_Connection is

   begin -- Close_Connection
      Shutdown_Socket (Request_Socket, Shut_Read_Write);
      Shutdown_Socket (Listen_Socket, Shut_Read_Write);
      Close_Socket (Request_Socket);
      Close_Socket (Listen_Socket);
   end Close_Connection;

end DJH.Modbus;
