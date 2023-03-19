-- HTTP interface to vera
-- Author    : David Haley
-- Created   : 02/04/2019
-- Last Edit : 08/06/2019
-- 20210608: moved to DJH library

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Sockets; use GNAT.Sockets;

package body DJH.Vera is

   Vera_Port : constant Port_Type := 3480;
   Response_1 : constant String := "HTTP/1.1 200 OK";
   Response_2 : constant String := "content-Type: application/json";
   Response_3 : constant String := "{ ""u:SetTargetResponse"": { ""JobID"": """;

   subtype States is Character range '0' .. '1';

   package Device_IO is new Ada.Text_IO.Integer_IO (Devices);

   function HTTP_GET (Name : in String;
                      Command : in String) return Unbounded_String is

      CrLf : constant String := ASCII.CR & ASCII.LF;

      Host_Entry : Host_Entry_Type := Get_Host_By_Name(Name);
      Address : Sock_Addr_Type;
      Socket : Socket_Type;
      Channel : Stream_Access;
      Data : Stream_Element_Array (1..1024);
      Last : Stream_Element_Offset;
      Result : Unbounded_String := Null_Unbounded_String;

   begin -- HTTP_GET
      Address.Addr := Addresses (Host_Entry, 1);
      Address.Port := Vera_Port;
      begin -- Create Socket
         Create_Socket (Socket);
         Connect_Socket (Socket, Address);
      exception
         when E : others =>
            raise Vera_Error with "socket creation - " & Exception_Message (E);
      end; -- Create Socket
      begin -- Stream Write
         Channel := Gnat.Sockets.Stream (Socket);
         String'Write (Channel, "GET /" & Command & " HTTP/1.1" & CRLF &
                         "Host: " & Name & CRLF & CRLF);
      exception
         when E : others =>
            raise Vera_Error with "stream write - " & Exception_Message (E);
      end; -- Stream Write
      loop -- until nothing to read
         begin -- Receive
            Receive_Socket (Socket, Data, Last, Peek_At_Incoming_Data);
         exception
            when E : others =>
               raise Vera_Error with "receive error - " & Exception_Message (E);
         end; -- Receive
         declare -- Parse Response
            Response : Stream_Element_Array (1 .. Last);
         begin -- get result
            Receive_Socket (Socket, Response, Last);
            for I in Stream_Element_Offset range 1 .. Last loop
               Result := Result & Character'Val (Response (I));
            end loop; -- I in Stream_Element_Offset range 1 .. Last
         end; -- get result
         Receive_Socket (Socket, Data, Last, Peek_At_Incoming_Data);
         exit when Last = 0;
      end loop; -- until nothing to read
      Close_Socket (Socket);
      return Result;
   end HTTP_GET;

   function Check_Response (Reply : in Unbounded_String) return Boolean is

      Response_1 : constant String := "HTTP/1.1 200 OK";
      Response_2 : constant String := "content-Type: application/json";
      Response_3 : constant String :=
        "{ ""u:SetTargetResponse"": { ""JobID"": """;
      Start_at : Positive := 1;
      First : Natural;

   begin -- Check_Response
      First := Index (Reply, Response_1, Start_At);
      if First > 0 then
         Start_At := First + Response_1'Length;
         First := Index (Reply, Response_2, Start_At);
         if First > 0 then
            Start_At := First + Response_2'Length;
            First := Index (Reply, Response_3, Start_At);
            if First > 0 then
               return True;
            end if; -- third response
         end if; -- second response
      end if; -- first response
      return False;
   end Check_Response;

   function Assemble_Message (Device : in Devices;
                              State : in States) return String is

      Request_String : constant String :=
        "/data_request?id=lu_action&output_format=json&DeviceNum=";
      Action_String : constant String :=
        "&serviceId=urn:upnp-org:serviceId:SwitchPower1&" &
        "action=SetTarget&newTargetValue=";

      Device_String : String (1 ..5); -- allow for 100 + devices

   begin -- Assemble_Message
      Device_IO.Put (Device_String, Device);
      return Request_String & Trim (Device_String, Left) &
        Action_String & State;
   end Assemble_Message;

   function Switch_On (Name : in String; Device : in Devices) return Boolean is
      -- turns switch Device on and returns true if successful

      Reply : Unbounded_String;

   begin -- Switch_On
      return Check_Response (HTTP_GET (Name, Assemble_Message (Device, '1')));
   end Switch_On;

   function Switch_Off (Name : in String; Device : in Devices) return Boolean is
      -- turns switch Device off and returns true if successful

   begin -- Switch_Off
      return Check_Response (HTTP_GET (Name, Assemble_Message (Device, '0')));
   end Switch_Off;

end DJH.Vera;
