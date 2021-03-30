-- This package provides a minimal Modbus TCP functionality, specifically to
-- provide read access to SMA Inverters for data loggong purposes.

-- Author    : David Haley
-- Created   : 12/02/2021
-- Last Edit : 18/03/2021

with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;
with GNAT.Sockets; use GNAT.Sockets;

package DJH.Modbus is

   Default_Modbus_Port : constant Port_Type := 502;

   subtype Unit_Ids is Stream_Element range 1 .. 255;
   -- Note broadcast Init_Id 0 is not implemented.

   subtype Registers is Unsigned_16;
   subtype Register_Addresses is Unsigned_16;
   type Register_Arrays is array (Register_Addresses range <>) of Registers;

   Modbus_Error : exception;

   procedure Connect (Server_Name : in String;
                      Server_Port : in Port_Type := Default_Modbus_Port);

   procedure Connect (Server_IP : in Inet_Addr_Type;
                      Server_Port : in Port_Type := Default_Modbus_Port);

   -- Use IPV4 only (GNAT.Sockets.Family_Inet)

   function Read_Register (Unit_Id : in Unit_Ids;
                           Register_Address : in Register_Addresses)
                           return Registers;
   -- uses Modbus function 3

   procedure Read_Registers (Unit_Id : in Unit_Ids;
                             Register_Array : out Register_Arrays);
   -- uses Modbus function 3

   procedure Close_Connection;

   -- Closes connection

   -- Type Conversions from 16 bit registers to SMA data types

   function To_U32 (Register_Array : in Register_Arrays) return Unsigned_32;

   -- Returns the Unsigned_32 representation of the first two Registers in
   -- Register Array, The first is treated as most significant than the second.

   function To_U64 (Register_Array : in Register_Arrays) return Unsigned_64;

   -- Returns the Unsigned_64 representation of the first four Registers in
   -- Register Array, The first is treated as most significant.

   function To_String (Register_Array : in Register_Arrays) return String;

   -- Returns string representation of Register_Array, the length is the lesser
   -- of twice the number of registers or the length up to the first zero byte.

end DJH.Modbus;
