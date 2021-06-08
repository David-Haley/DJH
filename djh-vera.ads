-- HTTP interface to vera
-- Author    : David Haley
-- Created   : 02/04/2019
-- Last Edit : 08/06/2019
-- 20210608: moved to DJH library

package DJH.Vera is

   subtype Devices is Positive range 1 .. 200; -- assumed device number range

   function Switch_On (Name : in String; Device : in Devices) return Boolean;
   -- turns switch Device on and returns true if successful

   function Switch_Off (Name : in String; Device : in Devices) return Boolean;
   -- turns switch Device off and returns true if successful

   Vera_Error : exception;

end DJH.Vera;
