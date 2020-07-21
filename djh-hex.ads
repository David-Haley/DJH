-- Author    : David Haley
-- Created   : 16/06/2020
-- Last Edit : 16/06/2020
-- Provides conversions to and from Stream_Elements hex characters

with Ada.Streams; use Ada.Streams;

package DJH.Hex is

   function To_Hex (N : in Stream_Element_Array) return String;

   function To_Stream (S : in String) return Stream_Element_Array;

end DJH.Hex;
