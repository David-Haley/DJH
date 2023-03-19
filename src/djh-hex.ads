-- Author    : David Haley
-- Created   : 16/06/2020
-- Last Edit : 08/01/2021
-- Provides conversions to and from Stream_Elements hex characters and String to
-- hex characters
-- 20210108: Remamed to avoid issues with Digest returning Strings
-- 20201207: String to hex and reverse added
-- Last Edit : 07/12/2020
-- Provides conversions to and from Stream_Elements hex characters and String to
-- hex characters
-- 20201207 : String to hex and reverse added

with Ada.Streams; use Ada.Streams;

package DJH.Hex is

   function To_Hex (N : in Stream_Element_Array) return String;

   function To_Stream (S : in String) return Stream_Element_Array;

   function String_To_Hex (N : in String) return String;

   function Hex_To_String (S : in String) return String;
   -- S string of hex digit pairs

end DJH.Hex;
