-- Author    : David Haley
-- Created   : 16/06/2020
-- Last Edit : 08/01/2021
-- Provides conversions to and from Stream_Elements hex characters and String to
-- hex characters
-- 20210108: Remamed to avoid issues with Digest returning Strings
-- 20201207: String to hex and reverse added

with Ada.Text_IO; use Ada.Text_IO;

package body DJH.Hex is

   package S_IO is new Ada.Text_IO.Modular_IO (Stream_Element);

   subtype Two_Digits is String (1 .. 2);
   Hex_Lookup : constant array (Stream_Element) of Two_Digits :=
     ("00", "01", "02", "03", "04", "05", "06", "07",
      "08", "09", "0a", "0b", "0c", "0d", "0e", "0f",
      "10", "11", "12", "13", "14", "15", "16", "17",
      "18", "19", "1a", "1b", "1c", "1d", "1e", "1f",
      "20", "21", "22", "23", "24", "25", "26", "27",
      "28", "29", "2a", "2b", "2c", "2d", "2e", "2f",
      "30", "31", "32", "33", "34", "35", "36", "37",
      "38", "39", "3a", "3b", "3c", "3d", "3e", "3f",
      "40", "41", "42", "43", "44", "45", "46", "47",
      "48", "49", "4a", "4b", "4c", "4d", "4e", "4f",
      "50", "51", "52", "53", "54", "55", "56", "57",
      "58", "59", "5a", "5b", "5c", "5d", "5e", "5f",
      "60", "61", "62", "63", "64", "65", "66", "67",
      "68", "69", "6a", "6b", "6c", "6d", "6e", "6f",
      "70", "71", "72", "73", "74", "75", "76", "77",
      "78", "79", "7a", "7b", "7c", "7d", "7e", "7f",
      "80", "81", "82", "83", "84", "85", "86", "87",
      "88", "89", "8a", "8b", "8c", "8d", "8e", "8f",
      "90", "91", "92", "93", "94", "95", "96", "97",
      "98", "99", "9a", "9b", "9c", "9d", "9e", "9f",
      "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
      "a8", "a9", "aa", "ab", "ac", "ad", "ae", "af",
      "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7",
      "b8", "b9", "ba", "bb", "bc", "bd", "be", "bf",
      "c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7",
      "c8", "c9", "ca", "cb", "cc", "cd", "ce", "cf",
      "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",
      "d8", "d9", "da", "db", "dc", "dd", "de", "df",
      "e0", "e1", "e2", "e3", "e4", "e5", "e6", "e7",
      "e8", "e9", "ea", "eb", "ec", "ed", "ee", "ef",
      "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
      "f8", "f9", "fa", "fb", "fc", "fd", "fe", "ff");

   function To_Hex (N : in Stream_Element_Array) return String is

      Result : String (1 .. Positive (2 * (N'Last - N'First + 1)));
      I_s : Positive := Result'First;

   begin -- To_Hex
      for I in N'Range loop
         Result (I_s .. I_s + 1) := Hex_Lookup (N (I));
         I_s := I_s + 2;
      end loop; -- I in N'Range
      return Result;
   end To_Hex;

   function String_To_Hex (N : in String) return String is

      Result : String (1 .. Positive (2 * (N'Last - N'First + 1)));
      I_s : Positive := Result'First;

   begin -- String_To_Hex
      for I in N'Range loop
         Result (I_s .. I_s + 1) := Hex_Lookup (Character'Pos (N (I)));
         I_s := I_s + 2;
      end loop; -- I in N'Range
      return Result;
   end String_To_Hex;

   function To_Stream (S : String) return Stream_Element_Array is

      Result : Stream_Element_Array (Stream_Element_Offset'First ..
                                       Stream_Element_Offset'First +
                                         Stream_Element_Offset
                                       (S'Last - S'First) / 2)
        := (others => 0);
      I : Integer := S'Last;
      J : Stream_Element_Offset := Result'Last;
      Last : Positive;

   begin -- To_Stream
      while I >= S'First loop
         if I > S'First then
            S_IO.Get ("16#" & S (I - 1 .. I) & '#', Result (J), Last);
            if Last /= 6 then
               raise Data_Error with "Bad Hex number " & S (I - 1 .. I);
            end if; -- Last /= 6;
         else
            S_IO.Get ("16#" & S (S'First) & '#', Result (Result'First), Last);
            if Last /= 5 then
               raise Data_Error with "Bad Hex digit " & S (I);
            end if; -- Last /= 5;
         end if; -- I > S'First
         I := I - 2;
         if J > Result'First then
            J := J - 1;
         end if; -- J > Result'First
      end loop; --  I >= S'First
      return Result;
   end To_Stream;

   function Hex_To_String (S : in String) return String is

      Result : String (1 .. S'Length / 2);
      I : Natural := S'Last;
      J : Natural := Result'Last;
      S_E : Stream_Element;
      Last : Positive;

   begin -- Hex_To_String
      if S'Length mod 2 /= 0 then
         raise Data_Error with "Odd number of hex digits";
      end if; -- S'Length mod 2 /= 0
      while I >= S'First loop
         S_IO.Get ("16#" & S (I - 1 .. I) & '#', S_E, Last);
         Result (J) := Character'Val (S_E);
         if Last /= 6 then
            raise Data_Error with "Bad Hex number " & S (I - 1 .. I);
         end if; -- Last /= 6;
         I := I - 2;
         J := J - 1;
      end loop; --  I >= S'First
      return Result;
   end Hex_To_String;

begin -- DJH.Hex
   S_IO.Default_Base := 16;
end DJH.Hex;
