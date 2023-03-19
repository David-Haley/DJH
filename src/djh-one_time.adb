with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Numerics.Discrete_Random;

-- Author    : David Haley
-- Created   : 07/12/2020
-- Last Edit : 12/01/2021
-- 20210112: Calculation of OTP_Index corrected to ensure full lenth of OTP
-- used.

package body DJH.One_Time is

   subtype Code_Chars is Character with Static_Predicate =>
     Code_Chars in ' ' .. '~' | No_Break_Space .. LC_Y_Diaeresis;
   -- Excludes control codes and reserved etc.

   subtype Ch_Indices is Natural range 0 .. 255;

   package To_Code_Chars is new
     Ada.Containers.Ordered_Maps (Ch_Indices, Code_Chars);
   use To_Code_Chars;

   package To_Ch_Indices is new
     Ada.Containers.Ordered_Maps (Code_Chars, Ch_Indices);
   use To_Ch_Indices;

   To_Code_Char : To_Code_Chars.Map;
   To_Ch_Index : To_Ch_Indices.Map;
   Map_Modulus : Ch_Indices;

   Procedure Build_Maps (To_Code_Char : out To_Code_Chars.Map;
                         To_Ch_Index : out To_Ch_Indices.Map;
                         Map_Modulus : out Ch_Indices) is

      -- Build maps produces two maps, one to go from a character to an Index
      -- and a second to go from an index to a character. Only printable
      -- characters are included resulting in two disjoint blocks of characters.
      -- Ada 2012 does not support Pred, Succ or iteration over on scalar types
      -- with static predicates. Map_Modulus is needed as the divisor in for
      -- the mod operator used to perform rotation within the map.

      Ch_Index : Ch_Indices := 0;

   begin -- Build_Maps
      -- lookup from number to character
      for I in Ch_Indices loop
         if Character'Val (I) in Code_Chars then
            Include (To_Code_Char, Ch_Index, Code_Chars'Val (I));
            --       map           Index     Character
            Ch_Index := Ch_Index + 1;
         end if; -- Character'Pos (I) in Code_Chars
      end loop; -- I in Ch_Indices
      -- lookup from character to number
      for I in Iterate (To_Code_Char) loop
         Include (To_Ch_Index, Element (I), Key (I));
         --       map          Character    Index
      end loop; -- I in Iterate (To_Code_Char)
      Map_Modulus := Ch_Indices (Length (To_Code_Char));
   end Build_Maps;

   function Generate (Pad_Length : in Positive) return String is

      -- Returns an OTP as string of specified length.

      subtype Ch_Range is Ch_Indices range 0 .. Map_Modulus - 1;

      package OTP_Random is new
        Ada.Numerics.Discrete_Random (Ch_Range);
      use OTP_Random;

      Gen : Generator;
      Result : String (1 .. Pad_Length);


   begin -- Generate
      Reset (Gen);
      for I in Positive range 1 .. Pad_Length loop
         Result (I) := To_Code_Char (Random (Gen));
      end loop; -- I in Positive range 1 .. Pad_Length)
      return Result;
   end Generate;

   function Encode (Plain_Text : in String;
                    OTP_Text : in String) return String is

      -- Encodes text from Plain_Text using OTP_Text and coded text is returned
      -- to Code_File. If the OTP_Text is shorter than Plain_Text the OTP_Text
      -- will be reused.

      Plain_Index, OTP_Index : Ch_Indices;
      Coded_Text : String (1 .. Plain_Text'Length);

   begin -- Encode
      for I in Positive range 1 .. Plain_Text'Length loop
         if Plain_Text (I) in Code_Chars then
            Plain_Index := To_Ch_Index (Plain_Text (I));
            OTP_Index :=
              To_Ch_Index (OTP_Text ((I - 1) mod (OTP_Text'Length) + 1));
            Coded_Text (I) :=
              To_Code_Char ((Plain_Index + OTP_Index) mod Map_Modulus);
            -- encoded by stepping forward OTP_Index characters
         else
            Coded_Text (I) := Plain_Text (I);
         end if; -- Plain_Text (I) in Code_Chars
      end loop; -- I in Positive range 1 .. Plain_Text'Length
      return Coded_Text;
   end Encode;

   function Decode (Coded_Text : in String;
                    OTP_Text : in String) return String is

      -- Decodes text from Code_Text using OTP_Text and plain text is returned.
      -- If the OTP_Text is shorter than Plain_Text the OTP_Text will be
      -- reused.

      Plain_Text : String (1 .. Coded_Text'Length);
      Code_Index, OTP_Index : Ch_Indices;

   begin -- Decode
      for I in Positive range 1 .. Coded_Text'Length loop
         if Coded_Text (I) in Code_Chars then
            Code_Index := To_Ch_Index (Coded_Text (I));
            OTP_Index :=
              To_Ch_Index (OTP_Text ((I - 1) mod (OTP_Text'Length) + 1));
            Plain_Text (I) :=
              To_Code_Char ((Code_Index - OTP_Index) mod Map_Modulus);
            -- decoded by stepping back OTP_Index characters
         else
            Plain_Text (I) := Coded_Text (I);
         end if; -- Coded_Text (I) in Code_Chars
      end loop; -- I in Positive range 1 .. Coded_Text'Length
      return Plain_Text;
   end Decode;

begin -- DJH.One_Time
   Build_Maps (To_Code_Char, To_Ch_Index, Map_Modulus);
end DJH.One_Time;
