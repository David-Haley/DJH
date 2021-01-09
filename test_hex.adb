-- Author    : David Haley
-- Created   : 16/06/2020
-- Last Edit : 07/12/2020

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with DJH.Hex; use DJH.Hex;

procedure Test_Hex is

   Zero_Set : Character_Set := To_Set ('0');
   Space_Set : Character_Set := To_Set (" ");
   Hex_String : String (1 .. 4);
   Stream : Stream_Element_Array (Stream_Element_Offset'First ..
                                    Stream_Element_Offset'First + 1);
   Output_File : File_Type;

   package S_IO is new Ada.Text_IO.Modular_IO (Stream_Element);

   Digit_Text : constant String := "0123456789";
   Lower_Text : constant String := "abcdefghijklmnopqrstuvwxyz";
   Upper_Text : constant String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

begin
   Create (Output_File, Out_File, "Hex_Test.txt");
   for I in Stream_Element loop
      for J in Stream_Element loop
         Stream (Stream'First) := I;
         Stream (Stream'Last) := J;
         Hex_String := To_Hex (Stream);
         Put (Output_File, Hex_String & ' ' &
                Trim (Hex_String, Zero_Set, Space_Set));
         if I = 0 then
            if J > 0 then
               if J /= To_Stream (Trim (Hex_String, Zero_Set, Space_Set))
                 (Stream_Element_Offset'First) then

                  Put (Output_File, " Error ");
                  S_IO.Put (Output_File, To_Stream (Trim (Hex_String, Zero_Set,
                            Space_Set)) (Stream_Element_Offset'First), 2, 16);
               end if; --  J /=  ...
            end if; -- J > 0
         else
            if Stream /= To_Stream (Trim (Hex_String, Zero_Set, Space_Set)) then
               Put (Output_File, " Error");
            end if; -- Stream (Stream'Last) /= ...
         end if; -- I = 0
         New_Line (Output_File);
      end loop; -- J in Stream_Element
   end loop; -- I in Stream_Element
   Put_Line (Output_File, "String tests");
   Put_Line (Output_File, Digit_Text);
   Put_Line (Output_File, String_To_Hex (Digit_Text));
   Put_Line (Output_File, Hex_To_String (String_To_Hex (Digit_Text)));
   New_Line (Output_File);
   Put_Line (Output_File, Lower_Text);
   Put_Line (Output_File, String_To_Hex (Lower_Text));
   Put_Line (Output_File, Hex_To_String (String_To_Hex (Lower_Text)));
   New_Line (Output_File);
   Put_Line (Output_File, Upper_Text);
   Put_Line (Output_File, String_To_Hex (Upper_Text));
   Put_Line (Output_File, Hex_To_String (String_To_Hex (Upper_Text)));
   Close (Output_File);
end Test_Hex;
