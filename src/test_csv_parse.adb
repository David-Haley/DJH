-- Author    : David Haley
-- Created   : 14/07/2020
-- Last Edit : 15/05/2022
-- 20231119 : Teating of No_Header and Row_Number included;
-- 20220515 : Updated to match changed file interface;

with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Parse_CSV;

procedure Test_Csv_Parse is

   type My_Labels is (Tom, Dick, Harry);

   package With_Header is new DJH.Parse_CSV (My_Labels);

   type No_Labels is (Column_1, Column_2, Column_3, Column_4);

   package Without_Header is new DJH.Parse_CSV (No_Labels);

begin -- Test_Csv_Parse
   begin -- Exception_Block
   With_Header.Read_Header (Argument (1));
   while With_Header.Next_Row loop
      Put_Line ("Row:" & With_Header.Row_Number'Img &
                  " Tom: """ & With_Header.Get_Value (Tom) & '"' &
                  " Dick: """ & With_Header.Get_Value (Dick) & '"' &
                  " Harry: """ & With_Header.Get_Value (Harry) & '"');
   end loop; -- Next_Row
   exception
      When E: others=>
         Put_Line (Exception_Message (E));
   end; -- Exception_Block
   With_Header.Close_CSV;
   New_Line;
   Put_Line ("Re-read file without treating the first row as a header");
   New_Line;
   Without_Header.No_Header (Argument (1));
   while Without_Header.Next_Row loop
      Put_Line ("Row:" & Without_Header.Row_Number'Img & ": " &
                  Column_1'Img & " """ & Without_Header.Get_Value (Column_1) &
                  """ " &
                  Column_2'Img & " """ & Without_Header.Get_Value (Column_2) &
                  """ " &
                  Column_3'Img & " """ & Without_Header.Get_Value (Column_3) &
                  """ " &
                  Column_4'Img & " """ & Without_Header.Get_Value (Column_4) &
               '"');
   end loop; -- Next_Row
   Without_Header.Close_CSV;
end Test_Csv_Parse;
