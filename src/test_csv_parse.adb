-- Author    : David Haley
-- Created   : 14/07/2020
-- Last Edit : 15/05/2022
-- 20220515 : Updated to match changed file interface;

with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with DJH.Parse_CSV;

procedure Test_Csv_Parse is

   type My_Labels is (Tom, Dick, Harry);

   package Parser is new DJH.Parse_CSV (My_Labels);
   use Parser;

   Row : Positive := 1;

begin
   Read_Header (Argument (1));
   while Next_Row loop
      Put_Line ("Row:" & Positive'Image (Row) &
                  " Tom: """ & Get_Value (Tom) & '"' &
                  " Dick: """ & Get_Value (Dick) & '"' &
                  " Harry: """ & Get_Value (Harry) & '"');
      Row := Row + 1;
   end loop; -- Next_Row
   Close_CSV;
end Test_Csv_Parse;
