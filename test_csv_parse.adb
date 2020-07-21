-- Author    : David Haley
-- Created   : 14/07/2020
-- Last Edit : 14/07/2020

with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with DJH.Parse_CSV;

procedure Test_Csv_Parse is

   type My_Labels is (Tom, Dick, Harry);

   package Parser is new DJH.Parse_CSV (My_Labels);
   use Parser;

   Row : Positive := 1;

begin
   Open (Input_File, In_File, Argument (1));
   Read_Header;
   while Next_Row loop
      Put_Line ("Row:" & Positive'Image (Row) &
                  " Tom: """ & Get_Value (Tom) & '"' &
                  " Dick: """ & Get_Value (Dick) & '"' &
                  " Harry: """ & Get_Value (Harry) & '"');
      Row := Row + 1;
   end loop; -- Next_Row
end Test_Csv_Parse;
