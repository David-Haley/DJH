-- Author:    David Haley
-- Created:   14/12/2020
-- Last Edit: 02/01/2021

with Ada.Text_IO; use Ada.Text_IO;
with DJH.Chinese_Remainder;

procedure Test_Chinese_Remainder is

   subtype Indices is Positive range 1 .. 3;
   subtype Numbers is Natural;

   package Chinese is new DJH.Chinese_Remainder (Indices, Numbers);
   use Chinese;

   Test : Rem_Mod_Arrays := ((2, 3), (3, 5), (2, 7));
   R, N : Numbers;

begin -- Test_Chinese_Remainder
   Solve (Test, R, N);
   Put_Line ("R:" & Numbers'Image (R) & " N:" & Numbers'Image (N));
   for I in Indices loop
      Put_Line ("R mod" & Test (I).N'Img & " =" &
                  Positive_Type'Image (R mod Test (I).N) &
                  " Expected:" & Test (I).A'Img);
   end loop; -- I in Indices
   Put_Line ("Also");
   for I in Indices loop
      Put_Line ("(R + N) mod" & Test (I).N'Img & " =" &
                  Positive_Type'Image ((R + N) mod Test (I).N) &
                  " Expected:" & Test (I).A'Img);
   end loop; -- I in Indices
end Test_Chinese_Remainder;
