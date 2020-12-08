-- Author    : David Haley
-- Created   : 07/12/2020
-- Last Edit : 07/12/2020

with Ada.Text_IO; use Ada.Text_IO;
with DJH.One_Time; use DJH.One_Time;

procedure Test_One_Time is

   Text_1 : constant String := "abcdefghijklmnopqrstuvwxyz";
   Text_2 : constant String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   OTP_Text : constant String := "0123456789";

begin
   Put_Line ("Random Key: """ & Generate (10) & '"');
   New_Line;
   Put_Line ("Key: """ & OTP_Text & '"');
   New_Line;
   Put_Line ("Plain Text: """ & Text_1 & '"');
   Put_Line ("Coded Text: """ & Encode (Text_1, OTP_Text) & '"');
   Put_Line ("Check Text: """ &
               Decode (Encode (Text_1, OTP_Text), OTP_Text) & '"');
   New_Line;
   Put_Line ("Plain Text: """ & Text_2 & '"');
   Put_Line ("Coded Text: """ & Encode (Text_2, OTP_Text) & '"');
   Put_Line ("Check Text: """ &
               Decode (Encode (Text_2, OTP_Text), OTP_Text) & '"');
end Test_One_Time;
