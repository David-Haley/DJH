--  This is a test program for for the JSON_Configuration package.

--  Author    : David Haley
--  Created   : 11/06/2026
--  Last Edit : 11/06/2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.JSON_Configuration;

procedure Test_JSON_Configuration is

   type Parameters is (Parameter_1, Parameter_2, Secret_Parameter, Also_Secret,
     No_Secret, Nothing);

   function To_Encrypt (Parameter : in Parameters) return Boolean is
      (Parameter = Secret_Parameter or Parameter = Also_Secret or
        Parameter = No_Secret);

   File_Name : constant String := "Configuration_Test.json";

   package My_Parameters is new
    DJH.JSON_Configuration (Parameters, File_Name, To_Encrypt);
   use My_Parameters;

   String_1 : constant String := "QWERTYUIOPqwertyuiop";
   String_2 : constant String := "asdfghjklASDFGHJKL";
   Secret_String : constant String := "ZXCVBNM0123456789zxcvbnm";
   Also_Secret_String : constant String := "qazwsxQAZWSX";

   function Check return Boolean is

      Empty_1, Empty_2 : Boolean := False;

   begin -- Check
      if Configuration_File_Exists then
         Put_Line (File_Name & " found");
      else
         Put_Line (File_Name & " not found");
      end if; -- Configuration_File_Exists
      begin -- No_Secret exception block
         Put (Get_Value (No_Secret));
      exception
         when E: JSON_Configuration_Error =>
            Put_Line ("No_Secret: " & Exception_Message (E));
            Empty_1 := True;
      end; -- No_Secret exception block
      begin -- Nothing exeeption block
         Put (Get_Value (Nothing));
      exception
         when E: JSON_Configuration_Error =>
            Put_Line ("Nothing: " & Exception_Message (E));
            Empty_2 := True;
      end; -- Nothing exeeption block
      return String_1 = Get_Value (Parameter_1) and
      String_2 = Get_Value (Parameter_2) and
      Secret_String = Get_Value (Secret_Parameter) and
      Also_Secret_String = Get_Value (Also_Secret) and Empty_1 and Empty_2;
   end Check;
   
begin -- Test_JSON_Configuration
   Put_Line ("Test_JSON_Configuration version 20260611");
   Set_Value (Parameter_1, String_1);
   Set_Value (Parameter_2, String_2);
   Set_Value (Secret_Parameter, Secret_String);
   Set_Value (Also_Secret, Also_Secret_String);
   if Check then
      Put_Line ("Saved values match");
   else
      Put_Line ("Saved values do not match");
   end if; -- Check
   Put_Line ("Writing " & File_Name);
   Write_Configuration;
   Set_Value (Parameter_1, "1");
   Set_Value (Parameter_2, "2");
   Set_Value (Secret_Parameter, "S");
   Set_Value (Also_Secret, "s");
   Set_Value (No_Secret, "****");
   Set_Value (Nothing, "####");
   Put_Line ("Reading " & File_Name);
   Read_Configuration;
   if Check then
      Put_Line ("Read back values match");
   else
      Put_Line ("Read back values do not match");
   end if; -- Check
end Test_JSON_Configuration;