-- Author    : David Haley
-- Created   : 02/04/2019
-- Last Edit : 08/08/2019
-- Test program for the Vera package

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with DJH.Vera; use DJH.Vera;

procedure Test_Vera is

   Test_Requested : Character := ' ';
   Device : Devices;

begin -- Test_Vera
   Device := Devices'Value (Argument (2));
   loop -- one test
      Put_Line ("0: End Tests");
      Put_line ("1: Device on");
      Put_line ("2: Device off");
      Put ("Test? ");
      Get (Test_Requested);
      case Test_Requested is
      when '0' =>
         exit;
      when '1' =>
         Put_Line (Boolean'Image (Switch_On (Argument (1), Device)));
      when '2' =>
         Put_Line (Boolean'Image (Switch_Off (Argument (1), Device)));
      when others =>
         Put_Line ("Invalid Command");
      end case; -- Test_Requested
   end loop; -- one test
end Test_Vera;
