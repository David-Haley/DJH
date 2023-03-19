-- Author    : David Haley
-- Created   : 27/05/2020
-- Last Edit : 09/06/2020
-- Trst integrator functionality using a pendlum

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with DJH.Integrator;

procedure Test_Analogue_Simulation is

   type Reals is digits 15;

   Model_Time : constant Reals := 10.0;
   Time_Delta : constant Reals := 1.0e-6;
   Last_Step : constant Natural := Natural (Model_Time / Time_Delta);
   g : constant Reals := 9.80665;
   L : constant Reals := 1.0;

   package Acceleration is new DJH.Integrator (Reals, Time_Delta);
   package Velocity is new DJH.Integrator (Reals, Time_Delta,
                                           10.0 / 180.0 * Pi);
   -- initial displacement 10 degrees

   package Real_IO is new Ada.Text_IO.Float_IO (Reals);
   use Real_IO;

   package Real_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Reals);
   use Real_Functions;

   Output_File : File_Type;

begin -- Test_Analogue_Simulation
   Create (Output_File, Out_File, "Simulation.txt");
   for Step in Natural range 0 .. Last_Step loop
      Acceleration.Input (- g / L * Sin (Velocity.Output));
      Velocity.Input (Acceleration.Output);
      if Step mod 1000 = 0 then
         Put (Output_File, Reals (Step) * Time_Delta, 2, 5, 0);
         Put (Output_File, Acceleration.Output, 3, 5, 0);
         Put (Output_File, Velocity.Output, 3, 5, 0);
         New_Line (Output_File);
      end if; -- Step mod 1000 = 0
      Acceleration.Tick;
      Velocity.Tick;
   end loop; -- Step in Natural range 0 .. Last_Step loop
   Close (Output_File);
end Test_Analogue_Simulation;
