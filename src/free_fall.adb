-- Author    : David Haley
-- Created   : 09/06/2020
-- Last Edit : 09/06/2020
-- Test integrator for free fall

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with DJH.Integrator;

procedure Free_Fall is

   type Reals is digits 15;
   g : Reals := 9.81;

   Time_Delta : constant Reals := 1.0e-6;

   package Velocity is new DJH.Integrator (Reals, Time_Delta);

   package Displacement is new DJH.Integrator (Reals, Time_Delta);

   package Real_IO is new Ada.Text_IO.Float_IO (Reals);
   use Real_IO;

   Output_File : File_Type;

begin -- Free_Fall
   Create (Output_File, Out_File, "Free_Fall.txt");
   for Simulation_Step in Natural range 0 .. 1000000 loop
      Velocity.Input (g);
      Displacement.Input (Velocity.Output);
      Velocity.Tick;
      Displacement.Tick;
   end loop; -- Simulation_Step in Natural range 0 .. 1000
   Put_line (Output_File, "Velocity:" &
               Reals'Image (Velocity.Output));
   Put_line (Output_File, "Displacement:" &
               Reals'Image (Displacement.Output));
   Close (Output_File);
end Free_Fall;
