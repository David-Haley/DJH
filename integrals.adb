-- Author    : David Haley
-- Created   : 28/05/2020
-- Last Edit : 28/05/2020
-- Trst integrator functionality using a pendlum

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with DJH.Integrator;

procedure Integrals is

   type Reals is digits 15;

   Time_Delta : constant Reals := 0.0001;

   package Sin_Int is new DJH.Integrator (Reals, Time_Delta);

   package Real_IO is new Ada.Text_IO.Float_IO (Reals);
   use Real_IO;

   package Real_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Reals);
   use Real_Functions;

   Simulation_Step : Natural := 0;
   Simulation_Time : Reals;
   Output_File : File_Type;

begin -- Integrals
   Create (Output_File, Out_File, "Integrals.txt");
   loop
      Simulation_Time := Reals (Simulation_Step) * Time_Delta;
      Sin_Int.Input (Sin (Simulation_Time));
      Sin_Int.Tick;
      exit when Simulation_Time > Pi;
      Simulation_Step := Simulation_Step + 1;
   end loop; -- Simulation_Step in Natural range 0 .. 1000
   Put_line (Output_File, "Sin (t) [0 .. Pi]:" &
               Reals'Image (Sin_Int.Output));
   Close (Output_File);
end Integrals;
