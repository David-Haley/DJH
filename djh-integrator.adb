-- Author    : David Haley
-- Created   : 27/05/2020
-- Last Edit : 27/05/2020
-- Simulation of analogue computer integrator

package body DJH.Integrator is

   Integral, Current_Input : Integrator_Float;
   Previous_Input : Integrator_Float := 0.0;

   procedure Tick is

   begin -- Tick
      Integral := Integral +
        Time_Increment * (Current_Input + Previous_Input) / 2.0;
      Previous_Input := Current_Input;
   end Tick;

   procedure Input (X : Integrator_Float) is

   begin -- Input
      Current_Input := X;
   end Input;

   function Output return Integrator_Float is

   begin -- Output
      return Integral;
   end Output;

begin -- DJH.Integrator
   Integral := Initial_State;
end DJH.Integrator;
