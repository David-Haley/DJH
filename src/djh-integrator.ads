-- Author    : David Haley
-- Created   : 27/05/2020
-- Last Edit : 27/05/2020
-- Simulation of analogue computer integrator

generic

   type Integrator_Float is digits <>;
   Time_Increment : Integrator_Float;
   Initial_State : Integrator_Float := 0.0;

package DJH.Integrator is

   procedure Tick;

   procedure Input (X : Integrator_Float);

   function Output return Integrator_Float;

end DJH.Integrator;
