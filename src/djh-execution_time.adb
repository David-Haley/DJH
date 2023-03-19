with Ada.Text_IO; Use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Execution_Time; use Ada.Execution_Time;

package body DJH.Execution_Time is

   -- Provides a one line addition to a program to record its execution time
   -- Author    : David Haley
   -- Created   : 10/12/2021
   -- Last Edit : 15/12/2021
   -- 20211215 : Reduced Resolution to ms and increased the seconds to allow
   -- for 7 digits of seconds

   procedure Put_CPU_Time is

      -- Displays CPU execution time to console

      Elapse_Time : CPU_Time;
      Seconds : Seconds_Count;
      Fraction : Time_Span;
      To_Display : Duration;

      package Duration_IO is new Ada.Text_IO.Fixed_IO (Duration);

   begin -- Put_CPU_Time
      Elapse_Time := Clock;
      Split (Elapse_Time, Seconds, Fraction);
      To_Display := To_Duration (Fraction);
      To_Display := To_Display + Duration (Seconds);
      Put ("CPU time:");
      Duration_IO.Put (To_Display, 7, 3, 0);
      Put_Line (" s");
   end Put_CPU_Time;

end DJH.Execution_Time;
