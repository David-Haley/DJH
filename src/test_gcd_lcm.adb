-- Author    : David Haley
-- Created   : 12/01/2024
-- Last Edit : 12/01/2024
-- Algorithm derived from Wikipedia (GCD) and Rsetta Code

with Ada.Text_IO; use Ada.Text_IO;
with DJH.Gcd_Lcm;

procedure Test_Gcd_Lcm is

   package Int_Gcd_Lcm is new DJH.Gcd_Lcm (Integer);
   use Int_Gcd_Lcm;

begin -- Test_Gcd_Lcm
   Put_Line ("LCM of 12, 18 is" & Integer'Image (Lcm (12, 18)));
   Put_Line ("LCM of -6, 14 is" & Integer'Image (Lcm (-6, 14)));
   Put_Line ("LCM of 35, 0 is" & Integer'Image (Lcm (35, 0)));
end Test_Gcd_Lcm;
