with Ada.Text_IO; use Ada.Text_IO;
with DJH.Modular_Inverse;

procedure Test_Modular_Inverse is

   package MI is new DJH.Modular_Inverse (Integer);

begin -- Test_Modular_Inverse
  -- This will output -48 (which is correct)
  Put_Line (MI.Inverse_Mod (42,2017)'img);
  -- The further line will raise an exception since the GCD will not be 1
  Put_Line (MI.Inverse_Mod (42,77)'img);
  exception when others => Put_Line ("The inverse doesn't exist.");
end Test_Modular_Inverse;
