-- Test for Basic statistics, calculates Mean and Variance.
-- Author    : David Haley
-- Created   : 09/08/2025
-- Last Edit : 09/08/2025

with Ada.Text_IO; USE Ada.Text_IO;
with DJH.Statistics;

procedure Test_Statistics is
-- Based on Wikipedia example.
-- en.wikipedia/wiki/Standard_deviation

	subtype Marks is Positive range 2 .. 9;
	type My_Real is digits 15;

   package Stats is new DJH.Statistics (Marks, My_Real);
	use Stats;

Data_Store : Data_Stores;

begin -- Test_Statistics
	Sample (Data_Store, 2);
	Sample (Data_Store, 4);
	Sample (Data_Store, 4);
	Sample (Data_Store, 4);
	Sample (Data_Store, 5);
	Sample (Data_Store, 5);
	Sample (Data_Store, 7);
	Sample (Data_Store, 9);
	Put_Line ("Numner of samples:" & Stats.Count (Data_Store)'Img);
	Put_Line ("Minimum:" & Minimum (Data_Store)'Img);
	Put_Line ("Maximum:" & Maximum (Data_Store)'Img);
	Put_Line ("Mean:" & Mean (Data_Store)'Img);
	Put_Line ("Standard deviatopn (whole population):" &
	  Standard_Deviation (Data_Store, True)'Img);
	Put_Line ("Standard deviatopn (sample of population):" &
	  Standard_Deviation (Data_Store)'Img);
	Put_Line ("Variance (whole population):" & Variance (Data_Store, True)'Img);
	Put_Line ("Variance (sample of population):" & Variance (Data_Store)'Img);
	Put_Line ("Frequency Distribution:");
	for I in Marks loop
		Put_Line ('[' & I'Img &']' & Frequency (Data_Store, I)'Img);
	end loop; -- I in Marks
end Test_Statistics;
