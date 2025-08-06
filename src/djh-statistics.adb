-- Basic statistics, calculates Mean and Variance.
-- Author    : David Haley
-- Created   : 09/10/2017
-- Last Edit : 06/08/2025

-- 20250806 : Moved to DJH and Data_Stores added;
-- 12/10/2017: Minimum and Maximum functions added
-- 13/10/2017: use My_Float, Frequency added;

with Ada.Numerics.Generic_Elementary_Functions;

package body DJH.Statistics is

   package My_Numerics is new
      Ada.Numerics.Generic_Elementary_Functions (My_Float);
   use My_Numerics;

   procedure Clear (Data_Store : in out Data_Stores) is
      -- Clears the data accumulators.

   begin -- Clear
      Clear (Data_Store.Occurance_Count);
      Data_Store.Sample_Count := 0;
      Data_Store.Sum := 0.0;
      Data_Store.Square_Sum := 0.0;
   end Clear;


   procedure Sample (Data_Store : in out Data_Stores;
                     Data : in Data_Sample) is
      -- Adds a sample to the data accummulators.

   begin -- Sample_Data
      Data_Store.Sum := @ + My_Float(Data);
      Data_Store.Square_Sum := @ + My_Float (Data) ** 2;
      Data_Store.Sample_Count := @ + 1;
      if Contains (Data_Store.Occurance_Count, Data) then
			Data_Store.Occurance_Count (Data) := 
			  Data_Store.Occurance_Count (Data) + 1;
		else
			Insert (Data_Store.Occurance_Count, Data, 0);
		end if; -- Contains (Data_Store.Occurance_Count, Data)
   end Sample;

   function Count (Data_Store : in Data_Stores) return Natural is
      -- Returns the count of Samples that have been added to the data
      -- accummulators.

   begin -- Count
      return Data_Store.Sample_Count;
   end Count;

   function Mean (Data_Store : in Data_Stores) return My_Float is
      -- Returns the Mean or average for Samples that have been added to the
      -- data accummulators.

   begin -- Mean
      if Data_Store.Sample_Count < 2 then
         raise Insufficient_Samples with "mean";
      else
         return Data_Store.Sum / My_Float (Data_Store.Sample_Count);
      end if; -- Data_Store.Sample_Count < 2
   end Mean;

   function Variance (Data_Store : in Data_Stores) return My_Float is
      -- Returns the Variance or standard deviation for Samples that have been
      -- added to the data accummulators.

   begin -- Variance
      if Data_Store.Sample_Count < 2 then
         raise Insufficient_Samples with "Variance";
      else
         return sqrt ((Data_Store.Square_Sum - Data_Store.Sum ** 2 /
           My_Float (Data_Store.Sample_Count)) /
           My_Float (Data_Store.Sample_Count - 1));
      end if; -- Data_Store.Sample_Count < 2
   end Variance;

   function Minimum (Data_Store : in Data_Stores) return Data_Sample is
      -- Returns the minimum value of the samples that have been added to the
      -- data accumulators.

   begin -- Minimum
		if Data_Store.Sample_Count < 1 then
         raise Insufficient_Samples with "Minimum";
      end if; -- Data_Store.Sample_Count < 1
      return First_Key (Data_Store.Occurance_Count);
   end Minimum;

   function Maximum (Data_Store : in Data_Stores) return Data_Sample is
      -- Returns the maximum value of the samples that have been added to the
      -- data accumulators.
   begin -- Maximum
		if Data_Store.Sample_Count < 1 then
         raise Insufficient_Samples with "Maximum";
      end if; -- Data_Store.Sample_Count < 1
      return Last_Key (Data_Store.Occurance_Count);
   end Maximum;

   function Frequency (Data_Store : in Data_Stores;
                       Data : in Data_Sample) return Natural is
   -- Returns the number of occurrances of samples entered with the specified
   -- value Data. return values can be used to build a histogram.

   begin -- Frequency
		if Contains (Data_Store.Occurance_Count, Data) then
         return Data_Store.Occurance_Count (Data);
      else
			return 0;
		end if; -- Contains (Data_Store.Occurance_Count, Data)
   end Frequency;

end DJH.Statistics;
