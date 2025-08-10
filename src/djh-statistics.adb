-- Basic statistics, calculates Mean and Variance.
-- Author    : David Haley
-- Created   : 09/10/2017
-- Last Edit : 09/08/2025

-- 20250809 : Corrected the frequency, the first isntance of a value should
-- count as 1 not 0, Corrected Variance and added Standard_Deviation. Results
-- Made generic floats.
-- 20250806 : Moved to DJH and Data_Stores added;
-- 12/10/2017: Minimum and Maximum functions added
-- 13/10/2017: use My_Float, Frequency added;

with Ada.Numerics.Generic_Elementary_Functions;

package body DJH.Statistics is

	subtype This_Float is Float_Type'Base;

   package My_Numerics is new
      Ada.Numerics.Generic_Elementary_Functions (This_Float);
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
      Data_Store.Sum := @ + This_Float (Data);
      Data_Store.Square_Sum := @ + This_Float (Data ** 2);
      Data_Store.Sample_Count := @ + 1;
      if Contains (Data_Store.Occurance_Count, Data) then
			Data_Store.Occurance_Count (Data) := 
			  Data_Store.Occurance_Count (Data) + 1;
		else
			Insert (Data_Store.Occurance_Count, Data, 1);
		end if; -- Contains (Data_Store.Occurance_Count, Data)
   end Sample;

   function Count (Data_Store : in Data_Stores) return Natural is
      -- Returns the count of Samples that have been added to the data
      -- accummulators.

   begin -- Count
      return Data_Store.Sample_Count;
   end Count;

   function Mean (Data_Store : in Data_Stores) return This_Float is
      -- Returns the Mean or average for Samples that have been added to the
      -- data accummulators.

   begin -- Mean
      if Data_Store.Sample_Count < 2 then
         raise Insufficient_Samples with "mean";
      else
         return Data_Store.Sum / This_Float (Data_Store.Sample_Count);
      end if; -- Data_Store.Sample_Count < 2
   end Mean;
   
   function Standard_Deviation (Data_Store : in Data_Stores;
     Population : in Boolean := False) return This_Float is
      -- Returns the Standard Deviation for samples that have been added to the
      -- data accummulators. Set Population to True if the data represents the
      -- whole population rather that a sample of the population.
      
   begin -- Standard_Deviation
      if Data_Store.Sample_Count < 2 then
         raise Insufficient_Samples with "Standard_Deviation";
      else
         return sqrt (Variance (Data_Store, Population));
      end if; -- Data_Store.Sample_Count
   end Standard_Deviation;

   function Variance (Data_Store : in Data_Stores;
     Population : in Boolean := False) return This_Float is
      -- Returns the Variance for samples that have been added to the data
      -- accummulators. Set Population to True if the data represents the whole
      -- population rather that a sample of the population.

   begin -- Variance
      if Data_Store.Sample_Count < 2 then
         raise Insufficient_Samples with "Variance";
      else
			if Population then
				return (Data_Store.Square_Sum - Data_Store.Sum ** 2 /
				  This_Float (Data_Store.Sample_Count)) /
				  This_Float (Data_Store.Sample_Count);
			else
				return (Data_Store.Square_Sum - Data_Store.Sum ** 2 /
				  This_Float (Data_Store.Sample_Count)) /
				  This_Float (Data_Store.Sample_Count - 1);
         end if; -- Population
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
