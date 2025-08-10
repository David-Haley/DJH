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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers.Ordered_Maps;

generic
   type Data_Sample is range <>;
   type Float_Type is digits <>;

package DJH.Statistics is

	type Data_Stores is Limited Private;

   procedure Clear (Data_Store : in out Data_Stores);
   -- Clears the data accumulators.

   procedure Sample (Data_Store : in out Data_Stores; Data : in Data_Sample);
   -- Adds a sample to the data accummulators.

   function Count (Data_Store : in Data_Stores) return Natural;
   -- Returns the count of Samples that have been added to the data
   -- accummulators.

   function Mean (Data_Store : in Data_Stores) return Float_Type'Base;
   -- Returns the Mean or average for Samples that have been added to the data
   -- accummulators.
   
   function Standard_Deviation (Data_Store : in Data_Stores;
     Population : in Boolean := False) return Float_Type'Base;
   -- Returns the Standard Deviation for samples that have been added to the
   -- data accummulators. Set Population to True if the data represents the
   -- whole population rather that a sample of the population.

   function Variance (Data_Store : in Data_Stores;
     Population : in Boolean := False) return Float_Type'Base;
   -- Returns the Variance for samples that have been added to the data
   -- accummulators. Set Population to True if the data represents the whole
   -- population rather that a sample of the population.

   function Minimum (Data_Store : in Data_Stores) return Data_Sample;
   -- Returns the minimum value of the samples that have been added to the data
   -- accumulators.

   function Maximum (Data_Store : in Data_Stores) return Data_Sample;
   -- Returns the maximum value of the samples that have been added to the data
   -- accumulators.

   function Frequency (Data_Store : in Data_Stores;
                       Data : in Data_Sample) return Natural;
   -- Returns the number of occurrances of of samples entered of the specified
   -- Value in the data. return values can be used to build a histogram.

   Insufficient_Samples : Exception;
   -- Raised if Mean or Variance is called when Count is less than two.
   
   Private
   
	package Occurance_Counts is new
	  Ada.Containers.Ordered_Maps (Data_Sample, Positive);
	  -- Note only values that have occured at least once are stored , hence
	  -- Positive.
	  
	use Occurance_Counts;

	type Data_Stores is record
		Sum, Square_Sum : Float_Type'Base := 0.0;
      Sample_Count : Natural := 0;
		Occurance_Count : Occurance_Counts.Map;
   end Record; -- Data_Stores
   
end DJH.Statistics;
