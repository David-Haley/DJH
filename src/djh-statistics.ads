-- Basic statistics, calculates Mean and Variance.
-- Author    : David Haley
-- Created   : 09/10/2017
-- Last Edit : 06/08/2025

-- 20250806 : Moved to DJH and Data_Stores added;
-- 12/10/2017: Minimum and Maximum functions added
-- 13/10/2017: use My_Float, Frequency added;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers.Ordered_Maps;

generic
   type Data_Sample is range <>;

package DJH.Statistics is

	type Data_Stores is Limited Private;

   type My_Float is digits 15;

   procedure Clear (Data_Store : in out Data_Stores);
   -- Clears the data accumulators.

   procedure Sample (Data_Store : in out Data_Stores; Data : in Data_Sample);
   -- Adds a sample to the data accummulators.

   function Count (Data_Store : in Data_Stores) return Natural;
   -- Returns the count of Samples that have been added to the data
   -- accummulators.

   function Mean (Data_Store : in Data_Stores) return My_Float;
   -- Returns the Mean or average for Samples that have been added to the data
   -- accummulators.

   function Variance (Data_Store : in Data_Stores) return My_Float;
   -- Returns the Variance or standard deviation for Samples that have been
   -- added to the data accummulators.

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
	  Ada.Containers.Ordered_Maps (Data_Sample, Natural);
	  
	use Occurance_Counts;

	type Data_Stores is record
		Sum, Square_Sum : My_Float := 0.0;
      Sample_Count : Natural := 0;
		Occurance_Count : Occurance_Counts.Map;
   end Record; -- Data_Stores
   
end DJH.Statistics;
