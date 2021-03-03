-- This package provides date and Time strings for the current time.
-- Author    : David Haley
-- Created   : 21/10/2017
-- Last Edit : 03/03/2021
-- Unification between the PC and Raspberry Pi versions
-- 20200711 : Date_Only added
-- 20200620 : Date string comment corected from DD/MM/YY to DD/MM/YYYY, Get_Date
-- added.
-- 20200517: Made child of DJH and Time_of_Day added
-- 20191107 : Posix_Times added and associated Time_String added.
-- 20191024: Hours_More_Than_24 switch added to allow accumulated hours more
-- than 23:59:59 to be displayed as hours.

with Ada.Calendar; use Ada.Calendar;

package DJH.Date_and_Time_Strings is

   type Elapsed_String_Types is (Include_Days, Exclude_Days,
                                 Hours_More_Than_24);

   subtype Time_Strings is String (1 .. 8); -- HH:MM:SS

   subtype Date_Strings is String (1 .. 10); -- DD/MM/YYYY

   type Posix_Times is new Integer;

   function Elapsed_Seconds
     (Seconds_Count : Natural;
      Elapsed_String_Type : in Elapsed_String_Types := Exclude_Days)
      return String;

   -- Returns strings as follows based on elapsed time input
   -- HH:MM:SS when Exclude_Days
   -- DDDDD HH:MM:SS when Include_Days
   -- HHHHHHH:MM:SS when Hours_More_Than_24


   -- The following functions are based on the clock time or
   -- Another_Time if specified

   type Date_String_Types is (Year_Only, Full_Date);

   function Date_String (Date_String_Type : in Date_String_Types := Full_Date;
                         Another_Time : in Time := Clock) return String;
   -- returns YYYY when Year_Only
   -- returns DD/MM/YYYY when Full_Date

   function Reverse_Date_String (Another_Time : in Time := Clock) return String;
   -- returns YYYYMMDD

   function Time_String (Another_Time : in Time := Clock) return Time_Strings;
   -- returns HH:MM:SS in twenty four hour mode

   function Time_String (Poxix_Time : in Posix_Times) return String;
   -- returns HH:MM:SS difference from current time
   
   function Time_of_Day (Time_String : in Time_Strings) return Day_Duration;
   -- returns Ada.Calendar.Formatting Day_Duration represented by a Time_String
   -- formatted as HH:MM:SS.

   function Get_Date (Date_String : in Date_Strings;
                      Seconds : Day_Duration := 0.0) return Time;
   -- returns Ada.Calendar.Time represenreg by Date_String formatter as
   -- DD/MM/YYYY

   function Date_Only (Another_Time : in Time := Clock) return Time;

   -- Returns Time with Day_Duration set to zero

end DJH.Date_and_Time_Strings;
