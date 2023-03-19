-- This package provides date and Time strings for the current time.
-- Author    : David Haley
-- Created   : 21/10/2017
-- Last Edit : 03/03/2021
-- 20210303 : Unification between PC and Raspberry Pi versions
-- 20200711 : Date_Only added
-- 20200620 : Date string comment corected from DD/MM/YY to DD/MM/YYYY, Get_Date
-- added.
-- 20191107 : Posix_Times added and associated Time_String added.
-- 20200517: Made child of DJH and Time_of_Day added
-- 20191024: Hours_More_Than_24 switch added to allow accumulated hours more
-- than 23:59:59 to be displayed as hours.

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with  Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body DJH.Date_and_Time_Strings is

   function Digit_Out (N : in Natural; Base : in Natural := 10)
                       return Character is

   begin
      return Character'Val (N mod Base + Character'Pos ('0'));
   end Digit_Out;

   function Elapsed_Seconds
     (Seconds_Count : Natural;
      Elapsed_String_Type : in Elapsed_String_Types := Exclude_Days)
      return String is

      -- Returns strings as follows based on elapsed time input
      -- HH:MM:SS when Exclude_Days
      -- DDDDD HH:MM:SS when Include_Days
      -- HHHHHHH:MM:SS when Hours_More_Than_24

      Seconds_per_Hour : constant Natural := 60 * 60;
      Seconds_per_Hundred_Hours : constant := Seconds_per_Hour * 100;
      Seconds_per_Day : constant Natural := 24 * Seconds_per_Hour;

      --                       12345
      Days_Result : String := "DDDDD ";

      --                                    12345
      Hundreds_of_Hours_Result : String := "HHHHH";

      --                  12345678
      Result : String := "HH:MM:SS";

      Zero_Set : constant Character_Set := To_Set ('0');

      Days : Natural := Seconds_Count / Seconds_per_Day;
      Hundreds_of_Hours : Natural := Seconds_Count / Seconds_per_Hundred_Hours;
      Seconds : Natural;


   begin -- Elapsed_Seconds
      if Elapsed_String_Type = Hours_More_Than_24 then
         Seconds := Seconds_Count -
           Hundreds_of_Hours * Seconds_per_Hundred_Hours;
      else
         Seconds := Seconds_Count - (Days * Seconds_per_Day);
      end if; -- Elapsed_String_Types = Hours_More_Than_24
      Result (8) := Digit_Out (Seconds); -- units of seconds
      Seconds := Seconds / 10;
      Result (7) := Digit_Out (Seconds, 6); -- tens of seconds
      Seconds := Seconds / 6;
      Result (5) := Digit_Out (Seconds); -- units of minutes
      Seconds := Seconds / 10;
      Result (4) := Digit_Out (Seconds, 6); -- tens of minutes
      Seconds := Seconds / 6;
      Result (2) := Digit_Out (Seconds); -- units of hours
      Seconds := Seconds / 10;
      Result (1) := Digit_Out (Seconds); -- tens of hours
      case Elapsed_String_Type is
         when Include_Days =>
            Days_Result (5) := Digit_Out (Days);
            Days := Days /10;
            Days_Result (4) := Digit_Out (Days);
            Days := Days /10;
            Days_Result (3) := Digit_Out (Days);
            Days := Days /10;
            Days_Result (2) := Digit_Out (Days);
            Days := Days /10;
            Days_Result (1) := Digit_Out (Days);
            return trim (Days_Result & Result, Zero_Set, Null_Set);
         when Exclude_Days =>
            return Result;
         when Hours_More_Than_24 =>
            Hundreds_of_Hours_Result (5) := Digit_Out (Hundreds_of_Hours);
            Hundreds_of_Hours := Hundreds_of_Hours / 10;
            Hundreds_of_Hours_Result (4) := Digit_Out (Hundreds_of_Hours);
            Hundreds_of_Hours := Hundreds_of_Hours / 10;
            Hundreds_of_Hours_Result (3) := Digit_Out (Hundreds_of_Hours);
            Hundreds_of_Hours := Hundreds_of_Hours / 10;
            Hundreds_of_Hours_Result (2) := Digit_Out (Hundreds_of_Hours);
            Hundreds_of_Hours := Hundreds_of_Hours / 10;
            Hundreds_of_Hours_Result (1) := Digit_Out (Hundreds_of_Hours);
            return trim (Hundreds_of_Hours_Result & Result, Zero_Set, Null_Set);
      end case; -- Elapsed_String_Type
   end Elapsed_Seconds;

   -- The following functions are based on the clock time or
   -- Another_Time if specifie

   function Date_String (Date_String_Type : in Date_String_Types := Full_Date;
                         Another_Time : in Time := Clock) return String is
      -- returns YYYY when Year_Only
      -- returns DD/MM/YYYY when Full_Date

      --                                1
      --                       1234567890
      Date_Result : string := "00/00/0000";
      --                       1234
      Year_Result : string := "0000";
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;

   begin -- Date_String
      Split (Another_Time, Year, Month, Day, Seconds);
      case Date_String_Type is
         when Year_Only =>
            Year_Result (1) := Digit_Out (Year / 1000);
            Year_Result (2) := Digit_Out (Year / 100);
            Year_Result (3) := Digit_Out (Year / 10);
            Year_Result (4) := Digit_Out (Year);
            return Year_Result;
         when Full_Date =>
            Date_Result (1) := Digit_Out (Day / 10);
            Date_Result (2) := Digit_Out (Day, 10);
            Date_Result (4) := Digit_Out (Month / 10);
            Date_Result (5) := Digit_Out (Month);
            Date_Result (7) := Digit_Out (Year / 1000);
            Date_Result (8) := Digit_Out (Year / 100);
            Date_Result (9) := Digit_Out (Year / 10);
            Date_Result (10) := Digit_Out (Year);
            return Date_Result;
      end case; -- Date_String_Type
   end Date_String;

   function Reverse_Date_String (Another_Time : in Time := Clock)
                                 return String is
      -- returns YYYYMMDD

      --                       12345678
      Date_Result : string := "YYYYMMDD";
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;

   begin -- Reverse_Date_String
      Split (Another_Time, Year, Month, Day, Seconds);
      Date_Result (7) := Digit_Out (Day / 10);
      Date_Result (8) := Digit_Out (Day, 10);
      Date_Result (5) := Digit_Out (Month / 10);
      Date_Result (6) := Digit_Out (Month);
      Date_Result (1) := Digit_Out (Year / 1000);
      Date_Result (2) := Digit_Out (Year / 100);
      Date_Result (3) := Digit_Out (Year / 10);
      Date_Result (4) := Digit_Out (Year);
      return Date_Result;
   end Reverse_Date_String;

   function Time_String (Another_Time : in Time := Clock) return Time_Strings is
      -- returns HH:MM:SS in twenty four hour mode

      In_Hour, InMinute : constant Natural := 60;
      Second_Count : Natural;
      --                        12345678
      Result : Time_Strings := "00:00:00";
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;

   begin -- Time_String
      Split (Another_Time, Year, Month, Day, Seconds);
      Second_Count := Natural (Seconds);
      Result (8) := Digit_Out (Second_Count); -- units of seconds
      Second_Count := Second_Count / 10;
      Result (7) := Digit_Out (Second_Count, 6); -- tens of seconds
      Second_Count := Second_Count / 6;
      Result (5) := Digit_Out (Second_Count); -- units of minutes
      Second_Count := Second_Count / 10;
      Result (4) := Digit_Out (Second_Count, 6); -- tens of minutes
      Second_Count := Second_Count / 6;
      Result (2) := Digit_Out (Second_Count); -- units of hours
      Second_Count := Second_Count / 10;
      Result (1) := Digit_Out (Second_Count); -- tens of hours
      return Result;
   end Time_String;

   function Time_of_Day (Time_String : in Time_Strings) return Day_Duration is
   -- returns Ada.Calendar.Formatting Day_Duration represented by a Time_String
   -- formatted as HH:MM:SS.

      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;

   begin -- Time_of_Day
      begin -- Hour exception block
         Hour := Hour_Number'Value (Time_String (1 .. 2));
      exception
         when others =>
            raise Time_Error with "Invalid Hours """ & Time_String (1 .. 2);
      end; -- Hour exception block
      if Time_String (3) /= ':' then
         raise Time_Error with "Expected ':' between HH and MM, found '" &
           Time_String (3) & ''';
      end if; -- Time_String (3) /= ':'
      begin -- Minute exception block
         Minute := Minute_Number'Value (Time_String (4 .. 5));
      exception
         when others =>
            raise Time_Error with "Invalid Minutes """ & Time_String (4 .. 5);
      end; -- Minute exception block
      if Time_String (6) /= ':' then
         raise Time_Error with "Expected ':' between HH and MM, found '" &
           Time_String (6) & ''';
      end if; -- Time_String (6) /= ':'
      begin -- Second exception block
         Second := Second_Number'Value (Time_String (7 .. 8));
      exception
         when others =>
            raise Time_Error with "Invalid Seconds """ & Time_String (7 .. 8);
      end; -- Second exception block
      return Seconds_Of (Hour, Minute, Second);
   end Time_of_Day;

   function Get_Date (Date_String : in Date_Strings;
                      Seconds : Day_Duration := 0.0) return Time is
      -- returns Ada.Calendar.Time represenreg by Date_String formatter as
      -- DD/MM/YYYY

      Day : Day_Number;
      Month : Month_Number;
      Year : Year_Number;

   begin -- Get_Date
      begin -- Day exception block
         Day := Day_Number'Value (Date_String (1 .. 2));
      exception
         when others =>
            raise Time_Error with "Invalid Days """ & Date_String (1 .. 2);
      end; -- Day exception block
      if Date_String (3) /= '/' then
         raise Time_Error with "Expected '/' between DD and MM, found '" &
           Date_String (3) & ''';
      end if; -- Date_String (3) /= '/'
      begin -- Month exception block
         Month := Month_Number'Value (Date_String (4 .. 5));
      exception
         when others =>
            raise Time_Error with "Invalid Months """ & Date_String (4 .. 5);
      end; -- Month exception block
      if Date_String (6) /= '/' then
         raise Time_Error with "Expected '/' between HH and MM, found '" &
           Date_String (6) & ''';
      end if; -- Date_String (6) /= '/'
      begin -- Year exception block
         Year := Year_Number'Value (Date_String (7 .. 10));
      exception
         when others =>
            raise Time_Error with "Invalid Seconds """ & Date_String (7 .. 10);
      end; -- Year exception block
      return Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
   end Get_Date;

   function Date_Only (Another_Time : in Time := Clock) return Time is

      -- Returns Time with Day_Duration set to zero

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;

   begin -- Date_Only
      Split (Another_Time, Year, Month, Day, Seconds);
      Seconds := 0.0;
      return Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
   end Date_Only;

function Time_String (Poxix_Time : in Posix_Times) return String is
      -- returns HH:MM:SS difference from current time

      Posix_Epoch : time := Time_Of (Year => 1970,
                                     Month => 1,
                                     Day => 1,
                                     Hour => 0,
                                     Minute => 0,
                                     Second => 0,
                                     Time_Zone => 0);
      Days : Day_Count;
      Seconds, Difference_Seconds : Duration;
      Leap_Seconds : Leap_Seconds_Count;


   begin -- Time_String
      Difference (Clock, Posix_Epoch, Days, Seconds, Leap_Seconds);
      Difference_Seconds := Duration (Poxix_Time
                                      - Posix_Times (Days * 24 * 3600)
                                      - Posix_Times (Seconds)
                                      - Posix_Times (Leap_Seconds));
      return Image (Difference_Seconds);
   end Time_String;
   
end DJH.Date_and_Time_Strings;
