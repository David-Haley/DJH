-- This package manages the event and error logging
-- Author    : David Haley
-- Created   : 20/02/2021
-- Last Edit : 11/04/2025

-- 20250411 : Open files as package during initialisation, file closeure made
-- conditional.
-- 20230923: Removal of redundant spaces from event log messages, error log now
-- comma delimited.
-- 20220820: Moved to DJH.Events_and_Errors
-- 20210303 : Unified version of data and time strings used

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;

package body DJH.Events_and_Errors is

   Event_File_Name : constant String := "Event_Log.txt";
   Error_File_Name : constant String := "Error_Log.txt";

   Event_File, Error_File : File_Type;
   
   procedure Put_Event (Event_Text : in String) is

      -- Writes an event message to Event_Log.txt

   begin -- Put_Event
      Put_Line (Event_File, Date_String (Full_Date) & ',' & Time_String &
                  ",""" & Event_Text & """");
      Flush (Event_File);
   end Put_Event;

   procedure Put_Error (Error_Text : in String;
                        Event : in Exception_Occurrence) is

      -- Writes an error message to Error_Log.txt

   begin -- Put_Error
      Put_Line (Error_File, Date_String (Full_Date) & ',' &
                  Time_String & ",""" & Error_Text & " - " &
                  Exception_Message (Event) & """");
      Flush (Error_File);
   end Put_Error;

   procedure Stop_Events is

      -- Explicitely closes Event and Error files
   begin -- Stop_Events
      if Is_Open (Event_File) then
         Close (Event_File);
      end if; -- Is_Open (Event_File)
      if Is_Open (Error_File) then
         Close (Error_File);
      end if; -- Is_Open (Error_File)
   end Stop_Events;
   
begin -- DJH.Events_and_Errors
   if Exists (Event_File_Name) then
      Open (Event_File, Append_File, Event_File_Name);
   else
      Create (Event_File, Out_File, Event_File_Name);
   end if; -- Exists (Output_File_Name)
   if Exists (Error_File_Name) then
      Open (Error_File, Append_File, Error_File_Name);
   else
      Create (Error_File, Out_File, Error_File_Name);
   end if; -- Exists (Error_File_Name)
end DJH.Events_and_Errors;
