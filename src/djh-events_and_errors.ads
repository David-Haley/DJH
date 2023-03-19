-- This package manages the event and error logging
-- Author    : David Haley
-- Created   : 20/02/2021
-- Last Edit : 20/08/2022
-- 20220820: Moved to DJH.Events_and_Errors

with Ada.Exceptions; use Ada.Exceptions;

package DJH.Events_and_Errors is

   procedure Start_Events;
   -- Opens Event and Error files

   procedure Put_Event (Event_Text : in String);
   -- Writes an event message to Event_Log.txt

   procedure Put_Error (Error_Text : in String;
                        Event : in Exception_Occurrence);
   -- Writes an error message to Error_Log.txt

   procedure Stop_Events;
   -- Explicitely closes Event and Error files

end DJH.Events_and_Errors;
