with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with DJH.Email; use DJH.Email;

procedure Test_Email is

   Password, Status_Report : Unbounded_String;
   Success : Boolean;

begin -- Test_Email
   Put ("Enter mail server password: ");
   Get_line (Password);
   Setup (SMTP_Server => "smtp.telstra.com",
          From_Name => "David Haley",
          From_Email => "david.haley@bigpond.com",
          User_Name => "david.haley@bigpond.com",
          Password => To_String (Password));
   Send_Email (Message_File_Name => "Test_Email_Body.txt",
               Attachment_File_Name => "c:\users\david\documents\brisbane " &
                 "southside masters\attendance_trial\" &
                 "report\202010\797173_20201031.txt",
               To_Name => "Josh Hemelaar",
               To_Email => "dj_haley@bigpond.net.au",
               Subject => "BSM Session Statement - October 2020",
               Status => Success,
               Status_Report => Status_Report);
   Put_Line (Boolean'Image (Success) & " """ & Status_Report & """");
end Test_Email;
