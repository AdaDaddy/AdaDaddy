-----------------------------------------------------------------------
--            Tres & Associates Development Tool                     --
--                                                                   --
--                   procedure load_meds                             --
--                                                                   --
--                           Body                                    --
--                                                                   --
--           Copyright (C) 2022 Tres & Associates                    --
--                                                                   --
-- This procedure acts as a program entry point for converting       --
-- streamlined MAR data to JSON format.                              --
-- The procedure will prompt the user for the name of the streamlined--
-- MAR file and will process the named file.                         --
--                                                                   --
-----------------------------------------------------------------------
with Ada.Text_IO;           use Ada.Text_IO;
with Meds_Loader;           use Meds_Loader;
with Ada.Directories;       use Ada.Directories;
with Ada.Calendar;          use Ada.Calendar;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
use Ada.Strings;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

procedure Load_Meds is
   File_Name     : Unbounded_String;
   Facility      : Unbounded_String;
   The_Medlist   : resident_meds;
   Dir           : String := "/home/jamesrogers/Ada/meds_loader/";
   Today         : Time   := Clock;
   The_Year      : String := Trim (Integer'Image (Year (Today)), Both);
   The_Month     : String := Trim (Integer'Image (Month (Today)), Both);
   The_Day       : String := Trim (Integer'Image (Day (Today)), Both);
   JSON_Filename : String :=
     "JSON_" & The_Year & "_" & The_Month & "_" & The_Day;

   -- a function to convert all embedded space characters ' '
   -- to underscore characters so that the resulting file name is
   -- compatible with all operating system file naming rules

   function clean_name (S : Unbounded_String) return String is
      Result : String := Trim (To_String (S), Both);
   begin
      for I in Result'Range loop
         if Result (I) = ' ' then
            Result (I) := '_';
         end if;
      end loop;
      return Result;
   end clean_name;

begin
   Set_Directory (Dir);
   Put ("Enter the facility name: ");
   Facility := Get_Line;
   Put ("Enter the file name of the streamlined MAR file to process: ");
   File_Name := Get_Line;

   -- process the streamlined MAR file
   read_text_medlist (The_Medlist, Dir & To_String (File_Name));

   -- Filter the data to remove departed residents
   Put ("Enter the name of the filter file: ");
   File_Name := Get_Line;
   filter_medlist
     (Medlist => The_Medlist, from => Dir & To_String (File_Name));

   display_medlist
     (The_Medlist,
      Dir & clean_name (Facility) & "_" & The_Year & "_" & The_Month &
      "_" & The_Day & ".txt", To_String(Facility));

   -- Output JSON formatted list

   write_medlist
     (The_Medlist,
      Dir & clean_name (Facility) & "_" & The_Year & "_" & The_Month & "_" &
      The_Day & ".json", To_String(Facility));

end Load_Meds;
