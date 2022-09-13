-----------------------------------------------------------------------
--            Tres & Associates Development Tool                     --
--                                                                   --
--                   package Meds_Loader                             --
--                                                                   --
--                           SPEC                                    --
--                                                                   --
--           Copyright (C) 2022 Tres & Associates                    --
--                                                                   --
-- This package defines the data structures and subprograms needed   --
-- to read a streamlined MAR format text file and convert the data   --
-- to JSON format for input into a non-sql database table.           --
--                                                                   --
-----------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Meds_Loader is

   -- Data structure containing the medication information
   -- associated with each resident in a facility
   type resident_meds is private;

   -- Convert a text file of patient medications to a resident_meds
   -- data structure
   procedure read_text_medlist (Medlist : out resident_meds; From : String);

   -- Output the resident_meds data in human-readable format
   procedure display_medlist (Medlist : resident_meds);

   -- Output the resident_meds data in JSON format
   procedure write_medlist (Medlist : resident_meds; filename : String);

private

   -- structure of data for a single medication
   type Med_Data is record
      Brand_Names  : Unbounded_String;
      Generic_Name : Unbounded_String;
      Instructions : Unbounded_String;
      Hours        : Unbounded_String;
   end record;

   package med_vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Med_Data);
   use med_vector;

   -- structure of a medication record for a single resident
   type resident_info is record
      Name         : Unbounded_String;
      Birthdate     : Unbounded_String;
      Diagnosis    : Unbounded_String;
      Allergies    : Unbounded_String;
      Sex          : Unbounded_String;
      Meds_Comment : Unbounded_String;
      Meds         : med_vector.Vector := med_vector.Empty_Vector;
   end record;

   package resident_vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => resident_info);
   use resident_vector;

   -- structure of a list of medication records for a facility
   type resident_meds is record
      List : resident_vector.Vector := resident_vector.Empty_Vector;
   end record;

end Meds_Loader;
