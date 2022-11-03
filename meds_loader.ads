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

   -- filter out resident(s) by names listed in the file
   -- designated by the From parameter;
   procedure filter_medlist (Medlist : in out resident_meds; from : String);

   -- Output the resident_meds data in human-readable format
   procedure display_medlist
     (Medlist : resident_meds; filename : String; Facility : String);

   -- Output the resident_meds data in JSON format
   procedure write_medlist
     (Medlist : resident_meds; filename : String; Facility : String);

private

   -- structure of data for a single medication
   type Med_Data is record
      Brand_Names  : Unbounded_String;
      Generic_Name : Unbounded_String;
      Instructions : Unbounded_String;
   end record;

   package med_vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Med_Data);
   use med_vector;

   -- type to index the array of meds according to their
   -- administration period
   type Med_Admin_Period is (Morning, Noon, Afternoon, Night, PRN);

   -- Array type to hold vectors of med_data for each administration
   -- period
   type Meds_Lists is array (Med_Admin_Period) of med_vector.Vector;

   -- structure of a resident_info record for a single resident
   -- The meds field is an array of med_vector.Vector so that the
   -- meds for each resident can be listed for each administration period
   type resident_info is record
      Id             : Unbounded_String := Null_Unbounded_String;
      Room_Number    : Unbounded_String := Null_Unbounded_String;
      Photo          : Unbounded_String := Null_Unbounded_String;
      Contact_Name   : Unbounded_String := Null_Unbounded_String;
      Contact_Number : Unbounded_String := Null_Unbounded_String;
      Contact_Email  : Unbounded_String := Null_Unbounded_String;
      Name           : Unbounded_String := Null_Unbounded_String;
      Birthdate      : Unbounded_String := Null_Unbounded_String;
      Diagnosis      : Unbounded_String := Null_Unbounded_String;
      Allergies      : Unbounded_String := Null_Unbounded_String;
      Sex            : Unbounded_String := Null_Unbounded_String;
      Meds_Comment   : Unbounded_String := Null_Unbounded_String;
      Meds           : Meds_Lists       := (others => med_vector.Empty_Vector);
   end record;

   -- instantiate Ada.Containers.Vectors for the resident_info type
   package resident_vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => resident_info);
   use resident_vector;

   -- structure of a list of medication records for a facility
   type resident_meds is record
      List : resident_vector.Vector := resident_vector.Empty_Vector;
   end record;

end Meds_Loader;
