-----------------------------------------------------------------------
--            Tres & Associates Development Tool                     --
--                                                                   --
--                   package Meds_Loader                             --
--                                                                   --
--                           Body                                    --
--                                                                   --
--           Copyright (C) 2022 Tres & Associates                    --
--                                                                   --
-- This package defines the data structures and subprograms needed   --
-- to read a streamlined MAR format text file and convert the data   --
-- to JSON format for input into a non-sql database table.           --
--                                                                   --
-----------------------------------------------------------------------

with GNATCOLL.JSON;     use GNATCOLL.JSON;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
use Ada.Strings;
with Ada.Containers; use Ada.Containers;

package body Meds_Loader is

   -- Read the streamlined MAR file and convert it to the data structures
   -- defined in the package specification
   procedure read_text_medlist (Medlist : out resident_meds; From : String) is
      The_File        : Ada.Text_IO.File_Type;
      Input_String    : String (1 .. 1_024);
      Inpt_Len        : Natural;
      The_Resident    : resident_info;
      Resident_Cursor : resident_vector.Cursor;
      Med_Cursor      : med_vector.Cursor;
      The_Med         : Med_Data;
      Colon_Index     : Natural;
      Param_Index     : Natural;
      Colon           : String := ":";
      Param           : String := "(";
   begin
      Open (File => The_File, Mode => In_File, Name => From);
      while not End_Of_File (The_File) loop
         Get_Line (File => The_File, Item => Input_String, Last => Inpt_Len);

         if Inpt_Len > 0 then
            declare
               Clean_Input : String :=
                 Trim (Source => Input_String (1 .. Inpt_Len), Side => Both);
            begin
               Colon_Index := Index (Source => Clean_Input, Pattern => Colon);
               -- if a colon was found in the input string
               if Colon_Index > 0 then
                  if Clean_Input (1 .. Colon_Index) = "Resident Name:" then
                     The_Resident.Allergies    := Null_Unbounded_String;
                     The_Resident.Birthdate    := Null_Unbounded_String;
                     The_Resident.Diagnosis    := Null_Unbounded_String;
                     The_Resident.Meds         := med_vector.Empty_Vector;
                     The_Resident.Meds_Comment := Null_Unbounded_String;
                     The_Resident.Name         := Null_Unbounded_String;
                     The_Resident.Sex          := Null_Unbounded_String;

                     resident_vector.Append
                       (Container => Medlist.List, New_Item => The_Resident);
                     Resident_Cursor   := Last (Medlist.List);
                     The_Resident.Name :=
                       To_Unbounded_String
                         (Clean_Input (Colon_Index + 1 .. Clean_Input'Last));
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);

                  elsif Clean_Input (1 .. Colon_Index) = "Birthdate:" then
                     The_Resident.Birthdate :=
                       To_Unbounded_String
                         (Clean_Input (Colon_Index + 1 .. Clean_Input'Last));
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Sex:" then
                     The_Resident.Sex :=
                       To_Unbounded_String
                         (Clean_Input (Colon_Index + 1 .. Clean_Input'Last));
                  elsif Clean_Input (1 .. Colon_Index) = "Meds:" then
                     The_Resident.Meds_Comment :=
                       To_Unbounded_String
                         (Clean_Input (Colon_Index + 1 .. Clean_Input'Last));
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Diagnosis:" then
                     The_Resident.Diagnosis :=
                       To_Unbounded_String
                         (Clean_Input (Colon_Index + 1 .. Clean_Input'Last));
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Allergies:" then
                     The_Resident.Allergies :=
                       To_Unbounded_String
                         (Clean_Input (Colon_Index + 1 .. Clean_Input'Last));
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Medication:" then
                     The_Med.Brand_Names  := Null_Unbounded_String;
                     The_Med.Generic_Name := Null_Unbounded_String;
                     The_Med.Hours        := Null_Unbounded_String;
                     The_Med.Instructions := Null_Unbounded_String;

                     med_vector.Append
                       (Container => The_Resident.Meds, New_Item => The_Med);
                     Med_Cursor  := Last (The_Resident.Meds);
                     Param_Index :=
                       Index (Source => Clean_Input, Pattern => Param);
                     if Param_Index > 0 then
                        The_Med.Brand_Names :=
                          To_Unbounded_String
                            (Clean_Input (Colon_Index + 1 .. Param_Index - 1));
                        The_Med.Generic_Name :=
                          To_Unbounded_String
                            (Clean_Input
                               (Param_Index + 1 .. Clean_Input'Last - 1));
                     else
                        The_Med.Brand_Names :=
                          To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last));
                        The_Med.Generic_Name := Null_Unbounded_String;
                     end if;
                     Replace_Element
                       (Container => The_Resident.Meds, Position => Med_Cursor,
                        New_Item  => The_Med);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Instructions:" then
                     The_Med.Instructions :=
                       To_Unbounded_String
                         (Clean_Input (Colon_Index + 1 .. Clean_Input'Last));
                     Replace_Element
                       (Container => The_Resident.Meds, Position => Med_Cursor,
                        New_Item  => The_Med);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Hours:" then
                     The_Med.Hours :=
                       To_Unbounded_String
                         (Clean_Input (Colon_Index + 1 .. Clean_Input'Last));
                     Replace_Element
                       (Container => The_Resident.Meds, Position => Med_Cursor,
                        New_Item  => The_Med);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  end if;
               end if;
            end;
         end if;
      end loop;
   end read_text_medlist;

   -- Output the converted streamlined MAR file in human-readable format
   -- This is a convenience for verifying the correctness of the data conversion
   procedure display_med_data (Med : Med_Data) is
   begin
      Put_Line ("Medication:");
      Put_Line ("Brand Names: " & To_String (Med.Brand_Names));
      Put_Line ("Generic Name: " & To_String (Med.Generic_Name));
      Put_Line ("Instructions: " & To_String (Med.Instructions));
      Put_Line ("Hours: " & To_String (Med.Hours));
      New_Line;
   end display_med_data;

   procedure display_resident_info (Resident : resident_info) is
   begin
      Put_Line ("Resident:");
      Put_Line ("Name: " & To_String (Resident.Name));
      Put_Line ("Birthdate: " & To_String (Resident.Birthdate));
      Put_Line ("Sex: " & To_String (Resident.Sex));
      Put_Line ("Diagnosis: " & To_String (Resident.Diagnosis));
      Put_Line ("Allergies: " & To_String (Resident.Allergies));
      Put_Line ("Meds Comment: " & To_String (Resident.Meds_Comment));
      New_Line;
      for The_Med of Resident.Meds loop
         display_med_data (The_Med);
      end loop;
      New_Line;
   end display_resident_info;

   procedure display_medlist (Medlist : resident_meds) is

   begin
      for Resident of Medlist.List loop
         display_resident_info (Resident);
      end loop;
   end display_medlist;

   -- Write Med list in JSON format
   -- This is the primary purpose of this tool
   procedure write_medlist (Medlist : resident_meds; filename : String) is
      Meds_List   : JSON_Value := Create_Object;
      Meds_Arr    : JSON_Array := Empty_Array;
      Output_File : File_Type;
      
      -- Internal function to convert a Med_Data object to a JSON_Value

      function Write_Med_Data (The_Med : Med_Data) return JSON_Value is
         Med_Obj  : JSON_Value := Create_Object;
         Res_List : JSON_Array := Empty_Array;
      begin
         Med_Obj.Set_Field
           (Field_Name => "Medication_Brand_Name",
            Field      => The_Med.Brand_Names);
         Med_Obj.Set_Field
           (Field_Name => "Medication Generic Name",
            Field      => The_Med.Generic_Name);
         Med_Obj.Set_Field
           (Field_Name => "Instructions", Field => The_Med.Instructions);
         Med_Obj.Set_Field (Field_Name => "Hours", Field => The_Med.Hours);
         return Med_Obj;
      end Write_Med_Data;

      -- Internal function to convert a resident_info object into a JSON_Value

      function Write_Resident_Data (Resident : resident_info) return JSON_Value
      is
         Res_Obj : JSON_Value := Create_Object;
         Meds    : JSON_Array := Empty_Array;
      begin
         Res_Obj.Set_Field (Field_Name => "Name", Field => Resident.Name);
         Res_Obj.Set_Field
           (Field_Name => "Birthdate", Field => Resident.Birthdate);
         Res_Obj.Set_Field (Field_Name => "Sex", Field => Resident.Sex);
         Res_Obj.Set_Field
           (Field_Name => "Diagnosis", Field => Resident.Diagnosis);
         Res_Obj.Set_Field
           (Field_Name => "Allergies", Field => Resident.Allergies);
         
         -- Convert each med in Resident.Meds to its corresponding JSON_Value
         -- and append that JSON_Value to the Meds JSON_Array
         for The_Med of Resident.Meds loop
            Append (Meds, Write_Med_Data (The_Med));
         end loop;

         -- Create the Meds array field in the Res_Obj
         Res_Obj.Set_Field ("Medications", Meds);
         return Res_Obj;
      end Write_Resident_Data;

   begin
      -- Process the Medlist data for each resident, appending it to the 
      -- Meds_Arr JSON_Array

      for Res of Medlist.List loop
         Append (Meds_Arr, Write_Resident_Data (Res));
      end loop;

      -- add the Meds_Arr to the Meds_List object to complete the
      -- JSON serializaion of the data

      Meds_List.Set_Field ("Residents", Meds_Arr);

      -- Output the Meds_List object to the file passed to the
      -- filename paramter of this procedure
      
      Create (File => Output_File, Mode => Out_File, Name => filename);
      Put_Line (File => Output_File, Item => Meds_List.Write);
      Close (File => Output_File);
   end write_medlist;

end Meds_Loader;
