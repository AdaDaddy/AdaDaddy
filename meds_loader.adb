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

with GNATCOLL.JSON;            use GNATCOLL.JSON;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
use Ada.Strings;
with Ada.Containers;         use Ada.Containers;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Meds_Loader is

   -- Read the streamlined MAR file and convert it to the data structures
   -- defined in the package specification
   procedure read_text_medlist (Medlist : out resident_meds; From : String) is
      The_File          : Ada.Text_IO.File_Type;
      Input_String      : String (1 .. 1_024);
      Inpt_Len          : Natural;
      The_Resident      : resident_info;
      Resident_Cursor   : resident_vector.Cursor;
      The_Med           : Med_Data;
      Colon_Index       : Natural;
      Param_Index       : Natural;
      Close_Param_Index : Natural;
      Colon             : String := ":";
      Param             : String := "(";
      Close_Param       : String := ")";
      Temp_Instr        : Unbounded_String;
      Temp_Hours        : Unbounded_String;

      function sanitize (Item : in String) return String is
         CR_pattern : String := "" & Ada.Characters.Latin_1.CR;
      begin
         if Index (Source => Item, Pattern => CR_pattern) > 0 then
            return
              Trim
                (Source => Item (Item'First .. Item'Last - 1), Side => Both);
         else
            return Trim (Source => Item, Side => Both);
         end if;
      end sanitize;

   begin
      Open (File => The_File, Mode => In_File, Name => From);
      while not End_Of_File (The_File) loop
         Get_Line (File => The_File, Item => Input_String, Last => Inpt_Len);

         if Inpt_Len > 0 then
            declare
               Clean_Input : String := sanitize (Input_String (1 .. Inpt_Len));
               --  Trim (Source => Input_String (1 .. Inpt_Len), Side => Both);
            begin
               Colon_Index := Index (Source => Clean_Input, Pattern => Colon);
               -- if a colon was found in the input string
               if Colon_Index > 0 then
                  if Clean_Input (1 .. Colon_Index) = "Resident Name:" then
                     The_Resident.Allergies      := Null_Unbounded_String;
                     The_Resident.Birthdate      := Null_Unbounded_String;
                     The_Resident.Diagnosis      := Null_Unbounded_String;
                     The_Resident.Meds := (others => med_vector.Empty_Vector);
                     The_Resident.Meds_Comment   := Null_Unbounded_String;
                     The_Resident.Name           := Null_Unbounded_String;
                     The_Resident.Sex            := Null_Unbounded_String;
                     The_Resident.Id             := Null_Unbounded_String;
                     The_Resident.Room_Number    := Null_Unbounded_String;
                     The_Resident.Photo          := Null_Unbounded_String;
                     The_Resident.Contact_Name   := Null_Unbounded_String;
                     The_Resident.Contact_Number := Null_Unbounded_String;
                     The_Resident.Contact_Email  := Null_Unbounded_String;

                     resident_vector.Append
                       (Container => Medlist.List, New_Item => The_Resident);
                     Resident_Cursor   := Last (Medlist.List);
                     The_Resident.Name :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Id" then
                     The_Resident.Id :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Room Number" then
                     The_Resident.Room_Number :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Photo" then
                     The_Resident.Photo :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Contact Name" then
                     The_Resident.Contact_Name :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Contact Number" then
                     The_Resident.Contact_Number :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Contact Email" then
                     The_Resident.Contact_Email :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Birthday:"
                    or else Clean_Input (1 .. Colon_Index) = "Birthdate:"
                  then

                     The_Resident.Birthdate :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Sex:" then
                     The_Resident.Sex :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                  elsif Clean_Input (1 .. Colon_Index) = "Meds:" then
                     The_Resident.Meds_Comment :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Diagnosis:" then
                     The_Resident.Diagnosis :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Allergies:" then
                     The_Resident.Allergies :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  elsif Clean_Input (1 .. Colon_Index) = "Medication:" then
                     The_Med.Brand_Names  := Null_Unbounded_String;
                     The_Med.Generic_Name := Null_Unbounded_String;
                     The_Med.Instructions := Null_Unbounded_String;

                     Param_Index :=
                       Index (Source => Clean_Input, Pattern => Param);
                     Close_Param_Index :=
                       Index (Source => Clean_Input, Pattern => Close_Param);
                     if Param_Index > 0 then
                        The_Med.Brand_Names :=
                          Trim
                            (To_Unbounded_String
                               (Clean_Input
                                  (Colon_Index + 1 .. Param_Index - 1)),
                             Both);
                        The_Med.Generic_Name :=
                          Trim
                            (To_Unbounded_String
                               (Clean_Input
                                  (Param_Index + 1 .. Close_Param_Index - 1)),
                             Both);
                     else
                        The_Med.Brand_Names :=
                          To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last));
                        The_Med.Generic_Name := Null_Unbounded_String;
                     end if;

                  elsif Clean_Input (1 .. Colon_Index) = "Instructions:" then
                     The_Med.Instructions :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);

                  elsif Clean_Input (1 .. Colon_Index) = "Hours:" then
                     Temp_Hours :=
                       Trim
                         (To_Unbounded_String
                            (Clean_Input
                               (Colon_Index + 1 .. Clean_Input'Last)),
                          Both);
                     if Index (Source => Temp_Hours, Pattern => "PRN") > 0 then
                        med_vector.Append
                          (Container => The_Resident.Meds (PRN),
                           New_Item  => The_Med);
                     end if;

                     if Index (Source => Temp_Hours, Pattern => "8am") > 0 then
                        med_vector.Append
                          (Container => The_Resident.Meds (Morning),
                           New_Item  => The_Med);
                     end if;

                     if Index (Source => Temp_Hours, Pattern => "12pm") > 0
                     then
                        med_vector.Append
                          (Container => The_Resident.Meds (Noon),
                           New_Item  => The_Med);
                     end if;

                     if Index (Source => Temp_Hours, Pattern => "2pm") > 0 then
                        med_vector.Append
                          (Container => The_Resident.Meds (Afternoon),
                           New_Item  => The_Med);
                     end if;

                     if Index (Source => Temp_Hours, Pattern => "8pm") > 0 then
                        med_vector.Append
                          (Container => The_Resident.Meds (Night),
                           New_Item  => The_Med);
                     end if;

                     Replace_Element
                       (Container => Medlist.List, Position => Resident_Cursor,
                        New_Item  => The_Resident);
                  end if;
               end if;
            end;
         end if;
      end loop;
   end read_text_medlist;

   -- Read through a file of resident names, removing the corresponding
   -- rersident_info record from Medlis.list
   procedure filter_medlist (Medlist : in out resident_meds; from : String) is
      The_File     : Ada.Text_IO.File_Type;
      The_Resident : resident_info;
      The_Name     : Unbounded_String;
      Found_Name   : Unbounded_String;
   begin
      Open (File => The_File, Mode => In_File, Name => from);
      while not End_Of_File (The_File) loop
         The_Name := Get_Line (File => The_File);
         The_Name := Trim (Source => The_Name, Side => Both);
         for Vect_Idx in 1 .. Positive (Medlist.List.Length) loop
            Found_Name :=
              Trim (Source => Medlist.List (Vect_Idx).Name, Side => Both);
            if The_Name = Found_Name then
               Medlist.List.Delete (Index => Vect_Idx);
               exit;
            end if;
         end loop;
      end loop;

   end filter_medlist;

   -- Output the converted streamlined MAR file in human-readable format
   -- This is a convenience for verifying the correctness of the data conversion
   procedure display_med_data
     (Med : Med_Data; The_File : Ada.Text_IO.File_Type)
   is
   begin
      Put_Line (File => The_File, Item => "Medication:");
      Put_Line
        (File => The_File,
         Item => "Brand Names: " & To_String (Med.Brand_Names));
      Put_Line
        (File => The_File,
         Item => "Generic Name: " & To_String (Med.Generic_Name));
      Put_Line
        (File => The_File,
         Item => "Instructions: " & To_String (Med.Instructions));
      New_Line (File => The_File);
   end display_med_data;

   procedure display_resident_info
     (Resident : resident_info; The_File : Ada.Text_IO.File_Type)
   is
   begin
      Put_Line (File => The_File, Item => "Resident:");
      Put_Line (File => The_File, Item => "Id: " & To_String (Resident.Id));
      Put_Line
        (File => The_File,
         Item => "Room Number: " & To_String (Resident.Room_Number));
      Put_Line
        (File => The_File,
         Item => "Contact Name: " & To_String (Resident.Contact_Name));
      Put_Line
        (File => The_File,
         Item => "Contact Number: " & To_String (Resident.Contact_Number));
      Put_Line
        (File => The_File,
         Item => "Contact Email: " & To_String (Resident.Contact_Email));
      Put_Line
        (File => The_File, Item => "Name: " & To_String (Resident.Name));
      Put_Line
        (File => The_File,
         Item => "Birthdate: " & To_String (Resident.Birthdate));
      Put_Line (File => The_File, Item => "Sex: " & To_String (Resident.Sex));
      Put_Line
        (File => The_File,
         Item => "Diagnosis: " & To_String (Resident.Diagnosis));
      Put_Line
        (File => The_File,
         Item => "Allergies: " & To_String (Resident.Allergies));
      Put_Line
        (File => The_File,
         Item => "Meds Comment: " & To_String (Resident.Meds_Comment));
      New_Line (File => The_File);
      for Period in Resident.Meds'Range loop
         case Period is
            when Morning =>
               Put_Line (File => The_File, Item => "8am:");
            when Noon =>
               Put_Line (File => The_File, Item => "12pm:");
            when Afternoon =>
               Put_Line (File => The_File, Item => "2pm:");
            when Night =>
               Put_Line (File => The_File, Item => "8pm:");
            when PRN =>
               Put_Line (File => The_File, Item => "PRN:");
         end case;

         for The_Med of Resident.Meds (Period) loop
            display_med_data (The_Med, The_File);
         end loop;
      end loop;

      New_Line (File => The_File);
   end display_resident_info;

   procedure display_medlist
     (Medlist : resident_meds; filename : String; Facility : String)
   is
      The_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create
        (File => The_File, Mode => Out_File, Name => filename);

      Ada.Text_IO.Put_Line (File => The_File, Item => Facility);
      for Resident of Medlist.List loop
         display_resident_info (Resident, The_File);
      end loop;
      Ada.Text_IO.Close (The_File);
   end display_medlist;

   -- Write Med list in JSON format
   -- This is the primary purpose of this tool
   procedure write_medlist
     (Medlist : resident_meds; filename : String; Facility : String)
   is
      Meds_List   : JSON_Value := Create_Object;
      Meds_Arr    : JSON_Array := Empty_Array;
      Output_File : File_Type;

      -- Internal function to convert a Med_Data object to a JSON_Value

      function Write_Med_Data (The_Med : Med_Data) return JSON_Value is
         Med_Obj : JSON_Value := Create_Object;
      begin
         Med_Obj.Set_Field
           (Field_Name => "Medication_Brand_Name",
            Field      => The_Med.Brand_Names);
         Med_Obj.Set_Field
           (Field_Name => "Medication Generic Name",
            Field      => The_Med.Generic_Name);
         Med_Obj.Set_Field
           (Field_Name => "Instructions", Field => The_Med.Instructions);
         return Med_Obj;
      end Write_Med_Data;

      -- Internal function to convert a resident_info object into a JSON_Value

      function Write_Resident_Data (Resident : resident_info) return JSON_Value
      is
         Res_Obj     : JSON_Value := Create_Object;
         Meds_period : JSON_Array := Empty_Array;
         Meds        : JSON_Array;
         Period_Obj  : JSON_Value := Create_Object;
      begin
         Res_Obj.Set_Field (Field_Name => "Id", Field => Resident.Id);
         Res_Obj.Set_Field
           (Field_Name => "Room Number", Field => Resident.Room_Number);
         Res_Obj.Set_Field (Field_Name => "Photo", Field => Resident.Photo);
         Res_Obj.Set_Field
           (Field_Name => "Contact Name", Field => Resident.Contact_Name);
         Res_Obj.Set_Field
           (Field_Name => "Contact Number", Field => Resident.Contact_Number);
         Res_Obj.Set_Field
           (Field_Name => "Contact Email", Field => Resident.Contact_Email);
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
         for period in Resident.Meds'Range loop
            Meds := Empty_Array;
            for The_Med of Resident.Meds (period) loop
               Append (Meds, Write_Med_Data (The_Med));
            end loop;

            case period is
               when Morning =>
                  Period_Obj.Set_Field
                    (Field_Name => "8am Medications", Field => Meds);
               when Noon =>
                  Period_Obj.Set_Field
                    (Field_Name => "12pm Medications", Field => Meds);
               when Afternoon =>
                  Period_Obj.Set_Field
                    (Field_Name => "2pm Medications", Field => Meds);
               when Night =>
                  Period_Obj.Set_Field
                    (Field_Name => "8pm Medications", Field => Meds);
               when PRN =>
                  Period_Obj.Set_Field
                    (Field_Name => "PRN Medications", Field => Meds);
            end case;
         end loop;

         -- Create the Meds array field in the Res_Obj
         Res_Obj.Set_Field ("Medications", Period_Obj);
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

      Meds_List.Set_Field (Facility & " Residents", Meds_Arr);

      -- Output the Meds_List object to the file passed to the
      -- filename paramter of this procedure

      Create (File => Output_File, Mode => Out_File, Name => filename);
      Ada.Text_IO.Put_Line (File => Output_File, Item => Meds_List.Write);
      Close (File => Output_File);
   end write_medlist;

end Meds_Loader;
