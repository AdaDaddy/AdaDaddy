-----------------------------------------------------------------------
--              TresCare Server Components                           --
--                                                                   --
--                     Person Pacakge                                --
--                                                                   --
--                        Spec                                       --
--                                                                   --
--           Copyright (c) Tres & Associates 2022                    --
--                                                                   --
-- This package contains all the information needed to define a      --
-- person in the eMAR system backend software.                       --
--                                                                   --
-- Examples of a person are a PCA, a DON, a Resident, a Contact      --
-- or a Physician.                                                   --
-----------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded;
with Ada.Calendar;          use Ada.Calendar;

package Person is
   package Resident_Name_Pkg is new Ada.Strings.Bounded.Generic_Bounded_Length
     (50);
   use Resident_Name_Pkg;

   -- A person name is a string up to 50 characters long

   subtype Person_Name is Bounded_String;

   -- A person_id is a string which may be role dependent or may not have
   -- a value. It may be an employee Id, a resident Id or whatever form
   -- of ID used by any other person. It is implemented using an
   -- unbounded string so that different ID formats can be used for
   -- different roles.

   subtype Person_Id is Unbounded_String;

   function set_id(id : in string) return Person_Id;

   function get_id(Pid : in Person_Id) return String;

   -- A birthdate is a sortable value consisting of the day, month and year
   -- a person was born.

   type birthdate is private;

   procedure set_birthdate
     (B  : in out birthdate; Mnth : Month_Number; Day : Day_Number;
      Yr :        Year_Number);
   function get_birthdate (B : in birthdate) return String;

   function get_birthday (B : in birthdate) return String;

   function get_age (B : in birthdate) return Natural;


   -- A phone number is a 10 element array of numeric characters

   subtype all_digits is Character range '0' .. '9';

   subtype Phone_Number is String (1 .. 10) with
        Dynamic_Predicate => (for all I of Phone_Number => I in all_digits);

   -- the function get_phone_number returns a phone number with specified
   -- divider character between the area code and the prefix and between
   -- the prefix and the base number
   -- for instance using the value '-' for the Divider parameter will
   -- output the value 8551234567 as 855-123-4567

   function get_phone_number
     (N : Phone_Number; Divider : Character) return String;

   -- a zip code is a 5 element array of characters

   subtype zip_code is String (1 .. 5) with
     Dynamic_Predicate => (for all I of zip_code => I in all_digits);

   -- Address_Type is a private record

   type Address_Type is private;

   function Set_Address
     (street : Unbounded_String; Apt : Unbounded_String;
      City   : Unbounded_String; State : Unbounded_String; Zip : zip_code)
      return Address_Type;
   function get_street (addr : in Address_Type) return String;
   function get_apt (addr : in Address_Type) return String;
   function get_city (addr : in Address_Type) return String;
   function get_state (addr : in Address_Type) return String;
   function get_zipcode (addr : in Address_Type) return String;

   -- type Person_Data is a tagged record allowing the type to be
   -- extended in the future without changing this package.

   type Person_Data is tagged private;

   procedure set_name (P : in out Person_Data; Name : in Person_Name);
   function get_name (P : in Person_Data) return String;
   procedure set_birthdate (P : in out Person_Data; Bday : in birthdate);
   function get_birtdate (P : in Person_Data) return birthdate;
   procedure set_phone (P : in out Person_Data; Phone : in Phone_Number);
   function get_phone (P : in Person_Data) return Phone_Number;
   procedure set_address (P : in out Person_Data; address : in Address_Type);
   function get_address (P : in Person_Data) return Address_Type;
   procedure set_email (P : in out Person_Data; email : in String);
   function get_email (P : in Person_Data) return String;

private
   type birthdate is record
      Month : Month_Number := Month_Number'First;
      Day   : Day_Number   := Day_Number'First;
      Year  : Year_Number  := Year_Number'First;
   end record;

   No_Birthdate : constant birthdate :=
     (Month_Number'First, Day_Number'First, Year_Number'First);
   No_Number : constant Phone_Number := (others => '0');

   type Address_Type is record
      Street    : Unbounded_String := Null_Unbounded_String;
      Apartment : Unbounded_String := Null_Unbounded_String;
      City      : Unbounded_String := Null_Unbounded_String;
      State     : Unbounded_String := Null_Unbounded_String;
      zip       : zip_code         := "00000";
   end record;

   No_Address : constant Address_Type :=
     (Null_Unbounded_String, Null_Unbounded_String, Null_Unbounded_String,
      Null_Unbounded_String, "00000");

   type Person_Data is tagged record
      Name    : Person_Name      := Null_Bounded_String;
      Id      : Person_Id        := Null_Unbounded_String;
      Dob     : birthdate        := No_Birthdate;
      Phone   : Phone_Number     := No_Number;
      Address : Address_Type     := No_Address;
      Email   : Unbounded_String := Null_Unbounded_String;
   end record;

end Person;
