-----------------------------------------------------------------------
--              TresCare Server Components                           --
--                                                                   --
--                     Med_Schedulese                                --
--                                                                   --
--                        Spec                                       --
--                                                                   --
--           Copyright (c) Tres & Associates 2022                    --
--                                                                   --
-- This package contains all the information needed to define a      --
-- Meds schedule in the eMAR system backend software.                --
--                                                                   --
-- This package defines the type Medicine_Schedule_Record which      --
-- contains the physician type from the Person.physician package,    --
-- the scheduled days for each dose, the medicine name and the list  --
-- of hours during a dosage day when the medication should be        --
-- administered.                                                     --
--                                                                   --
-- Although several residents may receive the same medication each   --
-- medication schedule must be specific to each resident. There will --
-- be expected differences in the prescibing physician, dosage       --
-- amount, dosage days and dosage times from one resident to another --
-----------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Person.physician;      use Person.physician;

package med_schedules is

   -- define an enumeration for abbreviations for the days of
   -- the week.
   type days_of_week is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   -- publicly declare the private type
   -- medicine_schedule_record
   type medicine_schedule_record is private;

private
   -- Type scheduled_days is an array of Boolean values
   -- indexed by days_of_week. For each schedule record the
   -- days a medication is to be given will have a True value
   -- and the days the medication is not to be given will
   -- have a False value

   type scheduled_days is array (days_of_week) of Boolean with
      Default_Component_Value => False;

   type med_hour is mod 24;

   type med_minute is mod 60;

   -- Define a record containing the Hour (24 hour clock) and
   -- minute values when a medication should be administered

   type med_time is record
      Hour   : med_hour;
      Minute : med_minute;
   end record;

   -- Define a Vector of med_time records to support
   -- administration of a medication multiple times during
   -- a scheduled day

   package med_vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => med_time);

   subtype Med_Times is med_vector.Vector;

   -- Define the medicine_schedule_record which will contain
   -- the medicine administration schedule for one medication
   -- for one resident

   type medicine_schedule_record is record
      Medicine_Name         : Unbounded_String;
      Dosage                : Unbounded_String;
      Dosage_Days           : scheduled_days;
      Prescribing_Physician : physician;
      Dosage_Times          : Med_Times := med_vector.Empty_Vector;
      Special_Instructions : Unbounded_String;
   end record;

end med_schedules;
