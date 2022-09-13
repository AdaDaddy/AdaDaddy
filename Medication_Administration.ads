with Ada.Strings.Unbounded_String; use Ada.Strings.Unbounded_String;

package Medication_Administration is
    type Admnistration_Record (Medication_Given : Boolean) is tagged private;
private
   type Reason_Type is (OOB, Missing_Medication, Resident_Refused, Other);

   Admin_Time : String; -- formatted as MM DD YYYY HH Minutes
   type Administration_Record (Medication_Given : Boolean) is tagged record
       Patient_Id      : Unbounded_String;
       Scheduled_Time  : Admin_Time;
       Medication_Name : Unbounded_String; -- Copied from schedule
       case Medication_Given is
          when False =>
             Reason      : Reason_Type := Resident_Refused;
             Explanation : Unbounded_String := Null_Unbounded_String;
          when True =>
             Null;
       end case;
   end record;
end Medication_Administration;