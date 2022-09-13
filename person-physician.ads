-----------------------------------------------------------------------
--              TresCare Server Components                           --
--                                                                   --
--                     Person.Physician                              --
--                                                                   --
--                        Spec                                       --
--                                                                   --
--           Copyright (c) Tres & Associates 2022                    --
--                                                                   --
-- This package contains all the information needed to define a      --
-- person in the eMAR system backend software.                       --
--                                                                   --
-- This package is a child package to the Person package.            --
-- The type physician extends the Person_Data type from the person   --
-- package adding three additional fields specific to a physician    --
-----------------------------------------------------------------------

package Person.physician is
   type physician is new Person_Data with private;

private
   type physician is new Person_Data with record
      Specialty    : Unbounded_String;
      Office_Hours : Unbounded_String;
      License      : Unbounded_String;
   end record;

end Person.physician;
