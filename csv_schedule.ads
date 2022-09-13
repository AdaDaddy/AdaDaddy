-----------------------------------------------------------------------
--             Tres & Associaates Server Components                  --
--                                                                   --
--                   csv_schedule package                            --
--                                                                   --
--                           Spec                                    --
--                                                                   --
--              Copyright (c) 2022 Tres & Associates                 --
--                                                                   --
-----------------------------------------------------------------------

-- This package reads a csv formatted schedule from WhyIWork,
-- sorts the schedule by date and allows the selection of the schedule
-- for an individual employee name.
--
-- The package also provides the ability to print the schedule as a
-- sorted or unsorted schedule and as a selected subset or as the
-- entire schedule.
-----------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;          use Ada.Calendar;

package csv_schedule is

   type Schedule_Line is private;
   type Whole_Schedule is array (Positive range <>) of Schedule_Line;

   function read (File : String) return Whole_Schedule;
   procedure Sort (Sched : in out Whole_Schedule);
   procedure Print (Sched : in Whole_Schedule);
   function selection
     (Sched : in Whole_Schedule; Employee : Unbounded_String)
      return Whole_Schedule;

private

   type Schedule_Line is record
      Sort_Date : Time;
      Date      : Unbounded_String := Null_Unbounded_String;
      Employee  : Unbounded_String := Null_Unbounded_String;
      Position  : Unbounded_String := Null_Unbounded_String;
      Site      : Unbounded_String := Null_Unbounded_String;
      Start     : Unbounded_String := Null_Unbounded_String;
      Finish    : Unbounded_String := Null_Unbounded_String;
      Hours     : Unbounded_String := Null_Unbounded_String;
   end record;

end csv_schedule;
