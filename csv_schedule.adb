pragma Ada_2012;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;

package body csv_schedule is

   package sched_vect is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Schedule_Line);
   use sched_vect;

   -- function String_To_Time reads a Schedule_Line, converting the
   -- Date string in the schedule line to an Ada Time value so
   -- that the schedule can be properly sorted by time and not
   -- simply by character values and month abbreviations stored
   -- in the Date field.

   function String_To_Time (Sched : Schedule_Line) return Time is
      Result   : Time;
      Day      : Day_Number;
      Month    : Month_Number;
      Year     : Year_Number;
      end_pos  : Natural;
      Mnth     : Unbounded_String;
      the_day  : Unbounded_String;
      the_Year : Unbounded_String;

      type months is
        (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   begin
      Mnth  := Unbounded_Slice (Source => Sched.Date, Low => 1, High => 3);
      Month :=
        Month_Number'Val (months'Pos (months'Value (To_String (Mnth))) + 1);
      end_pos :=
        Index
          (Source => Sched.Date, Pattern => ",", From => 4,
           Going  => Ada.Strings.Forward);
      the_day :=
        Unbounded_Slice (Source => Sched.Date, Low => 4, High => end_pos - 1);
      Day := Day_Number'Value (To_String (the_day));

      the_Year :=
        Unbounded_Slice
          (Source => Sched.Date, Low => end_pos + 1,
           High   => Length (Sched.Date));
      Year   := Year_Number'Value (To_String (the_Year));
      Result := Time_Of (Year => Year, Month => Month, Day => Day);
      return Result;

   end String_To_Time;

   ----------
   -- read --
   ----------

   -- Read the csv formatted schedule file one line at a time.
   -- Extract the date, employee, position, site, start, finish
   -- and hour fields. Create a Sort_Date field from the csv date
   -- field. Append each Sched_Line to Temp_Vect.
   --
   -- After all file lines are processed convert the Temp_vect to
   -- a Whole_Schedule array and return the array.

   function read (File : String) return Whole_Schedule is
      The_Line   : Unbounded_String;
      Sched_Line : Schedule_Line;
      The_File   : File_Type;
      Count      : Natural := 0;

      Temp_Vect : Vector := Empty_Vector;

      start_Pos     : Positive;
      end_Pos       : Natural;
      Date_Pattern  : constant String := "" & '"';
      Field_Pattern : constant String := ",";
   begin
      Open (File => The_File, Mode => In_File, Name => File);

      -- Read the lines of the schedule file

      while not End_Of_File (The_File) loop

         -- read the file one line at a time

         The_Line := Get_Line (The_File);
         Count    := Count + 1;

         -- skip header row
         start_Pos := 9;
         if Count > 1 then

            -- extract Date field

            end_Pos :=
              Index
                (Source => The_Line, Pattern => Field_Pattern,
                 From   => start_Pos, Going => Ada.Strings.Forward);
            Sched_Line.Date :=
              Unbounded_Slice
                (Source => The_Line, Low => 2, High => end_Pos - 2);
            Sched_Line.Sort_Date := String_To_Time (Sched_Line);
            start_Pos            := end_Pos + 1;

            -- get Employee field
            end_Pos :=
              Index
                (Source => The_Line, Pattern => Field_Pattern,
                 From   => start_Pos, Going => Ada.Strings.Forward);
            Sched_Line.Employee :=
              Unbounded_Slice
                (Source => The_Line, Low => start_Pos, High => end_Pos - 1);
            start_Pos := end_Pos + 2;
            -- Get position field
            end_Pos :=
              Index
                (Source => The_Line, Pattern => Field_Pattern,
                 From   => start_Pos, Going => Ada.Strings.Forward);
            Sched_Line.Position :=
              Unbounded_Slice
                (Source => The_Line, Low => start_Pos, High => end_Pos - 1);
            start_Pos := end_Pos + 2;
            -- get site field
            end_Pos :=
              Index
                (Source => The_Line, Pattern => Field_Pattern,
                 From   => start_Pos, Going => Ada.Strings.Forward);
            Sched_Line.Site :=
              Unbounded_Slice
                (Source => The_Line, Low => start_Pos, High => end_Pos - 1);
            start_Pos := end_Pos + 2;
            -- get start field
            end_Pos :=
              Index
                (Source => The_Line, Pattern => Field_Pattern,
                 From   => start_Pos, Going => Ada.Strings.Forward);
            Sched_Line.Start :=
              Unbounded_Slice
                (Source => The_Line, Low => start_Pos, High => end_Pos - 1);
            start_Pos := end_Pos + 1;
            -- get finish field
            end_Pos :=
              Index
                (Source => The_Line, Pattern => Field_Pattern,
                 From   => start_Pos, Going => Ada.Strings.Forward);
            Sched_Line.Finish :=
              Unbounded_Slice
                (Source => The_Line, Low => start_Pos, High => end_Pos - 1);
            start_Pos := end_Pos + 2;
            -- get hour field
            end_Pos :=
              Index
                (Source => The_Line, Pattern => Field_Pattern,
                 From   => start_Pos, Going => Ada.Strings.Forward);
            Sched_Line.Hours :=
              Unbounded_Slice
                (Source => The_Line, Low => start_Pos, High => end_Pos - 1);

            -- append the populated Sched_Line to Temp_Vect
            Temp_Vect.Append (New_Item => Sched_Line);
         end if;
      end loop;

      declare
         Result : Whole_Schedule (1 .. Natural (Temp_Vect.Length) - 1);
      begin
         for I in Result'Range loop
            Result (I) := Element (Container => Temp_Vect, Index => I);
         end loop;
         return Result;
      end;
   end read;

   ----------
   -- Sort --
   ----------

   -- Sort the Whole_Schedule array by Sort_Date, changing the
   -- Sched parameter and not returning a sorted copy of the
   -- Whole_Schedule.

   procedure Sort (Sched : in out Whole_Schedule) is

      function less
        (Left : Schedule_Line; Right : Schedule_Line) return Boolean is
        (Left.Sort_Date < Right.Sort_Date);

      procedure Sched_sort is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Schedule_Line,
         Array_Type => Whole_Schedule, "<" => less);

   begin
      Sched_sort (Sched);
   end Sort;

   -----------
   -- Print --
   -----------

   -- Print an instance of Whole_Schedule.
   -- This instance may be a sorted schedule, an unsorted schedule or
   -- a subset of a schedule following selection by a particular
   -- employee.

   procedure Print (Sched : in Whole_Schedule) is
      Temp : Whole_Schedule := Sched;
   begin

      Put_Line ("Sorted:");
      Sort (Temp);
      for line of Temp loop
         Put_Line
           (To_String (line.Date) & " | " & To_String (line.Employee) & " | " &
            To_String (line.Position) & " | " & To_String (line.Site) & " | " &
            To_String (line.Start) & " | " & To_String (line.Finish) & " | " &
            To_String (line.Hours));
      end loop;
   end Print;

   ---------------
   -- selection --
   ---------------

   -- Create a subset of the schedule passed as the parameter
   -- Sched, selecting only the Sched array elements with
   -- employee fields matching the Employee parameter.
   -- Return the subset schedule.
   --
   -- The Sched parameter may be either a sorted schedule or an
   -- unsorted schedule.

   function selection
     (Sched : in Whole_Schedule; Employee : Unbounded_String)
      return Whole_Schedule
   is
      Temp : Vector;

   begin
      for line of Sched loop
         if line.Employee = Employee then
            Append (Container => Temp, New_Item => line);
         end if;
      end loop;
      declare
         Result : Whole_Schedule (1 .. Natural (Length (Temp)));
      begin
         for I in Result'Range loop
            Result (I) := Element (Container => Temp, Index => I);
         end loop;
         return Result;
      end;

   end selection;

end csv_schedule;
