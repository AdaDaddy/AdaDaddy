-----------------------------------------------------------------------
--                Tres & Associates Server Components                --
--                                                                   --
--                        package Generic_Buffer                     --
--                                                                   --
--                              SPEC                                 --
--                                                                   --
--               Copyright (C) 2022 Tres & Associates                --
--                                                                   --
-- This package declares a generic buffer to be used in a producer   --
-- consumer pattern allowing N producers and M consumers to share    --
-- a buffer where N > 0 and M > 0.                                   --
-- This buffer supports guaranteed delivery of every produced value  --
--                                                                   --
-- Generic parameters : element_type is the type of data contained   --
--                      in the shared buffer                         --
-- Buff_Size : The capacity of the shared buffer counted in          --
--             element_type objects                                  --
-----------------------------------------------------------------------

generic
   type element_type is private;
   Buff_Size : Positive;
package Generic_Buffer is
   subtype Buff_Index is Integer range 0 .. Buff_Size - 1;
   type buff_Array is array (Buff_Index) of element_type;

   protected type Buff_type is
      entry Write (Item : in element_type);
      entry Read (Item : out element_type);
   private
      Buffer    : buff_Array;
      Write_Idx : Buff_Index := 0;
      Read_Idx  : Buff_Index := 0;
      Count     : Natural    := 0;
   end Buff_type;

end Generic_Buffer;
