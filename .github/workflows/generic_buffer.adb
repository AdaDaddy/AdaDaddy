-----------------------------------------------------------------------
--                Tres & Associates Server Components                --
--                                                                   --
--                        package Generic_Buffer                     --
--                                                                   --
--                              Body                                 --
--                                                                   --
--               Copyright (C) 2022 Tres & Associates                --
--                                                                   --
-- This package implements a generic buffer to be used in a producer --
-- consumer pattern allowing N producers and M consumers to share    --
-- a buffer where N > 0 and M > 0.                                   --
-- This buffer supports guaranteed delivery of every produced value  --
--                                                                   --
-- Generic parameters : element_type is the type of data conatined   --
--                      in the shared buffer                         --
-- Buff_Size : The capacity of the shared buffer counted in          --
--             element_type objects                                  --
-----------------------------------------------------------------------

package body Generic_Buffer is

   ---------------
   -- Buff_type --
   ---------------

   protected body Buff_type is

      -----------
      -- Write --
      -----------

      entry Write (Item : in element_type) when Count < Buff_Size is
      begin
         Buffer(Write_Idx) := Item;
         Write_Idx := (Write_Idx + 1) mod Buff_Size;
         Count := Count + 1;
      end Write;

      ----------
      -- Read --
      ----------

      entry Read (Item : out element_type) when Count > 0 is
      begin
         Item := Buffer(Read_Idx);
         Read_Idx := (Read_Idx + 1) mod Buff_Size;
         Count := Count - 1;
      end Read;

   end Buff_type;

end Generic_Buffer;
