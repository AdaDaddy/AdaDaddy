-----------------------------------------------------------------------
--                Tres & Associates Server Components                --
--                                                                   --
--                   procedure Test_Generic_Buffer                   --
--                                                                   --
--                              body                                 --
--                                                                   --
--               Copyright (C) 2022 Tres & Associates                --
--                                                                   --
-- This procedure executes the Generic_Buffer package.               --
-- This example creates one task type for the consumer tasks         --
-- 2 instances of the Consumer task type are created and executed    --
--                                                                   --
-- The main procedure executes in the main task and acts as the      --
-- producer.                                                         --
-- The main procedure instantiates the Generic_Buffer package using  --
-- Integer as the generic parameter element_type and 16 as the       --
-- generic parameter Buff_Size.                                      --
-- The producer produces 32 values and then signifies the end of     --
-- data by filling the buffer with Integer'First values.             --
-- Each time the main procedure writes 16 values to the buffer it    --
-- delays (sleeps) for 10.0 seconds before writing more values to    --
-- the buffer.                                                       --
-----------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Generic_Buffer;

procedure Test_Generic_Buffer is
   package int_buffer is new Generic_Buffer(element_type => Integer,
                                            Buff_Size    => 16);
   The_Buffer : aliased Int_Buffer.Buff_Type;
   type Buffer_Access is access all Int_Buffer.Buff_type;

   Buff_Ptr : Buffer_Access := The_Buffer'Access;

   task type consumer (Buf : Buffer_Access);

   task body consumer is
      Num : Integer;
   begin
      loop
         Buf.Read(Num);
         Put_line("Consumer read" & Num'Image);
         delay 0.01;
         exit when Num = Integer'First;
      end loop;
      Put_line("Consumer terminating.");
   end Consumer;

   C1    : Consumer(Buff_Ptr);
   C2    : Consumer(Buff_Ptr);
   Batch : Natural := 0;

begin
   Put_Line("The main task is the producer.");
   for I in 1..32 loop
      Put_Line("Producer produced" & I'Image);
      The_Buffer.Write(I);
      Batch := Batch + 1;
      if Batch = 16 then
         Batch := 0;
         Put_Line("Producer delaying between data batches");
         delay 10.0;
      end if;
   end loop;
   for I in 1..10 loop
      The_Buffer.Write(Integer'First);
   end loop;
   Put_Line("Producer terminating.");
end Test_Generic_Buffer;