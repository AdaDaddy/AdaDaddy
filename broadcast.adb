-----------------------------------------------------------------------
--             Tres & Associates Server Components                   --
--                                                                   --
--                      package Broadcast                            --
--                                                                   --
--                            Body                                   --
--                                                                   --
--             Copyright (C) 2022 Tres & Associates                  --
-----------------------------------------------------------------------

pragma Ada_2012;
with Ada.Containers.Vectors;

package body Broadcast is

   -- The package maintains a singleton, per instantiation, of a vector
   -- of Buffer_Access. Each subscribed consumer of the instantiated
   -- Bufffer protected type has an individual Buffer object created
   -- upon registration. The Buffer_Access value the the dynamically
   -- created Buffer is added to the Buffer_Vector and simultaneously
   -- returned to the registering task.

   package Buffer_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Buffer_Access);
   use Buffer_Vector;

   -- The protected object subscription is a singleton for each
   -- instance of the broadcast package. It is implemented as
   -- a protected object to eliminate all race conditions arising
   -- from consumers registering, and therefore modifying the
   -- list of buffer_accese and the producer attempting to
   -- write to each buffer accessed in the list.

   protected subscription is
      procedure register (The_Buffer : out Buffer_Access);
      procedure broadcast (msg : in Element_Type);
   private
      List : Buffer_Vector.Vector := Buffer_Vector.Empty_Vector;
   end subscription;

   protected body subscription is

      -- register the receiving task and in the process add a
      -- new buffer_access element to the List

      procedure register (The_Buffer : out Buffer_Access) is
         Temp : Buffer_Access := new Buffer;
      begin
         List.Append (Temp);
         The_Buffer := Temp;
      end register;

      -- write the value of Msg to every element of List

      procedure broadcast (Msg : in Element_Type) is
      begin
         for receiver of List loop
            receiver.Write (Msg);
         end loop;
      end broadcast;

   end subscription;

   ------------
   -- Buffer --
   ------------

   protected body Buffer is

      -----------
      -- Write --
      -----------

      -- Write a value to the Buffer. Write is called by
      -- the Broadcast procedure.

      procedure Write (Item : in Element_Type) is
      begin
         Message := Item;
         Is_New  := True;
      end Write;

      ----------
      -- Read --
      ----------

      -- Read a value from the Buffer.
      -- Read is called by the registered task.

      entry Read (Item : out Element_Type) when Is_New is
      begin
         Item   := Message;
         Is_New := False;
      end Read;

   end Buffer;

   -- This function modifies the subscription list while
   -- hiding the entire subscription list from external tasks.
   -- The newly created Buffer_access value is returned to
   -- the calling task.

   function register return Buffer_Access is
      temp : Buffer_Access;
   begin
      subscription.register (temp);
      return temp;
   end register;

   -- This procedure call the broadcast procedure within
   -- the subscription protected object while hiding the
   -- subscription protected object from all external tasks.

   procedure broadcast (msg : in Element_Type) is
   begin
      subscription.broadcast (msg);
   end broadcast;

end Broadcast;
