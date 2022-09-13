-----------------------------------------------------------------------
--                Tres & Associates Server Components                --
--                                                                   --
--                        package Broadcast                          --
--                                                                   --
--                              SPEC                                 --
--                                                                   --
--               Copyright (C) 2022 Tres & Associates                --
--                                                                   --
-- This package creates an vector of pointers to a generic protected --
-- object. As each consumer registers to receive the message type    --
-- used to instantiate the generic protected object the a new        --
-- protected object is created and the access value of that object   --
-- is passed back to the consumer. The access value is also appended --
-- to a vector of access values so that the producer will be able to --
-- send data to each protected object accessed in the vector.        --
-----------------------------------------------------------------------

generic
   type Element_Type is private;
package Broadcast is

   -- Consumer calls the Read entry for its registered buffer object.
   -- Producer calls the Broadcast procedure to send the data to all
   -- registered consumers.
   --
   -- Buffer is a single element buffer. The consumer will always read
   -- the most recent value read to Buffer but will not read
   -- the same message instance more than once.

   protected type Buffer is
      procedure Write (Item : in Element_Type);
      entry Read (Item : out Element_Type);
   private
      Message   : Element_Type;
      Is_New    : Boolean := False;
   end Buffer;

   type Buffer_Access is access Buffer;

   -- Register function dynamically allocates a Buffer object,
   -- adds the object access to an internal vector and returns
   -- the object access to the consumer registering to receive
   -- the data.
   function register return Buffer_Access;

   -- Broadcast procedure iterates through the internal vector of
   -- buffer_access calling the write procedure for each element in
   -- the vector.
   procedure Broadcast ( Msg : in Element_Type);
end Broadcast;
