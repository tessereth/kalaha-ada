with Board_Package;
generic
   with package Board_Package_I is new Board_Package (<>);
package Basic_AI is
   use Board_Package_I;
   type Kalaha_AI is interface;

   function Next (Ai : Kalaha_AI; Board : Board_T; Player : Player_T) return Side_Index is abstract;

   type Human_AI is new Kalaha_AI with null record;

   overriding function Next (Ai : Human_AI; Board : Board_T; Player : Player_T) return Side_Index;

   type First_Valid_AI is new Kalaha_AI with null record;

   overriding function Next (Ai : First_Valid_AI; Board : Board_T; Player : Player_T) return Side_Index;

   type Random_AI is new Kalaha_AI with null record;

   overriding function Next (Ai : Random_AI; Board : Board_T; Player : Player_T) return Side_Index;

   subtype Huristic_Val is Integer;

   type Huristic_Ptr is access function (Board : Board_T;
                                         Player : Player_T) return Huristic_Val;

   type Minimax_AI is new Kalaha_AI with record
      Huristic : Huristic_Ptr;
      Depth : Positive;
   end record;

   overriding function Next (Ai : Minimax_AI;
                             Board : Board_T;
                             Player : Player_T) return Side_Index;

   function Pond_Diff (Board : Board_T; Player : Player_T) return Huristic_Val;

end Basic_AI;
