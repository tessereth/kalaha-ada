with Basic_AI;

generic
   with package Basic_AI_I is new Basic_AI (<>);

package Tree_AI is
   use Basic_AI_I;
   use Board_Package_I;

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

end Tree_AI;
