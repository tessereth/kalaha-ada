with Basic_AI;

generic
   with package Basic_AI_I is new Basic_AI (<>);

package Tree_AI is
   use Basic_AI_I;
   use Board_Package_I;

   type Minimax_AI is new Kalaha_AI with null record;

   overriding function Next (Ai : Minimax_AI; Board : Board_T; Player : Player_T) return Side_Index;

end Tree_AI;
