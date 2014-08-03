package body Tree_AI is

   overriding function Next (Ai : Minimax_AI; Board : Board_T; Player : Player_T) return Side_Index is
   begin
      return Side_Index'Invalid_Value;
   end Next;

end Tree_AI;
