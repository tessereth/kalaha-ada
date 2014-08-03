with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Numerics.Discrete_Random;

package body Basic_AI is

   package Side_Random is new Ada.Numerics.Discrete_Random (Side_Index);
   use Side_Random;
   Side_Gen : Generator;

   overriding function Next (Ai : Human_AI; Board : Board_T; Player : Player_T) return Side_Index is
      pragma Unreferenced (Ai);
      package Move_IO is new Integer_IO (Side_Index);
      use Move_IO;
      Choice : Side_Index;
      Valid : Boolean := False;
   begin
      while not Valid loop
         declare
         begin
            Get (Choice);
            Valid := Valid_Move (Board, (Player, False, Choice));
            if not Valid then
               Put_Line ("Invalid Move. Try again.");
               Put_Line (Board.Pretty_String (Player));
               Put_Line ("Player " & Player'Img & ": ");
            end if;
         exception
            when Ada.IO_Exceptions.Data_Error =>
               Skip_Line;
               Put_Line ("Enter a move between 1 and 6.");
               Put_Line (Board.Pretty_String (Player));
               Put_Line ("Player " & Player'Img & ": ");
         end;
      end loop;
      return Choice;
   end Next;

   overriding function Next (Ai : First_Valid_AI; Board : Board_T; Player : Player_T) return Side_Index is
      pragma Unreferenced (Ai);
   begin
      for S in Side_Index'Range loop
         if Valid_Move (Board, (Player, False, S)) then
            return S;
         end if;
      end loop;
      return Side_Index'Invalid_Value;
   end Next;

   overriding function Next (Ai : Random_AI; Board : Board_T; Player : Player_T) return Side_Index is
      pragma Unreferenced (Ai);
      Choice : Side_Index := Random (Side_Gen);
   begin
      while not Valid_Move (Board, (Player, False, Choice)) loop
         Choice := Random (Side_Gen);
      end loop;
      return Choice;
   end Next;

   function Minimax (Ai : Minimax_AI;
                     Board : Board_T;
                     Player, Current_Player : Player_T;
                     Depth : Natural) return Huristic_Val is
      Take_Max : constant Boolean := Current_Player = Player;
      Score : Huristic_Val;
      Best_Score : Huristic_Val;
      New_Board : Board_T;
      Next_Player : Player_T;
   begin
      if Depth = 0 or else Board.Finished then
         return Ai.Huristic (Board, Player);
      end if;
      if Take_Max then
         Best_Score := Huristic_Val'First;
      else
         Best_Score := Huristic_Val'Last;
      end if;
      for S in Side_Index'Range loop
         if Board.Valid_Move ((Current_Player, False, S)) then
            New_Board := Board;
            New_Board.Move (Current_Player, S, Next_Player);
            Score := Minimax (Ai, New_Board, Player, Next_Player, Depth - 1);
            if Take_Max then
               Best_Score := Huristic_Val'Max (Best_Score, Score);
            else
               Best_Score := Huristic_Val'Min (Best_Score, Score);
            end if;
         end if;
      end loop;
      return Best_Score;
   end Minimax;

   overriding function Next (Ai : Minimax_AI;
                             Board : Board_T;
                             Player : Player_T) return Side_Index is
      New_Board : Board_T;
      Next_Player : Player_T;
      Score : Huristic_Val;
      Best_Score : Huristic_Val := Huristic_Val'First;
      Best_Move : Side_Index := Side_Index'Invalid_Value;
   begin
      for S in Side_Index'Range loop
         if Board.Valid_Move ((Player, False, S)) then
            New_Board := Board;
            New_Board.Move ((Player, False, S), Next_Player);
            Score := Minimax (Ai, Board, Player, Next_Player, Ai.Depth);
            if Score >= Best_Score then
               Best_Score := Score;
               Best_Move  := S;
            end if;
         end if;
      end loop;
      return Best_Move;
   end Next;

   function Pond_Diff (Board : Board_T; Player : Player_T) return Huristic_Val is
   begin
      return Huristic_Val (Board.Ponds (Player) - Board.Ponds (Next (Player)));
   end Pond_Diff;

begin
   Reset (Side_Gen);
end Basic_AI;
