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

begin
   Reset (Side_Gen);
end Basic_AI;
