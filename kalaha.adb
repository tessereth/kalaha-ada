with Ada.Text_IO; use Ada.Text_IO;
with Board_Package;
with Ada.IO_Exceptions;

procedure Kalaha is
   type Player_T is (A, B);
   package Board_6_6 is new Board_Package (6, 6, Player_T);
   use Board_6_6;
   package Move_IO is new Integer_IO (Side_Index);
   use Move_IO;

   Board : Board_T := Initial_Board;
   Current_Player : Player_T := Player_T'First;
   Next_Player : Player_T;
   Choice : Side_Index;
begin
   Put_Line ("Welcome to Kalaha!");
   New_Line;

   Put_Line ("Enter a number between 1 and 6 to move.");
   while not Finished (Board) loop
      Put_Line (Board.Pretty_String (Current_Player));
      Put_Line ("Player " & Current_Player'Img & ": ");
      declare
      begin
         Get (Choice);
         Board.Move (Current_Player, Choice, Next_Player);
         Current_Player := Next_Player;
      exception
         when Invalid_Board_Index =>
            Put_Line ("Invalid move. Try again.");
         when Ada.IO_Exceptions.Data_Error =>
            Put_Line ("Enter a move between 1 and 6.");
            Skip_Line;
      end;
   end loop;
   Put_Line ("Game has finished");
   Put_Line (Board.Pretty_String (Current_Player));
   Put_Line ("Player " & Current_Player'Img);
   Put_Line ("Congratulations Player " & Winner (Board)'Img & " on winning!");
end Kalaha;
