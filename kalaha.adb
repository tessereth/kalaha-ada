with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Board_Package;
with Ada.IO_Exceptions;

procedure Kalaha is
   package Board_6_6 is new Board_Package(6, 6);
   use Board_6_6;
   package Move_IO is new Integer_IO (Side_Index);
   use Move_IO;

   Board : Board_T := Initial_Board;
   Current_Player : Player_T := Player_T'First;
   Next_Player : Player_T;
   Move : Side_Index;
begin
   Put_Line("Welcome to Kalaha!");
   New_Line(1);

   Put_Line("Enter a number between 1 and 6 to move.");
   while not Finished(Board) loop
      Put_Line(Board.To_String);
      Put_Line("Player " & Current_Player'Img & ": ");
      declare
      begin
         Get(Move);
         Board.Move(Current_Player, Move, Next_Player);
         Current_Player := Next_Player;
      exception
         when Ada.IO_Exceptions.Data_Error |
              Invalid_Board_Index =>
            Put_Line ("Invalid move. Try again.");
            delay 1.0;
      end;
   end loop;
   Put_Line("Game has finished");
   Put_Line("Congratulations Player " & Winner(Board)'img & " on winning!");
end Kalaha;
