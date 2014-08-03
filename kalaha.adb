with Ada.Text_IO; use Ada.Text_IO;
with Board_Package;
with Basic_AI;
-- with Tree_AI;

procedure Kalaha is
   type Player_T is (A, B);
   package Board_6_6 is new Board_Package (6, 6, Player_T);
   use Board_6_6;
   package Basic_AI_I is new Basic_AI (Board_6_6);
   use Basic_AI_I;
   -- package Tree_AI_I is new Tree_AI (Basic_AI_I);
   -- use Tree_AI_I;

   Board : Board_T := Initial_Board;
   Current_Player : Player_T := Player_T'First;
   Next_Player : Player_T;
   AIs : constant array (Player_T) of access Kalaha_AI'Class :=
     (new Random_AI, new Minimax_AI'(Huristic => Pond_Diff'Access, Depth => 6));
   Choice : Side_Index;
begin
   Put_Line ("Welcome to Kalaha!");
   New_Line;

   while not Finished (Board) loop
      Put_Line (Board.Pretty_String (Current_Player));
      Put ("Player " & Current_Player'Img & ": ");
      Choice := AIs (Current_Player).all.Next (Board, Current_Player);
      Put_Line (Choice'Img);
      Board.Move (Current_Player, Choice, Next_Player);
      Current_Player := Next_Player;
   end loop;
   Put_Line ("Game has finished");
   Put_Line (Board.Pretty_String (Current_Player));
   Put_Line ("Player " & Current_Player'Img);
   Put_Line ("Congratulations Player " & Winner (Board)'Img & " on winning!");
end Kalaha;
