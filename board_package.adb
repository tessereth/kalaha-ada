with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body board_package is

   function Is_Zero(Side : Side_T) return Boolean is
   begin
      for I of Side loop
         if I /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero;


   function Finished(Board : Board_T) return Boolean is
   begin
      for P in Player_T'Range loop
         if Is_Zero(Board.Sides(P)) then
            return True;
         end if;
      end loop;
      return False;
   end Finished;

   function Next(Player : Player_T) return Player_T is
   begin
      if Player = Player_T'Last then
         return Player_T'First;
      else
         return Player_T'Succ(Player);
      end if;
   end Next;


   function Next(Board_Idx : Board_Index; Player : in Player_T) return Board_Index is
   begin
      if Board_Idx.Is_Pond then
         if Board_Idx.Player /= Player then
            -- a player cannot be in an opponents pond
            raise Invalid_Board_Index;
         end if;
         return (Player   => Next(Board_Idx.Player),
                 Is_Pond  => False,
                 Side_Idx => Side_Index'First);
      elsif Board_Idx.Side_Idx = Side_Index'Last then
         if Board_Idx.Player = Player then
            return (Player   => Board_Idx.Player,
                    Is_Pond  => True,
                    Side_Idx => Side_Index'First);
         else
            return (Player   => Next(Board_Idx.Player),
                    Is_Pond  => False,
                    Side_Idx => Side_Index'First);
         end if;
         else
         return (Player   => Board_Idx.Player,
                 Is_Pond  => Board_Idx.Is_Pond,
                 Side_Idx => Side_Index'Succ(Board_Idx.Side_Idx));
      end if;
   end Next;

   procedure Next(Board_Idx : in out Board_Index; Player : in Player_T) is
   begin
      Board_Idx := Next(Board_Idx, Player);
   end Next;

   function Opposite(Board_Idx : in out Board_Index) return Board_Index is
      New_Side : constant Side_Index :=
        Side_Index'Last - (Board_Idx.Side_Idx - Side_Index'First);
   begin
      if Board_Idx.Is_Pond then
         raise Invalid_Board_Index;
      end if;
      return (Next(Board_Idx.Player), False, New_Side);
   end Opposite;

   function Get(Board : in Board_T; Board_Idx : in Board_Index) return Seed_Count is
   begin
      if Board_Idx.Is_Pond then
         return Board.Ponds(Board_Idx.Player);
      else
         return Board.Sides(Board_Idx.Player)(Board_Idx.Side_Idx);
      end if;
   end Get;

   procedure Set(Board : in out Board_T; Board_Idx : in Board_Index; Value : Seed_Count) is
   begin
      if Board_Idx.Is_Pond then
         Board.Ponds(Board_Idx.Player) := Value;
      else
         Board.Sides(Board_Idx.Player)(Board_Idx.Side_Idx) := Value;
      end if;
   end Set;

   procedure Add(Board : in out Board_T; Board_Idx : in Board_Index; Value : Seed_Count) is
   begin
      if Board_Idx.Is_Pond then
         Board.Ponds(Board_Idx.Player) := Board.Ponds(Board_Idx.Player) + Value;
      else
         Board.Sides(Board_Idx.Player)(Board_Idx.Side_Idx) :=
           Board.Sides(Board_Idx.Player)(Board_Idx.Side_Idx) + Value;
      end if;
   end Add;

   procedure Inc(Board : in out Board_T; Board_Idx : in Board_Index) is
   begin
      Board.Add(Board_Idx, 1);
   end Inc;

   procedure Move_Valid(Board : in out Board_T; Board_Idx : Board_Index; Next_Player : out Player_T) is
      Seeds : constant Seed_Count := Get(Board, Board_Idx);
      Current_Player : constant Player_T := Board_Idx.Player;
      Board_I : Board_Index := Board_Idx;
   begin
      -- validate the move
      if Get(Board, Board_Idx) <= 0 then
         raise Invalid_Board_Index;
      end if;
      -- Move the seeds
      Board.Set(Board_I, 0);
      for I in 1 .. Seeds loop
         Next(Board_I, Current_Player);
         Board.Inc(Board_I);
      end loop;
      -- if finished in pond, same player again
      if Board_I.Is_Pond then
         Next_Player := Board_Idx.Player;
      else
         Next_Player := Next(Board_Idx.Player);
      end if;
      -- if finished on side and it was empty, claim
      if not Board_I.Is_Pond and then Get(Board, Board_I) = 1
        and then Get(Board, Opposite(Board_I)) > 0 then
         Board.Add((Current_Player, True, Side_Index'First), Get(Board, Opposite(Board_I)));
         Board.Set(Opposite(Board_I), 0);
      end if;
   end Move_Valid;

   procedure Move(Board : in out Board_T; Player : in Player_T; Choice : in Side_Index; Next_Player : out Player_T) is
   begin
      Move_Valid(Board, (Player, False, Choice), next_player);
   end Move;

   procedure Move(Board : in out Board_T; Board_Idx : Board_Index; Next_Player : out Player_T) is
   begin
      if Board_Idx.Is_Pond then
         raise Invalid_Board_Index;
      end if;
      Move_Valid(Board, Board_Idx, next_player);
   end Move;

   function Winner(Board : Board_T) return Player_T is
      Best_Player : Player_T := Player_T'First;
      Best_Score : Seed_Count := Seed_Count'First;
   begin
      if not Finished(Board) then
         raise Game_Not_Finished;
      end if;
      for P in Player_T'Range loop
         if Board.Ponds(P) > Best_Score then
            Best_Score := Board.Ponds(P);
            Best_Player := P;
         end if;
      end loop;
      return Best_Player;
   end Winner;

   procedure Reset(Board : in out Board_T) is
   begin
      Board := Initial_Board;
   end Reset;

   function To_String(Board : in Board_T) return String is
      Str : Unbounded_String := Null_Unbounded_String;
   begin
      Str := Str & "Board {(Sides => (";
      for P in Player_T'Range loop
         Str := Str & "(";
         for S in Side_Index'Range loop
            Str := Str & Board.Sides(P)(S)'img & ", ";
         end loop;
         Str := Str & "), ";
      end loop;
      Str := Str & "), Ponds => (";
      for P in Player_T'Range loop
         Str := Str & Board.Ponds(P)'Img;
      end loop;
      Str := Str & ")}";
      return To_String(Str);
   end To_String;

end Board_Package;
