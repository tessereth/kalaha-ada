generic
   Side_Width : Positive;
   Initial_Seeds : Positive;

package Board_Package is

   subtype Side_Index is Integer range 0 .. Side_Width;
   subtype Seed_Count is Integer range 0 .. Initial_Seeds * Side_Width * 2;
   type Side_T is array (Side_Index) of Seed_Count;
   subtype Pond_T is Seed_Count;
   type Player_T is (A, B);
   type Sides_T is array (Player_T) of Side_T;
   type Ponds_T is array (Player_T) of Pond_T;

   type Board_T is tagged record
      Sides : Sides_T;
      Ponds : Ponds_T;
   end record;

   type Board_Index is record
      Player : Player_T;
      Is_Pond : Boolean;
      Side_Idx : Side_Index;
   end record;

   procedure Move(Board : in out Board_T; Player : in Player_T; Choice : in Side_Index; Next_Player : out Player_T);
   procedure Move(Board : in out Board_T; Board_Idx : Board_Index; Next_Player : out Player_T);
   function Finished(Board : Board_T) return Boolean;
   function Winner(Board : Board_T) return Player_T;
   procedure Reset(Board : in out Board_T);
   function Get(Board : in Board_T; Board_Idx : in Board_Index) return Seed_Count;
   procedure Set(Board : in out Board_T; Board_Idx : in Board_Index; Value : Seed_Count);
   procedure Add(Board : in out Board_T; Board_Idx : in Board_Index; Value : Seed_Count);
   procedure Inc(Board : in out Board_T; Board_Idx : in Board_Index);
   function Next(Board_Idx : Board_Index; Player : in Player_T) return Board_Index;
   procedure Next(Board_Idx : in out Board_Index; Player : in Player_T);
   function To_String(Board : in Board_T) return String;

   Initial_Board : constant Board_T :=
     (Sides => (others => (others => Initial_Seeds)),
      Ponds => (others=> Pond_T'First));

   Game_Not_Finished : exception;
   Invalid_Board_Index : exception;

end Board_Package;