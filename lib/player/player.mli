open Gameboard

type player_state = Idle | Hiding | Tasking | Dead

type player = {
  mutable pos: int * int;
  mutable state: player_state;
  mutable battery: int;
  icon: string
}

val player_move : gameboard -> player -> direction -> bool