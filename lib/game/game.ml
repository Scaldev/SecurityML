open Gameboard
open Player
open State
open Cli

let height = 3
let width = 3
let dim = (height, width)

let rooms = [
  { name="Room 1"; code="R1"; color="ffffff"; squares_id=[(0,0); (0,1)] };
  { name="Room 2"; code="R2"; color="ffffff"; squares_id=[(0,2); (1,2)] };
  { name="Room 3"; code="R3"; color="ffffff"; squares_id=[(1,0); (1,1)] };
  { name="Room 4"; code="R4"; color="ffffff"; squares_id=[(2,0); (2,1); (2,2)] };
]

let links = [
  ((0,1), W, Space);
  ((0,2), W, Door {closed=false});
  ((1,0), N, Door {closed=false});
  ((1,1), W, Space);
  ((1,2), N, Space);
  ((1,2), W, Vent {closed=false});
  ((2,0), N, Door {closed=false});
  ((2,1), W, Space);
  ((2,2), N, Door {closed=false});
  ((2,2), W, Space)
]

let game_start () = 

  let gb = init_gameboard dim links rooms in
  let p = { pos=(1,1); state=Idle; battery=100; icon="🤖" } in
  let gs = { gameboard=gb; player=p } in

  game_init gs