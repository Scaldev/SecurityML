open Gameboard

type player_state = Idle | Hiding | Tasking | Dead

type player = {
  mutable pos: int * int;
  mutable state: player_state;
  mutable battery: int;
  icon: string
}

let is_accessible (e: edge) : bool =
  match e with
  | Space | Curtain | Door { closed=false } -> true
  | _ -> false

let can_access_north (gb: gameboard) (i: int) (j: int) : bool =
  i > 0 && is_accessible gb.squares.(i).(j).north

let can_access_south (gb: gameboard) (i: int) (j: int) : bool =
  i < gb.height - 1 && is_accessible gb.squares.(i+1).(j).north

let can_access_west (gb: gameboard) (i: int) (j: int) : bool =
  j > 0 && is_accessible gb.squares.(i).(j).west

let can_access_east (gb: gameboard) (i: int) (j: int) : bool =
  j < gb.width - 1 && is_accessible gb.squares.(i).(j+1).west


let player_move_aux (gb: gameboard) (p: player) (can_access_dir: gameboard -> int -> int -> bool) (dst: square_id) : bool =
  let (i,j) = p.pos in
  if can_access_dir gb i j then (
    p.pos <- dst; true;
  ) else false

let player_move (gb: gameboard) (p: player) (d: direction) : bool =
  let (i,j) = p.pos in
  let (can_access_dir, dst) = match d with
    | North -> (can_access_north, (i-1, j))
    | South -> (can_access_south, (i+1, j))
    | West  -> (can_access_west , (i, j-1))
    | East  -> (can_access_east , (i, j+1))
in player_move_aux gb p can_access_dir dst