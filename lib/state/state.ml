open Gameboard
open Player

let (<<) = Fun.compose

type game_state = {
  gameboard: gameboard;
  player: player;
}

let player_on_dgameboard (dgb: dgameboard) (p: player) : dgameboard =
  let (i, j) = p.pos in
  let dsq = dgb.(i).(j) in
  dsq.(2) <- Array.append (Array.sub dsq.(2) 0 3) (Array.sub dsq.(2) 4 3);
  dsq.(2).(3) <- p.icon;
  dgb

let draw_game_state (gs: game_state) : string =
  let sm = gs.gameboard.squares in
  (draw_dgameboard << (player_on_dgameboard << dgameboard_of_squares) sm) gs.player
