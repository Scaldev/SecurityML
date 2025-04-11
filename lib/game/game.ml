open Gameboard
open Player

let player_on_gameboard (p: player) (dgb: dgameboard) : dgameboard =
  let (i, j) = p.pos in
  let dsq = dgb.(i).(j) in
  dsq.(2) <- Array.append (Array.sub dsq.(2) 0 3) (Array.sub dsq.(2) 4 3);
  dsq.(2).(3) <- p.icon;
  dgb;