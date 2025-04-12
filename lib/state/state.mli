open Gameboard
open Player

type game_state = {
  gameboard: gameboard;
  player: player;
}

val player_on_dgameboard : dgameboard -> player -> dgameboard

val draw_game_state : game_state -> string