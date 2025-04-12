open Gameboard
open Player

type game_state = {
  gameboard: gameboard;
  player: player;
}

val player_on_gameboard : player -> dgameboard -> dgameboard