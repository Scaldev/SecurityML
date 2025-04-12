open Gameboard
open Player
open State

let (<<) = Fun.compose

type command = Move of direction

type 'a result = Success of 'a | Error of string

(**
 * @param res a string describing either a success or an error.
 * @return res after being formatted.
 *)
let string_of_result (res: string result) : string =
  match res with
  | Success s -> "Success: " ^ s
  | Error e -> "Error: " ^ e

(****************************************************************************************************)
(*                                                                                                  *)
(*                                         PARSING THE COMMAND                                      *)
(*                                                                                                  *)
(****************************************************************************************************)

(**
 * @param d a direction.
 * @return a list of string describing that direction.
 *)
let alias_of_direction (d: direction) : string list =
  match d with
  | North -> ["n"; "north"; "u"; "up"]
  | South -> ["s"; "south"; "d"; "down"]
  | West  -> ["w"; "west" ; "l"; "left"]
  | East  -> ["e"; "east" ; "r"; "right"]

(**
 * @param arg the argument of a move command.
 * @param the result of the command after parsing arg.
 *)
let parse_move (arg: string) : command result =
  let ds = List.filter (List.mem arg << alias_of_direction) [North; South; West; East] in
  match ds with
  | [d] -> Success (Move d)
  | _   -> Error "unknown move direction"

(**
 * @param s the string of a command.
 * @return either that command, or an error described by a string.
 *)
let parse_command (s: string) : command result =
  let words = String.split_on_char ' ' s in
  match words with
  | "move" :: arg :: _ | "mv" :: arg :: _ -> parse_move arg
  | _ -> Error "unknown command"

(****************************************************************************************************)
(*                                                                                                  *)
(*                                       PROCESSING THE COMMAND                                     *)
(*                                                                                                  *)
(****************************************************************************************************)

(**
 * Update the player position according to d.
 * @param gs a game state.
 * @param d a direction.
 * @return true iff the player has moved.
 *)
let process_move (gs: game_state) (d: direction) : string result =
  let has_moved = player_move gs.gameboard gs.player d in
  if has_moved then
    Success ("moved to the " ^ (string_of_direction d))
  else
    Error "cannot move in that direction"

(**
 * Update the game state according to the command.
 * @param gs a game state.
 * @param c a command.
 * @return true iff the game state has been mutated.
 *)
let process_command (gs: game_state) (c: command) : string result =
  match c with
  | Move d -> process_move gs d

(**
 * Update the game state according to the command, if possible.
 * @param gs a game state.
 * @param line a command line string.
 * @return the output result after parsing and processing that line.
 *)
let process_command_line (gs: game_state) (line: string) : string result =
  match parse_command line with
    | Success s -> process_command gs s
    | Error _ -> Error "unknown command"

(****************************************************************************************************)
(*                                                                                                  *)
(*                                             GAME LOOP                                            *)
(*                                                                                                  *)
(****************************************************************************************************)

(**
 * Clean the console and print a new game screen displaying gs.
 * @param gs a game state.
 *)
let game_draw (gs: game_state) : unit =
    let gb = player_on_gameboard gs.player (dgameboard_of_squares gs.gameboard.squares) in
    print_string "\027[2J\027[H";
    flush stdout;
    print_string (draw_dgameboard gb);;

(**
 * Draw the game and the UI according to gs and cmd_res.
 * @param gs a game state.
 * @param cmd_res the result of a command line input.
 *)
let cli_draw (gs: game_state) (cmd_res: string result) : unit =
  game_draw gs;
  print_string ("\n" ^ string_of_result cmd_res ^ "\n");
  print_string "> "

(**
 * Read and standard input for a command line,
 * parse and process it and redraw the game screen.
 * @param gs a game state.
 *)
let rec game_loop (gs: game_state) : unit =
  let line = read_line () in
  let cmd_res = process_command_line gs line in
  cli_draw gs cmd_res;
  game_loop gs

(**
 * Draw gs and start and main game loop.
 * @param gs a game state.
 *)
let game_init (gs: game_state) : unit =
  cli_draw gs (Success "new game");
  game_loop gs