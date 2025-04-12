open Gameboard
open Player
open State

let (<<) = Fun.compose

type command = Move of direction

type 'a result = Success of 'a | Error of string

type cli_state = {
  game_state: game_state;
  mutex: Mutex.t;
  running: bool;
  mutable result: string result;
  mutable input: string;
}

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
let process_command (cs: cli_state) (c: command) : string result =
  match c with
  | Move d -> process_move cs.game_state d

(**
 * Update the game state according to the command, if possible.
 * @param gs a game state.
 * @param line a command line string.
 * @return the output result after parsing and processing that line.
 *)
let process_command_line (cs: cli_state) : string result =
  match parse_command cs.input with
    | Success s -> process_command cs s
    | Error _ -> Error "unknown command"

(****************************************************************************************************)
(*                                                                                                  *)
(*                                      INPUT - OUTPUT INTERFACE                                    *)
(*                                                                                                  *)
(****************************************************************************************************)

(**
 * Set raw mode to read the standard input without the need to wait
 * for an Enter key.
 *)
let set_raw_mode () =
  let term = Unix.tcgetattr Unix.stdin in
  let raw_term = { term with c_icanon = false; c_echo = false; } in
  Unix.tcsetattr Unix.stdin TCSANOW raw_term

(**
 * Reset to default mode when a failure happens.
 *)
let reset_mode () =
  let term = Unix.tcgetattr Unix.stdin in
  let canonical_term = { term with c_icanon = true; c_echo = true; } in
  Unix.tcsetattr Unix.stdin TCSANOW canonical_term


(**
 * Clean the terminal screen.
 *)
let clear_screen () =
  let top_left = "\027[H" in
  let clean = "\027[2J" in
  print_string (top_left ^ clean);
  flush stdout

(**
 * Print the game state drawing in standard output.
 * @param gs a game state.
 *)
let draw_game_state (gs: game_state) : unit =
  print_string (draw_game_state gs);
  flush stdout

(**
 * Print the game state drawing in standard output.
 * @param gs a game state.
 *)
let draw_command_line (cs: cli_state) =
  let l = string_of_int ((cs.game_state.gameboard.height * 4 + 1) + 2) in
  print_string ("\027[" ^ l ^ ";1H\027[2K");
  print_string (string_of_result cs.result);
  Printf.printf "\n>>> %s" cs.input;
  flush stdout

(**
 * Redraw the output (game state) and input (command line) every frame.
 * @param cs the state of the CLI.
 *)
let display_loop (cs: cli_state) =
  while cs.running do
    clear_screen ();
    Mutex.lock cs.mutex;
    draw_game_state cs.game_state;
    draw_command_line cs;
    Mutex.unlock cs.mutex;
    Thread.delay 0.033 (* ~ 30 fps *)
  done

(**
 * update cs according to c.
 * @param cs the state of the CLI.
 * @param c the last input char.
 *)
let process_char (cs: cli_state) (c: char) : unit =
  match c with
  | '\n' ->
      cs.result <- process_command_line cs;
      cs.input <- ""
  | '\127' ->
      if String.length cs.input > 0 then
        cs.input <- String.sub cs.input 0 (String.length cs.input - 1)
  | _ ->
      cs.input <- cs.input ^ (String.make 1 c)

(**
 * Listen for input char to process while the game is running.
 * @param cs the state of the CLI.
 *)
let cli_loop (cs: cli_state) : unit =
  while cs.running do
    let c = input_char stdin in
    Mutex.lock cs.mutex;
    process_char cs c;
    Mutex.unlock cs.mutex;
  done

(**
 * @param gs a game state.
 * @return a CLI state.
 *)
let cs_default (gs: game_state) : cli_state = {
  game_state = gs;
  mutex = Mutex.create ();
  result = (Success "new game");
  input = "";
  running = true;
}

(**
 * Initialize the game loop.
 * @param gs a game state.
 *)
let game_init (gs: game_state) : unit =

  set_raw_mode ();
  clear_screen ();

  let cs = cs_default gs in
  let _ = Thread.create display_loop cs in

  try
    cli_loop cs
  with e ->
    reset_mode ();
    raise e