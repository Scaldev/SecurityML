open Gameboard_init

(* Can't use char, since emojis aren't unicode characters. *)
type pixel = string

(**
 * A drawn square; it looks like that:
 * 
 *   +------    4 lines: 1 for the north wall
 *   |          and 3 for the room.
 *   |          The middle of the wall can change
 *   |          according to the link edge value.
 * 
 *)
type dsquare = pixel matrix

type dgameboard = dsquare matrix

val string_of_dgameboard : dgameboard -> string

val dgameboard_of_squares : square matrix -> dgameboard

val draw_dgameboard : dgameboard -> string 

val draw_squares : square matrix -> string