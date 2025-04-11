open Gameboard_init

let (<<) = Fun.compose

type pixel = string
type dsquare = pixel matrix
type dgameboard = dsquare matrix

(****************************************************************************************************)
(*                                                                                                  *)
(*                                 SQUARE MATRIX --> DSQUARE MATRIX                                 *)
(*                                                                                                  *)
(****************************************************************************************************)

let string_of_dsquare (ds: dsquare) : string =
  let string_of_pixel_line line = Array.fold_right (^) line "\n" in
  Array.fold_right ((^) << string_of_pixel_line) ds ""

let string_of_dgameboard (dgb: dgameboard) : string =
  let string_of_dsquare_line line = Array.fold_right ((^) << string_of_dsquare) line "\n" in
  Array.fold_right ((^) << string_of_dsquare_line) dgb ""

let draw_link_kind_north (e: edge) : pixel array =
  match e with
  | Space   -> [| "+"; " "; " "; " "; " "; " "; " " |]
  | Curtain -> [| "+"; "-"; "-"; " "; " "; "-"; "-" |]
  | Door _  -> [| "+"; "-"; "-"; " "; " "; "-"; "-" |]
  | Vent _  -> [| "+"; "-"; "-"; "$"; "$"; "-"; "-" |]
  | Wall    -> [| "+"; "-"; "-"; "-"; "-"; "-"; "-" |]


let draw_link_kind_west (e: edge) : pixel matrix =
  match e with
  | Space   -> [| [|" "|]; [|" "|]; [|" "|] |]
  | Curtain -> [| [|"|"|]; [|" "|]; [|"|"|] |]
  | Door _  -> [| [|"|"|]; [|" "|]; [|"|"|] |]
  | Vent _  -> [| [|"|"|]; [|"#"|]; [|"|"|] |]
  | Wall    -> [| [|"|"|]; [|"|"|]; [|"|"|] |]

let space_line = [| " "; " "; " "; " "; " "; " "; |]

let wall_east = [| [| [|"+"|]; [|"|"|]; [|"|"|]; [|"|"|]|] |]

let wall_south = [| [|"+"; "-"; "-"; "-"; "-"; "-"; "-"|] |]

let dsquare_of_square (s: square) : dsquare =
  let line_wall = [| draw_link_kind_north s.north |] in
  let lines_square = (Array.map (Fun.flip (Array.append) space_line) << draw_link_kind_west) (s.west) in
  Array.append line_wall lines_square
  

let format_line (l: string) : string =
  let black_bg = "\027[48;5;16m" in
  let clear_bg = "\027[0m" in
  black_bg ^ l ^ clear_bg ^ "\n"


let add_wall_east (line: dsquare array) : dsquare array =
  Array.append line wall_east


let add_wall_south (w: int) : dsquare array =
  Array.append (Array.init w (fun _ -> wall_south)) [|[|[|"+"|]|]|]


let dgameboard_of_squares (squares: square matrix) : dgameboard =

  let h = Array.length squares in
  let w = Array.length squares.(0) in
  let ls = Array.init_matrix h w (fun i -> fun j -> dsquare_of_square squares.(i).(j)) in
  let ls' = Array.map add_wall_east ls in
  let south_wall = [| add_wall_south w |] in
  Array.append ls' south_wall
  

(****************************************************************************************************)
(*                                                                                                  *)
(*                                DSQUARE MATRIX --> STRING                                         *)
(*                                                                                                  *)
(****************************************************************************************************)

let draw_pixel_line (line: pixel array) : string =
  Array.fold_right (^) line ""

let draw_dsquare_line_aux (line: dsquare array) (n: int) : string =
  if Array.length line.(0) <= n then
    ""
  else (
    let lines = (Array.map (Fun.flip Array.get n) line) in
    Array.fold_right ((^) << draw_pixel_line) lines ""
  )


let draw_dsquare_line (line: dsquare array) : string =
  Array.fold_right (^) (Array.map (format_line << draw_dsquare_line_aux line) [|0;1;2;3|]) ""


let draw_dgameboard (dgb: dgameboard) : string =
  Array.fold_right ((^) << draw_dsquare_line) dgb ""

(****************************************************************************************************)

let draw_squares (m: square matrix) : string =
  (draw_dgameboard << dgameboard_of_squares) m