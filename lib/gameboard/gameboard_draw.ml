open Gameboard_init

let (<<) = Fun.compose

let draw_link_kind_north (e: edge) : string =
  match e with
  | Space   -> "+      "
  | Curtain -> "+--  --"
  | Door _  -> "+--  --"
  | Vent _  -> "+--$$--"
  | Wall    -> "+------" 

let draw_link_kind_west (e: edge) : string list =
  match e with
  | Space   -> [" "; " "; " "]
  | Curtain -> ["|"; " "; "|"]
  | Door _  -> ["|"; " "; "|"]
  | Vent _  -> ["|"; "#"; "|"]
  | Wall    -> ["|"; "|"; "|"]

let format_line (l: string) : string =
  let black_bg = "\027[48;5;16m" in
  let clear_bg = "\027[0m" in
  black_bg ^ l ^ clear_bg ^ "\n"

(**
 * Each square is drawn on 4 string lines; this function returns
 * the concatenation of each square n-th string line.
 * @param line a list of drawn squares.
 * @param n the string line number of draw.
 * @return the nth string line drawn.
 *)
let draw_line_aux (line: string list list) (n: int) : string =
  let east_wall = if n == 0 then "+" else "|" in
  List.fold_right (^) (List.map (Fun.flip List.nth n) line) east_wall

(**
 * @param line a list of drawn squares.
 * @return the drawing of each square.
 *)
let draw_line (line: string list list) : string =
  List.fold_right (^) (List.map (format_line << draw_line_aux line) [0;1;2;3]) ""

let rec draw_border_south (w: int) =
  match w with
  | 0 -> "+"
  | _ -> "+------" ^ (draw_border_south (w-1))

let rec draw_frame (ls: string list list list) : string =
  match ls with
  | []      -> failwith "Can't draw a gameboard of height 0."
  | [l]     -> (draw_line l) ^ (format_line (draw_border_south (List.length l)))
  | l :: ls -> (draw_line l) ^ (draw_frame ls)

let draw_square (s: square) : string list =
  draw_link_kind_north (s.north) :: (List.map (Fun.flip (^) "      ") << draw_link_kind_west) (s.west)

let draw_squares (squares: square matrix) (dim: int * int) : string =
  let (h, w) = dim in
  let ls = List.init h (
    fun i -> List.init w (fun j -> draw_square squares.(i).(j))
  ) in
  (draw_frame ls)