type square_id = int * int

type door = { mutable closed: bool }
type vent = { mutable closed: bool }

type edge = Wall | Space | Curtain | Door of door | Vent of vent

type dir = N | W
type link = square_id * dir * edge

type direction = North | East | South | West

type room = {
  name: string;
  code: string;
  color: string;
  squares_id: square_id list;
}

type square = {
  id: int * int;
  room: room;
  north: edge;
  west: edge;
}

type 'a matrix = 'a Array.t Array.t

type gameboard = {
  height: int;
  width: int;
  squares: square matrix;
  rooms: room list;
}

(****************************************************************************************************)
(*                                                                                                  *)
(*                                               SHOW                                               *)
(*                                                                                                  *)
(****************************************************************************************************)

let string_of_square_id (s: square_id) : string =
  "(" ^ (string_of_int (fst s)) ^ "," ^ (string_of_int (snd s)) ^ ")"


let string_of_square_id_list (ss: square_id list) : string =
  List.fold_right (fun s -> fun acc -> (string_of_square_id s) ^ " :: " ^ acc) ss "Nil"


let string_of_room (r: room) : string =
  r.name ^ " [" ^ r.code ^ "] (color: #" ^ r.color ^ ") : " ^ string_of_square_id_list r.squares_id


(****************************************************************************************************)
(*                                                                                                  *)
(*                                          INIT GAMEBOARD                                          *)
(*                                                                                                  *)
(****************************************************************************************************)

(**
 * A hash table mapping a square_id to the list of directions
 * allowed from the square of that id.
 *)
 type table_edges = (square_id, edge * edge) Hashtbl.t
 type table_room = (square_id, room) Hashtbl.t

(**
 * @param dim the gameboard dimensions, as a (height, width) pair.
 * @param rooms a list of rooms on the gameboard.
 * @return a square to room hash table.
 *)
let find_room_of_square (dim: int * int) (rooms: room list) : table_room =

  let (h, w) = dim in
  let t = Hashtbl.create (h * w) in
  let add_square_room (r: room) (sid: square_id) = Hashtbl.add t sid r in
  let iter_squares_of_room (r: room) = List.iter (add_square_room r) r.squares_id in

  List.iter iter_squares_of_room rooms;
  t


(**
 * @param t a hash table mapping a square_id to their square neighbors' ids.
 * @param id1 a square id.
 * @param id2 a square id.
 * @return t after adding id2 to the list of ids neighbors to id1.
 *)
let add_edge (l: link) (t: table_edges) : table_edges =

  let (s, d, e) = l in
  let (north, west) = Hashtbl.find t s in
  let check_oob (_: int) = e in
  let es = match d with
    | N -> (check_oob (snd s), west )
    | W -> (north, check_oob (fst s))
  in
  
  Hashtbl.replace t s es;
  t


(**
 * @param dim the gameboard dimensions, as a (height, width) pair.
 * @param t an empty neighbors table.
 * @return t after mapping each square from (0,0) to (h-1,w-1) to [].
 *)
let init_edges (dim: int * int) (t: table_edges) : table_edges =
  let (h, w) = dim in
  for i = 0 to h-1 do for j = 0 to w-1 do
    Hashtbl.add t (i, j) (Wall, Wall)
  done; done;
  t


(**
 * @param dim the gameboard dimensions, as a (height, width) pair.
 * @param rooms a list of rooms on the gameboard.
 * @return a square to room hash table.
 *)
let find_edges_of_square (dim: int * int) (links: link list) : table_edges =
  let (h, w) = dim in
  let t = Hashtbl.create (h * w) |> init_edges dim in
  List.fold_right add_edge links t


let init_square (te: table_edges) (tr: table_room) (i: int) (j: int) : square =
  let (n, w) = Hashtbl.find te (i,j) in
  let r = Hashtbl.find tr (i,j) in
  { id = (i,j); room = r; north = n; west = w; }


(**
 * @param dim the gameboard dimensions, as a (height, width) pair.
 * @param rooms a list of rooms on the gameboard.
 * @return a square to room hash table.
 *)
let init_gameboard (dim: int * int) (ls: link list) (rs: room list) : gameboard =

  let (h, w) = dim in
  let te = find_edges_of_square dim ls in
  let tr = find_room_of_square dim rs in
  let ss = Array.init_matrix h w (init_square te tr) in

  { height = h; width = w; squares = ss; rooms = rs }