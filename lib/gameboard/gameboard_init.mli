type square_id = int * int

type door = { mutable closed: bool }
type vent = { mutable closed: bool }

(**
 * The kind of link between two squares.
 * - Space: everyone can go between the two.
 * - Curtain: different rooms, but everyone can go.
 * - Door: different rooms, everyone can go, can be closed.
 * - Vent: different rooms, only bots can go, can be closed.
 *)
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

val string_of_room : room -> string

val init_gameboard : int * int -> link list -> room list -> gameboard