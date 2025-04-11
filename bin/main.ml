open SecurityML

let (<<) = Fun.compose

let height = 3
let width = 3
let dim = (height, width)

let rooms = [
  { name="Room 1"; code="R1"; color="ffffff"; squares_id=[(0,0); (0,1)] };
  { name="Room 2"; code="R2"; color="ffffff"; squares_id=[(0,2); (1,2)] };
  { name="Room 3"; code="R3"; color="ffffff"; squares_id=[(1,0); (1,1)] };
  { name="Room 4"; code="R4"; color="ffffff"; squares_id=[(2,0); (2,1); (2,2)] };
]

let links = [
  ((0,1), W, Space);
  ((0,2), W, Door {closed=false});
  ((1,0), N, Door {closed=false});
  ((1,1), W, Space);
  ((1,2), N, Space);
  ((1,2), W, Vent {closed=false});
  ((2,0), N, Door {closed=false});
  ((2,1), W, Space);
  ((2,2), N, Door {closed=false});
  ((2,2), W, Space)
]

let gb = init_gameboard dim links rooms

let display_rooms (rs: room list) : unit =
  List.iter ((print_string << fun s -> s ^ "\n") << string_of_room) rs;;

display_rooms (gb.rooms);;

print_newline ();;

let p1 = { pos=(1,1); state=Idle; battery=100; icon="🤖" }

let gb1 = player_on_gameboard p1 (dgameboard_of_squares (gb.squares));;

print_string (draw_dgameboard gb1);;