
type direction = | North | East | South | West;

type position = (int, int);

type rover = {pos: position, dir: direction};

let rotate_left =
  fun
  | North => West
  | East => North
  | South => East
  | West => South;

let rotate_right =
  fun
  | North => East
  | East => South
  | South => West
  | West => North;

let rotate_rover_left r => {
  let d = rotate_left r.dir;
  {pos: r.pos, dir: d}
};

let rotate_rover_right r => {
  let d = rotate_right r.dir;
  {pos: r.pos, dir: d}
};

let move_rover {pos: (x, y), dir: d} =>
  switch d {
  | North => {pos: (x + 1, y), dir: d}
  | East => {pos: (x, y + 1), dir: d}
  | South => {pos: (x - 1, y), dir: d}
  | West => {pos: (x, y - 1), dir: d}
  };

let direction_to_string =
  fun
  | North => "North"
  | East => "East"
  | South => "South"
  | West => "West";

let print_rover {pos: (x, y), dir: d} => print_string (
  Printf.sprintf "(%d, %d) %s" x y (direction_to_string d)
);

print_rover {pos: (0, 0), dir: North};
