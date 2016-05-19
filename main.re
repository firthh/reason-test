
type direction = | North | East | South | West;

type position = (int, int);

type rover = {pos: position, dir: direction};

let rotateLeft =
  fun
  | North => West
  | East => North
  | South => East
  | West => South;

let rotateRight =
  fun
  | North => East
  | East => South
  | South => West
  | West => North;

let rotateRoverLeft r => {
  let d = rotateLeft r.dir;
  {pos: r.pos, dir: d}
};

let rotateRoverRight r => {
  let d = rotateRight r.dir;
  {pos: r.pos, dir: d}
};

let moveRover {pos: (x, y), dir: d} =>
  switch d {
  | North => {pos: (x + 1, y), dir: d}
  | East => {pos: (x, y + 1), dir: d}
  | South => {pos: (x - 1, y), dir: d}
  | West => {pos: (x, y - 1), dir: d}
  };
