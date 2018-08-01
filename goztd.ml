

type coordinate = int*int
type direction = North | East | South | West
type enemy = int*int
type unspawned = int

let get_1_2 (a,_) = a
let get_2_2 (_,a) = a

type level = { pos: coordinate;
               lives: int;
               enemies: enemy list;
               wave: unspawned list;
               path: coordinate list;
               towers: coordinate list}
(*type levelState = { pos: coordinate;
                lives: int;
                enemies: enemy list;
                wave: unspawned list;
                path: coordinate list;
                towers: coordinate list}*)

let draw_level_without_walls {pos=(x,y);}  =
  Grid.draw_string_in_box_at (x,y) "8"; (* draw worker *)

let draw_level (lev:level)  =
  List.iter (fun c -> Grid.color_box c Graphics.black) lev.path; (* draw
                                                                   * walls *)
  draw_level_without_walls lev  (* draw the rest *)

let level_1 =
  { pos=(2,4);
    lives=100;
    enemies = [(9,0);(9,1)];
    wave = [2,2,2,2,2];
    path = [(0,1),(1,1),(1,2),(1,3),(1,4),(1,5),(2,5),(3,5),(3,4),(3,3),(4,3),(5,3),(6,3),(6,4),(6,5),(6,6)];
  }

let enemies_of_level l=
  match l with
  | level(pos,lives,enemies,wave,path) -> enemies;
let pos_of_level l=
  match l with
  | level(pos,lives,enemies,wave,path) -> pos;
let lives_of_level l=
  match l with
  | level(pos,lives,enemies,wave,path) -> lives;
let wave_of_level l=
  match l with
  | level(pos,lives,enemies,wave,path) -> wave;
let path_of_level l=
  match l with
  | level(pos,lives,enemies,wave,path) -> path;

let spawnEnemy u=
  {
  enemies@[(0,1),u];
  }

let advance (e:enemy) = {
  (get_1_2(e),(get_2_2(e)+1))
}

let rec game_loop (l:level) = {
  let 'l = (
    pos_of_level l;
    lives_of_level l
    List.map advance (enemies_of_level l),
    wave_of_level l,
    path_of_level l
  )
  draw_level 'l;
  game_loop 'l;
}

let main () = {
  Graphics.open_graph " 600x600";
  Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  Grid.draw_grid ();
  draw_level level_1;   
  game_loop level_1;
  Graphics.close_graph ()
}
