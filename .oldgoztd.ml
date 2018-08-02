

(*          TYPES           *)
(* ------------------------ *)

type coordinate = int*int

let get_1_of_2 (a,_) = a
let get_2_of_2 (_,a) = a

type tile =
| Path of coordinate
| Floor of coordinate
| Enemy of int*int
| TowerFirst of coordinate
| Nothing of coordinate

type level = 
  { path:         tile list;
    path_start:   coordinate;
    path_end:     coordinate;
    lives:        int;
    enemy_list:   tile list;
    tower_first:  tile list;}

type direction = North | South | East | West

(*-----------------------*)
(*        Types          *)


let next_pos (x,y) d = 
match d with
| North -> (x,y+1)
| South -> (x,y-1)
| East -> (x+1,y)
| West -> (x-1,y)

let is_free_path (c:tile) (lev:level) = List.mem c lev.path

let tile_to_string t =
match t with
| Enemy(_,_) -> "1"
| Path(_,_) -> "="
| Floor(_,_) -> "x"
| TowerFirst(_,_) -> "T"
| Nothing(_,_) -> "\n"
| _ -> failwith "expected tile"

let rec list_to_string_helper l =
match l with
| [] -> []
| head::tail -> (tile_to_string head)::(list_to_string_helper tail) 

let rec level_1_path_init (l:tile list) =
match l with
| [] -> []
| Path(x,y)::tail-> Path(x,y)::level_1_path_init tail

let level_1_board_init = [
  Floor(0,0);Floor(0,1);Floor(0,2);Floor(0,3);Floor(0,4);Floor(0,5);Path(0,6);Floor(0,7);
  Floor(1,0);Floor(1,1);Floor(1,2);Floor(1,3);Floor(1,4);Floor(1,5);Path(1,6);Floor(1,7);
  Floor(2,0);Path(2,1);Path(2,2);Path(2,3);Floor(2,4);Floor(2,5);Path(2,6);Floor(2,7);
  Floor(3,0);Path(3,1);Floor(3,2);Path(3,3);Floor(3,4);Floor(3,5);Path(3,6);Floor(3,7);
  Floor(4,0);Path(4,1);Floor(4,2);Path(4,3);Path(4,4);Path(4,5);Path(4,6);Floor(4,7);
  Floor(5,0);Path(5,1);Floor(5,2);Floor(5,3);Floor(5,4);Floor(5,5);Floor(5,6);Floor(5,7);
  Path(6,0);Path(6,1);Floor(6,2);Floor(6,3);Floor(6,4);Floor(6,5);Floor(6,6);Floor(6,7);
  Floor(7,0);Floor(7,1);Floor(7,2);Floor(7,3);Floor(7,4);Floor(7,5);Floor(7,6);Floor(7,7)
]

(*entire grid 12X12*)
let level_1 ={
    path =        level_1_path_init (level_1_board_init);
    path_start =  (0,0);
    path_end =    (0,0);
    lives =       100;
    enemy_list =  [Enemy(1,1);Enemy(0,1);Enemy(2,2);Enemy(-1,1)];
    tower_first = [];
    }

let move_enemies enemy_lst =
  List.map (fun (Enemy(h,l)) -> (Enemy(h,(l+1)))) enemy_lst

let rec remove_dead_enemy (l:tile list) =
match l with
| [] -> []
| Enemy(h,p)::tail -> 
  if h <= 0
    then remove_dead_enemy tail
  else Enemy(h,p) :: remove_dead_enemy tail

let loosing_state (lev:level) =
lev.lives <= 0

let construct_level (lev:level) =
print_string (String.concat " " (list_to_string_helper level_1.path))

let rec main_loop (lev:level) = 
  if loosing_state lev
    then print_string "YOU LOOSE"
  else 
    construct_level lev