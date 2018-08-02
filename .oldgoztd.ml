

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
| Enemy(h,_) -> string_of_int h
| Path(_,_) -> "="
| Floor(_,_) -> "x"
| TowerFirst(_,_) -> "T"
| Nothing(_,_) -> "\n"
| _ -> failwith "expected tile"

let rec list_to_string_helper l =
match l with
| [] -> []
| head::tail -> (tile_to_string head)::(list_to_string_helper tail) 


(*entire grid 12X12*)
let level_1 ={
    path =        [Path(0,0);Path(0,1);Path(0,2);Path(0,3);Path(0,4);Path(0,5);Path(0,6);Path(0,7);Path(0,8);Path(0,9)];
    path_start =  (0,0);
    path_end =    (0,0);
    lives =       100;
    enemy_list =  [Enemy(1,1);Enemy(0,1);Enemy(2,2);Enemy(-1,1)];
    tower_first = [];
    }

let move_enemies enemy_list =
  List.map (fun (Enemy(h,l)) -> (Enemy(h,(l+1)))) enemy_list

let rec remove_dead_enemy (l:tile list) =
match l with
| [] -> []
| Enemy(h,p)::tail -> 
  if h <= 0
    then remove_dead_enemy tail
  else Enemy(h,p) :: remove_dead_enemy tail

let loosing_state (lev:level) =
lev.lives <= 0

(*takes level data outputs list with enemies where they are?*)
let construct_path (lev:level) =
  el=lev.enemy_list
  pl=lev.path
  match pl with
  | [] -> []
  | Path(x,y)::tail -> (construct_path_enemies el ) :: 

let rec construct_path_enemies (el:tile list) =
  match el with
  | [] ->
  | Enemy(h,p)::tail when p>-> Enemy(h,p)

let construct_level (lev:level) =
print_string ("Towers:" ^(string_of_int (List.length (lev.tower_first)))^"\nLives: "
^string_of_int lev.lives^"\n"^String.concat " " (list_to_string_helper level_1.path))

let apply_update_level (lev:level) = {
    path =        [Path(0,0);Path(1,0);Path(2,0);Path(3,0);Path(4,0);Path(5,0);Path(6,0);Path(7,0);Path(8,0);Path(9,0)];
    path_start =  (0,0);
    path_end =    (0,0);
    lives =       100;
    enemy_list =  [Enemy(1,1);Enemy(0,1);Enemy(2,2);Enemy(-1,1)];
    tower_first = [];
}




let rec main_loop (lev:level) = 
  if loosing_state lev
    then print_string "YOU LOOSE"
  else 
    let lev = 
    construct_level lev