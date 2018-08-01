



type coordinate = int*int

type enemy = int*coordinate

type tile =
| Path of coordinate
| Floor of coordinate
| Enemy of int*coordinate
| TowerFirst of coordinate
| Nothing of coordinate

type level = 
  { path: tile list;
    path_start: coordinate;
    path_end: coordinate;
    lives: int;
    enemy_list: tile list;
    tower_first: tile list;
    }

type direction = North | South | East | West

let next_pos (x,y) d = 
match d with
| North -> (x,y+1)
| South -> (x,y-1)
| East -> (x+1,y)
| West -> (x-1,y)

let is_free_path (c:tile) (lev:level) = List.mem c lev.path

let tile_to_string t =
match t with
| Enemy (1,(x,y)) -> "1"
| Path(x,y) -> "="
| Floor(x,y) -> "x"
| TowerFirst(x,y) -> "T"
| Nothing(x,y) -> "\n"
| _ -> failwith "expected tile"

let rec list_to_string_helper l =
match l with
| [] -> []
| head::tail -> (tile_to_string head)::(list_to_string_helper tail) 

(*
let tile_list_to_string l =
String.concat " " (list_to_string_helper l)

print_string (tile_list_to_string tile_list)
*)


(*entire grid 12X12*)
let level_1 =
{ path = [Path(0,0); Path(0,1); Path(0,2); Path(1,2); Path(2,2)];
  path_start = (0,0);
  path_end = (2,2);
  lives = 100;
  enemy_list = [];
  tower_first = []
}




(*

let rec spawn_next = 
if path_start is open
  then add enemy to list @ path_Start
else nothing

let rec move_enemy =
move each enemy starting with first forward one spot in path




*)

