



(*type coordinate = int*int*)

type tile =
| Path of int*int
| Floor of int*int
| Enemy of int*int
| TowerFirst of int*int
| Nothing of int*int

(*Find function from online, I thought I would need it*)

let rec find x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find x t

let succ i = i+1

type level = 
  { path: tile list;
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
| Enemy (1,(l)) -> "1"
| Path(x,y) -> "="
| Floor(x,y) -> "x"
| TowerFirst(x,y) -> "T"
| Nothing(x,y) -> "\n"
| _ -> failwith "expected tile"

let rec list_to_string_helper l =
match l with
| [] -> []
| head::tail -> (tile_to_string head)::(list_to_string_helper tail)

let move_enemies enemy_lst =
  List.map (fun (Enemy(h,l)) -> (Enemy(h,(l+1)))) enemy_lst

let rec lives_lost enemy_lst path=
  match enemy_lst with
  | [] -> 0
  | Enemy(h,l)::tail ->
    if List.length path < l
    then h + lives_lost tail path
    else lives_lost tail path

let rec to_remove enemy_lst path=
  match enemy_lst with
  | [] -> 0
  | Enemy(h,l)::tail ->
    if List.length path < l
    then 1 + lives_lost tail path
    else lives_lost tail path


(*
let tile_list_to_string l =
String.concat " " (list_to_string_helper l)

print_string (tile_list_to_string tile_list)
*)


(*entire grid 12X12*)
let level_1 =
{ path = [Path(0,0); Path(0,1); Path(0,2); Path(1,2); Path(2,2)];
  lives = 100;
  enemy_list = [Enemy(3,(3)); Enemy(3,(10)); Enemy(1,(9))];
  tower_first = []
}




(*

let rec spawn_next = 
if path_start is open
  then add enemy to list @ path_Start
else nothing




*)

