



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

let rec lives_lost enemy_lst path =
  match enemy_lst with
  | [] -> 0
  | Enemy(h,l)::tail ->
    if List.length path < l
    then h + lives_lost tail path
    else lives_lost tail path

let rec to_remove enemy_lst path =
  match enemy_lst with
  | [] -> 0
  | Enemy(h,l)::tail ->
    if List.length path < l
    then 1 + to_remove tail path
    else to_remove tail path

let enemy_to_coord enemy path =
match enemy with
| Enemy(h,l) -> (List.nth path (l-1))

let enemy_list_pos_to_coord enemy_lst path =
  List.map (fun (Enemy(h,l)) -> (List.nth path (l-1))) enemy_lst

let distance_t x1 y1 t =
  match t with
  |TowerFirst (x2,y2)-> 
    truncate (floor(sqrt (((float_of_int(x1)-.float_of_int(x2))**2.0)+.((float_of_int(y1)-.float_of_int(y2))**2.0))))



let rec enemies_in_range tower enemy_c_l path range= 
  match enemy_c_l with
  | [] -> []
  | Path(x,y)::tail -> 
    if distance_t x y tower <= range
    then (Path(x,y))::enemies_in_range tower tail path range
    else enemies_in_range tower tail path range



(*
let tile_list_to_string l =
String.concat " " (list_to_string_helper l)

print_string (tile_list_to_string tile_list)
*)


(*entire grid 12X12*)
let level_1 =
{ path = [Path(0,0); Path(0,1); Path(0,2); Path(1,2); Path(2,2); Path(3,2); Path(4,2); Path(4,1); Path(4,0)];
  lives = 100;
  enemy_list = [Enemy(3,(1)); Enemy(3,(2)); Enemy(9,(3)); Enemy(9,(4)); Enemy(9,(5)); Enemy(9,(6)); Enemy(9,(7))];
  tower_first = [TowerFirst (1,0); TowerFirst (2,0)]
}