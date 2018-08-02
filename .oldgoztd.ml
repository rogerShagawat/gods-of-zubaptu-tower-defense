

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
    tower_first:  tile list;
    wave:         int list;
    }

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

(*entire grid 12X12*)
let level_1 ={
    path =        [Path(0,0);Path(0,1);Path(0,2);Path(0,3);Path(0,4);Path(0,5);Path(0,6);Path(0,7);Path(0,8);Path(0,9)];
    path_start =  (0,0);
    path_end =    (0,0);
    lives =       100;
    enemy_list =  [];
    tower_first = [];
    wave = [1;2;3;4;5;6]
    }

let move_enemies enemy_list =
  List.map (fun (Enemy(h,l)) -> (Enemy(h,(l+1)))) enemy_list

let spawn_enemy (w:int list) =
  match w with
  | [] -> []
  | head::tail -> [Enemy(head,0)]

let remove_first l =
match l with
| []->[]
| head::tail -> tail

let rec remove_dead_enemy (l:tile list) =
match l with
| [] -> []
| Enemy(h,p)::tail -> 
  if h <= 0 || p>10
    then remove_dead_enemy tail
  else Enemy(h,p) :: remove_dead_enemy tail

(*          Graphics          *)
(*----------------------------*)

let rec list_to_string l =
match l with
| [] -> []
| head::tail -> (tile_to_string head)::(list_to_string tail) 

let rec construct_path_helper (n:int) =
match n with
| 0 -> []
| _ -> (Path(0,0))::construct_path_helper (n-1)

let construct_path (el:tile list) (pl:tile list) = 
el @ (construct_path_helper ((List.length pl) - (List.length el)))

let construct_level (lev:level) =
print_string ("Towers:" ^(string_of_int (List.length (lev.tower_first)))^"\nLives: "
^string_of_int lev.lives^"\n"^String.concat " " (list_to_string (construct_path (lev.enemy_list) (lev.path))));
print_newline ()

(*           Update, states, and lists          *)
(*----------------------------------------------*)

let apply_update_level (lev:level) = {
    path =        lev.path;
    path_start =  lev.path_start;
    path_end =    lev.path_end;
    lives =       lev.lives - (lives_lost lev.enemy_list lev.path);
    enemy_list =  (move_enemies (remove_dead_enemy lev.enemy_list)) @ spawn_enemy lev.wave;
    tower_first = lev.tower_first;
    wave = remove_first lev.wave;
}

let loosing_state (lev:level) =
lev.lives <= 0

let winning_state (lev:level) =
(((List.length lev.wave) <=0) && ((List.length lev.enemy_list) <=0))

let rec main_loop (lev:level) = 
  Unix.sleepf(0.5);
  if loosing_state lev
    then print_string ("YOU LOOSE"^(string_of_int lev.lives))
  else if winning_state lev
    then print_string ("YOU WIN!!!" ^(string_of_int lev.lives))
  else
    let lev = apply_update_level lev
    in construct_level lev;
    main_loop lev;
  
