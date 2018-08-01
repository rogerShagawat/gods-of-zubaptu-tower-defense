type coordinate = int*int

type enemy = int*coordinate

type tile =
| Path(coordinate)
| Floor(coordinate)
| Enemy(int,coordinate)
| TowerFirst(coordinate) 

type level = 
  { path: coordinate list;
    path_start: coordinate;
    path_end: coordinate;
    lives: int;
    enemy_list: enemy list;
    tower_first: coordinate list;
    tower_strong: coordinate list;
    tower_line: coordinate list;
    }

type direction = North | South | East | West

let next_pos (x,y) d = 
match d with
| North -> (x,y+1)
| South -> (x,y-1)
| East -> (x+1,y)
| West -> (x-1,y)

let is_free_path (c:coordinate) (lev:level) =
List.mem c lev.path

let tile_list = [Path(0,0); Path(0,1); Path(0,2)]

let tile_to_string l =
match l with
| Path -> '='
| Floor -> 'x'
| Enemy -> '1'
| TowerFirst -> 'T'
| Nothing -> '\n'

let rec list_to_string l =
match l with
| [] -> 
| head::tail -> tile_to_string head :: list_to_string tail 


(*entire grid 12X12*)
let level_1 =
{ path = ([(0,0),(0,1),(0,2),(1,2),(2,2)]);
  path_start = (0,0);
  path_end = (2,2);
  lives = 100;
  enemy_list = [enemy:e];
  tower_first = [];
  tower_strong = [];
  tower_lines = []
}

(*

let rec spawn_next = 
if path_start is open
  then add enemy to list @ path_Start
else nothing

let rec move_enemy =
move each enemy starting with first forward one spot in path




*)

