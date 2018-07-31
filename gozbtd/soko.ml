


(* ******************************************** *)
(* Data type declarations *)
(* ******************************************** *)

type coordinate = int*int

type level = { pos: coordinate;
               points: int;
               walls: coordinate list;
               boxes: coordinate list;
               places: coordinate list}

type direction = North | East | South | West

(* ******************************************** *)
(* Start of code that student's should write *)
(* ******************************************** *)

let next_pos (x,y) d =
  failwith "Complete me!"
    
let move_worker lev dir =
  failwith "Complete me!"
    
let is_free_coord (c:coordinate) (lev:level) =
  failwith "Complete me!"
    
let rec move_box boxes before after =
  failwith "Complete me!"

let step (lev:level) (dir:direction):level =
  failwith "Complete me!"

let process_key lev key =
  failwith "Complete me!"

(* ******************************************** *)
(* End of code that students should write *)
(* ******************************************** *)

let erase_level {pos=(x,y); boxes= bs; places = ps}  =
  Grid.erase_coordinates (bs@ps@[(x,y)])

let draw_state {pos=(x,y); boxes= bs; places = ps}  =
  Grid.draw_string_in_box_at (x,y) "8";
  List.iter (fun c -> Grid.draw_string_in_box_at c "[]") bs;
  List.iter (fun c -> Grid.draw_string_in_box_at c "x") ps

let winning_state {boxes= bs; places = ps} =
  List.for_all (fun c -> List.mem c ps) bs 


let draw_level_without_walls {pos=(x,y); points=p; walls = ws; boxes= bs; places = ps}  =
  Grid.draw_string_in_box_at (x,y) "8"; (* draw worker *)
  List.iter (fun c -> Grid.draw_string_in_box_at c "[]") bs; (* draw
                                                              * boxes *)
  List.iter (fun c -> Grid.draw_string_in_box_at c "x") ps (* draw
                                                            * places *)

let draw_level (lev:level)  =
  List.iter (fun c -> Grid.color_box c Graphics.black) lev.walls; (* draw
                                                                   * walls *)
  draw_level_without_walls lev  (* draw the rest *)


let rec main_loop (level:level) (copy:level) :unit =
  if winning_state level
  then
    begin
      (Grid.draw_string_at (200,510) "You won!");
      ignore @@ Graphics.wait_next_event [Graphics.Key_pressed]
    end
  else
    begin
      draw_level_without_walls level;
      let s = Graphics.wait_next_event [Graphics.Key_pressed]
      in match s.Graphics.key with
      | 'q' -> ()
      | 'r' ->  (Grid.draw_grid ();
                 draw_level copy;   
                 main_loop copy copy)
      | key -> (erase_level level;
                main_loop (process_key level key) copy)
    end



(* Sample Level *)

let level_1 =
  { pos=(2,4);
    points=0;
    walls = [(0,1);(1,1);(2,1);(3,1);(3,2);(4,2);(5,2);(5,3);(5,4);(5,5);(4,5);(3,5);(3,6);(3,7);(2,7);(1,7);(0,7);(0,6);(0,5);(0,4);(0,3);(0,2)];
    boxes = [(3,3);(1,4)];
    places = [(2,6);(1,4)]}


(* Main *)

let main () = 
  Graphics.open_graph " 600x600";
  Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  Grid.draw_string_at (50,550) "ASWD for movement; q to quit; r to restart";
  Grid.draw_grid ();
  draw_level level_1;   
  main_loop level_1 level_1;
  Graphics.close_graph ()
