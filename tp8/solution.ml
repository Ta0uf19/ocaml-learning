type directive = TurnLeft | TurnRight
               | StepForward of int
               | StepBackward of int ;;

type path = directive list ;;

let sample_path = ( StepForward 1 :: StepForward 2:: TurnLeft ::
                    StepBackward 3 :: TurnLeft :: StepForward 1:: []);;


(* inverse function *)

let inverse dir = 
  match dir with
  | TurnLeft -> TurnRight
  | TurnRight -> TurnLeft
  | StepForward (dist) -> StepBackward dist
  | StepBackward (dist) -> StepForward dist;;

(* print directive *)

let rec string_of_dir : directive -> string = function 
  | TurnLeft -> "Tourner à gauche"
  | TurnRight -> "Tourner à droite"
  | StepForward (dist) -> "Avancer de " ^ string_of_int dist ^ " pas"
  | StepBackward (dist) -> "Reculer de " ^ string_of_int dist ^ " pas"
;;

(* print path *)
let rec string_of_path : path -> string = function 
  | [] -> ""
  | t :: [] -> string_of_dir t; (*end*)
  | h::t -> string_of_dir h ^ " ; " ^ string_of_path t;;

(* simplify (à refaire)*)
let rec simplify : path -> path = function
  | [] -> [] 
  | x :: [] -> x :: []
  | TurnLeft :: TurnRight :: rest -> rest
  | TurnRight :: TurnLeft :: rest -> rest
  | (StepBackward _) :: (StepForward _) :: rest -> rest
  | (StepForward _) :: (StepBackward _) :: rest -> rest
  | x :: y :: rest -> x :: simplify (y :: rest) ;;

(*test simplfy*)
simplify [TurnRight; TurnLeft; TurnRight; TurnRight];;
simplify ([TurnRight; TurnRight; TurnRight]);;
simplify [(StepBackward(1)); (StepForward(1))];;
simplify [(StepBackward(1)); (StepForward(1)); (StepBackward(1)); TurnRight; TurnRight];;


type orientation = East | West | North | South;;
type hunter = (int * int * orientation);;


let string_of_orientation = function
  | North -> "nord"
  | South -> "sud"
  | East -> "est"
  | West -> "ouest";;

let string_of_hunter : hunter -> string = function
  | (x,y,o) -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ "," ^ string_of_orientation o ^")";;

(* test string_of_hunter *)
(*let h = Hunter (5,2,South);;
string_of_hunter h;;*)

let orientation_move d o =
  match (d,o) with
  | (TurnLeft,o) -> 
      (match o with
       | North -> West
       | West -> South
       | South -> East
       | East -> North) 
  | (TurnRight,o) -> 
      (match o with
       | North -> East
       | East -> South
       | South -> West
       | West -> North)
  | (_,o) -> o
;;
 
(*test orientation_move*)
(*orientation_move TurnRight North;;*)

let move h dir = 
  match (h,dir) with
  | ((x,y,o), TurnLeft) ->  (x,y, orientation_move TurnLeft o)
  | ((x,y,o), TurnRight) -> (x,y, orientation_move TurnRight o)
  | ((x,y,o), (StepForward dist)) -> 
      (match o with
       | North -> (x,y+dist,o)
       | East -> (x+dist,y,o)
       | West -> (x-dist,y,o)
       | South -> (x,y-dist,o))
      
  | ((x,y,o), (StepBackward dist)) -> 
      (match o with
       | North -> (x,y-dist,o)
       | East -> (x-dist,y,o)
       | West -> (x+dist,y,o)
       | South -> (x,y+dist,o))
;; 

(* test move *)
(*move (Hunter (0,0,North)) (StepBackward 5);;*)

let rec finally h p =
  match (h, p) with
  | (h,[]) -> h
  | (h,t::[]) -> move (h) t
  | (h, head::tail) -> finally (move (h) head) tail
;; 

(* test finally *)
(*finally ((0,0,North)) sample_path;;*)

(* à refaire *)
type obstacles = (int * int) list;;
let rec move_with_obstacles o h dir =
  let (x,y,_) = h in 
  if (List.mem (x,y) o) then h
  else
    move (h) dir
;;

move_with_obstacles [(1,0); (3,3); (2,3); (3,2); (3,3); (0,3); (3,0); (1,3); (2,0); (0,2); (3,0); (2,1); (0,2); (3,0); (2,2); (0,0); (1,1); (0,1); (1,1); (1,0); (1,3)] (1,0,North) (StepForward(3)) ;;
move_with_obstacles [(4,1); (4,2); (3,3); (0,1); (1,1); (2,0); (2,1); (4,2)] (3,4,North) (StepBackward(4));;
move_with_obstacles [(0,4); (3,1); (1,4)] (1,4,West) (StepBackward(1))