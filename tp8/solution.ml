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

(*test*)

let dir = sample_path;;
string_of_path dir;;

(*Question 2*)

type orientation = North | South | East | West;;
type hunter = Hunter of (int * int * orientation);;


let string_of_orientation = function
  | North -> "nord"
  | South -> "sud"
  | East -> "est"
  | West -> "ouest";;

let string_of_hunter : hunter -> string = function
  | Hunter (x,y,o) -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ "," ^ string_of_orientation o ^")";;

let h = Hunter (5,2,South);;
string_of_hunter h;;

let param_move d o =
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
  
(*param_move TurnRight North;;*)
let move h dir = 
  match (h,dir) with
  | (Hunter (x,y,o), TurnLeft) -> Hunter  (x,y, param_move TurnLeft o)
  | (Hunter (x,y,o), TurnRight) -> Hunter  (x,y, param_move TurnRight o)
  | (Hunter (x,y,o), (StepBackward dist|StepForward dist)) -> 
      (match o with
       | North -> Hunter (x,y+dist,o)
       | East -> Hunter (x-dist,y,o)
       | South -> Hunter (x+dist,y,o)
       | West -> Hunter (x,y-dist,o))
;; 
  
let h = Hunter (5,2,South);;
string_of_hunter h;;

(* test move *)
move (Hunter (0,0,South), TurnLeft);;