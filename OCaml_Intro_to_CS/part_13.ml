type stock = (string * float * float) list ;;
type sample = string * float * float;; (* name, weight, price *)
type solution = string * float * float;; (* name, weight_taken, total_val_extracted *)

(* 
1. Calculate price per kilogram
2. Sort samples by price per kilogram
3. Select samples to maximize value
4. Return the results
*)

let map f lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t ->
        aux (f h :: acc) t
    in
    aux [] lst
  ;;

let min v1 v2 = 
  if v1 < v2 then v1 else v2
;;

let rob_liquids (st : stock) (max_amt : float) =

  (* cal price per kg for each sample first *)
  let samples_w_price_per_kg = map (fun (name, weight, price) -> 
  (name, weight, price, price /. weight)) st
  in

  (* sort samples *)
  let sorted_samples = List.sort(fun (_, _, _, ppkg1) (_, _, _, ppkg2) -> compare ppkg1 ppkg2) samples_w_price_per_kg
  in

  (* 
  using sorted samples, we can run through the samples, and decrease the max_amt variable each call, append it to our 
  solution as well
  *)

  let rec aux remain_cap acc = function
    | [] -> acc
    | (name, weight, price, ppkg) :: t ->
      if remain_cap <= 0.0 then
        acc
      else
        let amt_taken = min weight remain_cap in
        let value_extracted = amt_taken *. ppkg in
        let new_solution = (name, amt_taken, value_extracted) in
        aux (remain_cap -. amt_taken) (new_solution :: acc) t
      in
      aux max_amt [] sorted_samples
;;

let museum_stock = [("water", 202.1, 25.2); ("mercury", 100.0, 150.0); ("cobra venom", 10.0, 1000.0)];;
let max_capacity = 300.0;;

let results = rob_liquids museum_stock max_capacity in 
List.iter (fun (name, amount_taken, value_extracted) ->
  Printf.printf "Take %.2f kg of %s (Value extracted: SGD %.2f)\n" amount_taken name value_extracted
) results;;

(* part 2 on solids *)

type solid_stock = (string * int * int) list;;
type solution_solids = string * int * int;;

let rob_solids (st : solid_stock) (max_amt : int) =

  (* cal price per kg for each sample first *)
  let samples_w_price_per_kg = map (fun (name, weight, price) -> 
  (name, weight, price, price / weight)) st
  in

  (* sort samples *)
  let sorted_samples = List.sort(fun (_, _, _, ppkg1) (_, _, _, ppkg2) -> compare ppkg1 ppkg2) samples_w_price_per_kg
  in

  (* 
  using sorted samples, we can run through the samples, and decrease the max_amt variable each call, append it to our 
  solution as well
  *)

  let rec aux remain_cap acc = function
    | [] -> acc
    | (name, weight, price, ppkg) :: t ->
      if remain_cap <= weight then
        aux remain_cap acc t
      else
        let amt_taken = min weight remain_cap in
        let value_extracted = amt_taken * ppkg in
        let new_solution = (name, amt_taken, value_extracted) in
        aux (remain_cap - amt_taken) (new_solution :: acc) t
      in
      aux max_amt [] sorted_samples
;;

let museum_stock_solids = [("Black Crochan", 314, 1618); ("Diamonds", 100, 400); ("cobra stones", 50, 1000)];;
let max_capacity_solids = 160;;

let results = rob_solids museum_stock_solids max_capacity_solids in 
List.iter (fun (name, amount_taken, value_extracted) ->
  Printf.printf "Take %d kg of %s (Value extracted: SGD %d)\n" amount_taken name value_extracted
) results;;

(* part 4 *)

type 'a queue = 'a option array * int ref * int ref;;
exception FullQueue;;
exception EmptyQueue;;
let empty_queue (n : int) : 'a queue = (Array.make n None, ref 0, ref 0);;

(* enqueue from back - right, pop from front - left*)
let enqueue (q : 'a queue) e = 
  match q with
  | (arr, front, back) -> 
    if (!back = Array.length arr && !front = 0) || (!back < Array.length arr && (!back + 1) mod Array.length arr = !front) then (* full *)
      raise FullQueue
    else
      (* enqueue *)
      Printf.printf "front: %d, back: %d\n" !front !back;
      if arr.(!back) = None then begin
        arr.(!back) <- Some e;
        back := (!back + 1) mod Array.length arr
      end else begin
        arr.(!back + 1) <- Some e;
        back := (!back + 1) mod Array.length arr
      end
;;

let dequeue (q : 'a queue) =
  match q with
  | (arr, front, back) ->
    Printf.printf "front: %d, back: %d\n" !front !back;
    if arr.(!front) = None then
      raise EmptyQueue
    else 
      let result = arr.(!front) in
      arr.(!front) <- None;
      front := (!front + 1) mod Array.length arr;
      result
;;


let print_queue (q : 'a queue) (string_of_elem : 'a -> string) =
  let (arr, front, back) = q in
  let n = Array.length arr in
  let rec aux i =
    if i <> !back then
      match arr.(i) with
      | Some elem -> 
          Printf.printf "%s " (string_of_elem elem);
          aux ((i + 1) mod n)
      | None -> aux ((i + 1) mod n)
  in
  Printf.printf "[";
  aux !front;
  Printf.printf "]\n"
;;

let q = empty_queue 5;;

enqueue q 1;;
enqueue q 2;;
enqueue q 3;;
enqueue q 4;;

print_queue q string_of_int; (* Expected: [1 2 3 4] *)
ignore (dequeue q);
print_queue q string_of_int; (* Expected: [2 3 4] *)
ignore (dequeue q);
print_queue q string_of_int; (* Expected: [3 4] *)
enqueue q 5;
enqueue q 6;
print_queue q string_of_int; (* Expected: [3 4 5 6] *)
ignore (dequeue q);
print_queue q string_of_int; (* Expected: [4 5 6] *)
ignore (dequeue q);
print_queue q string_of_int; (* Expected: [5 6] *)

(* Expected: Some 5 *)
dequeue q;;
print_queue q string_of_int; (* Expected: [6] *)

(* Expected: Some 6 *)
dequeue q;;
print_queue q string_of_int;; (* Expected: [] *)

(* Should raise EmptyQueue *)
(* dequeue q;; *)

(* Should raise FullQueue *)
(* enqueue q 7;;
enqueue q 8;;
enqueue q 9;;
enqueue q 10;;
enqueue q 11;;
enqueue q 12;; *)

(* Part 6 *)
(* 
let monty: int -> int -> bool -> float *)

(* 1. loop through trials 
2. For each loop, pick a door that is goat is doors > 2
3. if strat is true, then switch doors to a random one that is not the original
   *)
let () = Random.self_init ();;

(* let monty (doors : int) (trials : int) (strat : bool) : float =
  let winning_percentage = ref 0 in
  for _ = 1 to trials do

    let prize = Random.int doors in
    let picked_door = Random.int doors
    in

    let rec pick_shown_door () = 
      let shown_door = Random.int doors in
      if (shown_door = prize) || (shown_door = picked_door) then pick_shown_door () else shown_door
    in
    let shown_door = pick_shown_door () 
    in
    let final_pick = 
      if strat then 
        let rec pick_new_door () = 
          let new_picked_door = Random.int doors in
          if (new_picked_door = picked_door) || (shown_door = picked_door) then pick_new_door () else new_picked_door 

        in
        pick_new_door ()
      else
        picked_door
      in

      if final_pick = prize then
        incr winning_percentage
    done;
     (float_of_int !winning_percentage /. float_of_int trials) *. 100.0
;; *)
    
let monty (doors : int) (trials : int) (strat : bool) : float =
  let win_count = ref 0 in

  for _ = 1 to trials do
    let prize_door = Random.int doors in
    let picked_door = Random.int doors in
    
    let rec pick_shown_door exclude1 exclude2 =
      let shown_door = Random.int doors in
      if shown_door <> exclude1 && shown_door <> exclude2 then
        shown_door
      else
        pick_shown_door exclude1 exclude2
    in
    let shown_door = pick_shown_door picked_door prize_door in
    
    let final_door =
      if strat then
        let rec pick_new_door exclude1 exclude2 =
          let new_door = Random.int doors in
          if new_door <> exclude1 && new_door <> exclude2 then
            new_door
          else
            pick_new_door exclude1 exclude2
        in
        pick_new_door picked_door shown_door
      else
        picked_door
    in
    
    (* Step 5: Check if the final door is the prize door *)
    if final_door = prize_door then
      incr win_count
  done;

  (* Calculate winning percentage *)
  (float_of_int !win_count /. float_of_int trials) *. 100.0
;;


let () = 
  let doors = 3 in
  let trials = 1_000_000 in
  let strat = true in
  let winning_percentage = monty doors trials strat in
  Printf.printf "Winnning percentage with %d doors, %d trials (switching strategy: %b): %.2f%%\n"
    doors trials strat winning_percentage
;;

let () = 
  let doors = 3 in
  let trials = 1_000_000 in
  let strat = false in
  let winning_percentage = monty doors trials strat in
  Printf.printf "Winnning percentage with %d doors, %d trials (switching strategy: %b): %.2f%%\n"
    doors trials strat winning_percentage
;;


let random_example () =
  for _ = 1 to 10 do
  let rand_value = Random.int 100 in
  Printf.printf "Random value: %d\n" rand_value
  done
;;

random_example ()