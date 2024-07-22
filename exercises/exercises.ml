(* tail of a list *)
(* last ["a"; "b"; "c"; "d"];; *)

let rec tail ls =
  match ls with
  | [] -> None
  | [x] -> Some x
  | _ :: t -> tail t;;

let result = tail ["a"; "b"; "c"; "d"];; 
(* print_string result;; *)


(* last 2 elements of a list *)
(* # last_two ["a"; "b"; "c"; "d"];;
- : (string * string) option = Some ("c", "d")
# last_two ["a"];;
- : (string * string) option = None *)

let last_two lst =
  let rec aux = function
  | [] -> None
  | [x] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> aux t
in
aux lst
;;


let last = ["a" ; "b" ; "c" ; "d"];;
let last_one = [ "b" ];;
let last_none = [];;

let print_string_pair pairs =
  match pairs with
  | Some (x, y) -> Printf.printf "(%s, %s)" x y
  | None -> Printf.printf "No pair available\n"
;;

let () =
let result = last_two last in
  print_string_pair result
;;

(* nth element of a list *)
(* # List.nth ["a"; "b"; "c"; "d"; "e"] 2;;
- : string = "c"
# List.nth ["a"] 2;;
Exception: Failure "nth". *)

exception Failure of string;;

let nth lst n = 
  let rec aux acc = function
    | [] -> raise (Failure "nth")
    | h :: t ->
      if acc = n then h
      else
        aux (acc + 1) t
      in
      aux 0 lst
    ;;

let () = 
  print_endline (nth ["a"; "b"; "c"; "d"; "e"] 2); (* Should print "c" *)
  (* This will raise the Failure exception *)
  try
    print_endline (nth ["a"] 2)
  with
  | Failure msg -> print_endline ("Caught exception: " ^ msg)
;;

(* length of a list *)

(* reverse a list *)
let reverse lst = 
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in 
  aux [] lst
;;

(* print_endline (reverse ["a"; "b"; "c"]);; *)

(* palindrome *)

let is_palindrome lst =
  lst = reverse lst
;;

let () = 
  let result = is_palindrome ["x"; "a"; "m"; "a"; "x"] in
  Printf.printf "is_palindrome: %s\n" (if result = true then "true" else "false");
  let not_result = not (is_palindrome ["a"; "b"]) in
  Printf.printf "is_palindrome ['a'; 'b']: %s\n" (if not_result = true then "true" else "false")
;;


(* =================== INTERMIDIATE =================== *)
(* flattern a list *)
type 'a node = 
  | One of 'a
  | Many of 'a node list
;;

let flatten a_lst = 
  let rec aux acc = function
    | [] -> acc
    | One a :: t -> aux (a :: acc) t
    | Many lst :: t -> aux (aux acc lst) t
in
reverse (aux [] a_lst)
;;

let result = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

let print_lst lst =
  let rec aux = function
    | [] -> print_string "\n"
    | h :: t -> 
      print_string h;
      print_string " ";
      aux t
    in
    aux lst
  ;;

print_lst result;;

(* eliminate duplicates *)

let element_of k lst =
  let rec aux = function
    | [] -> false
    | h :: t ->
      if h = k then true
      else aux t
    in 
    aux lst
  ;;

let compress lst = 
  let rec aux acc = function
    | [] -> acc
    | h :: t -> 
      if element_of h acc then
        aux acc t
      else 
        aux (h :: acc) t
      in
      reverse (aux [] lst)
    ;;

let compress_res = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

print_string "Result of compress_res: ";;
print_lst compress_res;;

(* pack dupicates *)
let pack lst = 
  let rec aux acc curr_acc = function
    | [] -> acc
    | [x] -> (x :: curr_acc) :: acc
    | h :: (b :: _ as t) ->
      if h = b then aux acc (h :: curr_acc) t 
      else aux ((h :: curr_acc) :: acc) [] t in
      reverse (aux [] [] lst)
    ;;

let packed = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

let string_of_list lst =
  "[" ^ (String.concat "; " lst) ^ "]";;

let string_of_list_list lst =
  "[" ^ (String.concat "; " (List.map string_of_list lst)) ^ "]";;

print_endline (string_of_list_list packed);;

(* Decode a Run-Length Encoded List *)

(* #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

type 'a rle =
  | One of 'a
  | Many of int * 'a


let decode lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (num, y) :: t ->
      let rec append count curr =
        if count = num then curr
        else append (count + 1) (y :: curr)
      
      in
      aux (append 0 acc) t
    in

    reverse (aux [] lst)
  ;;

let encoded_list = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); Many (2, "d"); Many (4, "e")];;
let decoded_list = decode encoded_list;;

(* Print the decoded list *)
let () =
  List.iter print_string decoded_list;
  print_endline "";;

(* replicate the elements of a list of a given number of times *)

let replicate lst num =
  let rec aux acc  = function
    | [] -> acc
    | h :: t -> 
      let rec append count curr x =
        if count = num then
          curr
        else
          append (count + 1) (x :: curr) x
        in
        aux (append 0 acc h) t 
      in
      reverse (aux [] lst)
    ;;

let replicate_rst = replicate ["a"; "b"; "c"] 3;;

print_string "replicated list: ";;
print_lst replicate_rst;;

(* drop every nth elemnt from a list *)

let drop lst n = 
  let rec aux acc count = function
    | [] -> acc
    | h :: t ->
      if count = n then
        aux acc (count + 1) t
      else
        aux (h :: acc) (count + 1) t
      in 
      reverse (aux [] 1 lst)
    ;;

let drop_res = drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

print_lst drop_res;;

(* extract a slice from a list *)
let slice lst l r =
  let rec aux acc count = function
    | [] -> acc
    | h :: t ->
      if count >= l && count <= r then
        aux (h :: acc) (count + 1) t
      else
        aux acc (count + 1) t
      in
      reverse (aux [] 0 lst)
    ;;

let slice_res = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;

print_lst slice_res;;

(* rotate a list N places to the left *)
let rotate lst n = 
  let rec aux temp acc count = function
    | [] -> 
      let rec add res_acc = function
        | [] -> res_acc
        | h :: t -> add (h :: res_acc) t
      in
      add acc (reverse temp)
    | h :: t ->
      if count <= n then
        aux (h :: temp) acc (count + 1) t
      else 
        aux temp (h :: acc) (count + 1) t
      in
      reverse (aux [] [] 1 lst)
    ;;

let rotate_res = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;

print_lst rotate_res;;

(* let split list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
                     else aux (i - 1) (h :: acc) t  in
  aux n [] list

let rotate list n =
  let len = List.length list in
  (* Compute a rotation value between 0 and len - 1 *)
  let n = if len = 0 then 0 else (n mod len + len) mod len in
  if n = 0 then list
  else let a, b = split list n in b @ a;; *)

(* extract a given number of Randomly Selected Elements From a List *)

let rand_select list n =
  Random.init 0;
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
  in
  let extract_rand list len = 
    extract [] (Random.int len) list
  in
  let rec aux n acc list len = 
    if n = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (n - 1) (picked :: acc) rest (len - 1)
    in
    let len = List.length list in
      aux (min n len) [] list len
  ;;

let rand_select_res = rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 in
  print_lst rand_select_res;;

(* Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List *)
