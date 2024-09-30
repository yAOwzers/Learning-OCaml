(* read_dict : unit -> (int x string) list *)

let rec read_dict () = 
  try
    let i = read_int () in (* read int*)
      if i = 0 then [] else (* if zero, we are done*)
        let name = read_line () in (* otherwise, read a name *)
          (i, name) :: read_dict () (* build a dict entry, fetch another *)
  with
    Failure _ ->
      print_string "This is not a valid integer. Please try again. ";
      print_newline ();
      read_dict ()
      ;;

(* let value = read_dict ();; *)

type pair_list = (int * string) list;;

let print_pair (x, y) =
  Printf.printf "(%d, \"%s\")" x y

(* Function to print the list of pairs *)
let print_pair_list lst =
  Printf.printf "[";
  let rec aux = function
    | [] -> Printf.printf "]\n"
    | [pair] -> print_pair pair; aux []  (* Print last element without a comma *)
    | pair :: rest -> print_pair pair; Printf.printf "; "; aux rest
  in
  aux lst
;;

(* print_pair_list value;; *)

let entry_to_channel ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'
;;

let dictionary_to_channel ch d = 
  List.iter (entry_to_channel ch) d
;;

let dictionary_to_file filename dict =
  let ch = open_out filename in
    dictionary_to_channel ch dict;
    close_out ch
;;

dictionary_to_file "file.txt" (read_dict ());;

let entry_of_channel ch =
  let number = input_line ch in
    let name = input_line ch in
      (int_of_string number, name)
;;

let rec dictionary_of_channel ch = 
  try
    let e = entry_of_channel ch in
      e :: dictionary_of_channel ch
  with
    End_of_file -> []
;;

let dictionary_of_file filename = 
  let ch = open_in filename in
    let dict = dictionary_of_channel ch in
      close_in ch;
      dict
;;

print_pair_list (dictionary_of_file "file.txt");;