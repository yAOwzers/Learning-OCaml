(* (2 marks, plus 1 mark of extra credit) Recall that an important property of a tree-based
dictionary is height – i.e., the longest path from root to any leaf. This function calculates height:
let rec height (t : 'a tree) = match t with
| Lf -> 0
| Br (l, _, r) -> 1 + max (height l) (height r);;

Height is directly related to the worst-case lookup/insertion time, since both of these are O(h),
where h is the height of the dictionary, since the lookup or insertion might by misfortune
proceed along the longest path in the tree.
Notice that the definition of “height” above is slow (actually it’s O(n), since it needs to visit
every node). Please write a new kind of tree-based dictionary that, in addition to supporting
O(log n) lookup/insert, also supports an O(1) height function. Note: by O(log n), I mean the
wishy-washy average-case time bound supported by the book’s implementation, i.e. you do not
need to try to code up a worst-case tree. That is, fill in the following blanks: *)

type ('k, 'v) dict_tree = 
  | Lf
  | Br of int * 'k * 'v option * ('k, 'v) dict_tree * ('k, 'v) dict_tree
;;

let empty_dict : ('k, 'v) dict_tree = Lf ;;
(* Should take O(1) time, always *)

let rec height (t : ('k, 'v) dict_tree) =
  match t with
  | Lf -> 0
  | Br (h, _, _, _, _) -> h
;;

let max v1 v2 = 
  if v1 > v2 then v1
  else v2
;;

let update_height = function
  | Lf -> 0
  | Br (_, _, _, l, r) -> 1 + max (height l) (height r)
;;

(* Should take O(log m) time on wishy-washy average *)
let rec lookup (tr : ('k, 'v) dict_tree) k = 
  match tr with
  | Lf -> None
  | Br (_, k', v, l, r) -> 
    if k = k' then v (*found key*)
    else if k < k' then lookup l k (*go left since smaller*)
    else lookup r k
  ;;


(* Should take O(log m) time on wishy-washy average *)
let rec insert (tr : ('k, 'v) dict_tree) k v =
  match tr with
  | Lf -> Br (1, k, Some v, Lf, Lf) (* insert at the end *)
  | Br (_, k', v', l, r) -> 
    if k = k' then Br (height tr, k, Some v, Lf, Lf)
    else if k < k' then 
      let new_left = insert l k v in
      let new_height = update_height (Br (0, k', v', new_left, r)) in
      Br (new_height, k', v', insert l k v, r)
    else 
      let new_right = insert r k v in
      let new_height = update_height (Br (0, k', v', l, new_right)) in
      Br (new_height, k', v', l, new_right)
;;

let () =
  let dict = empty_dict in
  let dict = insert dict 5 "five" in
  let dict = insert dict 3 "three" in
  let dict = insert dict 8 "eight" in
  match lookup dict 5 with
  | None -> print_endline "Not found"
  | Some value -> print_endline value;
  Printf.printf "Height of the tree: %d\n" (height dict)
;;

(* For 1 mark of extra credit, also implement delete (using the deletion strategy from week 8, i.e.
you are not responsible for maintaining balance): *)

(* 4 cases
1. Tree is empty
2. Key to delete has one child
3. Key to delete has two children -> replace the node with in-order successor or predecessor
4. Key to delete is a leaf node *)

let rec find_min = function
  | Lf -> raise (Invalid_argument "Tree is Empty")
  | Br (_, k, v, Lf, _) -> (k, v) (* smallest since it doesnt have a left child, right child larger*)
  | Br (_, _, _ , l, _) -> find_min l
;;

(* ref: https://www.youtube.com/watch?v=DkOswl0k7s4&ab_channel=NoobCoder *)
(* Should take O(log m) time on wishy-washy average *)
let rec delete (tr : ('k, 'v) dict_tree) k = 
  match tr with
  | Lf -> Lf
  | Br (_, k', v', l, r) ->
    if k = k' then
      match l, r with
      | Lf, Lf -> Lf (* leaf node, no children *)
      | Lf, _ -> r (* only has a right child *)
      | _, Lf -> l (* only has a left child *)
      | _ -> (* two children *)
        let min_key, min_val = find_min r in 
        let new_right = delete r min_key in (* since the right child is larger, it should replace the node*)
        let new_height = update_height (Br (0, min_key, min_val, l, new_right)) in
        Br (new_height, min_key, min_val, l, new_right)
    else if k < k' then
      let new_left = delete l k in
      let new_height = update_height (Br (0, k', v', new_left, r)) in
      Br (new_height, k', v', new_left, r)
    else 
      let new_right = delete r k in
      let new_height = update_height (Br (0, k', v', l, new_right)) in
      Br (new_height, k', v', l, r) 
;;


(* Pretty print the tree with indentation *)
let rec print_tree ?(indent="") = function
  | Lf -> Printf.printf "%sLf\n" indent
  | Br (_, key, value, left, right) ->
      Printf.printf "%sBr (key: %s, value: %s)\n" indent (string_of_int key) 
        (match value with
         | Some v -> v
         | None -> "None");
      let new_indent = indent ^ "  " in
      print_tree ~indent:new_indent left;
      print_tree ~indent:new_indent right
  ;;

  let () =
  let dict = empty_dict in
  let dict = insert dict 5 "five" in
  let dict = insert dict 3 "three" in
  let dict = insert dict 8 "eight" in
  let dict = insert dict 7 "seven" in
  Printf.printf "Tree after insertions:\n";
  print_tree dict;
  
  let dict = delete dict 5 in
  Printf.printf "\nTree after deletion of key 5:\n";
  print_tree dict
;;

(* (1 mark, plus 1 mark of extra credit) Write
partition_tree : ‘a -> ‘a tree -> (‘a tree * ‘a tree)
that divides an ordered (i.e. sorted from left to right) tree into two subtrees such that the first
subtree contains all of the elements less than or equal to a given element and the second
subtree contains the rest of the elements. Important: neither subtree should have height
greater than the original tree and both subtrees should still be ordered. *)

type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree
;;

let rec partition_tree k tr = 
  match tr with
  | Lf -> (Lf, Lf)
  | Br (a, l, r) ->
    if a <= k then
      let (l1, r1) = partition_tree a r in
      (Br (a, l, l1), r1)
    else
      let (l2, r2) = partition_tree a l in
      (l2, Br (a, r2, r))
    ;;
  
let t = 
  Br (16,
    Br (8,
      Br (4, Br (2, Br (1, Lf, Lf), Br (3, Lf, Lf)),
      Br (6, Br (5, Lf, Lf), Br (7, Lf, Lf))),
    Br (12, Br (10, Br (9, Lf, Lf), Br (11, Lf, Lf)), Lf)),
  Br (24,
    Br (20, Br (18, Br (17, Lf, Lf), Br (19, Lf, Lf)),
    Br (22, Br (21, Lf, Lf), Br (23, Lf, Lf))),
  Br (28, Br (26, Br (25, Lf, Lf), Br (27, Lf, Lf)),
  Br (30, Br (29, Lf, Lf), Br (31, Lf, Lf)))))
;;

let (left, right) = partition_tree 12 t

let rec print_tree indent = function
  | Lf -> Printf.printf "%sLf\n" indent
  | Br (x, left, right) ->
    Printf.printf "%sBr (%d)\n" indent x;
    print_tree (indent ^ "  ") left;
    print_tree (indent ^ "  ") right

let () =
  Printf.printf "Left tree (<= 12):\n";
  print_tree "" left;
  Printf.printf "\nRight tree (> 12):\n";
  print_tree "" right
  ;;
  