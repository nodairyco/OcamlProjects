(*the job of this module is storing basic functions of java's stream api*)


(*filters a list*)
let rec filter list predicate = match list with
  |[] -> []
  |x::xs -> 
    if predicate x then 
      filter xs predicate @ [x]
    else
      filter xs predicate
;;

(*maps a list to another list*)
let rec map list mapping = match list with 
  |[] -> []
  |x::xs -> [mapping x] @ map xs mapping 
;;

(*sums all ints in a list*)
let rec sum (list:int list) = match list with 
  |[] -> 0
  |x::xs -> x + sum xs
;;

(*returns true if all elements of a list match the predicate*)
let rec all_match list predicate = match list with
  |[] -> true
  |x::xs -> 
    if !(predicate x) then
      false
    else
      all_match xs predicate
;;

(*returns true if any match*)
let rec any_match list predicate = match list with
  |[] -> false
  |x::xs -> 
    if predicate x then
      true
    else 
      any_match xs predicate
;;

(*concatinates two lists*)
let rec concat l1 l2 = match l1 with
  |[] -> l2 
  |x::xs -> [x] @ concat xs l2 
;;

(*returns the head of the list*)
let head list = match list with
  |[] -> None
  |x::_xs -> Some x
;;

(*returns the final element of the list*)
let tail list = match List.rev list with 
  |[] -> None
  |x::_xs -> Some x 
;;

(*works the same as forEach in java(kinda)*)
let rec consume list (consumer:('a -> unit)) = match list with 
  |[] -> []
  |x::xs -> consumer x; consume xs consumer
;;

(*returns n-th element of a list*)
let nth_element list n = 
  let y = n 
in 
let rec inner_loop in_list in_num control_num = match in_num with
  |0 -> 
    (if control_num<0 then 
      match List.rev in_list with [] -> failwith "index out of bounds"
    |x::_xs -> x
    else 
      match in_list with [] -> failwith "index out of bounds"
    |x::_xs -> x) 
  |_ when in_num > 0 -> (match in_list with [] -> failwith "index out of bounds"
    |_x::xs -> inner_loop xs (in_num - 1) control_num)
  |_ when in_num < 0 -> (match List.rev in_list with [] -> failwith "index out of bounds"
    |_x::xs -> inner_loop xs (in_num + 1) control_num)
    in 
  inner_loop list y n 
;;

(*finds the first matching element in the list and returns it*)
let rec find_match (list:'a list) (match_function:'a -> bool) = match list with 
  |[] -> None
  |x::xs -> 
    if match_function x then
      Some x
    else
      find_match xs match_function
;;

let rec generate_stream (generator_function:unit -> 'a) (length:int)= match length with 
0 -> []
|_ -> generator_function () :: generate_stream generator_function (length -1)
;;