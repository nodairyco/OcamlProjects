open Stream_manipulation.Stream_manipulation
open Basic_stream_functions

let temp_list = parse_list ["hi";"hello";"1";"2";"3"];;

let concat_list = concat [1;2;3] [4;5;6]
;;

(*consume concat_list (fun (x:int) -> print_endline (string_of_int x));;*)
let print_f b = Format.printf "%b@ \n" b;;

print_int (nth_element concat_list 2);
print_endline "";
print_int (nth_element concat_list (-2));
print_endline "";;

let example_list = map [1;2;3] (fun x -> x*x)
;;