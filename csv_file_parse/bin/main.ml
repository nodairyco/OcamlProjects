open Stream_manipulation.Stream_manipulation
open Basic_stream_functions
open Stream_manipulation.File_interactions
let file_input_list = read_file "bin/test.txt";;
let file_output_list = 
  let a = filter file_input_list (fun x -> match x with "" -> false |_ -> true)
in
  let b = parse_list a 
in 
  map (map b (fun x -> x*x + 9)) (fun x -> string_of_int x)
;;

write_file "bin/output.txt" file_output_list;;