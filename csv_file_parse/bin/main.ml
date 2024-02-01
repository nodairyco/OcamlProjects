open Stream_manipulation.Stream_manipulation
open Basic_stream_functions
open Stream_manipulation.File_interactions
open Csv
open Stream_manipulation.Generator 

let file_input_list = read_file "bin/test.txt";;
let file_output_list = 
  let a = filter file_input_list (fun x -> match x with "" -> false |_ -> true)
in
  let b = parse_list a 
in 
  Basic_stream_functions.map (Basic_stream_functions.map b (fun x -> x*x + 9)) 
  (fun x -> string_of_int x)
;;

write_file "bin/output.txt" file_output_list;;

let rec loop (func:unit) num = match num with 
0 -> func 
|_ -> func; loop func (num-1);;

loop random_text_generator 4 ;;