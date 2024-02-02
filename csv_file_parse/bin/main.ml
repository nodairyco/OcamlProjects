open Stream_manipulation.Stream_manipulation
open Basic_stream_functions
open Stream_manipulation.File_interactions
open Stream_manipulation.Generator
open Stream_manipulation.Csv_parser 
;;

random_text_generator "bin/output.csv";;
let input_list = parse_csv_file "bin/output.csv";;

let output_list = 
  let temp_list = (List.map (fun s -> String.to_seq s) input_list) in
  List.map (fun seq -> List.of_seq seq) temp_list |> flatten |> 
  List.map (fun y -> 
    if Random.int 3 = 3 then 
      string_of_int (int_of_char y) 
    else String.make 1 y)
;;

write_file "bin/output.txt" output_list 