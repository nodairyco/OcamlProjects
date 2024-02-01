
(*this module handles manipulating streams from files in various ways*)

(*making sig-type for module to hide aux methods*)
module type Stream_manipulation_sig = 
sig
  val parse_list:string list -> int list
end;;

(*making sig-type for module that will read and write to files*)
module type File_interactions_sig = sig
  val read_file:string -> string list
  val write_file:string -> string list -> unit
end;;

(*module that populates a file with random characters on each line*)
module type File_populator = sig
  val random_text_generator:string -> unit 
end;;

(*module that handles basic stream functions*)
module Stream_manipulation:Stream_manipulation_sig= struct
    open Basic_stream_functions
    (*checkes if a string can be converted to number*)
    let can_be_turned_to_int input_string = 
      try 
        let _y = int_of_string input_string in
        true
      with
      |Failure msg -> false
    (*auxilliary method that parses a list*)
    let rec aux_parse_list (list:string list) = match list with 
    |[] -> []
    |x::xs -> [int_of_string x] @ aux_parse_list xs
    
    (*first filters list, than parses it*)
    let parse_list (list:string list) = 
      let filtered_list = filter list can_be_turned_to_int 
    in
      aux_parse_list filtered_list
end;;

(*module for File interactions*)
module File_interactions:File_interactions_sig = struct
  let read_file (file_name:string) = 
    let file = open_in file_name in
    let rec aux_read_file () = 
      try
        let line = input_line file in
        line :: aux_read_file ()
      with
      |End_of_file -> []
    in
    aux_read_file ()
  
  let write_file (file_name:string) (list:string list) = 
    let ic = open_out file_name in
    let rec aux_write_file (list:string list) = match list with
    |[] -> ()
    |x::xs -> Printf.fprintf ic "%s\n" x; aux_write_file xs
  in
    aux_write_file list; close_out ic
end;;

module Generator:File_populator = struct
  (*util module for Generator*)
  module Generator_util = struct
    let array_of_available_chars : char list= 
    Basic_stream_functions.concat
    ['1';'2';'3';'4';'5';'6';'7';'8';'9';'0']
    (Basic_stream_functions.concat 
      (Basic_stream_functions.map 
        ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';
        'm';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] 
        (fun x -> Char.uppercase_ascii x))
      (['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';
        'm';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']))
    
    let generate_random_string = 
      let rec generator list int = match int with 
      |0 -> list 
      |_ -> Basic_stream_functions.nth_element array_of_available_chars 
      (Random.int (List.length array_of_available_chars - 1)) :: generator list (int - 1)
    in
      let rec construct_string (list:char list) acc = match list with
        |[] -> acc 
        |x::xs -> construct_string xs (acc^Char.escaped x)
    in
      Basic_stream_functions.generate_stream (fun () -> construct_string (generator [] (Random.int 90)) "") 200 
  end;;
  let random_text_generator =  
    let list =  Generator_util.generate_random_string 
  in 
  fun (y:string) -> File_interactions.write_file y list
end;;

