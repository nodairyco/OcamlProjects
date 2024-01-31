
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