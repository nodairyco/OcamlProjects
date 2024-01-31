
(*this module handles manipulating streams from files in various ways*)

(*making sig-type for module to hide aux methods*)
module type Stream_manipulation = 
sig
  val parse_list:string list -> int list
end;;

module Stream_manipulation:Stream_manipulation = struct
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
    |x::xs -> aux_parse_list xs @ [int_of_string x]
    
    (*first filters list, than parses it*)
    let parse_list (list:string list) = 
      let filtered_list = filter list can_be_turned_to_int 
    in
      aux_parse_list filtered_list
end;;