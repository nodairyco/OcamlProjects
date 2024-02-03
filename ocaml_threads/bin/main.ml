open Thread
open Event
open Cell.Cell
open Lock.Lock
open Queue 
open Sema.Sema
;;
module Echo = struct
  open Thread
  let echo () = print_string(read_line() ^ "\n")
  let main = let t1 = create echo ()in 
  join t1;
  print_int (id (self ()));
  print_string "\n"
end;;

module Exchange = struct
  open Event
  open Thread
  let thread ch = let x = sync(receive ch) in 
  print_endline x;
  sync (send ch "got it!")
  let main = let ch = new_channel () in 
  let _ = create thread ch in 
  print_endline "main is running...";
  sync(send ch "greetings!");
  print_endline ("He "^sync(receive ch)) 
end

let main = let x = new_cell 1
in print_int (get x); print_string "\n";
put x 2;
print_int (get x); print_string "\n"