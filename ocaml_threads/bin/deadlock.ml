open Cell.Cell
open Lock.Lock
open Thread 
open Event 

(*program that waites forever (lovely)*)
let dead = 
  let l1 = new_lock () in 
  let l2 = new_lock () in 
  let th (l1,l2) = 
    let a1 = acquire l1 in 
    let _ = delay 1.0 in 
    let a2 = acquire l2 in 
    release a2; release a1; 
    print_int (id (self ()));
    print_endline "finished"
  in let t1 = create th (l1,l2)
in let t2 = create th (l2,l1)
in join t1
;;

