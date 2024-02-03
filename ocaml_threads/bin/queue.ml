  type 'a queue = Queue of 'a list * 'a list;;

  let is_queue_empty = function 
      Queue([],[])-> true
      | _ -> false
  ;;

  let queue_to_list = function
      |Queue (first, []) -> first
      |Queue (first, last) -> first @ List.rev last
  ;;

  let enqueue x (Queue(first,last)) = Queue(first, x::last)
  ;;
  let dequeue = function 
    |Queue([],last)-> (
      match List.rev last with 
        | [] -> (None, Queue([],[]))
        | x::xs -> (Some x, Queue(xs, []))
        )
    |Queue(x::xs, last) -> (Some x, Queue(xs,last))
  ;;
let new_queue () = Queue([],[]);;