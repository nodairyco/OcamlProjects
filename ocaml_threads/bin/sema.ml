module type Sema = sig
  type sema 
  val new_sema : int -> sema 
  val up : sema -> unit 
  val down : sema -> unit 
end

module Sema:Sema = struct
  open Thread 
  open Event
  open Lock.Lock
  open Queue
  type sema = unit channel option channel 
  let up sema = sync (send sema None)
  let down sema = 
    let ack = (new_channel ():unit channel) in 
    sync  (send sema (Some ack));
    sync  (receive ack)
  let new_sema n =
    let sema = new_channel () in 
    let rec serve (n,(q:'a queue)) = match sync (receive sema) with
    None -> (match dequeue q with
      (None, q) -> serve (n+1, q)
      |(Some ack, q) -> sync (send ack ()); serve (n,q))
    |Some ack -> 
      if n>0 then 
        (sync (send ack ()); serve (n-1,q))
      else
        serve (n, enqueue ack q) 
      in let _ = create serve (n, new_queue())
    in sema 
end