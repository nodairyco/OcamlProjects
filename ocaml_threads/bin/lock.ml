module type Lock = sig
  type lock
  type ack 
  val new_lock : unit -> lock 
  val acquire : lock -> ack 
  val release : ack -> unit  
end

(*much like read/write lock from java, a bunch of tomfuckery does happen*)
module Lock:Lock = struct
  open Event
  open Thread
  type ack = unit Event.channel 
  type lock = ack Event.channel 
  let acquire (lock:lock) = 
    let ack = new_channel () in 
    sync (send lock (ack)); 
    ack 
  let release (ack:ack) = sync (send ack ()) 
  let new_lock () = let lock = new_channel () in 
  let rec acq_server () = rel_server (sync (receive lock)) and 
  rel_server ack = sync (receive ack); acq_server ()
  in 
    let _ = create acq_server ()
  in 
    lock 
end