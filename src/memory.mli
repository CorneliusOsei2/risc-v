module Memory : Map.S
(* Simulates processor's memory *)

val init : (Int32.t * bool) Memory.t
(** [init] returns a memory with all memory addresses initialized to 0*)

val pp_memory : (Int32.t * bool) Memory.t -> unit
(** [pp_memory mem] pretty prints the memory [mem]*)

val get_memory : Memory.key -> ('a * bool) Memory.t -> 'a
(** [get_memory addr mem] returns the first of the value stored at [addr] in memory 
    [mem] *)

val update_memory :
  int -> Int32.t -> (Int32.t * bool) Memory.t -> (Int32.t * bool) Memory.t
(** [update_memory addr mem] returns the first of the value stored at [addr] in memory 
    [mem] *)