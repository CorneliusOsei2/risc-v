module Memory : Map.S

val memory_init : (int * bool) Memory.t
(** [init] returns a memory with all memory addresses initialized to 0*)

val pp_memory : (int * bool) Memory.t -> unit
(** [pp_memory mem] pretty prints the memory [mem]*)

val get_memory : int -> ('a * bool) Memory.t -> 'a
(** [get_memory addr mem] returns the first of the value stored at [addr] in memory 
    [mem] *)

val update_memory : int -> 'a -> ('a * bool) Memory.t -> ('a * bool) Memory.t
(** [update_memory addr mem] returns the first of the value stored at [addr] in memory 
    [mem] *)