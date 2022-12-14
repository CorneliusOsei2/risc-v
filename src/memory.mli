(** Implements a RISC-V Processor Memory. *)

module Memory : Map.S
(** Simulates a byte-addressable 32-bit memory. 
  Memory addresses are word-aligned. *)

val init : unit -> (Int32.t * bool) Memory.t
(** [init ()] is a memory with all memory addresses initialized to 0.*)

val pp_memory : (Int32.t * bool) Memory.t -> unit
(** [pp_memory mem] pretty prints the memory [mem].*)

val get_memory : int -> ('a * bool) Memory.t -> 'a
(** [get_memory addr mem] returns the first of the value stored at [addr] in memory 
    [mem].*)

val update_memory :
  int -> Int32.t -> (Int32.t * bool) Memory.t -> (Int32.t * bool) Memory.t
(** [update_memory addr mem] returns the first of the value stored at [addr] in memory 
    [mem]. *)