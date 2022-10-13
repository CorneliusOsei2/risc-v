exception InvalidMemoryAddress

type t

module Memory : Map.S

val memory_load : int -> int Memory.t -> int
