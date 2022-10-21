val rtype : string list
val itype : string list
val stype : string list
val utype : string list

val process_rtype :
  string ->
  string ->
  string ->
  string ->
  (int * bool) Registers.RegisterFile.t ->
  (int * bool) Registers.RegisterFile.t

val process_itype :
  string ->
  string ->
  string ->
  int ->
  (int * bool) Registers.RegisterFile.t ->
  (int * bool) Registers.RegisterFile.t

val process_utype :
  string ->
  string ->
  string ->
  string ->
  (int * bool) Registers.RegisterFile.t ->
  (int * bool) Registers.RegisterFile.t

val process_stype :
  string ->
  string ->
  string ->
  string ->
  (int * bool) Registers.RegisterFile.t ->
  (int * bool) Registers.RegisterFile.t

val process_input_insns : string list -> unit
