val with_command_in : (in_channel -> 'a) -> string -> 'a
val command : int -> string
val home : unit -> string
val date : string -> string
val touch : Unix.file_perm -> string -> unit
