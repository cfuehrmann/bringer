val with_command_in : string -> (in_channel -> 'a) -> 'a
val command : int -> string
val home : unit -> string
val date : string -> string
val touch : Unix.file_perm -> string -> unit
