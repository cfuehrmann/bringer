val with_command_in : string -> (in_channel -> 'a) -> 'a
val with_command : string -> (in_channel * out_channel -> 'a) -> 'a
val command_of : int -> string
val home : unit -> string
val date : string -> string
val touch : Unix.file_perm -> string -> unit
