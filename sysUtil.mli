val with_file_in : (in_channel -> 'a) -> string -> 'a
val line_frequencies : string -> (string, int * int) Hashtbl.t
val insert_at_beginning : string -> string -> unit
val time : ('a -> 'b) -> 'a -> 'b
