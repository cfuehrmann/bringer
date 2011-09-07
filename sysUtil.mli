val with_file_in : string -> (in_channel -> 'a) -> 'a
val with_file_out : string -> (out_channel -> 'a) -> 'a
val line_frequencies : string -> (string, int * int) Hashtbl.t
val prepend : string -> string -> out_channel -> unit
val time : ('a -> 'b) -> 'a -> 'b
val filter_channel : in_channel -> out_channel -> (string -> bool) -> unit
val filter_file : string -> out_channel -> (string -> bool) -> unit
