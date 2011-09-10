val current_desktop : unit -> int
val windows_per_desktop : unit -> (int, int * string * string * string) Hashtbl.t
val window_list_per_desktop : unit -> (int, (int * string * string * string) list) Hashtbl.t
val desktop_list : ((int, (int * string * string * string) list) Hashtbl.t) -> (int * (int * string * string * string) list) list
