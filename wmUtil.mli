val current_desktop : unit -> int
val windows_per_desktop : unit -> (int, int * string) Hashtbl.t
val window_list_per_desktop : unit -> (int, (int * string) list) Hashtbl.t
val desktop_list : unit -> (int * (int * string) list) list
