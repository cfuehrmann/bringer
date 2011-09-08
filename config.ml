let description_of_window windowId command arguments host title =
	match command with
	| "urxvt" | "chrome" | "java" | "python2.7" ->
			command ^ ": " ^ title
	| "--unknown--" -> title
	| _ ->
			command ^ " " ^ arguments
