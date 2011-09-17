let trim s n =
	if String.length s > n then Str.string_before s n ^ "..." else s

let description_of_window windowId command arguments host title =
	let d =
		match command with
		| "urxvt"
		| "chrome"
		| "java"
		| "python2.7" -> command ^ ": " ^ title
		| "firefox-bin" -> "Firefox: " ^ title
		| "emacs" -> command ^ " " ^ arguments
		| _ -> title in
	trim d 60
