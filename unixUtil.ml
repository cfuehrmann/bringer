let with_command_in command f =
	let ic = Unix.open_process_in command in
	try
		let result = f ic
		and _ = Unix.close_process_in ic in
		result
	with
	| e ->
			let _ = Unix.close_process_in ic in
			raise e

let with_command command f =
	let ic = Unix.open_process command in
	try
		let result = f ic
		and _ = Unix.close_process ic in
		result
	with
	| e ->
			let _ = Unix.close_process ic in
			raise e

let command_of pid =
	let c = Printf.sprintf "ps -p %d -o command=" pid
	and f ic =
		let line = input_line ic in
		Str.replace_first (Str.regexp "^[^ ]*/") "" line in
	try
		with_command_in c f
	with
	| End_of_file -> "--unknown--"

let home () = with_command_in "echo ~" input_line

let date format = with_command_in ("date " ^ format) input_line

let touch mod_mask path =
	let fd = Unix.openfile path [ Unix.O_CREAT ] mod_mask in
	Unix.close fd

