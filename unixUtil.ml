let with_command_in f command =
	let ic = Unix.open_process_in command in
	try
		let result = f ic
		and _ = Unix.close_process_in ic in
		result
	with
	| e ->
			let _ = Unix.close_process_in ic in
			raise e

let command pid =
	let c = Printf.sprintf "ps -p %d -o command=" pid
	and f ic =
		let line = input_line ic in
		Str.replace_first (Str.regexp "^[^ ]*/") "" line in
	try
		with_command_in f c
	with
	| End_of_file -> raise Not_found

let home () = with_command_in input_line "echo ~"

let date format = with_command_in input_line ("date " ^ format)

let touch mod_mask file_name =
    let fd = Unix.openfile file_name [ Unix.O_CREAT ] mod_mask in
    Unix.close fd
