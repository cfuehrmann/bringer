module String = ExtLib.String
module Hashtbl = ExtLib.Hashtbl

open ListUtil

let with_file_in f name =
	let ic = open_in name in
	try
		let result = f ic
		and _ = close_in ic in
		result
	with
	| e ->
			let _ = close_in ic in
			raise e

let line_frequencies file_name =
	let result = Hashtbl.create 50 in
	let f ic =
		let rec loop i =
			(let line =
					let line = input_line ic in
					String.strip line in
				match Hashtbl.find_option result line with
				| Some (count, most_recent) ->
						Hashtbl.replace result line (count + 1, most_recent)
				| None -> Hashtbl.add result line (1, i));
			loop (i + 1)
		in loop 0 in
	try
		with_file_in f file_name
	with
	| End_of_file -> result

let insert_at_beginning file_name line =
	let lines =
		let f ic =
			let rec loop lines =
				try
					let line = input_line ic in
					loop (line :: lines)
				with
				| End_of_file -> lines
			in List.rev (loop []) in
		with_file_in f file_name in
	let oc = open_out file_name in
	try
		output_string oc (string_of_list (fun s -> s) "\n" (line :: lines));
		close_out oc
	with
	| e -> close_out oc; raise e

let time f x =
	let t = Sys.time() in
	let result = f x in
	Printf.printf "Took %fs\n" (Sys.time() -. t);
	result
