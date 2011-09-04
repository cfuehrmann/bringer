module String = ExtLib.String
module Hashtbl = ExtLib.Hashtbl
open ListUtil

let line_frequencies file_name =
	let result = Hashtbl.create 50 in
	begin
		let ic = open_in file_name in
		try
			let rec loop i =
				begin
					let line =
						let line = input_line ic in
						String.strip line in
					match Hashtbl.find_option result line with
					| Some (count, most_recent) -> 
						Hashtbl.replace result line (count + 1, most_recent)
					| None -> Hashtbl.add result line (1, i)
				end;
				loop (i + 1)
			in loop 0
		with
		| End_of_file -> close_in ic
		| e -> close_in ic; raise e
	end;
	result

let insert_at_beginning file_name line =
	let lines =
		let ic = open_in file_name in
		try
			let rec loop lines =
				try
					let line = input_line ic in
					loop (line :: lines)
				with
				| End_of_file -> close_in ic; lines
			in List.rev (loop [])
		with
		| e -> close_in ic; raise e in
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
