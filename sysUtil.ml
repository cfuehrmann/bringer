module String = ExtLib.String
module Hashtbl = ExtLib.Hashtbl

open ListUtil

let with_file_in name f =
	let ic = open_in name in
	try
		let result = f ic
		and _ = close_in ic in
		result
	with
	| e ->
			let _ = close_in ic in
			raise e

let with_file_out name f =
	let ic = open_out name in
	try
		let result = f ic
		and _ = close_out ic in
		result
	with
	| e ->
			let _ = close_out ic in
			raise e

let line_frequencies file_name =
	let result = Hashtbl.create 50 in
	try
		with_file_in file_name (fun ic ->
						let rec loop i =
							(let line =
									let line = input_line ic in
									String.strip line in
								match Hashtbl.find_option result line with
								| Some (count, most_recent) ->
										Hashtbl.replace result line (count + 1, most_recent)
								| None -> Hashtbl.add result line (1, i));
							loop (i + 1)
						in loop 0)
	with
	| End_of_file -> result

(* A bit like "cat", except that the first argument is a string *)
let prepend line in_file oc =
	with_file_in in_file (fun ic ->
					output_string oc (line ^ "\n");
					try
						while true do
							let l = input_line ic in
							output_string oc (l ^ "\n")
						done
					with
					| End_of_file -> ())

let filter_channel ic oc p =
	try
		while true do
			let l = input_line ic in
			if p l then output_string oc (l ^ "\n")
		done
	with
	| End_of_file -> ()

let filter_file in_file oc p =
	with_file_in in_file (fun ic -> filter_channel ic oc p)

let time f x =
	let t = Sys.time() in
	let result = f x in
	Printf.printf "Took %fs\n" (Sys.time() -. t);
	result
