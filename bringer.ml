(* #!/usr/bin/lablgtk2 todo: unit tests? github + co? gtk? proper          *)
(* treatment of over-long window descriptions                              *)

module String = ExtLib.String
module Hashtbl = ExtLib.Hashtbl
open ListUtil
open UnixUtil
open WmUtil
open SysUtil

let history_file = home () ^ "/" ^ ".bringerHistory"

let desktop_lines =
	let f =
		let cd = current_desktop () in
		fun s (d, l) ->
				match l with
				| [] -> ""
				| (w, c) :: _ ->
						let star = if d = cd then " *" else " "
						and description = string_of_list snd " --- " l in
						s ^ "desktop" ^ string_of_int d ^ star ^ description ^ "\n" in
	List.fold_left f "" (desktop_list ())

let history_list =
	let l =
		list_from_hashtbl (line_frequencies history_file) in
	let compare (c1, (f1, mr1)) (c2, (f2, mr2)) =
		let n = compare f2 f1 in
		if n = 0 then compare mr1 mr2 else n in
	List.sort compare l

let history_lines =
	match history_list with
	| [] -> ""
	| (c, f) :: t ->
			let rec loop s = function
				| [] -> s
				| (c, f) :: t -> loop (s ^ "\n" ^ c) t
			and s =
				let t = Unix.localtime (Unix.time ()) in
				let dashes = String.make (73 - String.length c) '-' in
				Printf.sprintf "%s %s %d:%02d" c dashes t.Unix.tm_hour t.Unix.tm_min in
			loop s t

let exec_with_history command =
	let pid = Unix.fork () in
	if pid = 0 then
		let _ = Unix.execvp "sh" [| command; "-c"; ("exec " ^ command) |] in
		exit (-1)
	else insert_at_beginning history_file command

let _ = touch 0o600 history_file

let _ =
	try
		let user_input =
			let ic, oc = Unix.open_process "dmenu -i -l 13" in
			try
				output_string oc desktop_lines;
				output_string oc history_lines;
				output_string oc ("\n" ^ String.make 80 '-' );
				begin
					let ic = Unix.open_process_in "dmenu_path" in
					try
						while true do
							let line = input_line ic in
							output_string oc ("\n" ^ line)
						done
					with
					| End_of_file ->
							let _ = Unix.close_process_in ic in
							()
					| e ->
							let _ = Unix.close_process_in ic in
							raise e
				end;
				close_out oc;
				let input = input_line ic
				and _ = Unix.close_process (ic, oc) in
				input
			with
			| e ->
					let _ = Unix.close_process (ic, oc) in
					raise e in
		if Str.string_match (Str.regexp "^desktop\\([0-9]+\\) .*") user_input 0 then
			let _ =
				Sys.command ("wmctrl -s" ^ (Str.matched_group 1 user_input)) in
			()
		else if Str.string_match (Str.regexp "^-*$") user_input 0 then ()
		else
			let command =
				if Str.string_match (Str.regexp "^\\(.*\\)[ ]----*.*$") user_input 0 then
					Str.matched_group 1 user_input
				else user_input in
			exec_with_history command
	with
	| End_of_file -> ()
