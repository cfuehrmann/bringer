module String = ExtLib.String
module Hashtbl = ExtLib.Hashtbl

open ListUtil
open UnixUtil
open WmUtil
open SysUtil

let history_file = home () ^ "/" ^ ".bringerHistory"

let description_of_window (windowId, command, host, title) =
	if Str.string_match (Str.regexp "^\\([^ ]*\\)\\(.*\\)$") command 0 then
		let c = Str.matched_group 1 command
		and args = String.strip (Str.matched_group 2 command) in
		Config.description_of_window windowId c args host title
	else command

let desktop_lines desktop_list =
	let f =
		let cd = current_desktop () in
		fun s (d, l) ->
				let star = if d = cd then "*" else " "
				and description = string_of_list description_of_window " ----- " l in
				s ^ star ^ string_of_int d ^ " " ^ description ^ "\n" in
	List.fold_left f "" desktop_list

let history_list history_file =
	let l = list_from_hashtbl (line_frequencies history_file) in
	let compare (c1, (f1, mr1)) (c2, (f2, mr2)) =
		let n = compare f2 f1 in
		if n = 0 then compare mr1 mr2 else n in
	List.sort compare l

let history_lines history_list =
	match history_list with
	| [] -> ""
	| (c, f) :: t ->
			let rec loop s = function
				| [] -> s
				| (c, f) :: t -> loop (s ^ "\n" ^ c) t
			and s = Printf.sprintf "%s %s %s" c "*****" (date "+'%a %e, %H:%M'") in
			loop s t

let exec_with_history command =
	let pid = Unix.fork () in
	if pid = 0 then
		let _ = Unix.execvp "sh" [| command; "-c"; ("exec " ^ command) |] in
		exit (-1)
	else
		let tmp = history_file ^ ".tmp" in
		with_file_out tmp (prepend command history_file);
		Sys.rename tmp history_file

let delete_from_history line =
	let tmp = history_file ^ ".tmp" in
	with_file_out tmp (fun oc ->
					filter_file history_file oc (fun l -> l <> line));
	Sys.rename tmp history_file

let _ =
	try
		let rec loop () =
			let _ = touch 0o600 history_file in
			let history_list = history_list history_file in
			let history_lines = history_lines history_list in
			let windows_per_desktop = window_list_per_desktop () in
			let desktop_lines = desktop_lines (desktop_list windows_per_desktop) in
			print_endline desktop_lines;
			print_endline "";
			let user_input =
				let ic, oc = Unix.open_process "dmenu -i -l 13" in
				try
					output_string oc desktop_lines;
					output_string oc history_lines;
					if history_lines <> "" then output_string oc "\n" else ();
					output_string oc (String.make 80 '-');
					(let ic = Unix.open_process_in "dmenu_path" in
						try
							while true do
								let line = input_line ic in
								let is_in_history = List.mem line (List.map fst history_list) in
								if is_in_history then () else output_string oc ("\n" ^ line)
							done
						with
						| End_of_file ->
								let _ = Unix.close_process_in ic in
								()
						| e ->
								let _ = Unix.close_process_in ic in
								raise e);
					close_out oc;
					let input = input_line ic
					and _ = Unix.close_process (ic, oc) in
					input
				with
				| e ->
						let _ = Unix.close_process (ic, oc) in
						raise e in
			let history_time_match = "^\\(.*\\)[ ]\\*\\*\\*\\*\\**.*" in
			let desktop_match = "^\\( \\|\\*\\)\\([0-9]+\\) .*" in
			(* Delete the first history line with !d *)
			if Str.string_match (Str.regexp (history_time_match ^ "!d"))
				user_input 0 then
				let line = Str.matched_group 1 user_input in
				delete_from_history line
			else (* Delete another history line with !d *)
			if Str.string_match (Str.regexp "^\\(.*\\)!d") user_input 0 then
				let line = Str.matched_group 1 user_input in
				delete_from_history line;
				loop ()
			else (* Close window n with !cn *)
			if Str.string_match
				(Str.regexp (desktop_match ^ "!c\\(.*\\)")) user_input 0 then
				let windows =
					let desktop = int_of_string (Str.matched_group 2 user_input) in
					Hashtbl.find windows_per_desktop desktop
				and index =
					let n = Str.matched_group 3 user_input in
					if n = "" then 0 else int_of_string n in
				let (id, _, _, _) = List.nth windows index in
				let _ = Unix.system ("wmctrl -i -c " ^ (string_of_int id)) in
				loop ()
			else (* Bring window n to the current desktop with !bn *)
			if Str.string_match
				(Str.regexp (desktop_match ^ "!b\\(.*\\)")) user_input 0 then
				let windows =
					let desktop = int_of_string (Str.matched_group 2 user_input) in
					Hashtbl.find windows_per_desktop desktop
				and index =
					let n = Str.matched_group 3 user_input in
					if n = "" then 0 else int_of_string n in
				let (id, _, _, _) = List.nth windows index in
				let _ = Unix.system ("wmctrl -i -R " ^ (string_of_int id)) in
				()
			else (* Switch to a desktop *)
			if Str.string_match (Str.regexp desktop_match) user_input 0 then
				let _ = Unix.system ("wmctrl -s" ^ (Str.matched_group 2 user_input)) in
				()
			else(* Do nothing in case of a separator *)
			if Str.string_match (Str.regexp "^-*$") user_input 0 then ()
			else (* Execute as a command *)
			let command =
				if Str.string_match (Str.regexp history_time_match) user_input 0 then
					Str.matched_group 1 user_input
				else user_input in
			exec_with_history command in
		loop ()
	with
	| End_of_file -> ()
