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
					let filter l = String.strip l <> String.strip line in
					filter_file history_file oc filter);
	Sys.rename tmp history_file

let history_time_match = "^\\(.*\\)[ ]\\*\\*\\*\\*\\**.*"
let desktop_match = "^\\( \\|\\*\\)\\([0-9]+\\) .*"
let separator = String.make 80 '-'

let _ =
	try
		let rec loop () =
			touch 0o600 history_file;
			let history_list = history_list history_file
			and windows_per_desktop = window_list_per_desktop () in
			let the_desktop_list = desktop_list windows_per_desktop in
			let the_desktop_lines = desktop_lines the_desktop_list in
			let user_input =
				with_command "dmenu -i -l 13" (fun (ic, oc) ->
								output_string oc the_desktop_lines;
								let l = history_lines history_list in
								output_string oc l;
								if l <> "" then output_string oc "\n";
								output_string oc separator;
								(with_command_in "dmenu_path" (fun ic ->
													try
														while true do
															let l = input_line ic in
															let is_in_history =
																List.mem l (List.map fst history_list) in
															if not is_in_history then
																output_string oc ("\n" ^ l)
														done
													with
													| End_of_file -> ()));
								close_out oc;
								input_line ic) in
			(* Delete the first history line with !d *)
			if Str.string_match (Str.regexp (history_time_match ^ "!d"))
				user_input 0 then
				let l = Str.matched_group 1 user_input in
				delete_from_history l
			else (* Delete another history line with !d *)
			if Str.string_match (Str.regexp "^\\(.*\\)!d") user_input 0 then
				let l = Str.matched_group 1 user_input in
				delete_from_history l;
				loop ()
			else (* Close window n with !cn *)
			if Str.string_match (Str.regexp (desktop_match ^ "!c\\(.*\\)"))
				user_input 0 then
				let windows =
					let desktop = Str.matched_group 2 user_input in
					Hashtbl.find windows_per_desktop (int_of_string desktop)
				and index =
					let n = Str.matched_group 3 user_input in
					if n = "" then 0 else int_of_string n in
				let (id, _, _, _) = List.nth windows index in
				let _ = Unix.system ("wmctrl -i -c " ^ (string_of_int id)) in
				(* Wait for up to 2 seconds until the window list changes *)
				let rec wait n =
					if n = 0 then loop () else begin
						Thread.delay 0.2;
						let windows_per_desktop_new = window_list_per_desktop () in
						let desktop_list_new = desktop_list windows_per_desktop_new in
						let desktop_lines_new = desktop_lines desktop_list_new in
						if the_desktop_lines = desktop_lines_new then wait (n - 1) else
							loop ()
					end in
				wait 10
			else (* Bring window n to the current desktop with !bn *)
			if Str.string_match
				(Str.regexp (desktop_match ^ "!b\\(.*\\)")) user_input 0 then
				let windows =
					let desktop = Str.matched_group 2 user_input in
					Hashtbl.find windows_per_desktop (int_of_string desktop)
				and index =
					let n = Str.matched_group 3 user_input in
					if n = "" then 0 else int_of_string n in
				let (id, _, _, _) = List.nth windows index in
				let _ = Unix.system ("wmctrl -i -R " ^ (string_of_int id)) in
				()
			else (* Push the active window to selected desktop with !pn *)
			if Str.string_match
				(Str.regexp (desktop_match ^ "!p")) user_input 0 then
				let desktop = Str.matched_group 2 user_input in
				let _ = Unix.system ("wmctrl -r :ACTIVE: -t " ^ desktop) in
				()
			else (* Switch to a desktop *)
			if Str.string_match (Str.regexp desktop_match) user_input 0 then
				let desktop = Str.matched_group 2 user_input in
				let _ = Unix.system ("wmctrl -s" ^ desktop) in
				()
			else(* Do nothing in case of a separator *)
			if user_input = separator then ()
			else (* Execute as a command *)
			let command =
				if Str.string_match (Str.regexp history_time_match) user_input 0 then
					Str.matched_group 1 user_input
				else user_input in
			exec_with_history command in
		loop ()
	with
	| End_of_file -> () (* In case the user presses ESC *)
