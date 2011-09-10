module Hashtbl = ExtLib.Hashtbl
open UnixUtil
open ListUtil

let current_desktop () =
	let f ic =
		let rec loop () =
			Scanf.fscanf ic "%i %s %s@\n"
				(fun desktop marker rest ->
							if marker = "*" then desktop else loop ()) in
		loop () in
	with_command_in f "wmctrl -d"

let windows_per_desktop () =
	let result = Hashtbl.create 5 in
	let f ic =
		let rec loop () =
			Scanf.fscanf ic "%i %i %i %s %s@\n"
				(fun window desktop pid host title ->
							Hashtbl.add result desktop (window, command_of pid, host, title));
			loop () in
		loop () in
	try
		with_command_in f "wmctrl -lp"
	with
	| End_of_file -> result

let window_list_per_desktop () =
	let result = Hashtbl.create 4 in
	begin
		let f k v =
			match Hashtbl.find_option result k with
			| Some l ->
					let l =
						let compare (window1, command1, host1, title1)
								(window2, command2, host2, title2) =
							let n = compare command1 command2 in
							if n = 0 then compare window1 window2 else n in
						List.merge compare [v] l in
					Hashtbl.replace result k l
			| None -> Hashtbl.add result k [v] in
		Hashtbl.iter f (windows_per_desktop ())
	end;
	result

let desktop_list windows_per_desktop =
	let compare_desktops (d1, l1) (d2, l2) =
		let rec compare_window_lists = function
			| [(w1, c1, h1, title1)], [(w2, c2, h2, title2)] ->
					let n = compare c1 c2 in
					if n = 0 then compare w1 w2 else n
			| [], [] -> 0
			| [], _ -> -1
			| _, [] -> 1
			| (w1, c1, h1, title1) :: t1, (w2, c2, h2, title2) :: t2 ->
					let n = compare c1 c2 in
					if n = 0 then compare_window_lists (t1, t2) else n in
		compare_window_lists (l1, l2) in
	List.sort compare_desktops (list_from_hashtbl windows_per_desktop)

