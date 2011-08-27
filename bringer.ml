(* #!/usr/bin/lablgtk2                                                        *)
(* todo: improve bird's eye view structure;
unit tests? *)

module String = ExtLib.String;;
module Hashtbl = ExtLib.Hashtbl;;
open ListUtil;;

let command pid =
  let ic = Unix.open_process_in (Printf.sprintf "ps -p %d -o command=" pid) in
  try
    let result = 
      let line = input_line ic in 
      Str.replace_first (Str.regexp "^[^ ]*/") "" line
    and _ = Unix.close_process_in ic in
    result
  with 
    | e -> 
      let _ = Unix.close_process_in ic in 
      raise e;;
                          
let home () =
  let ic = Unix.open_process_in "echo ~"  in
  try
    let result = 
      let line = input_line ic in 
      line
    and _ = Unix.close_process_in ic in
    result
  with 
    | e -> 
      let _ = Unix.close_process_in ic in 
      raise e;;
                          
let current_desktop () =
  let ic = Unix.open_process_in "wmctrl -d" in
  try
    let rec loop () =
      Scanf.fscanf ic "%i %s %s@\n" 
	(fun desktop marker rest ->
	  if marker = "*" then desktop else loop ()) in
    let result = loop () 
    and _ = Unix.close_process_in ic in 
    result
  with 
    | e -> 
      let _ = Unix.close_process_in ic in
      raise e;;

let windows_per_desktop () =
  let result = Hashtbl.create 5 
  and ic = Unix.open_process_in "wmctrl -lp" in
  try
    let rec loop () =
      Scanf.fscanf ic "%i %i %i %s %s@\n"	   
	(fun window desktop pid host title ->
	  Hashtbl.add result desktop (window, command pid));
      loop () in
    loop ()
  with 
    | End_of_file -> 
      let _ = Unix.close_process_in ic in 
      result
    | e -> 
      let _ = Unix.close_process_in ic in 
      raise e;;

let window_list_per_desktop () = 
  let result = Hashtbl.create 4 in
  begin
    let f k v = 
      match Hashtbl.find_option result k with
	| Some l -> 
	  let l =
 	    let compare (window1, command1) (window2, command2) =
	      let n = compare command1 command2 in
	      if n = 0 then compare window1 window2 else n in
	    List.merge compare [v] l in 	
	  Hashtbl.replace result k l
	| None -> Hashtbl.add result k [v] in
    Hashtbl.iter f (windows_per_desktop ())
  end;
  result;;

let desktop_list () =
  let compare_desktops  (d1, l1) (d2, l2) =
    let rec compare_window_lists = function
      | [(w1, c1)], [(w2, c2)] ->
	let n = compare c1 c2 in
	if n = 0 then compare w1 w2 else n
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | (w1, c1) :: t1, (w2, c2) :: t2 -> 
	let n = compare c1 c2 in
	if n = 0 then compare_window_lists (t1, t2) else n in
    compare_window_lists (l1, l2) in 
  List.sort compare_desktops (list_from_hashtbl (window_list_per_desktop ()));;

let gotos () =
  let f =
    let cd = current_desktop () in
    fun s (d, l) ->	    
      match l with 
	| [] -> ""
	| (w, c) :: _ -> 
	  let star = if d = cd then " *" else " "
	  and description = string_of_list snd " --- " l in
	  s ^ "desktop" ^ string_of_int d ^ star ^ description ^ "\n" in 
  List.fold_left f "" (desktop_list ());;

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
	    | Some (count, most_recent) -> Hashtbl.replace result line (count + 1, most_recent)
	    | None -> Hashtbl.add result line (1, i)
	end;
	loop (i + 1)
      in loop 0
    with 
      | End_of_file -> close_in ic
      | e -> close_in ic; raise e
  end;
  result;;

let history_list history_file = 
  let l = 
    list_from_hashtbl (line_frequencies history_file) in
  let compare (c1, (f1, mr1)) (c2, (f2, mr2)) = 
    let n = compare f2 f1 in
    if n = 0 then compare mr1 mr2 else n in
  List.sort compare l;;

let history history_file =
  match history_list history_file with
    | [] -> ""
    | (c, f) :: t ->
      let rec loop s = function
	| [] -> s
	| (c, f) :: t -> loop (s ^ "\n" ^ c) t
      and s = 
	let t = Unix.localtime (Unix.time ()) in
	let dashes = String.make (73 - String.length c) '-' in 
	Printf.sprintf "%s %s %d:%02d" c dashes t.Unix.tm_hour t.Unix.tm_min in
      loop s t;;

let run_command command history_file =
  let pid = Unix.fork () in
  if pid = 0 then 
    let _ = Unix.execvp "sh" [| command; "-c"; ("exec " ^ command) |] in 
    exit (-1)
  else
    let lines =
      let ic = open_in history_file in
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
    let oc = open_out history_file in
    try
      output_string oc (string_of_list (fun s -> s) "\n" (command :: lines));
      close_out oc
    with 
      | e -> close_out oc; raise e;;

let time f x =
  let t = Sys.time() in
  let result = f x in
  Printf.printf "Took %fs\n" (Sys.time() -. t);
  result;;

let history_file = home () ^ "/" ^ ".bringerHistory" in
begin
  let fd = Unix.openfile history_file [ Unix.O_CREAT ] 0o600 in
  Unix.close fd
end;
try
  let user_input =
    let ic, oc = Unix.open_process "dmenu -i -l 13" in
    try
      output_string oc (gotos ());
      output_string oc (history history_file);
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
    run_command command history_file
with 
  | End_of_file -> ();;
