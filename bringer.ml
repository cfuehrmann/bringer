(* #!/usr/bin/lablgtk2                                                        *)
(* todo: weiter mit fixes bei "count_line_frequencies" *)

let rec string_of_list string_of_element delimiter = function
  | [h] -> string_of_element h
  | h :: t -> 
    string_of_element h ^ delimiter ^ 
      string_of_list string_of_element delimiter t
  | [] -> "";; 

let list_from_hashtbl h = Hashtbl.fold (fun k v l -> (k, v) :: l) h [];;

(* todo: for performance, use ps -Ao pid,command *) 
let command pid =
  let ic = Unix.open_process_in (Printf.sprintf "ps -p %d -o command=" pid) in
  try
    let line = input_line ic in 
    let result = Str.replace_first (Str.regexp "^[^ ]*/") "" line in
    let _ = Unix.close_process_in ic in
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
    let result = loop () in 
    let _ = Unix.close_process_in ic in 
    result
  with 
    | e -> 
      let _ = Unix.close_process_in ic in
      raise e;;

let windows_per_desktop () =
  let result = Hashtbl.create 5 in
  let ic = Unix.open_process_in "wmctrl -lp" in
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
      if Hashtbl.mem result k then
	let l = Hashtbl.find result k in
	let l2 =
 	  let compare (window1, command1) (window2, command2) =
	    let n = compare command1 command2 in
	    if n = 0 then compare window1 window2 else n in
	  List.merge compare [v] l in 	
	Hashtbl.replace result k l2
      else Hashtbl.add result k [v] in
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
	  let star = if d = cd then " *" else " " in
	  let ls = string_of_list snd " --- " l in
	  s ^ "g " ^ (string_of_int w) ^ star ^ ls ^ "\n" in 
  List.fold_left f "" (desktop_list ());;

let count_line_frequencies file_name =
  let result = Hashtbl.create 50 in
  begin  
    let ic = open_in file_name in
    begin
      try
	while true do
	  let trim =
	    begin
	      Str.global_replace 
		(Str.regexp "[ \t]+$") "" (input_line ic)
	    end in
	  if Hashtbl.mem result trim then
	    let count = Hashtbl.find result trim in
	    Hashtbl.replace result trim (count + 1)
	  else
	    Hashtbl.add result trim 1
	done
      with 
	| End_of_file -> () 
    end;
    let _ = close_in ic in ()
  end;
  result;;

let history () = 
  string_of_list (fun s -> s) "\n"
    begin
      let l =
	List.map (function c, f -> "h " ^ c)
	  begin
	    List.sort 
	      (fun (c1, f1) (c2, f2) -> compare f2 f1)
	      begin
		list_from_hashtbl
		  begin
		    count_line_frequencies 
		      "/home/carsten/.bringerHistory"
		  end
	      end
	  end in
      match l with 
	| [] -> []
	| h :: t -> 
	  let m =
	    let time = Unix.localtime (Unix.time ()) in
	    Printf.sprintf "%s --- %d:%.2d" h time.Unix.tm_hour time.Unix.tm_min in
	  m :: t
    end;;

let run_command m =
  let p = Unix.fork () in
  if p = 0 then 
    begin
      let _ = Unix.execvp "sh" [| m; "-c"; ("exec " ^ m) |] in 
      exit (-1)
    end
  else
    let q = Queue.create () in 
    begin
      let ic = open_in "/home/carsten/.bringerHistory" in
      try
	while true do
	  Queue.push (input_line ic) q
	done
      with 
	| End_of_file -> close_in ic
    end;
    begin
      let oc = open_out "/home/carsten/.bringerHistory" in
      output_string oc (m ^ "\n"); 
      try
	while true do
	  output_string oc (Queue.pop q ^ "\n")
	done
      with 
	| Queue.Empty -> close_out oc
    end;;

let l = 
  begin
    let d = Unix.openfile "/home/carsten/.bringerHistory" [ Unix.O_CREAT ] 0o600 in
    Unix.close d
  end;
  let ic, oc = Unix.open_process "dmenu -i -l 11" in
  output_string oc (gotos ());
  output_string oc (history ());
  begin
    let i = Unix.open_process_in "dmenu_path" in
    try
      while true do
	output_string oc ("\n" ^ input_line i)
      done
    with 
      | End_of_file -> ignore (Unix.close_process_in i)
  end;
  close_out oc;
  let l = 
    try input_line ic with
      |	e -> 
	let _ = Unix.close_process (ic, oc) in
	raise e in  
  let _ = Unix.close_process (ic, oc) in
  l in
if Str.string_match (Str.regexp "^g \\([0-9]+\\) .*") l 0 then
  let _ = Sys.command 
    ("wmctrl -ia" ^ (Str.matched_group 1 l)) in () 
else  
  let command =
    if Str.string_match (Str.regexp "^h \\(.*\\)") l 0 then 
      let n = Str.matched_group 1 l in
      let m = List.hd (Str.split (Str.regexp " --- ") n) in 
      m
    else l in
  run_command command;;
