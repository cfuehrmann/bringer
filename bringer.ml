(* #!/usr/bin/lablgtk2 *)

let rec string_of_list string_of_element delimiter = 
  function
    | [h] -> string_of_element h
    | h :: t -> 
      string_of_element h ^ delimiter ^ 
	string_of_list string_of_element delimiter t
    | [] -> "";; 

let list_of_hashtbl h = 
  Hashtbl.fold (fun k v l -> (k, v) :: l) 
    h [];;

(* todo: for performance, use ps -Ao pid,command *) 
let get_command pid =
  let ic = 
    Unix.open_process_in 
      ("ps -p " ^ string_of_int pid ^ " -o command=") in
  Str.replace_first 
    (Str.regexp "^[^ ]*/") "" (input_line ic);;

let get_current_desktop () =
  let ic = Unix.open_process_in "wmctrl -d" in
  let rec f () =
    Scanf.fscanf ic "%i %s %s@\n"	   
      begin 
	fun desktop marker rest ->
	  if marker = "*" then desktop else f () 
      end in  
  try
    let result = f () in 
    let _ = Unix.close_process_in ic in result
  with 
      End_of_file -> 
	let _ = Unix.close_process_in ic in
	raise Not_found;;
  
let get_windows_per_desktop () =
  let result = Hashtbl.create 5 in
  begin
    let ic = Unix.open_process_in "wmctrl -lp" in
    begin
      try
	while true do
	  Scanf.fscanf ic "%i %i %i %s %s@\n"	   
	    begin 
	      fun window desktop pid host title ->
		Hashtbl.add result desktop
		  (window, get_command pid)
	    end
	done
      with 
	  End_of_file -> ()
    end;
    let _ = Unix.close_process_in ic in ()
  end;
  result;;

let get_window_list_per_desktop () = 
  let result = Hashtbl.create 4 in
  begin
    Hashtbl.iter 
      begin
	fun k v -> 
	  if Hashtbl.mem result k then
	    let l = Hashtbl.find result k in
	    Hashtbl.replace result k
	      begin
	      List.merge 
		begin
		  fun (w1, c1) (w2, c2) -> 
		    let x = compare c1 c2 in
		    if x = 0 then compare w1 w2 else x
		end
		[v] l
	      end
	  else Hashtbl.add result k [v]
      end
      (get_windows_per_desktop ())
  end;
  result;;

let get_desktop_list () =
  List.sort
    begin
      let rec comparer = function
	| [(w1, c1)], [(w2, c2)] ->
	  let s = compare c1 c2 in
	  if s = 0 then compare w1 w2 else s
	| [], [] -> 0
	| [], x -> -1
	| x, [] -> 1
	| (w1, c1) :: t1, (w2, c2) :: t2 -> 
	  let s = compare c1 c2 in
	  if s = 0 then comparer (t1, t2) else s in
      fun (d1, l1) (d2, l2) -> comparer (l1, l2)
    end
    (list_of_hashtbl (get_window_list_per_desktop ()));;

let get_goto_lines () =
  let cd = get_current_desktop () in
  let rec f = function
    | [] -> ""
    | (d, l) :: t ->
      let ws =
	match l with 
	  | [] -> "\n"
	  | (w, c) :: t -> string_of_int w in
      let star = if d = cd then "*" else "" in
      let ls = string_of_list snd " --- " l in
      "g " ^ ws ^ " " ^ star ^ ls ^ "\n" ^ f t in 
  f (get_desktop_list ());;

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
	  End_of_file -> () 
    end;
    let _ = close_in ic in ()
  end;
  result;;

let get_history_lines () =
  string_of_list (fun s -> s) "\n"
    begin
      let l =
	List.map (function c, f -> "h " ^ c)
	  begin
	    List.sort 
	      (fun (c1, f1) (c2, f2) -> compare f2 f1)
	      begin
		list_of_hashtbl
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
	    h ^ " --- " ^ 
	      string_of_int time.Unix.tm_hour ^ ":" ^ 
	      string_of_int time.Unix.tm_min in
	  m :: t
    end;;

let run_command m =
  let pid = Unix.create_process m [| |] Unix.stdin 
    Unix.stdout Unix.stderr in
  let q = Queue.create () in 
  begin
    let ic = open_in "/home/carsten/.bringerHistory" in
    try
      while true do
	Queue.push (input_line ic) q
      done
    with 
	End_of_file -> close_in ic
  end;
  begin
    let oc = open_out "/home/carsten/.bringerHistory" in
    output_string oc (m ^ "\n"); 
    try
      while true do
	output_string oc (Queue.pop q ^ "\n")
      done
    with 
	Queue.Empty -> close_out oc
  end;
  pid;;

let l = 
  let ic, oc = Unix.open_process "dmenu -i -l 11" in
  output_string oc (get_goto_lines ());
  output_string oc (get_history_lines ());
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
  Sys.command ("wmctrl -ia" ^ (Str.matched_group 1 l)) 
else  
  let command =
    if Str.string_match (Str.regexp "^h \\(.*\\)") l 0 then 
      let n = Str.matched_group 1 l in
      let m = List.hd (Str.split (Str.regexp " --- ") n) in 
      m
    else l in
  run_command command;;
