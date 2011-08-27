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
                  
let touch mod_mask file_name = 
  let fd = Unix.openfile file_name [ Unix.O_CREAT ] mod_mask in
  Unix.close fd;;
        
