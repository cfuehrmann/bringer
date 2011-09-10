open OUnit
open UnixUtil
open SysUtil
open ListUtil

let max_pid () =
	with_file_in "/proc/sys/kernel/pid_max" (fun ic ->
					Scanf.fscanf ic "%d"
						(fun pid ->
									let result = pid in
									close_in ic;
									result))

let pid_not_found () =
	assert_equal "--unknown--" (UnixUtil.command_of (max_pid () + 1))

let line_frequencies_of_fresh_file () =
	let f = "test" in
	let _ = touch 0o600 f in
	let h = line_frequencies f in
	let l = Hashtbl.length h in
	assert_equal 0 l

let list_from_empty_hashtable () =
	let h = Hashtbl.create 0 in
	let l = list_from_hashtbl h in
	assert_equal 0 (List.length l)

let else_experiment () =
	if true then print_string "0"
	else print_string "1"; print_string "2"

let suite =
	"suite" >::: [
	"pid_not_found" >:: pid_not_found;
	"line_frequencies_of_fresh_file" >:: line_frequencies_of_fresh_file;
	"list_from_empty_hashtable" >:: list_from_empty_hashtable;
	]

let _ = run_test_tt ~verbose: true suite
