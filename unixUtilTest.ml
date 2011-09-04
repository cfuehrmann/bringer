open OUnit
open UnixUtil
open SysUtil

let max_pid () =
	let f ic =
		Scanf.fscanf ic "%d"
			(fun pid ->
						let result = pid in
						close_in ic;
						result) in
	with_file_in f "/proc/sys/kernel/pid_max"

let pid_not_found () =
	assert_raises Not_found (fun () -> UnixUtil.command (max_pid () + 1))

let suite =
	"suite" >:::
	["pid_not_found" >:: pid_not_found;
	"bla" >:: pid_not_found]

let _ = run_test_tt ~verbose: true suite
