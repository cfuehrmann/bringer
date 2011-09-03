open OUnit
open UnixUtil

let max_pid () =
	let ic = open_in "/proc/sys/kernel/pid_max" in
	try
		Scanf.fscanf ic "%d"
			(fun pid ->
						let result = pid in
						close_in ic;
						result)
	with
	| e ->
			close_in ic;
			raise e

let pid_not_found () =
	assert_raises Not_found (fun () -> UnixUtil.command (max_pid () + 1))

let suite =
	"suite" >:::
	["pid_not_found" >:: pid_not_found;
	"bla" >:: pid_not_found]

let _ = run_test_tt ~verbose: true suite
