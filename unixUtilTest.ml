open OUnit
open UnixUtil
open SysUtil

let max_pid () =
	with_file_in "/proc/sys/kernel/pid_max" (fun ic ->
					Scanf.fscanf ic "%d"
						(fun pid ->
									let result = pid in
									close_in ic;
									result))

let pid_not_found () =
	assert_raises Not_found (fun () -> UnixUtil.command_of (max_pid () + 1))

let suite =
	"suite" >:::
	["pid_not_found" >:: pid_not_found;
	"bla" >:: pid_not_found]

let _ = run_test_tt ~verbose: true suite
