let rec string_of_list string_of_element delimiter = function
	| [h] -> string_of_element h
	| h :: t ->
			string_of_element h ^ delimiter ^
			string_of_list string_of_element delimiter t
	| [] -> ""

let list_from_hashtbl tbl = Hashtbl.fold (fun k v l -> (k, v) :: l) tbl []

