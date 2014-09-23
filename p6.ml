(* Extract a slice from a list. (medium) *)

let slice list sn en =
  let rec aux acc si ei = function
    | [] -> List.rev acc
    | h :: t -> if (si <= 0 && ei >= 0)
		then aux (h :: acc) (si - 1) (ei - 1) t
		else aux acc (si - 1) (ei - 1) t in
  aux [] sn en list;;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
(* - : string list = ["c"; "d"; "e"; "f"; "g"] *)
