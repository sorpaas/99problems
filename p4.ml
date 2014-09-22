(* Drop every N'th element from a list. (medium)
 *)

let drop list n =
  let rec aux acc i = function
    | [] -> acc
    | h :: t -> if i = n then aux acc (i + 1) t
		else aux (h :: acc) (i + 1) t in
  List.rev (aux [] 1 list);;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
