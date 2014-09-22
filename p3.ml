(* Replicate the elements of a list a given number of times. (medium)
 *)

let replicate list count =
  let rec aux acc rc = function
    | [] -> acc
    | h :: t as w -> if rc = 0 then aux acc count t
		else aux (h :: acc) (rc - 1) w in
  List.rev (aux [] count list);;

replicate ["a";"b";"c"] 3;;
