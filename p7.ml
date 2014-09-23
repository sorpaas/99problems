(* Rotate a list N places to the left. (medium) *)

let split list n =
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | h :: t as l -> if i = 0 then (List.rev acc, l)
		     else aux (h :: acc) (i - 1) t in
  aux [] n list;;

split ["a"; "b"; "c"] 2;;

let rotate list n =
  let len = List.length list in
  let loc = if n >= 0 then n mod len
	    else len + (n mod len) in
  let s = split list loc in
  (snd s) @ (fst s);;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
