(* Run-length encoding of a list (direct solution). (medium)
 *
 * Implement the so-called run-length encoding data compression method
 * directly. I.e. don't explicitly create the sublists containing the
 * duplicates, as in problem "Pack consecutive duplicates of list
 * elements into sublists", but only count them. As in problem "Modified
 * run-length encoding", simplify the result list by replacing the
 * singleton lists (1 X) by X.
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode list =
  let rle count x = if count = 1 then One x else Many (count, x) in
  let rec aux count acc = function
    | [] -> acc
    | [x] -> (rle (count + 1) x) :: acc
    | a :: (b :: _ as t) ->
       if a = b then aux (count + 1) acc t
       else aux 0 ((rle (count + 1) a) :: acc) t in
  List.rev (aux 0 [] list);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
