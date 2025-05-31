(* Compiler environment implementation *)

module M = Map.Make (String)
include M

let of_list (lst : (string * 'a) list) : 'a t =
  List.fold_left (fun acc (k, v) -> add k v acc) empty lst
;;

(* [merge lhs rhs] keeps the bindings in [lhs] when both maps contain the
   same key, otherwise it takes the binding from [rhs].  The implementation
   folds over [rhs] and adds the bindings that do not already exist in
   [lhs]. *)
let merge lhs rhs =
  fold (fun key value acc -> if mem key acc then acc else add key value acc) rhs lhs
;;
