open Core
module S = Chatmd_shell_spec.Shell_spec
module D = Chatmd_shell_spec.Duration
module B = Registry_code_budget

let scalar equal budget left right =
  B.work budget;
  equal left right
;;

let string = B.equal_string
let bool = scalar Bool.equal
let int = scalar Int.equal
let int64 = scalar Int64.equal
let float = scalar Float.equal
let duration = scalar D.equal
let bytes = scalar D.equal_bytes

let option equal budget left right =
  B.work budget;
  match left, right with
  | None, None -> true
  | Some left, Some right -> equal budget left right
  | None, Some _ | Some _, None -> false
;;

let rec list equal budget left right =
  B.work budget;
  match left, right with
  | [], [] -> true
  | left :: lefts, right :: rights ->
    equal budget left right && list equal budget lefts rights
  | [], _ :: _ | _ :: _, [] -> false
;;

let setting equal budget (left : 'a S.setting) (right : 'a S.setting) =
  B.work budget;
  match left, right with
  | S.Inherit, S.Inherit | S.Clear, S.Clear -> true
  | Set left, Set right -> equal budget left right
  | (Inherit | Clear | Set _), _ -> false
;;

let path
      budget
      (left : Chatmd_shell_spec.Path_expr.t)
      (right : Chatmd_shell_spec.Path_expr.t)
  =
  B.work budget;
  match left, right with
  | Chatmd_shell_spec.Path_expr.Absolute left, Absolute right -> string budget left right
  | ( Relative { base = left_base; path = left_path }
    , Relative { base = right_base; path = right_path } ) ->
    scalar Chatmd_shell_spec.Path_expr.equal_base budget left_base right_base
    && string budget left_path right_path
  | Absolute _, Relative _ | Relative _, Absolute _ -> false
;;
