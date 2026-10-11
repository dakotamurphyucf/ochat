open Core
open Chatml_lang

let apply fn ~name args =
  match Chatml_eval.apply_value_result fn args with
  | Ok value -> value
  | Error err ->
    let message =
      match fn with
      | VClosure _ when String.equal err.message "Function arity mismatch" ->
        Printf.sprintf "%s: function arity mismatch" name
      | VBuiltin _ | VClosure _ -> err.message
      | _ -> Printf.sprintf "%s: expected a function" name
    in
    failwith message
;;
