open Core
open Chatml_lang

let apply fn ~name args =
  match Chatml_eval.apply_value_result fn args with
  | Ok value -> value
  | Error err ->
    let message =
      match fn with
      | VClosure closure when List.length closure.params <> List.length args ->
        Printf.sprintf "%s: function arity mismatch" name
      | VBuiltin _ | VClosure _ -> err.message
      | _ -> Printf.sprintf "%s: expected a function" name
    in
    let err =
      if String.equal message err.message
      then err
      else
        Chatml_diagnostic.create
          ?span:err.span
          ?site:err.site
          ~related:err.related
          message
    in
    raise (Runtime_error err)
;;
