open! Core
module L = Chatml.Chatml_lang
module Context_value = Chatml_context_value
module Policy_value = Chatml_policy_value
module Approval_value = Chatml_approval_value
module Interceptor_value = Chatml_interceptor_value
module Effect_value = Chatml_effect_value
module Result_value = Chatml_result_value
module Audit_value = Chatml_audit_value

let max_string_bytes = 32_768
let max_array_items = 1_024

let bounded value = value

let strings values =
  values
  |> fun values ->
  List.take values max_array_items
  |> List.map ~f:(fun value -> L.VString (bounded value))
  |> Array.of_list
  |> fun values -> L.VArray values
;;

let record fields = L.VRecord (String.Map.of_alist_exn fields)

let origin = function
  | Shell_access.Context.Tool -> "tool"
  | Moderator -> "moderator"
  | Host name -> "host:" ^ name
;;

let request_kind = function
  | Shell_access.Context.Structured -> "structured"
  | Script_file -> "script_file"
  | Raw_shell -> "raw_shell"
;;

let context (value : Shell_access.Context.t) =
  Context_value.of_context value |> Context_value.encode
;;

let event ~phase ~fields = record (("phase", L.VString phase) :: fields)
