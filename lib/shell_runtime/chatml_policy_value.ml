open! Core
module C = Chatml_codec
module L = Chatml.Chatml_lang

type match_ =
  { rule_id : string
  ; action : Shell_access.Policy.action
  ; reason : string option
  }

type t =
  { action : Shell_access.Policy.action
  ; reason : string
  ; matches : match_ list
  }

let action_name = Shell_access.Policy.string_of_action

let action path value =
  Result.bind (C.string ~path value) ~f:(function
    | "allow" -> Ok Shell_access.Policy.Allow
    | "ask" -> Ok Ask
    | "deny" -> Ok Deny
    | _ ->
      Error
        (Chatmd_shell_spec.Diagnostic.error
           ~path
           ~code:"shell.chatml_codec"
           "unknown policy action"))
;;

let of_match (value : Shell_access.Policy.match_) =
  { rule_id = value.rule_id; action = value.action; reason = value.reason }
;;

let of_decision (value : Shell_access.Policy.decision) =
  { action = value.action; reason = value.reason; matches = List.map value.matches ~f:of_match }
;;

let encode_match value =
  C.encode_record
    [ "rule_id", L.VString value.rule_id
    ; "action", VString (action_name value.action)
    ; "reason", C.encode_option (fun value -> L.VString value) value.reason
    ]
;;

let encode value =
  C.encode_record
    [ "version", L.VString "shell-policy-v1"
    ; "action", VString (action_name value.action)
    ; "reason", VString value.reason
    ; "matches", VArray (Array.of_list_map value.matches ~f:encode_match)
    ]
;;

let decode_match index value =
  let path = [ "policy"; "matches"; Int.to_string index ] in
  let open Result.Let_syntax in
  let%bind fields =
    C.record ~path ~allowed:[ "rule_id"; "action"; "reason" ] ~required:[ "rule_id"; "action"; "reason" ] value
  in
  let%bind rule_id = C.field ~path fields "rule_id" >>= C.string ~path:(path @ [ "rule_id" ]) in
  let%bind action = C.field ~path fields "action" >>= action (path @ [ "action" ]) in
  let%map reason =
    C.field ~path fields "reason"
    >>= C.option ~path:(path @ [ "reason" ]) C.string
  in
  { rule_id; action; reason }
;;

let decode_matches = function
  | L.VArray values ->
    Array.to_list values |> List.mapi ~f:decode_match |> Result.all
  | _ ->
    Error
      (Chatmd_shell_spec.Diagnostic.error
         ~path:[ "policy"; "matches" ]
         ~code:"shell.chatml_codec"
         "expected array")
;;

let decode value =
  let path = [ "policy" ] in
  let open Result.Let_syntax in
  let%bind fields =
    C.record ~path ~allowed:[ "version"; "action"; "reason"; "matches" ] ~required:[ "version"; "action"; "reason"; "matches" ] value
  in
  let%bind version = C.field ~path fields "version" >>= C.string ~path:(path @ [ "version" ]) in
  let%bind () =
    if String.equal version "shell-policy-v1"
    then Ok ()
    else Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "version" ]) ~code:"shell.chatml_codec" "unsupported version")
  in
  let%bind action = C.field ~path fields "action" >>= action (path @ [ "action" ]) in
  let%bind reason = C.field ~path fields "reason" >>= C.string ~path:(path @ [ "reason" ]) in
  let%map matches = C.field ~path fields "matches" >>= decode_matches in
  { action; reason; matches }
;;
