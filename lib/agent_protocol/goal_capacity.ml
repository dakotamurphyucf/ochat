open! Core

type t =
  { encoded_bytes : int
  ; fields : int
  ; nodes : int
  }

let max_projection_bytes = 128 * 1024
let max_document_bytes = 256 * 1024
let encoded_bytes t = t.encoded_bytes
let fields t = t.fields
let nodes t = t.nodes

let measure json =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_json ~max_depth:64 json in
  let rec counts = function
    | `Object members ->
      List.fold
        members
        ~init:(List.length members, 1)
        ~f:(fun (fields, nodes) (_, json) ->
          let child_fields, child_nodes = counts json in
          fields + child_fields, nodes + child_nodes)
    | `Array values ->
      List.fold values ~init:(0, 1) ~f:(fun (fields, nodes) json ->
        let child_fields, child_nodes = counts json in
        fields + child_fields, nodes + child_nodes)
    | `Null | `String _ | `Number _ | `True | `False -> 0, 1
  in
  let fields, nodes = counts json in
  Ok { encoded_bytes = String.length (Jsonaf.to_string json); fields; nodes }
;;

(* Capacity witnesses are private JSON size/count calculations, not decodable
   domain states. Text admits only tab/newline among ASCII controls, so quotes
   attain its worst two encoded bytes per UTF-8 input byte. Source script IDs
   permit other controls; NUL attains their worst six encoded bytes per byte.
   ID/key/source bounds follow Id.Make.maximum_length, Idempotency_key.maximum_length
   and Run_source.validate. The capacity regression discovers these maxima through
   their actual constructors and measures legal postrequest transitions. *)
let for_request request =
  let open Result.Let_syntax in
  let text count = `String (String.make count '"') in
  let id = `String (String.make 96 'a') in
  let counter = `String (Int64.to_string Int64.max_value) in
  let source =
    `Object
      [ "script_id", `String (String.make 256 '\000')
      ; "source_sha256", `String (String.make 64 'a')
      ; "generation", `Number (Int.to_string Int.max_value)
      ; "installation_epoch", counter
      ]
  in
  let session = `Object [ "server_id", id; "session_id", id ] in
  let receipt kind =
    `Object
      [ "run_id", id
      ; "principal_id", id
      ; "source", source
      ; "key", `String (String.make 256 'a')
      ; "request_sha256", `String (String.make 64 'a')
      ; "kind", `String kind
      ; "run_revision", counter
      ; "session_revision", counter
      ; "committed_at", `String (String.make 64 'a')
      ]
  in
  let episode = `Object [ "session", session; "admission", receipt "admission" ] in
  let settlement = `Object [ "episode", episode; "receipt", receipt "terminal" ] in
  let witness =
    `Object
      [ "id", id
      ; "session", session
      ; "creator", id
      ; "source", source
      ; "objective", text Goal_text.Objective.max_bytes
      ; "progress", text Goal_text.Progress.max_bytes
      ; ( "budget"
        , `Object
            [ "admitted_turns", counter
            ; "admitted_attempts", counter
            ; ( "observed_tokens"
              , `Object [ "component", `String "cache_write_input"; "ceiling", counter ] )
            ; "deadline", `String (String.make 64 'a')
            ] )
      ; ( "state"
        , `Object
            [ "kind", `String "open"
            ; ( "admission"
              , `Object
                  [ "kind", `String "held"
                  ; ( "reason"
                    , `Object
                        [ "kind", `String "needs_input"
                        ; "reason", text Goal_text.Reason.max_bytes
                        ] )
                  ] )
            ] )
      ; "episode", episode
      ; "achievement_request", Goal_achievement.Request.to_json request
      ; "request_settlement", settlement
      ; "previous_episode_settlement", settlement
      ; "revision", counter
      ]
  in
  let%bind () =
    Extension_codec.validate_json ~max_bytes:max_projection_bytes ~max_depth:64 witness
  in
  measure witness
;;
