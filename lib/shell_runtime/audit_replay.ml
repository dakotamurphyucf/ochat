open! Core

type event =
  { schema_version : string
  ; sequence : int64
  ; timestamp : float
  ; session_id : string option
  ; runtime_id : string
  ; manifest_sha256 : string
  ; request_id : string
  ; plan_id : string option
  ; name : string
  ; fields : (string * Jsonaf.t) list
  ; previous_event_sha256 : string option
  ; event_sha256 : string option
  }
[@@deriving sexp]

type request =
  { request_id : string
  ; runtime_id : string
  ; manifest_sha256 : string
  ; command : string option
  ; command_sha256 : string option
  ; cwd_sha256 : string option
  ; request_kind : string option
  ; effects : string list
  ; policy_action : string option
  ; approval_answer : string option
  ; interceptors : string list
  ; backend : string option
  ; stdout_bytes : int
  ; stderr_bytes : int
  ; exit_kind : string option
  ; exit_code : int option
  ; signal : int option
  ; rejected_reason : string option
  ; completed : bool
  ; events : event list
  }
[@@deriving sexp]

type error =
  { code : string
  ; line : int option
  ; message : string
  }
[@@deriving sexp, compare, equal]

let error ?line code message = Error { code; line; message }

let find fields name = List.Assoc.find fields name ~equal:String.equal

let string fields name =
  match find fields name with
  | Some (`String value) -> Ok value
  | _ -> error "shell.audit_field" ("missing string field: " ^ name)
;;

let optional_string fields name =
  match find fields name with
  | None | Some `Null -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> error "shell.audit_field" ("invalid optional string field: " ^ name)
;;

let int64 fields name =
  match find fields name with
  | Some (`Number value) ->
    (try Ok (Int64.of_string value) with
     | _ -> error "shell.audit_field" ("invalid integer field: " ^ name))
  | _ -> error "shell.audit_field" ("missing integer field: " ^ name)
;;

let float fields name =
  match find fields name with
  | Some (`Number value) ->
    (try Ok (Float.of_string value) with
     | _ -> error "shell.audit_field" ("invalid float field: " ^ name))
  | _ -> error "shell.audit_field" ("missing float field: " ^ name)
;;

let parse_event fields =
  let open Result.Let_syntax in
  let%bind schema_version = optional_string fields "schema_version" in
  let%bind sequence = int64 fields "sequence" in
  let%bind timestamp = float fields "timestamp" in
  let%bind session_id = optional_string fields "session_id" in
  let%bind runtime_id = string fields "runtime_id" in
  let%bind manifest_sha256 = string fields "manifest_sha256" in
  let%bind request_id = string fields "request_id" in
  let%bind plan_id = optional_string fields "plan_id" in
  let%bind name = string fields "event" in
  let%bind previous_event_sha256 = optional_string fields "previous_event_sha256" in
  let%map event_sha256 = optional_string fields "event_sha256" in
  { schema_version = Option.value schema_version ~default:"legacy-unversioned"
  ; sequence
  ; timestamp
  ; session_id
  ; runtime_id
  ; manifest_sha256
  ; request_id
  ; plan_id
  ; name
  ; fields
  ; previous_event_sha256
  ; event_sha256
  }
;;

let parse_line line_number line =
  try
    match Jsonaf.of_string line with
    | `Object fields ->
      parse_event fields
      |> Result.map_error ~f:(fun error -> { error with line = Some line_number })
    | _ -> error ~line:line_number "shell.audit_json" "audit line is not an object"
  with
  | exn -> error ~line:line_number "shell.audit_json" (Exn.to_string exn)
;;

let load ~fs ~path =
  try
    Eio.Path.(fs / path)
    |> Eio.Path.load
    |> String.split_lines
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.mapi ~f:(fun index line -> parse_line (index + 1) line)
    |> Result.combine_errors
  with
  | exn ->
    Error
      [ { code = "shell.audit_read_failed"
        ; line = None
        ; message = Exn.to_string exn
        }
      ]
;;

let rotated_paths ~fs path =
  let rec loop index paths =
    if index > 1_024
    then paths
    else
      let candidate = sprintf "%s.%d" path index in
      if Eio.Path.is_file Eio.Path.(fs / candidate)
      then loop (index + 1) (candidate :: paths)
      else paths
  in
  loop 1 [] @ [ path ]
;;

let load_rotated ~fs ~path =
  rotated_paths ~fs path
  |> List.map ~f:(fun path -> load ~fs ~path)
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  |> Result.map ~f:List.concat
;;

let unsigned_json event =
  event.fields
  |> List.filter ~f:(fun (name, _) -> not (String.equal name "event_sha256"))
  |> fun fields -> Jsonaf.to_string (`Object fields)
;;

let integrity_errors events =
  let initial_previous =
    match events with
    | [] -> None
    | first :: _ ->
      if Int64.equal first.sequence 0L then None else first.previous_event_sha256
  in
  let _, errors =
    List.fold events ~init:(initial_previous, []) ~f:(fun (previous, errors) event ->
      match event.event_sha256 with
      | None -> previous, errors
      | Some claimed ->
        let actual = Digestif.SHA256.(to_hex (digest_string (unsigned_json event))) in
        let errors =
          if Option.equal String.equal event.previous_event_sha256 previous
          then errors
          else
            { code = "shell.audit_chain_previous"
            ; line = None
            ; message = sprintf "sequence %Ld has the wrong previous digest" event.sequence
            }
            :: errors
        in
        let errors =
          if String.Caseless.equal claimed actual
          then errors
          else
            { code = "shell.audit_chain_digest"
            ; line = None
            ; message = sprintf "sequence %Ld has an invalid event digest" event.sequence
            }
            :: errors
        in
        Some claimed, errors)
  in
  List.rev errors
;;

let sequence_errors events =
  let rec loop errors = function
    | left :: (right :: _ as rest) ->
      let errors =
        if Int64.equal (Int64.succ left.sequence) right.sequence
        then errors
        else
          { code = "shell.audit_sequence_gap"
          ; line = None
          ; message = sprintf "sequence %Ld is followed by %Ld" left.sequence right.sequence
          }
          :: errors
      in
      loop errors rest
    | [] | [ _ ] -> List.rev errors
  in
  loop [] events
;;

let schema_errors events =
  List.filter_map events ~f:(fun event ->
    if
      String.equal event.schema_version "ochat-shell-audit-v1"
      || String.equal event.schema_version "legacy-unversioned"
    then None
    else
      Some
        { code = "shell.audit_schema_version"
        ; line = None
        ; message = "unsupported shell audit schema: " ^ event.schema_version
        })
;;

let validate events =
  let errors = schema_errors events @ sequence_errors events @ integrity_errors events in
  if List.is_empty errors then Ok () else Error errors
;;

let field_string event name =
  match find event.fields name with Some (`String value) -> Some value | _ -> None
;;

let field_int event name =
  match find event.fields name with
  | Some (`Number value) -> Option.try_with (fun () -> Int.of_string value)
  | _ -> None
;;

let field_strings event name =
  match find event.fields name with
  | Some (`Array values) ->
    List.filter_map values ~f:(function `String value -> Some value | _ -> None)
  | _ -> []
;;

let latest events name =
  List.find_map (List.rev events) ~f:(fun event -> field_string event name)
;;

let output_bytes events channel =
  List.sum
    (module Int)
    events
    ~f:(fun event ->
      if Option.value_map (field_string event "channel") ~default:false ~f:(String.equal channel)
      then Option.value (field_int event "bytes") ~default:0
      else 0)
;;

let request_of_events (events : event list) =
  let first = List.hd_exn events in
  { request_id = first.request_id
  ; runtime_id = first.runtime_id
  ; manifest_sha256 = first.manifest_sha256
  ; command = latest events "command"
  ; command_sha256 = latest events "command_sha256"
  ; cwd_sha256 = latest events "cwd_sha256"
  ; request_kind = latest events "request_kind"
  ; effects = List.find_map events ~f:(fun event ->
      let values = field_strings event "effects" in
      Option.some_if (not (List.is_empty values)) values)
      |> Option.value ~default:[]
  ; policy_action = latest events "policy_action"
  ; approval_answer = latest events "approval_answer"
  ; interceptors = List.filter_map events ~f:(fun event -> field_string event "interceptor")
  ; backend = latest events "backend"
  ; stdout_bytes = output_bytes events "stdout"
  ; stderr_bytes = output_bytes events "stderr"
  ; exit_kind = latest events "exit_kind"
  ; exit_code = List.find_map (List.rev events) ~f:(fun event -> field_int event "exit_code")
  ; signal = List.find_map (List.rev events) ~f:(fun event -> field_int event "signal")
  ; rejected_reason = latest events "reason"
  ; completed = List.exists events ~f:(fun event ->
      String.equal event.name "finished"
      || String.equal event.name "terminated"
      || String.equal event.name "rejected")
  ; events
  }
;;

let requests (events : event list) =
  List.fold events ~init:String.Map.empty ~f:(fun grouped (event : event) ->
    Map.add_multi grouped ~key:event.request_id ~data:event)
  |> Map.data
  |> List.map ~f:(fun (events : event list) ->
    List.sort events ~compare:(fun (left : event) (right : event) ->
      Int64.compare left.sequence right.sequence)
    |> request_of_events)
;;

let request events ~request_id =
  List.find (requests events) ~f:(fun request -> String.equal request.request_id request_id)
  |> Result.of_option
       ~error:
         { code = "shell.audit_request_not_found"
         ; line = None
         ; message = "request not found: " ^ request_id
         }
;;

let render_event event =
  sprintf
    "%Ld  %.3f  %-20s  request=%s%s"
    event.sequence
    event.timestamp
    event.name
    event.request_id
    (Option.value_map event.plan_id ~default:"" ~f:(fun id -> " plan=" ^ id))
;;

let render_request request =
  sprintf
    "request %s\nruntime: %s\nmanifest: %s\nrequest kind: %s\ncommand: %s\ncommand identity: %s\ncwd identity: %s\neffects: %s\npolicy: %s\napproval: %s\nbackend: %s\noutput: stdout=%d stderr=%d\nresult: %s%s%s"
    request.request_id
    request.runtime_id
    request.manifest_sha256
    (Option.value request.request_kind ~default:"<unknown>")
    (Option.value request.command ~default:"<metadata only>")
    (Option.value request.command_sha256 ~default:"<none>")
    (Option.value request.cwd_sha256 ~default:"<none>")
    (String.concat ~sep:", " request.effects)
    (Option.value request.policy_action ~default:"<none>")
    (Option.value request.approval_answer ~default:"<none>")
    (Option.value request.backend ~default:"<none>")
    request.stdout_bytes
    request.stderr_bytes
    (Option.value request.exit_kind ~default:"<unfinished>")
    (Option.value_map request.exit_code ~default:"" ~f:(sprintf " code=%d"))
    (Option.value_map request.signal ~default:"" ~f:(sprintf " signal=%d"))
;;
