open! Core
module Error = Protocol_error
module C = Developer_codec
module D = Document_schema
module Common = Developer_common
module Repl = Developer_repl_wire

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode

let optional fields name decode =
  let* value = C.optional fields name in
  Option.value_map value ~default:(Ok None) ~f:(fun json ->
    Result.map (decode json) ~f:Option.some)
;;

let finite value = value > 0 && value <= Authoring_codec.maximum_wire_integer

let valid_execution_id value =
  (not (String.is_empty value))
  && String.length value <= 128
  && String.for_all value ~f:(function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false)
;;

let finite_json limits =
  List.for_all
    [ D.Limits.max_bytes limits
    ; D.Limits.max_depth limits
    ; D.Limits.max_fields limits
    ; D.Limits.max_nodes limits
    ]
    ~f:finite
;;

let json_limits t =
  `Object
    [ "max_bytes", C.integer_json (D.Limits.max_bytes t)
    ; "max_depth", C.integer_json (D.Limits.max_depth t)
    ; "max_fields", C.integer_json (D.Limits.max_fields t)
    ; "max_nodes", C.integer_json (D.Limits.max_nodes t)
    ]
;;

let json_limits_of_json json =
  let* fields =
    C.fields ~allowed:[ "max_bytes"; "max_depth"; "max_fields"; "max_nodes" ] json
  in
  let* max_bytes = get fields "max_bytes" C.positive in
  let* max_depth = get fields "max_depth" C.positive in
  let* max_fields = get fields "max_fields" C.positive in
  let* max_nodes = get fields "max_nodes" C.positive in
  D.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes
  |> Result.map_error ~f:(fun _ -> Error.invalid_request "invalid trace JSON limits")
;;

module Features = struct
  let trace = "developer.trace"
end

module Limits = struct
  type t =
    { request : D.Limits.t
    ; page : D.Limits.t
    ; response : D.Limits.t
    ; max_page_items : int
    ; max_cursor_bytes : int
    }

  let create ~request ~page ~response ~max_page_items ~max_cursor_bytes =
    if
      (not (finite_json request && finite_json page && finite_json response))
      || (not (finite max_page_items && finite max_cursor_bytes))
      || max_cursor_bytes > D.Limits.max_bytes request
      || max_cursor_bytes > D.Limits.max_bytes page
    then C.invalid "invalid trace wire limits"
    else Ok { request; page; response; max_page_items; max_cursor_bytes }
  ;;

  let request t = t.request
  let page t = t.page
  let response t = t.response
  let max_page_items t = t.max_page_items
  let max_cursor_bytes t = t.max_cursor_bytes

  let to_json t =
    `Object
      [ "request", json_limits t.request
      ; "page", json_limits t.page
      ; "response", json_limits t.response
      ; "max_page_items", C.integer_json t.max_page_items
      ; "max_cursor_bytes", C.integer_json t.max_cursor_bytes
      ]
  ;;

  let of_json json =
    let* () = C.validate ~limits:D.Limits.default json in
    let* fields =
      C.fields
        ~allowed:[ "request"; "page"; "response"; "max_page_items"; "max_cursor_bytes" ]
        json
    in
    let* request = get fields "request" json_limits_of_json in
    let* page = get fields "page" json_limits_of_json in
    let* response = get fields "response" json_limits_of_json in
    let* max_page_items = get fields "max_page_items" C.positive in
    let* max_cursor_bytes = get fields "max_cursor_bytes" C.positive in
    create ~request ~page ~response ~max_page_items ~max_cursor_bytes
  ;;
end

module Locator = struct
  type t =
    | Command of Common.Lookup.t
    | Repl_statement of
        { instance : Repl.Wire_id.Instance.t
        ; epoch : Repl.Wire_id.Epoch.t
        ; key : Idempotency_key.t
        }
    | Repl_task of Common.Lookup.t

  let to_json = function
    | Command lookup ->
      `Object [ "kind", `String "command"; "lookup", Common.Lookup.to_json lookup ]
    | Repl_task lookup ->
      `Object [ "kind", `String "repl_task"; "lookup", Common.Lookup.to_json lookup ]
    | Repl_statement { instance; epoch; key } ->
      `Object
        [ "kind", `String "repl_statement"
        ; "instance", `String (Repl.Wire_id.Instance.to_string instance)
        ; "epoch", `String (Repl.Wire_id.Epoch.to_string epoch)
        ; "key", Idempotency_key.to_json key
        ]
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.request limits) json in
    let* fields =
      C.fields ~allowed:[ "kind"; "lookup"; "instance"; "epoch"; "key" ] json
    in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "command" | "repl_task" ->
      let* () = C.fields ~allowed:[ "kind"; "lookup" ] json |> Result.map ~f:ignore in
      let* lookup =
        get fields "lookup" (Common.Lookup.of_json ~limits:(Limits.request limits))
      in
      if String.equal kind "command" then Ok (Command lookup) else Ok (Repl_task lookup)
    | "repl_statement" ->
      let* () =
        C.fields ~allowed:[ "kind"; "instance"; "epoch"; "key" ] json
        |> Result.map ~f:ignore
      in
      let* instance =
        get fields "instance" (fun value ->
          let* id = C.string ~max_bytes:96 value in
          Repl.Wire_id.Instance.of_string id)
      in
      let* epoch =
        get fields "epoch" (fun value ->
          let* id = C.string ~max_bytes:96 value in
          Repl.Wire_id.Epoch.of_string id)
      in
      let* key = get fields "key" Idempotency_key.of_json in
      Ok (Repl_statement { instance; epoch; key })
    | _ -> C.invalid "unknown trace locator kind"
  ;;

  let equal left right =
    match left, right with
    | Command a, Command b | Repl_task a, Repl_task b ->
      Id.Developer_run.equal a.run_id b.run_id && Idempotency_key.equal a.key b.key
    | Repl_statement a, Repl_statement b ->
      Repl.Wire_id.Instance.equal a.instance b.instance
      && Repl.Wire_id.Epoch.equal a.epoch b.epoch
      && Idempotency_key.equal a.key b.key
    | Command _, (Repl_task _ | Repl_statement _)
    | Repl_task _, (Command _ | Repl_statement _)
    | Repl_statement _, (Command _ | Repl_task _) -> false
  ;;
end

let canonical_sequence = function
  | `String encoded ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value > 0L) && String.equal encoded (Int64.to_string value)
       -> Ok value
     | Some _ | None -> C.invalid "invalid trace sequence")
  | `Null | `True | `False | `Number _ | `Array _ | `Object _ ->
    C.invalid "expected trace sequence string"
;;

module Cursor = struct
  type t =
    { locator : Locator.t
    ; execution_id : string
    ; submission_fingerprint : Chatml_source.Digest.t
    ; selection_fingerprint : Chatml_source.Digest.t
    ; after_sequence : int64
    }

  let to_json t =
    `Object
      [ "format_version", C.integer_json 1
      ; "locator", Locator.to_json t.locator
      ; "execution_id", `String t.execution_id
      ; "submission_fingerprint", C.digest_json t.submission_fingerprint
      ; "selection_fingerprint", C.digest_json t.selection_fingerprint
      ; "after_sequence", `String (Int64.to_string t.after_sequence)
      ]
  ;;

  let create
        ~limits
        ~locator
        ~execution_id
        ~submission_fingerprint
        ~selection_fingerprint
        ~after_sequence
    =
    if (not (valid_execution_id execution_id)) || Int64.(after_sequence <= 0L)
    then C.invalid "invalid trace cursor"
    else (
      let t =
        { locator
        ; execution_id
        ; submission_fingerprint
        ; selection_fingerprint
        ; after_sequence
        }
      in
      let* cursor_limits =
        D.Limits.create
          ~max_bytes:(Limits.max_cursor_bytes limits)
          ~max_depth:(D.Limits.max_depth (Limits.request limits))
          ~max_fields:(D.Limits.max_fields (Limits.request limits))
          ~max_nodes:(D.Limits.max_nodes (Limits.request limits))
        |> Result.map_error ~f:(fun _ -> Error.invalid_request "invalid cursor limits")
      in
      let* () = C.validate ~limits:cursor_limits (to_json t) in
      Ok t)
  ;;

  let locator t = t.locator
  let execution_id t = t.execution_id
  let submission_fingerprint t = t.submission_fingerprint
  let selection_fingerprint t = t.selection_fingerprint
  let after_sequence t = t.after_sequence

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.request limits) json in
    let* fields =
      C.fields
        ~allowed:
          [ "format_version"
          ; "locator"
          ; "execution_id"
          ; "submission_fingerprint"
          ; "selection_fingerprint"
          ; "after_sequence"
          ]
        json
    in
    let* format_version = get fields "format_version" C.positive in
    if format_version <> 1
    then C.invalid "unsupported trace cursor format"
    else
      let* locator = get fields "locator" (Locator.of_json ~limits) in
      let* execution_id = get fields "execution_id" (C.string ~max_bytes:128) in
      let* submission_fingerprint = get fields "submission_fingerprint" C.digest in
      let* selection_fingerprint = get fields "selection_fingerprint" C.digest in
      let* after_sequence = get fields "after_sequence" canonical_sequence in
      create
        ~limits
        ~locator
        ~execution_id
        ~submission_fingerprint
        ~selection_fingerprint
        ~after_sequence
  ;;
end

module Page_request = struct
  type t =
    { locator : Locator.t
    ; after : Cursor.t option
    ; limit : int
    }

  let to_json t =
    `Object
      ([ "locator", Locator.to_json t.locator; "limit", C.integer_json t.limit ]
       @ C.optional_field "after" t.after Cursor.to_json)
  ;;

  let create ~limits ~locator ~after ~limit =
    if limit <= 0 || limit > Limits.max_page_items limits
    then C.invalid "invalid trace page count"
    else if
      Option.exists after ~f:(fun cursor ->
        not (Locator.equal locator (Cursor.locator cursor)))
    then C.invalid "trace cursor has a different locator"
    else
      let* () =
        match after with
        | None -> Ok ()
        | Some cursor ->
          Cursor.of_json ~limits (Cursor.to_json cursor) |> Result.map ~f:ignore
      in
      let t = { locator; after; limit } in
      let* () = C.validate ~limits:(Limits.request limits) (to_json t) in
      Ok t
  ;;

  let locator t = t.locator
  let after t = t.after
  let limit t = t.limit

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.request limits) json in
    let* fields = C.fields ~allowed:[ "locator"; "after"; "limit" ] json in
    let* locator = get fields "locator" (Locator.of_json ~limits) in
    let* after = optional fields "after" (Cursor.of_json ~limits) in
    let* limit = get fields "limit" C.positive in
    create ~limits ~locator ~after ~limit
  ;;
end

module Location = struct
  type t =
    { source : Chatml_source.Id.t
    ; digest : Chatml_source.Digest.t
    ; start_byte : int
    ; end_byte : int
    ; start_line : int
    ; start_column : int
    ; end_line : int
    ; end_column : int
    }

  let to_json t =
    `Object
      [ "source", C.source_id_json t.source
      ; "digest", C.digest_json t.digest
      ; "start_byte", C.integer_json t.start_byte
      ; "end_byte", C.integer_json t.end_byte
      ; "start_line", C.integer_json t.start_line
      ; "start_column", C.integer_json t.start_column
      ; "end_line", C.integer_json t.end_line
      ; "end_column", C.integer_json t.end_column
      ]
  ;;

  let create
        ~source
        ~digest
        ~start_byte
        ~end_byte
        ~start_line
        ~start_column
        ~end_line
        ~end_column
    =
    let numbers =
      [ start_byte; end_byte; start_line; start_column; end_line; end_column ]
    in
    if
      List.exists numbers ~f:(fun value ->
        value < 0 || value > Authoring_codec.maximum_wire_integer)
      || start_line = 0
      || end_line = 0
      || end_byte < start_byte
      || end_line < start_line
      || start_column > start_byte
      || end_column > end_byte
      || (Int.equal end_line start_line
          && end_column - start_column <> end_byte - start_byte)
      || (Int.equal end_byte start_byte
          && (end_line <> start_line || end_column <> start_column))
    then C.invalid "invalid trace location"
    else
      let* () = Authoring_codec.validate_source_id source in
      Ok
        { source
        ; digest
        ; start_byte
        ; end_byte
        ; start_line
        ; start_column
        ; end_line
        ; end_column
        }
  ;;

  let source t = t.source
  let digest t = t.digest
  let start_byte t = t.start_byte
  let end_byte t = t.end_byte
  let start_line t = t.start_line
  let start_column t = t.start_column
  let end_line t = t.end_line
  let end_column t = t.end_column

  let of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "source"
          ; "digest"
          ; "start_byte"
          ; "end_byte"
          ; "start_line"
          ; "start_column"
          ; "end_line"
          ; "end_column"
          ]
        json
    in
    let* source = get fields "source" C.source_id in
    let* digest = get fields "digest" C.digest in
    let* start_byte = get fields "start_byte" C.nonnegative in
    let* end_byte = get fields "end_byte" C.nonnegative in
    let* start_line = get fields "start_line" C.positive in
    let* start_column = get fields "start_column" C.nonnegative in
    let* end_line = get fields "end_line" C.positive in
    let* end_column = get fields "end_column" C.nonnegative in
    create
      ~source
      ~digest
      ~start_byte
      ~end_byte
      ~start_line
      ~start_column
      ~end_line
      ~end_column
  ;;
end

module Header = struct
  module Source_ref = struct
    type t =
      { id : Chatml_source.Id.t
      ; digest : Chatml_source.Digest.t
      ; byte_length : int
      }

    let to_json t =
      `Object
        [ "id", C.source_id_json t.id
        ; "digest", C.digest_json t.digest
        ; "byte_length", C.integer_json t.byte_length
        ]
    ;;

    let create ~id ~digest ~byte_length =
      if byte_length < 0 || byte_length > Authoring_codec.maximum_wire_integer
      then C.invalid "invalid trace source length"
      else
        let* () = Authoring_codec.validate_source_id id in
        Ok { id; digest; byte_length }
    ;;

    let id t = t.id
    let digest t = t.digest
    let byte_length t = t.byte_length

    let of_json json =
      let* fields = C.fields ~allowed:[ "id"; "digest"; "byte_length" ] json in
      let* id = get fields "id" C.source_id in
      let* digest = get fields "digest" C.digest in
      let* byte_length = get fields "byte_length" C.nonnegative in
      create ~id ~digest ~byte_length
    ;;
  end

  type admission =
    | Submitted_capture
    | Prepared_graph
  [@@deriving equal, sexp_of]

  type t =
    { execution_id : string
    ; admission : admission
    ; submission_fingerprint : Chatml_source.Digest.t
    ; selection_fingerprint : Chatml_source.Digest.t
    ; graph_fingerprint : Chatml_source.Digest.t option
    ; contract_fingerprint : Chatml_source.Digest.t option
    ; environment_fingerprint : Chatml_source.Digest.t option
    ; sources : Source_ref.t list
    }

  let admission_name = function
    | Submitted_capture -> "submitted_capture"
    | Prepared_graph -> "prepared_graph"
  ;;

  let to_json t =
    `Object
      ([ "execution_id", `String t.execution_id
       ; "admission", `String (admission_name t.admission)
       ; "submission_fingerprint", C.digest_json t.submission_fingerprint
       ; "selection_fingerprint", C.digest_json t.selection_fingerprint
       ; "sources", `Array (List.map t.sources ~f:Source_ref.to_json)
       ]
       @ C.optional_field "graph_fingerprint" t.graph_fingerprint C.digest_json
       @ C.optional_field "contract_fingerprint" t.contract_fingerprint C.digest_json
       @ C.optional_field
           "environment_fingerprint"
           t.environment_fingerprint
           C.digest_json)
  ;;

  let validate_sources sources =
    let* groups =
      List.fold_result sources ~init:String.Map.empty ~f:(fun groups source ->
        let id = Source_ref.id source in
        let namespace = Chatml_source.Id.package_namespace id in
        let path = Chatml_source.Id.relative_path id in
        let* portable =
          Source.Portable_path.create path
          |> Result.map_error ~f:(fun _ -> Error.invalid_request "invalid trace path")
        in
        Ok
          (Map.update groups namespace ~f:(fun paths ->
             portable :: Option.value paths ~default:[])))
    in
    Map.fold groups ~init:(Ok ()) ~f:(fun ~key:_ ~data:paths result ->
      let* () = result in
      match Source.Portable_path.first_collision paths ~checkpoint:Fn.id with
      | None -> Ok ()
      | Some _ -> C.invalid "ambiguous trace source inventory")
  ;;

  let create
        ~execution_id
        ~admission
        ~submission_fingerprint
        ~selection_fingerprint
        ~graph_fingerprint
        ~contract_fingerprint
        ~environment_fingerprint
        ~sources
    =
    if not (valid_execution_id execution_id)
    then C.invalid "invalid trace execution ID"
    else
      let* () =
        match
          admission, graph_fingerprint, contract_fingerprint, environment_fingerprint
        with
        | Submitted_capture, None, None, None -> Ok ()
        | Prepared_graph, Some _, Some _, Some _ -> Ok ()
        | Submitted_capture, _, _, _ | Prepared_graph, _, _, _ ->
          C.invalid "trace header admission/fingerprint mismatch"
      in
      let* () = validate_sources sources in
      Ok
        { execution_id
        ; admission
        ; submission_fingerprint
        ; selection_fingerprint
        ; graph_fingerprint
        ; contract_fingerprint
        ; environment_fingerprint
        ; sources
        }
  ;;

  let execution_id t = t.execution_id
  let admission t = t.admission
  let submission_fingerprint t = t.submission_fingerprint
  let selection_fingerprint t = t.selection_fingerprint
  let graph_fingerprint t = t.graph_fingerprint
  let contract_fingerprint t = t.contract_fingerprint
  let environment_fingerprint t = t.environment_fingerprint
  let sources t = t.sources

  let of_json ~limits json =
    let* fields =
      C.fields
        ~allowed:
          [ "execution_id"
          ; "admission"
          ; "submission_fingerprint"
          ; "selection_fingerprint"
          ; "graph_fingerprint"
          ; "contract_fingerprint"
          ; "environment_fingerprint"
          ; "sources"
          ]
        json
    in
    let* execution_id = get fields "execution_id" (C.string ~max_bytes:128) in
    let* admission =
      get fields "admission" (fun value ->
        let* name = C.string ~max_bytes:32 value in
        match name with
        | "submitted_capture" -> Ok Submitted_capture
        | "prepared_graph" -> Ok Prepared_graph
        | _ -> C.invalid "unknown trace admission")
    in
    let* submission_fingerprint = get fields "submission_fingerprint" C.digest in
    let* selection_fingerprint = get fields "selection_fingerprint" C.digest in
    let* graph_fingerprint = optional fields "graph_fingerprint" C.digest in
    let* contract_fingerprint = optional fields "contract_fingerprint" C.digest in
    let* environment_fingerprint = optional fields "environment_fingerprint" C.digest in
    let* sources =
      get
        fields
        "sources"
        (C.array ~max_items:(D.Limits.max_nodes (Limits.page limits)) Source_ref.of_json)
    in
    create
      ~execution_id
      ~admission
      ~submission_fingerprint
      ~selection_fingerprint
      ~graph_fingerprint
      ~contract_fingerprint
      ~environment_fingerprint
      ~sources
  ;;
end

module Event = struct
  type phase =
    | Preparation
    | Compilation
    | Dependency_initialization
    | Entry_initialization
    | Evaluation
    | Task
    | Host_wait
  [@@deriving equal, sexp_of]

  type operation =
    | Tool_call
    | Tool_spawn
    | Job_start
    | Model_call
    | Model_spawn
    | Process_run
    | Schedule
    | Job_read
    | Local_transaction
    | Other
  [@@deriving equal, sexp_of]

  type failure =
    | Parse
    | Typecheck
    | Link
    | Runtime
    | Host_operation
    | Permission
    | Resource_limit
    | Other
  [@@deriving equal, sexp_of]

  type kind =
    | Phase_started of phase
    | Phase_finished of phase
    | Statement_entered
    | Call_entered
    | Effect_attempted of operation
    | Effect_host_returned of operation
    | Effect_host_failed of operation
    | Host_wait_entered
    | Host_wait_resumed
    | Job_admitted
    | Job_terminal
    | Recovered_failure of failure
    | Unrecovered_failure of failure
  [@@deriving equal, sexp_of]

  type t =
    { sequence : int64
    ; kind : kind
    ; site : Location.t option
    ; correlation : string option
    }

  let phase_name = function
    | Preparation -> "preparation"
    | Compilation -> "compilation"
    | Dependency_initialization -> "dependency_initialization"
    | Entry_initialization -> "entry_initialization"
    | Evaluation -> "evaluation"
    | Task -> "task"
    | Host_wait -> "host_wait"
  ;;

  let phase_of_name = function
    | "preparation" -> Ok Preparation
    | "compilation" -> Ok Compilation
    | "dependency_initialization" -> Ok Dependency_initialization
    | "entry_initialization" -> Ok Entry_initialization
    | "evaluation" -> Ok Evaluation
    | "task" -> Ok Task
    | "host_wait" -> Ok Host_wait
    | _ -> C.invalid "unknown trace phase"
  ;;

  let operation_name = function
    | Tool_call -> "tool_call"
    | Tool_spawn -> "tool_spawn"
    | Job_start -> "job_start"
    | Model_call -> "model_call"
    | Model_spawn -> "model_spawn"
    | Process_run -> "process_run"
    | Schedule -> "schedule"
    | Job_read -> "job_read"
    | Local_transaction -> "local_transaction"
    | Other -> "other"
  ;;

  let operation_of_name = function
    | "tool_call" -> Ok Tool_call
    | "tool_spawn" -> Ok Tool_spawn
    | "job_start" -> Ok Job_start
    | "model_call" -> Ok Model_call
    | "model_spawn" -> Ok Model_spawn
    | "process_run" -> Ok Process_run
    | "schedule" -> Ok Schedule
    | "job_read" -> Ok Job_read
    | "local_transaction" -> Ok Local_transaction
    | "other" -> Ok Other
    | _ -> C.invalid "unknown trace operation"
  ;;

  let failure_name = function
    | Parse -> "parse"
    | Typecheck -> "typecheck"
    | Link -> "link"
    | Runtime -> "runtime"
    | Host_operation -> "host_operation"
    | Permission -> "permission"
    | Resource_limit -> "resource_limit"
    | Other -> "other"
  ;;

  let failure_of_name = function
    | "parse" -> Ok Parse
    | "typecheck" -> Ok Typecheck
    | "link" -> Ok Link
    | "runtime" -> Ok Runtime
    | "host_operation" -> Ok Host_operation
    | "permission" -> Ok Permission
    | "resource_limit" -> Ok Resource_limit
    | "other" -> Ok Other
    | _ -> C.invalid "unknown trace failure category"
  ;;

  let kind_json = function
    | Phase_started phase ->
      `Object [ "kind", `String "phase_started"; "phase", `String (phase_name phase) ]
    | Phase_finished phase ->
      `Object [ "kind", `String "phase_finished"; "phase", `String (phase_name phase) ]
    | Statement_entered -> `Object [ "kind", `String "statement_entered" ]
    | Call_entered -> `Object [ "kind", `String "call_entered" ]
    | Effect_attempted operation ->
      `Object
        [ "kind", `String "effect_attempted"
        ; "operation", `String (operation_name operation)
        ]
    | Effect_host_returned operation ->
      `Object
        [ "kind", `String "effect_host_returned"
        ; "operation", `String (operation_name operation)
        ]
    | Effect_host_failed operation ->
      `Object
        [ "kind", `String "effect_host_failed"
        ; "operation", `String (operation_name operation)
        ]
    | Host_wait_entered -> `Object [ "kind", `String "host_wait_entered" ]
    | Host_wait_resumed -> `Object [ "kind", `String "host_wait_resumed" ]
    | Job_admitted -> `Object [ "kind", `String "job_admitted" ]
    | Job_terminal -> `Object [ "kind", `String "job_terminal" ]
    | Recovered_failure failure ->
      `Object
        [ "kind", `String "recovered_failure"; "failure", `String (failure_name failure) ]
    | Unrecovered_failure failure ->
      `Object
        [ "kind", `String "unrecovered_failure"
        ; "failure", `String (failure_name failure)
        ]
  ;;

  let kind_of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "phase"; "operation"; "failure" ] json in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    let exact allowed decode =
      let* () = C.fields ~allowed json |> Result.map ~f:ignore in
      decode fields
    in
    match kind with
    | "phase_started" ->
      exact [ "kind"; "phase" ] (fun fields ->
        let* phase = get fields "phase" (C.string ~max_bytes:32) in
        Result.map (phase_of_name phase) ~f:(fun x -> Phase_started x))
    | "phase_finished" ->
      exact [ "kind"; "phase" ] (fun fields ->
        let* phase = get fields "phase" (C.string ~max_bytes:32) in
        Result.map (phase_of_name phase) ~f:(fun x -> Phase_finished x))
    | "effect_attempted" ->
      exact [ "kind"; "operation" ] (fun fields ->
        let* operation = get fields "operation" (C.string ~max_bytes:32) in
        Result.map (operation_of_name operation) ~f:(fun x -> Effect_attempted x))
    | "effect_host_returned" ->
      exact [ "kind"; "operation" ] (fun fields ->
        let* operation = get fields "operation" (C.string ~max_bytes:32) in
        Result.map (operation_of_name operation) ~f:(fun x -> Effect_host_returned x))
    | "effect_host_failed" ->
      exact [ "kind"; "operation" ] (fun fields ->
        let* operation = get fields "operation" (C.string ~max_bytes:32) in
        Result.map (operation_of_name operation) ~f:(fun x -> Effect_host_failed x))
    | "recovered_failure" ->
      exact [ "kind"; "failure" ] (fun fields ->
        let* failure = get fields "failure" (C.string ~max_bytes:32) in
        Result.map (failure_of_name failure) ~f:(fun x -> Recovered_failure x))
    | "unrecovered_failure" ->
      exact [ "kind"; "failure" ] (fun fields ->
        let* failure = get fields "failure" (C.string ~max_bytes:32) in
        Result.map (failure_of_name failure) ~f:(fun x -> Unrecovered_failure x))
    | "statement_entered" -> exact [ "kind" ] (fun _ -> Ok Statement_entered)
    | "call_entered" -> exact [ "kind" ] (fun _ -> Ok Call_entered)
    | "host_wait_entered" -> exact [ "kind" ] (fun _ -> Ok Host_wait_entered)
    | "host_wait_resumed" -> exact [ "kind" ] (fun _ -> Ok Host_wait_resumed)
    | "job_admitted" -> exact [ "kind" ] (fun _ -> Ok Job_admitted)
    | "job_terminal" -> exact [ "kind" ] (fun _ -> Ok Job_terminal)
    | _ -> C.invalid "unknown trace event kind"
  ;;

  let to_json t =
    `Object
      ([ "sequence", `String (Int64.to_string t.sequence); "kind", kind_json t.kind ]
       @ C.optional_field "site" t.site Location.to_json
       @ C.optional_field "correlation" t.correlation (fun token -> `String token))
  ;;

  let create ~sequence ~kind ~site ~correlation =
    if
      Int64.(sequence <= 0L)
      || Option.exists correlation ~f:(fun token -> not (valid_execution_id token))
    then C.invalid "invalid trace event"
    else Ok { sequence; kind; site; correlation }
  ;;

  let sequence t = t.sequence
  let kind t = t.kind
  let site t = t.site
  let correlation t = t.correlation

  let of_json json =
    let* fields = C.fields ~allowed:[ "sequence"; "kind"; "site"; "correlation" ] json in
    let* sequence = get fields "sequence" canonical_sequence in
    let* kind = get fields "kind" kind_of_json in
    let* site = optional fields "site" Location.of_json in
    let* correlation = optional fields "correlation" (C.string ~max_bytes:128) in
    create ~sequence ~kind ~site ~correlation
  ;;
end

module Terminal = struct
  type disposition =
    | Succeeded
    | Failed of Event.failure
    | Cancelled
    | Denied
    | Interrupted
  [@@deriving equal, sexp_of]

  type t =
    { receipt_version : int
    ; disposition : disposition
    }

  let disposition_json = function
    | Succeeded -> `Object [ "kind", `String "succeeded" ]
    | Failed failure ->
      `Object
        [ "kind", `String "failed"; "failure", `String (Event.failure_name failure) ]
    | Cancelled -> `Object [ "kind", `String "cancelled" ]
    | Denied -> `Object [ "kind", `String "denied" ]
    | Interrupted -> `Object [ "kind", `String "interrupted" ]
  ;;

  let to_json t =
    `Object
      [ "receipt_version", C.integer_json t.receipt_version
      ; "disposition", disposition_json t.disposition
      ]
  ;;

  let create ~receipt_version ~disposition =
    if not (finite receipt_version)
    then C.invalid "invalid trace terminal receipt version"
    else Ok { receipt_version; disposition }
  ;;

  let receipt_version t = t.receipt_version
  let disposition t = t.disposition

  let of_json json =
    let* fields = C.fields ~allowed:[ "receipt_version"; "disposition" ] json in
    let* receipt_version = get fields "receipt_version" C.positive in
    let* disposition =
      get fields "disposition" (fun json ->
        let* fields = C.fields ~allowed:[ "kind"; "failure" ] json in
        let* kind = get fields "kind" (C.string ~max_bytes:32) in
        match kind with
        | "failed" ->
          let* failure = get fields "failure" (C.string ~max_bytes:32) in
          Result.map (Event.failure_of_name failure) ~f:(fun x -> Failed x)
        | "succeeded" | "cancelled" | "denied" | "interrupted" ->
          let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
          (match kind with
           | "succeeded" -> Ok Succeeded
           | "cancelled" -> Ok Cancelled
           | "denied" -> Ok Denied
           | "interrupted" -> Ok Interrupted
           | "failed" | _ -> C.invalid "unknown trace disposition")
        | _ -> C.invalid "unknown trace disposition")
    in
    create ~receipt_version ~disposition
  ;;
end

let nonnegative_sequence = function
  | `String "0" -> Ok 0L
  | json -> canonical_sequence json
;;

module Page = struct
  type drop =
    { at_least : int64
    ; overflowed : bool
    }

  type state =
    | Incomplete
    | Complete of Terminal.t

  type t =
    { locator : Locator.t
    ; after : Cursor.t option
    ; header : Header.t
    ; events : Event.t list
    ; last_sequence : int64
    ; omitted_after_prefix : drop option
    ; last_retained_site_context : Location.t option
    ; omitted_site_count : int
    ; state : state
    ; resume : Cursor.t option
    ; next : Cursor.t option
    }

  let drop_json drop =
    `Object
      [ "at_least", `String (Int64.to_string drop.at_least)
      ; ("overflowed", if drop.overflowed then `True else `False)
      ]
  ;;

  let drop_of_json json =
    let* fields = C.fields ~allowed:[ "at_least"; "overflowed" ] json in
    let* at_least = get fields "at_least" canonical_sequence in
    let* overflowed = get fields "overflowed" C.boolean in
    Ok { at_least; overflowed }
  ;;

  let state_json = function
    | Incomplete -> `Object [ "kind", `String "incomplete" ]
    | Complete terminal ->
      `Object [ "kind", `String "complete"; "terminal", Terminal.to_json terminal ]
  ;;

  let state_of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "terminal" ] json in
    let* kind = get fields "kind" (C.string ~max_bytes:16) in
    match kind with
    | "incomplete" ->
      let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
      Ok Incomplete
    | "complete" ->
      let* terminal = get fields "terminal" Terminal.of_json in
      Ok (Complete terminal)
    | _ -> C.invalid "unknown trace page state"
  ;;

  let to_json t =
    `Object
      ([ "locator", Locator.to_json t.locator
       ; "header", Header.to_json t.header
       ; "events", `Array (List.map t.events ~f:Event.to_json)
       ; "last_sequence", `String (Int64.to_string t.last_sequence)
       ; "omitted_site_count", C.integer_json t.omitted_site_count
       ; "state", state_json t.state
       ]
       @ C.optional_field "after" t.after Cursor.to_json
       @ C.optional_field "omitted_after_prefix" t.omitted_after_prefix drop_json
       @ C.optional_field
           "last_retained_site_context"
           t.last_retained_site_context
           Location.to_json
       @ C.optional_field "resume" t.resume Cursor.to_json
       @ C.optional_field "next" t.next Cursor.to_json)
  ;;

  let same_cursor left right =
    Locator.equal (Cursor.locator left) (Cursor.locator right)
    && String.equal (Cursor.execution_id left) (Cursor.execution_id right)
    && Chatml_source.Digest.equal
         (Cursor.submission_fingerprint left)
         (Cursor.submission_fingerprint right)
    && Chatml_source.Digest.equal
         (Cursor.selection_fingerprint left)
         (Cursor.selection_fingerprint right)
    && Int64.equal (Cursor.after_sequence left) (Cursor.after_sequence right)
  ;;

  let bound_cursor ~locator ~header cursor =
    Locator.equal locator (Cursor.locator cursor)
    && String.equal (Header.execution_id header) (Cursor.execution_id cursor)
    && Chatml_source.Digest.equal
         (Header.submission_fingerprint header)
         (Cursor.submission_fingerprint cursor)
    && Chatml_source.Digest.equal
         (Header.selection_fingerprint header)
         (Cursor.selection_fingerprint cursor)
  ;;

  let create
        ~limits
        ~locator
        ~after
        ~header
        ~events
        ~last_sequence
        ~omitted_after_prefix
        ~last_retained_site_context
        ~omitted_site_count
        ~state
        ~resume
        ~next
    =
    let after_sequence = Option.value_map after ~default:0L ~f:Cursor.after_sequence in
    let final_sequence = Option.map (List.last events) ~f:Event.sequence in
    let valid_events =
      List.fold events ~init:(Some after_sequence) ~f:(fun previous event ->
        match previous with
        | None -> None
        | Some sequence
          when Int64.(Event.sequence event = succ sequence)
               && Int64.(Event.sequence event <= last_sequence) ->
          Some (Event.sequence event)
        | Some _ -> None)
      |> Option.is_some
    in
    let expected_resume =
      match final_sequence, after with
      | None, cursor -> cursor
      | Some _, _ -> resume
    in
    let valid_resume =
      match final_sequence, resume, expected_resume with
      | None, None, None -> true
      | None, Some current, Some prior -> same_cursor current prior
      | Some sequence, Some current, _ ->
        bound_cursor ~locator ~header current
        && Int64.equal (Cursor.after_sequence current) sequence
      | None, None, Some _ | None, Some _, None | Some _, None, _ -> false
    in
    if
      Int64.(last_sequence < 0L || after_sequence > last_sequence)
      || omitted_site_count < 0
      || omitted_site_count > Authoring_codec.maximum_wire_integer
      || List.length events > Limits.max_page_items limits
      || (List.is_empty events && Int64.(after_sequence < last_sequence))
      || (not
            (Bool.equal
               (Option.is_some next)
               (Option.value_map final_sequence ~default:false ~f:(fun sequence ->
                  Int64.(sequence < last_sequence)))))
      || (not valid_events)
      || (not valid_resume)
      || Option.exists after ~f:(fun cursor -> not (bound_cursor ~locator ~header cursor))
      || Option.exists resume ~f:(fun cursor ->
        not (bound_cursor ~locator ~header cursor))
      || Option.exists next ~f:(fun cursor ->
        not (Option.exists resume ~f:(same_cursor cursor) && not (List.is_empty events)))
      || Option.exists omitted_after_prefix ~f:(fun drop -> Int64.(drop.at_least <= 0L))
    then C.invalid "inconsistent trace page"
    else
      let* () =
        List.fold_result [ after; resume; next ] ~init:() ~f:(fun () cursor ->
          match cursor with
          | None -> Ok ()
          | Some cursor ->
            Cursor.of_json ~limits (Cursor.to_json cursor) |> Result.map ~f:ignore)
      in
      let t =
        { locator
        ; after
        ; header
        ; events
        ; last_sequence
        ; omitted_after_prefix
        ; last_retained_site_context
        ; omitted_site_count
        ; state
        ; resume
        ; next
        }
      in
      let* () = C.validate ~limits:(Limits.page limits) (to_json t) in
      Ok t
  ;;

  let locator t = t.locator
  let after t = t.after
  let header t = t.header
  let events t = t.events
  let last_sequence t = t.last_sequence
  let omitted_after_prefix t = t.omitted_after_prefix
  let last_retained_site_context t = t.last_retained_site_context
  let omitted_site_count t = t.omitted_site_count
  let state t = t.state
  let resume t = t.resume
  let next t = t.next

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.page limits) json in
    let* fields =
      C.fields
        ~allowed:
          [ "locator"
          ; "after"
          ; "header"
          ; "events"
          ; "last_sequence"
          ; "omitted_after_prefix"
          ; "last_retained_site_context"
          ; "omitted_site_count"
          ; "state"
          ; "resume"
          ; "next"
          ]
        json
    in
    let* locator = get fields "locator" (Locator.of_json ~limits) in
    let* after = optional fields "after" (Cursor.of_json ~limits) in
    let* header = get fields "header" (Header.of_json ~limits) in
    let* events =
      get
        fields
        "events"
        (C.array ~max_items:(Limits.max_page_items limits) Event.of_json)
    in
    let* last_sequence = get fields "last_sequence" nonnegative_sequence in
    let* omitted_after_prefix = optional fields "omitted_after_prefix" drop_of_json in
    let* last_retained_site_context =
      optional fields "last_retained_site_context" Location.of_json
    in
    let* omitted_site_count = get fields "omitted_site_count" C.nonnegative in
    let* state = get fields "state" state_of_json in
    let* resume = optional fields "resume" (Cursor.of_json ~limits) in
    let* next = optional fields "next" (Cursor.of_json ~limits) in
    create
      ~limits
      ~locator
      ~after
      ~header
      ~events
      ~last_sequence
      ~omitted_after_prefix
      ~last_retained_site_context
      ~omitted_site_count
      ~state
      ~resume
      ~next
  ;;
end

module Availability = struct
  type t =
    | Unavailable
    | Expired
    | Retained of Page.t
end

let supported_methods = [ "developer.trace.capabilities"; "developer.trace.page" ]

module Capabilities = struct
  type t =
    { methods : string list
    ; limits : Limits.t
    }

  let max_json_bytes = 65_536

  let to_json t =
    `Object
      [ "format_version", C.integer_json 1
      ; "methods", `Array (List.map t.methods ~f:(fun method_ -> `String method_))
      ; "limits", Limits.to_json t.limits
      ]
  ;;

  let bootstrap_limits () =
    D.Limits.create ~max_bytes:max_json_bytes ~max_depth:16 ~max_fields:128 ~max_nodes:256
    |> Result.map_error ~f:(fun _ -> Error.invalid_request "invalid trace bootstrap")
  ;;

  let create ~methods ~limits =
    if not (List.equal String.equal methods supported_methods)
    then C.invalid "invalid trace capability methods"
    else (
      let t = { methods; limits } in
      let* bound = bootstrap_limits () in
      let* () = C.validate ~limits:bound (to_json t) in
      Ok t)
  ;;

  let methods t = t.methods
  let limits t = t.limits

  let of_json json =
    let* bound = bootstrap_limits () in
    let* () = C.validate ~limits:bound json in
    let* fields = C.fields ~allowed:[ "format_version"; "methods"; "limits" ] json in
    let* format_version = get fields "format_version" C.positive in
    if format_version <> 1
    then C.invalid "unsupported trace capability format"
    else
      let* methods =
        get fields "methods" (C.array ~max_items:2 (C.string ~max_bytes:64))
      in
      let* limits = get fields "limits" Limits.of_json in
      create ~methods ~limits
  ;;
end

module Request = struct
  type t =
    | Capabilities
    | Page of Page_request.t

  let supported_methods = supported_methods

  let method_name = function
    | Capabilities -> "developer.trace.capabilities"
    | Page _ -> "developer.trace.page"
  ;;

  let params = function
    | Capabilities -> `Object []
    | Page request -> Page_request.to_json request
  ;;

  let of_method_and_params ~limits ~method_ ~params =
    let* () = C.validate ~limits:(Limits.request limits) params in
    match method_ with
    | "developer.trace.capabilities" ->
      let* () = C.fields ~allowed:[] params |> Result.map ~f:ignore in
      Ok Capabilities
    | "developer.trace.page" ->
      Result.map (Page_request.of_json ~limits params) ~f:(fun x -> Page x)
    | _ -> C.invalid "unknown trace method"
  ;;
end

module Response = struct
  type t =
    | Capabilities of Capabilities.t
    | Page of Availability.t

  let method_name = function
    | Capabilities _ -> "developer.trace.capabilities"
    | Page _ -> "developer.trace.page"
  ;;

  let raw_json = function
    | Capabilities value -> Capabilities.to_json value
    | Page Unavailable -> `Object [ "kind", `String "unavailable" ]
    | Page Expired -> `Object [ "kind", `String "expired" ]
    | Page (Retained page) ->
      `Object [ "kind", `String "retained"; "page", Page.to_json page ]
  ;;

  let of_method_and_json ~limits ~method_ json =
    let* () =
      C.validate
        ~limits:
          (if String.equal method_ "developer.trace.capabilities"
           then D.Limits.default
           else Limits.response limits)
        json
    in
    match method_ with
    | "developer.trace.capabilities" ->
      Result.map (Capabilities.of_json json) ~f:(fun value -> Capabilities value)
    | "developer.trace.page" ->
      let* fields = C.fields ~allowed:[ "kind"; "page" ] json in
      let* kind = get fields "kind" (C.string ~max_bytes:16) in
      (match kind with
       | "unavailable" | "expired" ->
         let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
         Ok (Page (if String.equal kind "expired" then Expired else Unavailable))
       | "retained" ->
         let* page = get fields "page" (Page.of_json ~limits) in
         Ok (Page (Retained page))
       | _ -> C.invalid "unknown trace availability")
    | _ -> C.invalid "unknown trace response method"
  ;;

  let to_json t ~limits =
    let json = raw_json t in
    let* _ = of_method_and_json ~limits ~method_:(method_name t) json in
    Ok json
  ;;
end
