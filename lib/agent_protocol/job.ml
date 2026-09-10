open Core

module Jsonaf = struct
  include Jsonaf

  let equal = exactly_equal
end

type kind =
  | Model_call
  | Nested_agent
  | Scheduled_event
  | Async_tool
  | Shell_process
  | Compaction
[@@deriving compare, equal, sexp]

type dependency =
  { invocation_id : Id.Invocation.t
  ; work : Invocation.work
  ; deadline : Timestamp.t
  ; completion_schema : Jsonaf.t option [@sexp.option]
  ; max_output_bytes : int
  ; max_output_depth : int
  }
[@@deriving equal, sexp]

let dependency_of_sexp_generated = dependency_of_sexp
let sexp_of_dependency_generated = sexp_of_dependency

let dependency_of_sexp sexp =
  let sexp =
    match sexp with
    | Sexp.List fields ->
      Sexp.List
        (List.map fields ~f:(function
           | Sexp.List [ Atom "job_id"; id ] ->
             Sexp.List [ Atom "work"; List [ Atom "Job"; id ] ]
           | field -> field))
    | other -> other
  in
  dependency_of_sexp_generated sexp
;;

let sexp_of_dependency dependency =
  match sexp_of_dependency_generated dependency with
  | Sexp.List fields ->
    Sexp.List
      (List.map fields ~f:(function
         | Sexp.List [ Atom "work"; List [ Atom "Job"; id ] ] ->
           Sexp.List [ Atom "job_id"; id ]
         | field -> field))
  | other -> other
;;

type status =
  | Queued
  | Running
  | Waiting_permission of Id.Permission.t
  | Waiting_completion of dependency
  | Succeeded
  | Failed of Protocol_error.t
  | Cancelled
  | Interrupted of string
[@@deriving sexp]

type retry_policy =
  | Never
  | Safe_retry of
      { max_attempts : int
      ; backoff_ms : int
      }
  | Idempotent of
      { key : Idempotency_key.t
      ; max_attempts : int
      ; backoff_ms : int
      }
[@@deriving sexp]

type discard_reason = Authority_changed [@@deriving equal, sexp]

type delivery =
  | Not_required
  | Pending
  | Delivered of Timestamp.t
  | Discarded of
      { at : Timestamp.t
      ; reason : discard_reason
      }
[@@deriving equal, sexp]

type launch_owner =
  | Invocation of Id.Invocation.t
  | Moderator_event of Id.Moderator_execution.t
[@@deriving equal, sexp]

type launch =
  { owner : launch_owner
  ; parent_job : (Id.Job.t * int) option [@sexp.option]
  ; nested_depth : int
  ; moderator_source : Invocation.observer option [@sexp.option]
  }
[@@deriving equal, sexp]

type t =
  { id : Id.Job.t
  ; session_id : Id.Session.t
  ; generation : int
  ; kind : kind
  ; payload : Jsonaf.t
  ; status : status
  ; retry_policy : retry_policy
  ; attempt : int
  ; created_at : Timestamp.t
  ; started_at : Timestamp.t option
  ; next_run_at : Timestamp.t option
  ; completed_at : Timestamp.t option
  ; result : Jsonaf.t option
  ; delivery : delivery
  ; launch : launch option [@sexp.option]
  ; progress : Job_progress.t option [@sexp.option]
  }
[@@deriving sexp]

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let launch_to_json launch =
  let kind, id =
    match launch.owner with
    | Invocation id -> "invocation", Id.Invocation.to_json id
    | Moderator_event id -> "moderator_event", Id.Moderator_execution.to_json id
  in
  `Object
    ([ ( "schema_version"
       , `Number
           (match launch.moderator_source with
            | None -> "1"
            | Some _ -> "2") )
     ; "owner_type", `String kind
     ; "owner_id", id
     ; "nested_depth", `Number (Int.to_string launch.nested_depth)
     ]
     @ Option.to_list
         (Option.map launch.moderator_source ~f:(fun source ->
            ( "moderator_source"
            , `Object
                [ "script_id", `String source.Invocation.script_id
                ; "source_sha256", `String source.source_sha256
                ] )))
     @ Option.to_list
         (Option.map launch.parent_job ~f:(fun (id, attempt) ->
            ( "parent_job"
            , `Object
                [ "id", Id.Job.to_json id; "attempt", `Number (Int.to_string attempt) ] )))
    )
;;

let launch_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "schema_version"
      ; "owner_type"
      ; "owner_id"
      ; "parent_job"
      ; "nested_depth"
      ; "moderator_source"
      ]
  in
  let%bind version =
    Json_codec.required_as
      fields
      "schema_version"
      (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%bind () =
    match version with
    | 1 | 2 -> Ok ()
    | _ ->
      Error
        (Protocol_error.create
           Incompatible_protocol
           ~message:"unsupported job launch schema"
           ~retryable:false
           ())
  in
  let%bind kind = Json_codec.required_as fields "owner_type" Json_codec.string in
  let%bind moderator_source =
    Json_codec.optional_as fields "moderator_source" (fun json ->
      let%bind fields = Json_codec.fields json in
      let%bind () = Extension_codec.closed fields [ "script_id"; "source_sha256" ] in
      let%bind script_id = Json_codec.required_as fields "script_id" Json_codec.string in
      let%bind source_sha256 =
        Json_codec.required_as fields "source_sha256" Json_codec.string
      in
      match
        (not (String.is_empty script_id))
        && String.length source_sha256 = 64
        && String.for_all source_sha256 ~f:(function
          | '0' .. '9' | 'a' .. 'f' -> true
          | _ -> false)
      with
      | true -> Ok Invocation.{ script_id; source_sha256 }
      | false -> Error (Protocol_error.invalid_request "invalid job moderator source"))
  in
  let%bind () =
    match version, moderator_source with
    | 1, None | 2, Some _ -> Ok ()
    | _ -> Error (Protocol_error.invalid_request "job launch source requires schema 2")
  in
  let%bind owner =
    match kind with
    | "invocation" ->
      Json_codec.required_as fields "owner_id" Id.Invocation.of_json
      |> Result.map ~f:(fun id -> Invocation id)
    | "moderator_event" ->
      Json_codec.required_as fields "owner_id" Id.Moderator_execution.of_json
      |> Result.map ~f:(fun id -> Moderator_event id)
    | _ -> Error (Protocol_error.invalid_request "unknown job launch owner")
  in
  let%bind parent_job =
    Json_codec.optional_as fields "parent_job" (fun json ->
      let%bind fields = Json_codec.fields json in
      let%bind () = Extension_codec.closed fields [ "id"; "attempt" ] in
      let%bind id = Json_codec.required_as fields "id" Id.Job.of_json in
      let%map attempt =
        Json_codec.required_as
          fields
          "attempt"
          (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
      in
      id, attempt)
  in
  let%bind nested_depth =
    Json_codec.required_as
      fields
      "nested_depth"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  match parent_job, nested_depth with
  | None, 0 -> Ok { owner; parent_job; nested_depth; moderator_source }
  | Some _, depth when depth > 0 ->
    Ok { owner; parent_job; nested_depth; moderator_source }
  | _ ->
    Error
      (Protocol_error.invalid_request "job launch depth differs from parent ownership")
;;

let validate_stored_completion t stored =
  let open Result.Let_syntax in
  let%bind () =
    match t.status, Stored_completion.outcome stored with
    | Succeeded, Succeeded
    | Failed _, (Failed | Expired)
    | Cancelled, Cancelled
    | Interrupted _, Failed -> Ok ()
    | _ ->
      Error
        (Protocol_error.invalid_request "async tool completion differs from job status")
  in
  match stored with
  | Inline completion -> Completion.validate completion
  | Artifact { reference; _ } ->
    let%bind _ = Job_artifact.of_json (Job_artifact.to_json reference) in
    (match
       Id.Session.equal reference.session_id t.session_id
       && Id.Job.equal reference.job_id t.id
       && Int.equal reference.generation t.generation
       && Int.equal reference.attempt t.attempt
       && Option.is_some t.completed_at
     with
     | true -> Ok ()
     | false ->
       Error
         (Protocol_error.invalid_request "job artifact differs from its terminal owner"))
;;

let validate_delivery t =
  match t.delivery, t.status, t.completed_at with
  | ( Discarded { at; _ }
    , (Succeeded | Failed _ | Cancelled | Interrupted _)
    , Some completed )
    when Timestamp.compare at completed >= 0 -> Ok ()
  | Discarded _, _, _ ->
    Error (Protocol_error.invalid_request "discarded delivery requires a terminal job")
  | (Not_required | Pending | Delivered _), _, _ -> Ok ()
;;

let validate_result t =
  let open Result.Let_syntax in
  let%bind () = validate_delivery t in
  match t.kind, t.result with
  | Async_tool, Some (`Object fields as encoded)
    when Option.exists (List.Assoc.find fields "type" ~equal:String.equal) ~f:(function
           | `String "artifact" -> true
           | _ -> false) ->
    Result.bind (Stored_completion.of_json encoded) ~f:(validate_stored_completion t)
  | _ -> Ok ()
;;

let terminal_result t =
  let open Result.Let_syntax in
  let%bind () = validate_result t in
  match t.status with
  | Queued | Running | Waiting_permission _ | Waiting_completion _ -> Ok None
  | Succeeded | Failed _ | Cancelled | Interrupted _ ->
    let%bind completion =
      match t.kind with
      | Async_tool ->
        let%bind encoded =
          Result.of_option
            t.result
            ~error:
              (Protocol_error.invalid_request "terminal async tool job has no completion")
        in
        let%bind stored = Stored_completion.of_json encoded in
        let%map () = validate_stored_completion t stored in
        stored
      | Model_call | Nested_agent | Scheduled_event | Shell_process | Compaction ->
        let%bind completion =
          match t.status with
          | Succeeded -> Ok (Completion.Succeeded (Option.value t.result ~default:`Null))
          | Failed error ->
            Ok
              (Completion.Failed
                 { code = Protocol_error.code_to_string error.code
                 ; message = error.message
                 ; retryable = error.retryable
                 ; details = error.data
                 })
          | Cancelled -> Ok (Completion.Cancelled "job cancelled")
          | Interrupted reason ->
            Ok
              (Completion.Failed
                 { code = "interrupted"
                 ; message = reason
                 ; retryable = false
                 ; details = `Null
                 })
          | Queued | Running | Waiting_permission _ | Waiting_completion _ -> assert false
        in
        let%map () = Completion.validate completion in
        Stored_completion.Inline completion
    in
    Ok (Some completion)
;;

let unavailable_artifact _ =
  Error
    (Protocol_error.create
       Blob_unavailable
       ~message:"job completion requires an authorized artifact loader"
       ~retryable:false
       ())
;;

let terminal_completion ?(load_artifact = unavailable_artifact) t =
  let open Result.Let_syntax in
  let%bind stored = terminal_result t in
  match stored with
  | None -> Ok None
  | Some stored ->
    Stored_completion.materialize ~load:load_artifact stored |> Result.map ~f:Option.some
;;

let kind_values =
  [ "model_call", Model_call
  ; "nested_agent", Nested_agent
  ; "scheduled_event", Scheduled_event
  ; "async_tool", Async_tool
  ; "shell_process", Shell_process
  ; "compaction", Compaction
  ]
;;

let kind_to_string kind =
  List.Assoc.find_exn
    (List.map kind_values ~f:(fun (name, kind) -> kind, name))
    kind
    ~equal:equal_kind
;;

let kind_of_json = Json_codec.enum ~name:"job kind" kind_values

let status_name = function
  | Queued -> "queued"
  | Running -> "running"
  | Waiting_permission _ -> "waiting_permission"
  | Waiting_completion _ -> "waiting_completion"
  | Succeeded -> "succeeded"
  | Failed _ -> "failed"
  | Cancelled -> "cancelled"
  | Interrupted _ -> "interrupted"
;;

let status_to_json status =
  match status with
  | Queued | Running | Succeeded | Cancelled ->
    `Object [ "type", `String (status_name status) ]
  | Waiting_permission id ->
    `Object
      [ "type", `String "waiting_permission"; "permission_id", Id.Permission.to_json id ]
  | Waiting_completion dependency ->
    let version, target =
      match dependency.work with
      | Invocation.Job id -> "1", ("job_id", Id.Job.to_json id)
      | Subscription _ -> "2", ("work", Invocation.work_to_json dependency.work)
    in
    `Object
      [ "type", `String "waiting_completion"
      ; "schema_version", `Number version
      ; "invocation_id", Id.Invocation.to_json dependency.invocation_id
      ; target
      ; "deadline", Timestamp.to_json dependency.deadline
      ; "completion_schema", Option.value dependency.completion_schema ~default:`Null
      ; "max_output_bytes", `Number (Int.to_string dependency.max_output_bytes)
      ; "max_output_depth", `Number (Int.to_string dependency.max_output_depth)
      ]
  | Failed error ->
    `Object [ "type", `String "failed"; "error", Protocol_error.to_json error ]
  | Interrupted reason ->
    `Object [ "type", `String "interrupted"; "reason", `String reason ]
;;

let status_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "queued" -> Ok Queued
  | "running" -> Ok Running
  | "waiting_permission" ->
    Result.map
      (Json_codec.required_as fields "permission_id" Id.Permission.of_json)
      ~f:(fun id -> Waiting_permission id)
  | "succeeded" -> Ok Succeeded
  | "waiting_completion" ->
    let%bind version =
      Json_codec.required_as
        fields
        "schema_version"
        (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
    in
    let%bind () =
      match version with
      | 1 | 2 -> Ok ()
      | _ ->
        Error
          (Protocol_error.create
             Incompatible_protocol
             ~message:"unsupported job dependency schema"
             ~retryable:false
             ())
    in
    let%bind () =
      Extension_codec.closed
        fields
        ([ "type"
         ; "schema_version"
         ; "invocation_id"
         ; "deadline"
         ; "completion_schema"
         ; "max_output_bytes"
         ; "max_output_depth"
         ]
         @
         match version with
         | 1 -> [ "job_id" ]
         | _ -> [ "work" ])
    in
    let%bind invocation_id =
      Json_codec.required_as fields "invocation_id" Id.Invocation.of_json
    in
    let%bind work =
      match version with
      | 1 ->
        Result.map (Json_codec.required_as fields "job_id" Id.Job.of_json) ~f:(fun id ->
          Invocation.Job id)
      | _ -> Json_codec.required_as fields "work" Invocation.work_of_json
    in
    let%bind deadline = Json_codec.required_as fields "deadline" Timestamp.of_json in
    let%bind completion_schema =
      Json_codec.optional_as fields "completion_schema" (fun json -> Ok json)
    in
    let%bind max_output_bytes =
      Json_codec.required_as
        fields
        "max_output_bytes"
        (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
    in
    let%map max_output_depth =
      Json_codec.required_as
        fields
        "max_output_depth"
        (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
    in
    Waiting_completion
      { invocation_id
      ; work
      ; deadline
      ; completion_schema
      ; max_output_bytes
      ; max_output_depth
      }
  | "failed" ->
    Result.map (Json_codec.required_as fields "error" Protocol_error.of_json) ~f:(fun e ->
      Failed e)
  | "cancelled" -> Ok Cancelled
  | "interrupted" ->
    Result.map (Json_codec.required_as fields "reason" Json_codec.string) ~f:(fun x ->
      Interrupted x)
  | _ -> Error (Protocol_error.invalid_request "unknown job status")
;;

let retry_fields ~max_attempts ~backoff_ms =
  [ "max_attempts", `Number (Int.to_string max_attempts)
  ; "backoff_ms", `Number (Int.to_string backoff_ms)
  ]
;;

let retry_to_json = function
  | Never -> `Object [ "type", `String "never" ]
  | Safe_retry { max_attempts; backoff_ms } ->
    `Object (("type", `String "safe_retry") :: retry_fields ~max_attempts ~backoff_ms)
  | Idempotent { key; max_attempts; backoff_ms } ->
    `Object
      (("type", `String "idempotent")
       :: ("key", Idempotency_key.to_json key)
       :: retry_fields ~max_attempts ~backoff_ms)
;;

let decode_retry_limits fields =
  let open Result.Let_syntax in
  let%bind max_attempts =
    Json_codec.required_as
      fields
      "max_attempts"
      (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%map backoff_ms =
    Json_codec.required_as
      fields
      "backoff_ms"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  max_attempts, backoff_ms
;;

let retry_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "never" -> Ok Never
  | "safe_retry" ->
    Result.map (decode_retry_limits fields) ~f:(fun (max_attempts, backoff_ms) ->
      Safe_retry { max_attempts; backoff_ms })
  | "idempotent" ->
    let%bind key = Json_codec.required_as fields "key" Idempotency_key.of_json in
    let%map max_attempts, backoff_ms = decode_retry_limits fields in
    Idempotent { key; max_attempts; backoff_ms }
  | _ -> Error (Protocol_error.invalid_request "unknown job retry policy")
;;

let delivery_to_json = function
  | Not_required -> `Object [ "type", `String "not_required" ]
  | Pending -> `Object [ "type", `String "pending" ]
  | Delivered at ->
    `Object [ "type", `String "delivered"; "delivered_at", Timestamp.to_json at ]
  | Discarded { at; reason = Authority_changed } ->
    `Object
      [ "type", `String "discarded"
      ; "schema_version", `Number "1"
      ; "discarded_at", Timestamp.to_json at
      ; "reason", `String "authority_changed"
      ]
;;

let delivery_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "not_required" ->
    let%map () = Extension_codec.closed fields [ "type" ] in
    Not_required
  | "pending" ->
    let%map () = Extension_codec.closed fields [ "type" ] in
    Pending
  | "delivered" ->
    let%bind () = Extension_codec.closed fields [ "type"; "delivered_at" ] in
    Result.map
      (Json_codec.required_as fields "delivered_at" Timestamp.of_json)
      ~f:(fun x -> Delivered x)
  | "discarded" ->
    let%bind () =
      Extension_codec.closed fields [ "type"; "schema_version"; "discarded_at"; "reason" ]
    in
    let%bind _ =
      Json_codec.required_as
        fields
        "schema_version"
        (Json_codec.bounded_int ~min:1 ~max:1)
    in
    let%bind at = Json_codec.required_as fields "discarded_at" Timestamp.of_json in
    let%map reason =
      Json_codec.required_as
        fields
        "reason"
        (Json_codec.enum
           ~name:"job delivery discard reason"
           [ "authority_changed", Authority_changed ])
    in
    Discarded { at; reason }
  | _ -> Error (Protocol_error.invalid_request "unknown job delivery state")
;;

let to_json t =
  let fields =
    [ Some ("id", Id.Job.to_json t.id)
    ; Some ("session_id", Id.Session.to_json t.session_id)
    ; Some ("generation", `Number (Int.to_string t.generation))
    ; Some ("kind", `String (kind_to_string t.kind))
    ; Some ("payload", t.payload)
    ; Some ("status", status_to_json t.status)
    ; Some ("retry_policy", retry_to_json t.retry_policy)
    ; Some ("attempt", `Number (Int.to_string t.attempt))
    ; Some ("created_at", Timestamp.to_json t.created_at)
    ; optional_field "started_at" t.started_at Timestamp.to_json
    ; optional_field "next_run_at" t.next_run_at Timestamp.to_json
    ; optional_field "completed_at" t.completed_at Timestamp.to_json
    ; optional_field "result" t.result Fn.id
    ; Some ("delivery", delivery_to_json t.delivery)
    ; optional_field "launch" t.launch launch_to_json
    ; optional_field "progress" t.progress Job_progress.to_json
    ]
    |> List.filter_opt
  in
  `Object fields
;;

let validate_delivery_transition ~previous t =
  let open Result.Let_syntax in
  let%bind () = validate_delivery t in
  match previous, t.delivery with
  | Some ({ delivery = Discarded _; _ } as previous), _ ->
    (match Jsonaf.exactly_equal (to_json previous) (to_json t) with
     | true -> Ok ()
     | false -> Error (Protocol_error.invalid_request "discarded job is immutable"))
  | Some previous, Discarded _ ->
    let%bind () = validate_delivery { previous with delivery = t.delivery } in
    (match previous.delivery with
     | Pending
       when Jsonaf.exactly_equal
              (to_json { previous with delivery = t.delivery })
              (to_json t) -> Ok ()
     | _ ->
       Error
         (Protocol_error.invalid_request
            "only an unchanged pending terminal job can discard delivery"))
  | None, Discarded _ ->
    Error (Protocol_error.invalid_request "discarded delivery requires an existing job")
  | _, (Not_required | Pending | Delivered _) -> Ok ()
;;

let decode_identity fields =
  let open Result.Let_syntax in
  let%bind id = Json_codec.required_as fields "id" Id.Job.of_json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%map kind = Json_codec.required_as fields "kind" kind_of_json in
  id, session_id, generation, kind
;;

let decode_execution fields =
  let open Result.Let_syntax in
  let%bind status = Json_codec.required_as fields "status" status_of_json in
  let%bind retry_policy = Json_codec.required_as fields "retry_policy" retry_of_json in
  let%map attempt =
    Json_codec.required_as
      fields
      "attempt"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  status, retry_policy, attempt
;;

let decode_times fields =
  let open Result.Let_syntax in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%bind started_at = Json_codec.optional_as fields "started_at" Timestamp.of_json in
  let%bind next_run_at = Json_codec.optional_as fields "next_run_at" Timestamp.of_json in
  let%map completed_at = Json_codec.optional_as fields "completed_at" Timestamp.of_json in
  created_at, started_at, next_run_at, completed_at
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id, session_id, generation, kind = decode_identity fields in
  let%bind payload = Json_codec.required fields "payload" in
  let%bind status, retry_policy, attempt = decode_execution fields in
  let%bind created_at, started_at, next_run_at, completed_at = decode_times fields in
  let result = Json_codec.optional fields "result" in
  let%bind delivery = Json_codec.required_as fields "delivery" delivery_of_json in
  let%bind launch = Json_codec.optional_as fields "launch" launch_of_json in
  let%bind progress = Json_codec.optional_as fields "progress" Job_progress.of_json in
  let%bind () =
    match status with
    | Waiting_completion dependency ->
      (match kind, started_at, next_run_at, completed_at, result with
       | Async_tool, Some _, None, None, None
         when attempt > 0
              && (match dependency.work with
                  | Invocation.Job target -> not (Id.Job.equal id target)
                  | Subscription _ -> true)
              && Timestamp.compare dependency.deadline created_at >= 0 -> Ok ()
       | _ -> Error (Protocol_error.invalid_request "invalid waiting job lifecycle"))
    | _ -> Ok ()
  in
  let t =
    { id
    ; session_id
    ; generation
    ; kind
    ; payload
    ; status
    ; retry_policy
    ; attempt
    ; created_at
    ; started_at
    ; next_run_at
    ; completed_at
    ; result
    ; delivery
    ; launch
    ; progress
    }
  in
  let%map () = validate_result t in
  t
;;

module List_request = struct
  type nonrec t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; status : string option
    ; kind : kind option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ optional_field "status" t.status (fun x -> `String x)
      ; optional_field "kind" t.kind (fun x -> `String (kind_to_string x))
      ]
      |> List.filter_opt
    in
    `Object
      ((("session_id", Id.Session.to_json t.session_id) :: Page.Request.to_fields t.page)
       @ fields)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind page = Page.Request.of_fields fields in
    let%bind status = Json_codec.optional_as fields "status" Json_codec.string in
    let%map kind = Json_codec.optional_as fields "kind" kind_of_json in
    { session_id; page; status; kind }
  ;;
end

module Get_request = struct
  type t =
    { session_id : Id.Session.t
    ; job_id : Id.Job.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id; "job_id", Id.Job.to_json t.job_id ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%map job_id = Json_codec.required_as fields "job_id" Id.Job.of_json in
    { session_id; job_id }
  ;;
end

module Cancel_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; job_id : Id.Job.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "job_id", Id.Job.to_json t.job_id
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind attachment_id =
      Json_codec.required_as fields "attachment_id" Id.Attachment.of_json
    in
    let%bind job_id = Json_codec.required_as fields "job_id" Id.Job.of_json in
    let%map idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    { session_id; attachment_id; job_id; idempotency_key }
  ;;
end

module Cancel_result = struct
  type nonrec t =
    { job : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  let to_json t = `Object (("job", to_json t.job) :: Mutation_result.to_fields t.mutation)

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind job = Json_codec.required_as fields "job" of_json in
    let%map mutation = Mutation_result.of_fields fields in
    { job; mutation }
  ;;
end
