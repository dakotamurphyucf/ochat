open Core

type kind =
  | Model_call
  | Nested_agent
  | Scheduled_event
  | Async_tool
  | Shell_process
  | Compaction
[@@deriving compare, equal, sexp]

type status =
  | Queued
  | Running
  | Waiting_permission of Id.Permission.t
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

type delivery =
  | Not_required
  | Pending
  | Delivered of Timestamp.t
[@@deriving sexp]

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
  }
[@@deriving sexp]

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
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
;;

let delivery_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "not_required" -> Ok Not_required
  | "pending" -> Ok Pending
  | "delivered" ->
    Result.map
      (Json_codec.required_as fields "delivered_at" Timestamp.of_json)
      ~f:(fun x -> Delivered x)
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
    ]
    |> List.filter_opt
  in
  `Object fields
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
  let%map delivery = Json_codec.required_as fields "delivery" delivery_of_json in
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
  }
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
