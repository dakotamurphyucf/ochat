open Core
open Extension_codec
module Error = Protocol_error

type context =
  { id : Id.Subscription.t
  ; session_id : Id.Session.t
  ; generation : int
  ; invocation_id : Id.Invocation.t
  ; kind : string
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t
  ; completion_schema : Jsonaf.t option
  ; wake : Completion.wake
  ; ingress_capability : Id.Capability.t option
  }
[@@deriving sexp]

type t =
  { context : context
  ; epoch : int
  ; timer_id : Id.Schedule.t option
  ; job_id : Id.Job.t option
  ; result : Completion.t option
  ; completed_at : Timestamp.t option
  }
[@@deriving sexp]

let failure code message = Error (Error.create code ~message ~retryable:false ())

let optional_validate value f =
  match value with
  | None -> Ok ()
  | Some value -> f value
;;

let validate t =
  let open Result.Let_syntax in
  let c = t.context in
  let%bind () = validate_id Id.Subscription.to_json Id.Subscription.of_json c.id in
  let%bind () = validate_id Id.Session.to_json Id.Session.of_json c.session_id in
  let%bind () = validate_id Id.Invocation.to_json Id.Invocation.of_json c.invocation_id in
  let%bind () =
    optional_validate
      c.ingress_capability
      (validate_id Id.Capability.to_json Id.Capability.of_json)
  in
  let%bind () =
    optional_validate t.timer_id (validate_id Id.Schedule.to_json Id.Schedule.of_json)
  in
  let%bind () = optional_validate t.job_id (validate_id Id.Job.to_json Id.Job.of_json) in
  let%bind () = text ~name:"subscription kind" ~max:128 c.kind in
  let%bind () =
    optional_validate c.completion_schema (fun schema -> validate_json schema)
  in
  let%bind () = optional_validate t.result Completion.validate in
  if c.generation < 0 || t.epoch < 0
  then invalid "negative subscription generation or epoch"
  else if Timestamp.compare c.deadline c.created_at <= 0
  then invalid "subscription deadline must follow creation"
  else (
    match t.result, t.completed_at with
    | None, None -> Ok ()
    | Some result, Some at ->
      if t.epoch = 0 || Option.is_some t.timer_id || Option.is_some t.job_id
      then invalid "terminal subscription retains active handles or initial epoch"
      else if Timestamp.compare at c.created_at < 0
      then invalid "subscription completion predates creation"
      else (
        match result with
        | Completion.Expired when Timestamp.compare at c.deadline < 0 ->
          invalid "subscription expired before its deadline"
        | _ -> Ok ())
    | _ -> invalid "subscription result and completion time must appear together")
;;

let create context =
  let t =
    { context
    ; epoch = 0
    ; timer_id = None
    ; job_id = None
    ; result = None
    ; completed_at = None
    }
  in
  Result.map (validate t) ~f:(fun () -> t)
;;

let next_epoch t expected =
  if t.epoch <> expected
  then failure Conflict "stale subscription epoch"
  else if t.epoch = Int.max_value
  then failure Resource_limit "subscription epoch exhausted"
  else Ok (t.epoch + 1)
;;

let arm t ~expected_epoch ~timer_id ~job_id =
  let open Result.Let_syntax in
  let%bind () = validate t in
  if Option.is_some t.result
  then failure Already_resolved "subscription is terminal"
  else (
    let%bind epoch = next_epoch t expected_epoch in
    let next = { t with epoch; timer_id; job_id } in
    let%map () = validate next in
    next)
;;

let finish t ~expected_epoch ~now result =
  let open Result.Let_syntax in
  let%bind () = validate t in
  match t.result with
  | Some _ -> Ok (t, false)
  | None ->
    let%bind epoch = next_epoch t expected_epoch in
    let next =
      { t with
        epoch
      ; timer_id = None
      ; job_id = None
      ; result = Some result
      ; completed_at = Some now
      }
    in
    let%map () = validate next in
    next, true
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = validate next in
  match previous with
  | None ->
    if
      next.epoch = 0
      && Option.is_none next.result
      && Option.is_none next.timer_id
      && Option.is_none next.job_id
    then Ok ()
    else failure Invalid_state "subscription must start unarmed"
  | Some previous ->
    let%bind () = validate previous in
    if not (Sexp.equal (sexp_of_context previous.context) (sexp_of_context next.context))
    then failure Conflict "subscription context is immutable"
    else if Option.is_some previous.result
    then
      if Sexp.equal (sexp_of_t previous) (sexp_of_t next)
      then Ok ()
      else failure Already_resolved "terminal subscription cannot change"
    else if previous.epoch = Int.max_value || next.epoch <> previous.epoch + 1
    then failure Conflict "subscription epoch must advance exactly once"
    else Ok ()
;;

let optional name value encode =
  Option.to_list (Option.map value ~f:(fun v -> name, encode v))
;;

let to_json t =
  let c = t.context in
  `Object
    ([ "schema_version", `Number "1"
     ; "id", Id.Subscription.to_json c.id
     ; "session_id", Id.Session.to_json c.session_id
     ; "generation", `Number (Int.to_string c.generation)
     ; "invocation_id", Id.Invocation.to_json c.invocation_id
     ; "kind", `String c.kind
     ; "created_at", Timestamp.to_json c.created_at
     ; "deadline", Timestamp.to_json c.deadline
     ; "wake", Completion.wake_to_json c.wake
     ; "epoch", `Number (Int.to_string t.epoch)
     ]
     @ optional "completion_schema" c.completion_schema Fn.id
     @ optional "ingress_capability" c.ingress_capability Id.Capability.to_json
     @ optional "timer_id" t.timer_id Id.Schedule.to_json
     @ optional "job_id" t.job_id Id.Job.to_json
     @ optional "result" t.result Completion.to_json
     @ optional "completed_at" t.completed_at Timestamp.to_json)
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = validate_json ~max_bytes:(18 * 1024 * 1024) ~max_depth:136 json in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    closed
      fields
      [ "schema_version"
      ; "id"
      ; "session_id"
      ; "generation"
      ; "invocation_id"
      ; "kind"
      ; "created_at"
      ; "deadline"
      ; "wake"
      ; "epoch"
      ; "completion_schema"
      ; "ingress_capability"
      ; "timer_id"
      ; "job_id"
      ; "result"
      ; "completed_at"
      ]
  in
  let integer = Json_codec.bounded_int ~min:0 ~max:Int.max_value in
  let%bind version = Json_codec.required_as fields "schema_version" integer in
  let%bind () =
    if version = 1
    then Ok ()
    else failure Incompatible_protocol "unsupported subscription version"
  in
  let%bind id = Json_codec.required_as fields "id" Id.Subscription.of_json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind generation = Json_codec.required_as fields "generation" integer in
  let%bind invocation_id =
    Json_codec.required_as fields "invocation_id" Id.Invocation.of_json
  in
  let%bind kind = Json_codec.required_as fields "kind" Json_codec.string in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%bind deadline = Json_codec.required_as fields "deadline" Timestamp.of_json in
  let%bind wake = Json_codec.required_as fields "wake" Completion.wake_of_json in
  let%bind epoch = Json_codec.required_as fields "epoch" integer in
  let completion_schema = Json_codec.optional fields "completion_schema" in
  let%bind ingress_capability =
    Json_codec.optional_as fields "ingress_capability" Id.Capability.of_json
  in
  let%bind timer_id = Json_codec.optional_as fields "timer_id" Id.Schedule.of_json in
  let%bind job_id = Json_codec.optional_as fields "job_id" Id.Job.of_json in
  let%bind result = Json_codec.optional_as fields "result" Completion.of_json in
  let%bind completed_at =
    Json_codec.optional_as fields "completed_at" Timestamp.of_json
  in
  let context =
    { id
    ; session_id
    ; generation
    ; invocation_id
    ; kind
    ; created_at
    ; deadline
    ; wake
    ; completion_schema
    ; ingress_capability
    }
  in
  let t = { context; epoch; timer_id; job_id; result; completed_at } in
  let%map () = validate t in
  t
;;
