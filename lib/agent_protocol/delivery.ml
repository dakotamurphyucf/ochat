open Core
open Extension_codec
module Error = Protocol_error

type source =
  | Moderator
  | Job_adapter
  | External_ingress
[@@deriving compare, equal, sexp]

type context =
  { id : Id.Delivery.t
  ; session_id : Id.Session.t
  ; generation : int
  ; invocation_id : Id.Invocation.t option
  ; work : Invocation.work option
  ; correlation : string
  ; source : source
  ; completion : Completion.t
  ; wake : Completion.wake
  ; created_at : Timestamp.t
  }
[@@deriving sexp]

type status =
  | Pending
  | Committed of
      { history_id : History_entry.Id.t
      ; at : Timestamp.t
      }
  | Failed of Invocation.tool_error
[@@deriving sexp]

type t =
  { context : context
  ; attempt : int
  ; status : status
  }
[@@deriving sexp]

let failure code message = Error (Error.create code ~message ~retryable:false ())

let optional_validate value f =
  match value with
  | None -> Ok ()
  | Some value -> f value
;;

let history_id_of_json json =
  let open Result.Let_syntax in
  let%bind text = Json_codec.string json in
  History_entry.Id.of_string text |> Result.map_error ~f:Error.invalid_request
;;

let history_id_to_json id = `String (History_entry.Id.to_string id)

let validate t =
  let open Result.Let_syntax in
  let c = t.context in
  let%bind () = validate_id Id.Delivery.to_json Id.Delivery.of_json c.id in
  let%bind () = validate_id Id.Session.to_json Id.Session.of_json c.session_id in
  let%bind () =
    optional_validate
      c.invocation_id
      (validate_id Id.Invocation.to_json Id.Invocation.of_json)
  in
  let%bind () =
    optional_validate c.work (fun work ->
      Invocation.validate_outcome (Pending (work, `Null)))
  in
  let%bind () = text ~name:"delivery correlation" ~max:256 c.correlation in
  let%bind () = Completion.validate c.completion in
  if c.generation < 0 || t.attempt < 1 || t.attempt > 64
  then invalid "invalid delivery generation or attempt"
  else (
    match t.status with
    | Pending -> Ok ()
    | Failed error -> Invocation.validate_outcome (Fail error)
    | Committed { history_id; at } ->
      let%bind () = validate_id history_id_to_json history_id_of_json history_id in
      if Timestamp.compare at c.created_at < 0
      then invalid "delivery commit predates creation"
      else Ok ())
;;

let create context =
  let t = { context; attempt = 1; status = Pending } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let commit t ~history_id ~now =
  let open Result.Let_syntax in
  let%bind () = validate t in
  match t.status with
  | Committed old ->
    if History_entry.Id.equal old.history_id history_id
    then Ok t
    else failure Conflict "delivery is already committed to another history entry"
  | Failed _ -> failure Invalid_state "failed delivery must be explicitly retried"
  | Pending ->
    let next = { t with status = Committed { history_id; at = now } } in
    let%map () = validate next in
    next
;;

let fail t error =
  let open Result.Let_syntax in
  let%bind () = validate t in
  match t.status with
  | Committed _ -> failure Already_resolved "committed delivery cannot fail"
  | Failed _ -> Ok t
  | Pending ->
    let next = { t with status = Failed error } in
    let%map () = validate next in
    next
;;

let retry t ~max_attempts =
  let open Result.Let_syntax in
  let%bind () = validate t in
  if max_attempts < 1 || max_attempts > 64
  then invalid "delivery retry bound must be between 1 and 64"
  else (
    match t.status with
    | Failed _ when t.attempt < max_attempts ->
      Ok { t with attempt = t.attempt + 1; status = Pending }
    | Failed _ -> failure Resource_limit "delivery retry limit reached"
    | Pending | Committed _ -> failure Invalid_state "only failed delivery can retry")
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = validate next in
  match previous with
  | None ->
    (match next.status with
     | Pending when next.attempt = 1 -> Ok ()
     | _ -> failure Invalid_state "new delivery must be pending on its first attempt")
  | Some previous ->
    let%bind () = validate previous in
    if not (Sexp.equal (sexp_of_context previous.context) (sexp_of_context next.context))
    then failure Conflict "delivery context is immutable"
    else if Sexp.equal (sexp_of_t previous) (sexp_of_t next)
    then Ok ()
    else (
      match previous.status, next.status with
      | Pending, (Failed _ | Committed _) when next.attempt = previous.attempt -> Ok ()
      | Failed _, Pending when next.attempt = previous.attempt + 1 -> Ok ()
      | _ -> failure Invalid_state "invalid delivery transition")
;;

let source_values =
  [ "moderator", Moderator
  ; "job_adapter", Job_adapter
  ; "external_ingress", External_ingress
  ]
;;

let source_to_json source =
  `String
    (fst (List.find_exn source_values ~f:(fun (_, value) -> equal_source value source)))
;;

let status_to_json = function
  | Pending -> `Object [ "type", `String "pending" ]
  | Committed { history_id; at } ->
    `Object
      [ "type", `String "committed"
      ; "history_id", history_id_to_json history_id
      ; "at", Timestamp.to_json at
      ]
  | Failed error ->
    `Object [ "type", `String "failed"; "error", Invocation.outcome_to_json (Fail error) ]
;;

let status_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  match kind with
  | "pending" ->
    let%map () = closed fields [ "type" ] in
    Pending
  | "committed" ->
    let%bind () = closed fields [ "type"; "history_id"; "at" ] in
    let%bind history_id = Json_codec.required_as fields "history_id" history_id_of_json in
    let%map at = Json_codec.required_as fields "at" Timestamp.of_json in
    Committed { history_id; at }
  | "failed" ->
    let%bind () = closed fields [ "type"; "error" ] in
    let%bind error = Json_codec.required_as fields "error" Invocation.outcome_of_json in
    (match error with
     | Fail error -> Ok (Failed error)
     | _ -> invalid "delivery failure requires an error")
  | _ -> invalid "unknown delivery status"
;;

let optional name value encode =
  Option.to_list (Option.map value ~f:(fun v -> name, encode v))
;;

let to_json t =
  let c = t.context in
  `Object
    ([ "schema_version", `Number "1"
     ; "id", Id.Delivery.to_json c.id
     ; "session_id", Id.Session.to_json c.session_id
     ; "generation", `Number (Int.to_string c.generation)
     ; "correlation", `String c.correlation
     ; "source", source_to_json c.source
     ; "completion", Completion.to_json c.completion
     ; "wake", Completion.wake_to_json c.wake
     ; "created_at", Timestamp.to_json c.created_at
     ; "attempt", `Number (Int.to_string t.attempt)
     ; "status", status_to_json t.status
     ]
     @ optional "invocation_id" c.invocation_id Id.Invocation.to_json
     @ optional "work" c.work Invocation.work_to_json)
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
      ; "correlation"
      ; "source"
      ; "completion"
      ; "wake"
      ; "created_at"
      ; "attempt"
      ; "status"
      ; "invocation_id"
      ; "work"
      ]
  in
  let integer = Json_codec.bounded_int ~min:0 ~max:Int.max_value in
  let%bind version = Json_codec.required_as fields "schema_version" integer in
  let%bind () =
    if version = 1
    then Ok ()
    else failure Incompatible_protocol "unsupported delivery version"
  in
  let%bind id = Json_codec.required_as fields "id" Id.Delivery.of_json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind generation = Json_codec.required_as fields "generation" integer in
  let%bind invocation_id =
    Json_codec.optional_as fields "invocation_id" Id.Invocation.of_json
  in
  let%bind work = Json_codec.optional_as fields "work" Invocation.work_of_json in
  let%bind correlation = Json_codec.required_as fields "correlation" Json_codec.string in
  let%bind source =
    Json_codec.required_as
      fields
      "source"
      (Json_codec.enum ~name:"delivery source" source_values)
  in
  let%bind completion = Json_codec.required_as fields "completion" Completion.of_json in
  let%bind wake = Json_codec.required_as fields "wake" Completion.wake_of_json in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%bind attempt = Json_codec.required_as fields "attempt" integer in
  let%bind status = Json_codec.required_as fields "status" status_of_json in
  let context =
    { id
    ; session_id
    ; generation
    ; invocation_id
    ; work
    ; correlation
    ; source
    ; completion
    ; wake
    ; created_at
    }
  in
  let t = { context; attempt; status } in
  let%map () = validate t in
  t
;;
