open Core
open Extension_codec
module Error = Protocol_error

type source =
  | Moderator
  | Job_adapter
  | External_ingress
[@@deriving compare, equal, sexp]

module Subscription_binding = Delivery_subscription_binding

type ownership =
  { source : Invocation.observer
  ; creator : Job.launch_owner
  ; subscription_binding : Subscription_binding.t option [@sexp.option]
  }
[@@deriving equal, sexp]

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
  ; ownership : ownership option [@sexp.option]
  }
[@@deriving equal, sexp]

type status =
  | Pending
  | Committed of
      { history_id : History_entry.Id.t
      ; at : Timestamp.t
      }
  | Failed of Invocation.tool_error
[@@deriving equal, sexp]

type wake_disposition =
  | Pending_wake
  | Accepted_wake of Id.Operation.t
  | Discarded_wake of string
[@@deriving equal, sexp]

type t =
  { context : context
  ; attempt : int
  ; status : status
  ; wake_disposition : wake_disposition option [@sexp.option]
  ; disclosure_pins : (string * string) list option [@sexp.option]
  ; completion_projection : Completion_projection.t option [@sexp.option]
  }
[@@deriving equal, sexp]

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

let validate_ownership (ownership : ownership) =
  let open Result.Let_syntax in
  let%bind () =
    text ~name:"delivery script identity" ~max:256 ownership.source.script_id
  in
  let%bind () =
    if
      String.length ownership.source.source_sha256 = 64
      && String.for_all ownership.source.source_sha256 ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    then Ok ()
    else invalid "delivery source digest must be lowercase SHA256"
  in
  let%bind () =
    match ownership.creator with
    | Job.Invocation id ->
      Id.Invocation.of_string (Id.Invocation.to_string id) |> Result.map ~f:ignore
    | Moderator_event id ->
      Id.Moderator_execution.of_string (Id.Moderator_execution.to_string id)
      |> Result.map ~f:ignore
  in
  optional_validate ownership.subscription_binding Subscription_binding.validate
;;

let ownership_to_json_with_binding (ownership : ownership) binding_fields =
  let kind, id =
    match ownership.creator with
    | Job.Invocation id -> "invocation", Id.Invocation.to_json id
    | Moderator_event id -> "moderator_event", Id.Moderator_execution.to_json id
  in
  `Object
    ([ "script_id", `String ownership.source.script_id
     ; "source_sha256", `String ownership.source.source_sha256
     ; "creator_type", `String kind
     ; "creator_id", id
     ]
     @ binding_fields)
;;

let ownership_to_json ownership =
  ownership_to_json_with_binding
    ownership
    (Option.to_list
       (Option.map ownership.subscription_binding ~f:(fun binding ->
          "subscription_binding", Subscription_binding.to_json binding)))
;;

type binding_field =
  | Missing
  | Nullable

let ownership_to_storage_json ownership ~binding_field =
  let fields =
    match ownership.subscription_binding, binding_field with
    | None, Missing -> []
    | None, Nullable -> [ "subscription_binding", `Null ]
    | Some binding, (Missing | Nullable) ->
      [ "subscription_binding", Subscription_binding.to_json binding ]
  in
  ownership_to_json_with_binding ownership fields
;;

let ownership_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    closed
      fields
      [ "script_id"
      ; "source_sha256"
      ; "creator_type"
      ; "creator_id"
      ; "subscription_binding"
      ]
  in
  let%bind script_id = Json_codec.required_as fields "script_id" Json_codec.string in
  let%bind source_sha256 =
    Json_codec.required_as fields "source_sha256" Json_codec.string
  in
  let%bind () = text ~name:"delivery script identity" ~max:256 script_id in
  let%bind () =
    match
      String.length source_sha256 = 64
      && String.for_all source_sha256 ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    with
    | true -> Ok ()
    | false -> invalid "delivery source digest must be lowercase SHA256"
  in
  let%bind kind = Json_codec.required_as fields "creator_type" Json_codec.string in
  let%bind creator =
    match kind with
    | "invocation" ->
      Json_codec.required_as fields "creator_id" Id.Invocation.of_json
      |> Result.map ~f:(fun id -> Job.Invocation id)
    | "moderator_event" ->
      Json_codec.required_as fields "creator_id" Id.Moderator_execution.of_json
      |> Result.map ~f:(fun id -> Job.Moderator_event id)
    | _ -> invalid "unknown delivery creator"
  in
  let%map subscription_binding =
    Json_codec.optional_as fields "subscription_binding" (function
      | `Null -> Ok None
      | value -> Subscription_binding.of_json value |> Result.map ~f:Option.some)
    |> Result.map ~f:Option.join
  in
  { source = { script_id; source_sha256 }; creator; subscription_binding }
;;

let pins_to_json pins = `Object (List.map pins ~f:(fun (name, pin) -> name, `String pin))

let validate_pins pins =
  let open Result.Let_syntax in
  let%bind () =
    Json_codec.validate_limits
      ~max_bytes:(2 * 1024 * 1024)
      ~max_depth:2
      (pins_to_json pins)
  in
  let%map _ =
    List.fold_result pins ~init:None ~f:(fun previous (name, pin) ->
      let%bind () = text ~name:"notification disclosure tool" ~max:256 name in
      let%bind () =
        match previous with
        | Some old when String.compare old name >= 0 ->
          invalid "notification disclosure pins must be unique and ordered"
        | _ -> Ok ()
      in
      match
        String.length pin = 64
        && String.for_all pin ~f:(function
          | '0' .. '9' | 'a' .. 'f' -> true
          | _ -> false)
      with
      | true -> Ok (Some name)
      | false -> invalid "notification disclosure pin must be lowercase SHA256")
  in
  ()
;;

let pins_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind pins =
    List.map (Json_codec.to_alist fields) ~f:(fun (name, json) ->
      Result.map (Json_codec.string json) ~f:(fun pin -> name, pin))
    |> Result.all
  in
  let pins = List.sort pins ~compare:(fun (a, _) (b, _) -> String.compare a b) in
  let%map () = validate_pins pins in
  pins
;;

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
  let%bind () = optional_validate t.disclosure_pins validate_pins in
  let%bind () =
    match t.completion_projection with
    | None -> Ok ()
    | Some projection ->
      let%bind () = Completion_projection.validate projection in
      let%bind () =
        match projection.result_reference with
        | None -> Ok ()
        | Some reference ->
          (match c.completion, c.work with
           | Completion.Succeeded value, Some (Invocation.Job job_id)
             when Jsonaf.exactly_equal value (Job_result_reference.to_json reference)
                  && Id.Job.equal job_id reference.job_id
                  && Id.Session.equal c.session_id reference.session_id
                  && Int.equal c.generation reference.generation
                  && Int.equal projection.job_attempt reference.attempt -> Ok ()
           | _ -> invalid "delivery differs from its explicit result reference")
      in
      (match c.source, c.ownership, c.invocation_id, c.work, t.disclosure_pins with
       | Job_adapter, None, Some _, Some (Invocation.Job _), Some _ ->
         (match projection.rejected, c.completion with
          | false, _ -> Ok ()
          | true, Completion.Failed error
            when Invocation.equal_tool_error error Completion_projection.rejection ->
            Ok ()
          | true, _ ->
            invalid "rejected completion projection must retain its bounded error")
       | _ -> invalid "completion projection requires a pinned standalone job adapter")
  in
  let%bind () =
    match c.ownership, c.source with
    | None, _ -> Ok ()
    | Some ownership, Moderator -> validate_ownership ownership
    | Some _, (Job_adapter | External_ingress) ->
      invalid "moderator-owned delivery has a different source kind"
  in
  let%bind () =
    match c.ownership with
    | None -> Ok ()
    | Some ownership ->
      (match ownership.subscription_binding, c.work with
       | None, _ -> Ok ()
       | Some binding, Some (Invocation.Subscription id) ->
         let%bind () = Subscription_binding.validate binding in
         if Id.Subscription.equal binding.subscription_id id
         then Ok ()
         else invalid "delivery binding differs from its subscription work"
       | Some _, (None | Some (Invocation.Job _)) ->
         invalid "delivery subscription binding requires subscription work")
  in
  let%bind () =
    match t.wake_disposition, c.wake, t.status with
    | None, _, _ -> Ok ()
    | Some disposition, Completion.Request_turn, Committed _ ->
      (match disposition with
       | Pending_wake -> Ok ()
       | Accepted_wake id -> validate_id Id.Operation.to_json Id.Operation.of_json id
       | Discarded_wake reason ->
         text ~name:"notification wake disposition" ~max:1024 reason)
    | Some _, _, _ ->
      invalid "wake disposition requires a committed request-turn delivery"
  in
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

let create ?disclosure_pins ?completion_projection context =
  let t =
    { context
    ; attempt = 1
    ; status = Pending
    ; wake_disposition = None
    ; disclosure_pins
    ; completion_projection
    }
  in
  Result.map (validate t) ~f:(fun () -> t)
;;

let commit ?(track_wake = false) t ~history_id ~now =
  let open Result.Let_syntax in
  let%bind () = validate t in
  match t.status with
  | Committed old ->
    if History_entry.Id.equal old.history_id history_id
    then Ok t
    else failure Conflict "delivery is already committed to another history entry"
  | Failed _ -> failure Invalid_state "failed delivery must be explicitly retried"
  | Pending ->
    let wake_disposition =
      match track_wake, t.context.wake with
      | true, Completion.Request_turn -> Some Pending_wake
      | _ -> None
    in
    let next = { t with status = Committed { history_id; at = now }; wake_disposition } in
    let%map () = validate next in
    next
;;

let settle_wake t disposition =
  let open Result.Let_syntax in
  let%bind () = validate t in
  match t.wake_disposition with
  | Some current when equal_wake_disposition current disposition -> Ok t
  | Some Pending_wake ->
    let next = { t with wake_disposition = Some disposition } in
    let%map () = validate next in
    next
  | None -> failure Invalid_state "delivery has no pending wake request"
  | Some (Accepted_wake _ | Discarded_wake _) ->
    failure Already_resolved "notification wake already has a disposition"
;;

let accept_wake t ~operation_id = settle_wake t (Accepted_wake operation_id)
let discard_wake t ~reason = settle_wake t (Discarded_wake reason)

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
    if
      (not (equal_context previous.context next.context))
      || (not
            (Option.equal
               Completion_projection.equal
               previous.completion_projection
               next.completion_projection))
      || not
           (Option.equal
              (List.equal (Tuple2.equal ~eq1:String.equal ~eq2:String.equal))
              previous.disclosure_pins
              next.disclosure_pins)
    then failure Conflict "delivery context is immutable"
    else if equal previous next
    then Ok ()
    else (
      match previous.status, next.status with
      | Pending, Failed _ when next.attempt = previous.attempt -> Ok ()
      | Pending, Committed _ when next.attempt = previous.attempt ->
        (match next.wake_disposition with
         | None | Some Pending_wake -> Ok ()
         | Some (Accepted_wake _ | Discarded_wake _) ->
           failure Invalid_state "new notification wake must start pending")
      | Failed _, Pending when next.attempt = previous.attempt + 1 -> Ok ()
      | Committed _, Committed _
        when equal_status previous.status next.status
             && Int.equal previous.attempt next.attempt ->
        (match previous.wake_disposition, next.wake_disposition with
         | Some Pending_wake, Some (Accepted_wake _ | Discarded_wake _) -> Ok ()
         | _ -> failure Invalid_state "invalid notification wake transition")
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

let wake_disposition_to_json = function
  | Pending_wake -> `Object [ "type", `String "pending" ]
  | Accepted_wake id ->
    `Object [ "type", `String "accepted"; "operation_id", Id.Operation.to_json id ]
  | Discarded_wake reason ->
    `Object [ "type", `String "discarded"; "reason", `String reason ]
;;

let wake_disposition_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  match kind with
  | "pending" ->
    let%map () = closed fields [ "type" ] in
    Pending_wake
  | "accepted" ->
    let%bind () = closed fields [ "type"; "operation_id" ] in
    let%map id = Json_codec.required_as fields "operation_id" Id.Operation.of_json in
    Accepted_wake id
  | "discarded" ->
    let%bind () = closed fields [ "type"; "reason" ] in
    let%map reason = Json_codec.required_as fields "reason" Json_codec.string in
    Discarded_wake reason
  | _ -> invalid "unknown notification wake disposition"
;;

let to_json_body t =
  let c = t.context in
  let body =
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
  in
  match t.disclosure_pins, t.wake_disposition, c.ownership with
  | Some pins, wake, ownership ->
    `Object
      ([ "schema_version", `Number "4"
       ; "delivery", body
       ; "disclosure_pins", pins_to_json pins
       ]
       @ optional "ownership" ownership ownership_to_json
       @ optional "wake_disposition" wake wake_disposition_to_json)
  | None, None, None -> body
  | None, None, Some ownership ->
    `Object
      [ "schema_version", `Number "2"
      ; "delivery", body
      ; "ownership", ownership_to_json ownership
      ]
  | None, Some disposition, ownership ->
    `Object
      ([ "schema_version", `Number "3"
       ; "delivery", body
       ; "wake_disposition", wake_disposition_to_json disposition
       ]
       @ optional "ownership" ownership ownership_to_json)
;;

let to_json_without_subscription t =
  match t.completion_projection with
  | None -> to_json_body t
  | Some projection ->
    `Object
      [ "schema_version", `Number "5"
      ; "delivery", to_json_body t
      ; "completion_projection", Completion_projection.to_json projection
      ]
;;

let to_json t =
  match t.context.ownership with
  | None -> to_json_without_subscription t
  | Some { subscription_binding = None; _ } -> to_json_without_subscription t
  | Some ({ subscription_binding = Some binding; _ } as ownership) ->
    let legacy =
      { t with
        context =
          { t.context with
            ownership = Some { ownership with subscription_binding = None }
          }
      }
    in
    `Object
      [ "schema_version", `Number "6"
      ; "delivery", to_json_without_subscription legacy
      ; "subscription_binding", Subscription_binding.to_json binding
      ]
;;

let of_json_body json =
  let open Result.Let_syntax in
  let%bind () = validate_json ~max_bytes:(18 * 1024 * 1024) ~max_depth:136 json in
  let%bind fields = Json_codec.fields json in
  let integer = Json_codec.bounded_int ~min:0 ~max:Int.max_value in
  let%bind version = Json_codec.required_as fields "schema_version" integer in
  let%bind fields, ownership, wake_disposition, disclosure_pins =
    match version with
    | 1 -> Ok (fields, None, None, None)
    | 2 ->
      let%bind () = closed fields [ "schema_version"; "delivery"; "ownership" ] in
      let%bind ownership = Json_codec.required_as fields "ownership" ownership_of_json in
      let%map fields = Json_codec.required_as fields "delivery" Json_codec.fields in
      fields, Some ownership, None, None
    | 3 ->
      let%bind () =
        closed fields [ "schema_version"; "delivery"; "ownership"; "wake_disposition" ]
      in
      let%bind ownership = Json_codec.optional_as fields "ownership" ownership_of_json in
      let%bind wake_disposition =
        Json_codec.required_as fields "wake_disposition" wake_disposition_of_json
      in
      let%map fields = Json_codec.required_as fields "delivery" Json_codec.fields in
      fields, ownership, Some wake_disposition, None
    | 4 ->
      let%bind () =
        closed
          fields
          [ "schema_version"
          ; "delivery"
          ; "ownership"
          ; "wake_disposition"
          ; "disclosure_pins"
          ]
      in
      let%bind ownership = Json_codec.optional_as fields "ownership" ownership_of_json in
      let%bind wake_disposition =
        Json_codec.optional_as fields "wake_disposition" wake_disposition_of_json
      in
      let%bind disclosure_pins =
        Json_codec.required_as fields "disclosure_pins" pins_of_json
      in
      let%map fields = Json_codec.required_as fields "delivery" Json_codec.fields in
      fields, ownership, wake_disposition, Some disclosure_pins
    | _ -> failure Incompatible_protocol "unsupported delivery version"
  in
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
    ; ownership
    }
  in
  let t =
    { context
    ; attempt
    ; status
    ; wake_disposition
    ; disclosure_pins
    ; completion_projection = None
    }
  in
  let%map () = validate t in
  t
;;

let of_json_without_subscription json =
  let open Result.Let_syntax in
  let%bind () = validate_json ~max_bytes:(18 * 1024 * 1024) ~max_depth:138 json in
  let%bind fields = Json_codec.fields json in
  let%bind version =
    Json_codec.required_as
      fields
      "schema_version"
      (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  match version with
  | 5 ->
    let%bind () =
      closed fields [ "schema_version"; "delivery"; "completion_projection" ]
    in
    let%bind value = Json_codec.required_as fields "delivery" of_json_body in
    let%bind projection =
      Json_codec.required_as fields "completion_projection" Completion_projection.of_json
    in
    let value = { value with completion_projection = Some projection } in
    let%map () = validate value in
    value
  | _ -> of_json_body json
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = validate_json ~max_bytes:(18 * 1024 * 1024) ~max_depth:140 json in
  let%bind fields = Json_codec.fields json in
  let%bind version =
    Json_codec.required_as
      fields
      "schema_version"
      (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%bind value =
    if version = 6
    then (
      let%bind () =
        closed fields [ "schema_version"; "delivery"; "subscription_binding" ]
      in
      let%bind value =
        Json_codec.required_as fields "delivery" of_json_without_subscription
      in
      let%bind binding =
        Json_codec.required_as fields "subscription_binding" Subscription_binding.of_json
      in
      match value.context.ownership with
      | Some ({ subscription_binding = None; _ } as ownership) ->
        Ok
          { value with
            context =
              { value.context with
                ownership = Some { ownership with subscription_binding = Some binding }
              }
          }
      | None | Some { subscription_binding = Some _; _ } ->
        invalid "delivery epoch envelope requires exactly one legacy owner")
    else (
      let%bind value = of_json_without_subscription json in
      match value.context.ownership with
      | None | Some { subscription_binding = None; _ } -> Ok value
      | Some { subscription_binding = Some _; _ } ->
        invalid "delivery subscription binding requires envelope version 6")
  in
  let%map () = validate value in
  value
;;

module Storage = struct
  type nonrec binding_field = binding_field =
    | Missing
    | Nullable

  let nullable decode = function
    | `Null -> Ok None
    | json -> Result.map (decode json) ~f:Option.some
  ;;

  let option encode value = Option.value_map value ~default:`Null ~f:encode

  let to_json_with_binding_field (t : t) ~binding_field =
    let c = t.context in
    `Object
      [ "id", Id.Delivery.to_json c.id
      ; "session_id", Id.Session.to_json c.session_id
      ; "generation", `Number (Int.to_string c.generation)
      ; "invocation_id", option Id.Invocation.to_json c.invocation_id
      ; "work", option Invocation.work_to_json c.work
      ; "correlation", `String c.correlation
      ; "source", source_to_json c.source
      ; "completion", Completion.to_json c.completion
      ; "wake", Completion.wake_to_json c.wake
      ; "created_at", Timestamp.to_json c.created_at
      ; ( "ownership"
        , option
            (fun ownership -> ownership_to_storage_json ownership ~binding_field)
            c.ownership )
      ; "attempt", `Number (Int.to_string t.attempt)
      ; "status", status_to_json t.status
      ; "wake_disposition", option wake_disposition_to_json t.wake_disposition
      ; "disclosure_pins", option pins_to_json t.disclosure_pins
      ; ( "completion_projection"
        , option Completion_projection.to_json t.completion_projection )
      ]
  ;;

  let to_json t = to_json_with_binding_field t ~binding_field:Nullable

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let get name decode = Json_codec.required_as fields name decode in
    let integer = Json_codec.bounded_int ~min:0 ~max:Int.max_value in
    let%bind id = get "id" Id.Delivery.of_json in
    let%bind session_id = get "session_id" Id.Session.of_json in
    let%bind generation = get "generation" integer in
    let%bind invocation_id = get "invocation_id" (nullable Id.Invocation.of_json) in
    let%bind work = get "work" (nullable Invocation.work_of_json) in
    let%bind correlation = get "correlation" Json_codec.string in
    let%bind source =
      get "source" (Json_codec.enum ~name:"delivery source" source_values)
    in
    let%bind completion = get "completion" Completion.of_json in
    let%bind wake = get "wake" Completion.wake_of_json in
    let%bind created_at = get "created_at" Timestamp.of_json in
    let%bind ownership = get "ownership" (nullable ownership_of_json) in
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
      ; ownership
      }
    in
    let%bind attempt = get "attempt" integer in
    let%bind status = get "status" status_of_json in
    let%bind wake_disposition =
      get "wake_disposition" (nullable wake_disposition_of_json)
    in
    let%bind disclosure_pins = get "disclosure_pins" (nullable pins_of_json) in
    let%bind completion_projection =
      get "completion_projection" (nullable Completion_projection.of_json)
    in
    let t =
      { context
      ; attempt
      ; status
      ; wake_disposition
      ; disclosure_pins
      ; completion_projection
      }
    in
    let%map () = validate t in
    t
  ;;
end

let unchecked_t_of_sexp = t_of_sexp

let t_of_sexp sexp =
  let value = unchecked_t_of_sexp sexp in
  match validate value with
  | Ok () -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;
