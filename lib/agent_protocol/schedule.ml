open Core

type misfire =
  | Deliver_once_immediately
  | Skip_if_expired
  | Fail
[@@deriving compare, equal, sexp]

type status =
  | Scheduled
  | Delivering
  | Delivered
  | Cancelled
  | Failed of Protocol_error.t
[@@deriving sexp]

type due =
  | At of Timestamp.t
  | After_ms of int
[@@deriving sexp]

type ownership =
  { source : Invocation.observer
  ; creator : Job.launch_owner
  ; subscription : (Id.Subscription.t * int) option [@sexp.option]
  }
[@@deriving equal, sexp]

type t =
  { id : Id.Schedule.t
  ; session_id : Id.Session.t
  ; generation : int
  ; payload : Jsonaf.t
  ; created_at : Timestamp.t
  ; next_due_at : Timestamp.t
  ; misfire : misfire
  ; status : status
  ; delivery_count : int
  ; last_delivery_at : Timestamp.t option
  ; ownership : ownership option [@sexp.option]
  ; delivery_cancellation : string option [@sexp.option]
  }
[@@deriving sexp]

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let misfire_values =
  [ "deliver_once_immediately", Deliver_once_immediately
  ; "skip_if_expired", Skip_if_expired
  ; "fail", Fail
  ]
;;

let misfire_to_string value =
  List.Assoc.find_exn
    (List.map misfire_values ~f:(fun (name, value) -> value, name))
    value
    ~equal:equal_misfire
;;

let misfire_of_json = Json_codec.enum ~name:"schedule misfire policy" misfire_values

let status_to_json = function
  | Scheduled -> `Object [ "type", `String "scheduled" ]
  | Delivering -> `Object [ "type", `String "delivering" ]
  | Delivered -> `Object [ "type", `String "delivered" ]
  | Cancelled -> `Object [ "type", `String "cancelled" ]
  | Failed error ->
    `Object [ "type", `String "failed"; "error", Protocol_error.to_json error ]
;;

let status_name = function
  | Scheduled -> "scheduled"
  | Delivering -> "delivering"
  | Delivered -> "delivered"
  | Cancelled -> "cancelled"
  | Failed _ -> "failed"
;;

let status_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "scheduled" -> Ok Scheduled
  | "delivering" -> Ok Delivering
  | "delivered" -> Ok Delivered
  | "cancelled" -> Ok Cancelled
  | "failed" ->
    Result.map (Json_codec.required_as fields "error" Protocol_error.of_json) ~f:(fun e ->
      Failed e)
  | _ -> Error (Protocol_error.invalid_request "unknown schedule status")
;;

let due_to_json = function
  | At timestamp ->
    `Object [ "type", `String "at"; "timestamp", Timestamp.to_json timestamp ]
  | After_ms delay ->
    `Object [ "type", `String "after_ms"; "delay_ms", `Number (Int.to_string delay) ]
;;

let due_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "at" ->
    Result.map (Json_codec.required_as fields "timestamp" Timestamp.of_json) ~f:(fun x ->
      At x)
  | "after_ms" ->
    Result.map
      (Json_codec.required_as
         fields
         "delay_ms"
         (Json_codec.bounded_int ~min:0 ~max:Int.max_value))
      ~f:(fun x -> After_ms x)
  | _ -> Error (Protocol_error.invalid_request "unknown schedule due policy")
;;

let ownership_to_json ownership =
  let kind, id =
    match ownership.creator with
    | Job.Invocation id -> "invocation", Id.Invocation.to_json id
    | Moderator_event id -> "moderator_event", Id.Moderator_execution.to_json id
  in
  `Object
    ([ "schema_version", `Number "1"
     ; ( "source"
       , `Object
           [ "script_id", `String ownership.source.script_id
           ; "source_sha256", `String ownership.source.source_sha256
           ] )
     ; "creator_type", `String kind
     ; "creator_id", id
     ]
     @ Option.to_list
         (Option.map ownership.subscription ~f:(fun (id, epoch) ->
            ( "subscription"
            , `Object
                [ "id", Id.Subscription.to_json id
                ; "epoch", `Number (Int.to_string epoch)
                ] ))))
;;

let ownership_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "schema_version"; "source"; "creator_type"; "creator_id"; "subscription" ]
  in
  let%bind version =
    Json_codec.required_as
      fields
      "schema_version"
      (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%bind () =
    match version with
    | 1 -> Ok ()
    | _ ->
      Error
        (Protocol_error.create
           Incompatible_protocol
           ~message:"unsupported schedule ownership version"
           ~retryable:false
           ())
  in
  let%bind source =
    Json_codec.required_as fields "source" (fun json ->
      let%bind fields = Json_codec.fields json in
      let%bind () = Extension_codec.closed fields [ "script_id"; "source_sha256" ] in
      let%bind script_id = Json_codec.required_as fields "script_id" Json_codec.string in
      let%map source_sha256 =
        Json_codec.required_as fields "source_sha256" Json_codec.string
      in
      Invocation.{ script_id; source_sha256 })
  in
  let%bind creator_type =
    Json_codec.required_as fields "creator_type" Json_codec.string
  in
  let%bind creator =
    match creator_type with
    | "invocation" ->
      Json_codec.required_as fields "creator_id" Id.Invocation.of_json
      |> Result.map ~f:(fun id -> Job.Invocation id)
    | "moderator_event" ->
      Json_codec.required_as fields "creator_id" Id.Moderator_execution.of_json
      |> Result.map ~f:(fun id -> Job.Moderator_event id)
    | _ -> Error (Protocol_error.invalid_request "unknown schedule creator")
  in
  let%map subscription =
    Json_codec.optional_as fields "subscription" (fun json ->
      let%bind fields = Json_codec.fields json in
      let%bind () = Extension_codec.closed fields [ "id"; "epoch" ] in
      let%bind id = Json_codec.required_as fields "id" Id.Subscription.of_json in
      let%map epoch =
        Json_codec.required_as
          fields
          "epoch"
          (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
      in
      id, epoch)
  in
  { source; creator; subscription }
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    match t.delivery_cancellation, t.ownership, t.status, t.delivery_count with
    | None, _, _, _ -> Ok ()
    | Some reason, Some _, Delivered, 1 ->
      Extension_codec.text ~name:"timer delivery cancellation" ~max:256 reason
    | Some _, _, _, _ ->
      Error
        (Protocol_error.invalid_request
           "only an enqueued owned timer can cancel delivery")
  in
  match t.ownership with
  | None -> Ok ()
  | Some ownership ->
    let open Result.Let_syntax in
    let%bind _ = ownership_of_json (ownership_to_json ownership) in
    let%bind () =
      Extension_codec.text
        ~name:"schedule moderator ID"
        ~max:256
        ownership.source.script_id
    in
    let hash = ownership.source.source_sha256 in
    let%bind () =
      match
        String.length hash = 64
        && String.for_all hash ~f:(function
          | '0' .. '9' | 'a' .. 'f' -> true
          | _ -> false)
      with
      | true -> Ok ()
      | false ->
        Error (Protocol_error.invalid_request "schedule source must be lowercase SHA256")
    in
    let%bind () =
      match t.generation >= 0 && Timestamp.compare t.next_due_at t.created_at >= 0 with
      | true -> Ok ()
      | false ->
        Error
          (Protocol_error.invalid_request "invalid owned schedule generation or due time")
    in
    (match t.status, t.delivery_count, t.last_delivery_at with
     | (Scheduled | Delivering | Cancelled | Failed _), 0, None | Delivered, 0, None ->
       Ok ()
     | Delivered, 1, Some at when Timestamp.compare at t.created_at >= 0 -> Ok ()
     | _ -> Error (Protocol_error.invalid_request "invalid owned schedule delivery state"))
;;

let to_json t =
  let fields =
    [ Some ("id", Id.Schedule.to_json t.id)
    ; Some ("session_id", Id.Session.to_json t.session_id)
    ; Some ("generation", `Number (Int.to_string t.generation))
    ; Some ("payload", t.payload)
    ; Some ("created_at", Timestamp.to_json t.created_at)
    ; Some ("next_due_at", Timestamp.to_json t.next_due_at)
    ; Some ("misfire", `String (misfire_to_string t.misfire))
    ; Some ("status", status_to_json t.status)
    ; Some ("delivery_count", `Number (Int.to_string t.delivery_count))
    ; optional_field "last_delivery_at" t.last_delivery_at Timestamp.to_json
    ]
    |> List.filter_opt
  in
  match t.ownership with
  | None -> `Object fields
  | Some ownership ->
    `Object
      ([ ( "schema_version"
         , `Number
             (match t.delivery_cancellation with
              | None -> "2"
              | Some _ -> "3") )
       ; "schedule", `Object fields
       ; "ownership", ownership_to_json ownership
       ]
       @ Option.to_list
           (optional_field "delivery_cancellation" t.delivery_cancellation (fun reason ->
              `String reason)))
;;

let decode_identity fields =
  let open Result.Let_syntax in
  let%bind id = Json_codec.required_as fields "id" Id.Schedule.of_json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%map generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  id, session_id, generation
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind fields, ownership, delivery_cancellation =
    match Json_codec.optional fields "schema_version" with
    | None ->
      (match
         Option.is_some (Json_codec.optional fields "ownership")
         || Option.is_some (Json_codec.optional fields "schedule")
         || Option.is_some (Json_codec.optional fields "delivery_cancellation")
       with
       | true ->
         Error
           (Protocol_error.invalid_request
              "owned schedule requires its versioned envelope")
       | false -> Ok (fields, None, None))
    | Some encoded ->
      let%bind version = Json_codec.bounded_int ~min:1 ~max:Int.max_value encoded in
      let%bind () =
        match version with
        | 2 | 3 -> Ok ()
        | _ ->
          Error
            (Protocol_error.create
               Incompatible_protocol
               ~message:"unsupported schedule version"
               ~retryable:false
               ())
      in
      let%bind () =
        Extension_codec.closed
          fields
          ([ "schema_version"; "schedule"; "ownership" ]
           @
           match version with
           | 3 -> [ "delivery_cancellation" ]
           | _ -> [])
      in
      let%bind delivery_cancellation =
        match version with
        | 3 ->
          Json_codec.required_as fields "delivery_cancellation" Json_codec.string
          |> Result.map ~f:Option.some
        | _ -> Ok None
      in
      let%bind ownership = Json_codec.required_as fields "ownership" ownership_of_json in
      let%bind fields = Json_codec.required_as fields "schedule" Json_codec.fields in
      let%map () =
        Extension_codec.closed
          fields
          [ "id"
          ; "session_id"
          ; "generation"
          ; "payload"
          ; "created_at"
          ; "next_due_at"
          ; "misfire"
          ; "status"
          ; "delivery_count"
          ; "last_delivery_at"
          ]
      in
      fields, Some ownership, delivery_cancellation
  in
  let%bind id, session_id, generation = decode_identity fields in
  let%bind payload = Json_codec.required fields "payload" in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%bind next_due_at = Json_codec.required_as fields "next_due_at" Timestamp.of_json in
  let%bind misfire = Json_codec.required_as fields "misfire" misfire_of_json in
  let%bind status = Json_codec.required_as fields "status" status_of_json in
  let%bind delivery_count =
    Json_codec.required_as
      fields
      "delivery_count"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind last_delivery_at =
    Json_codec.optional_as fields "last_delivery_at" Timestamp.of_json
  in
  let value =
    { id
    ; session_id
    ; generation
    ; payload
    ; created_at
    ; next_due_at
    ; misfire
    ; status
    ; delivery_count
    ; last_delivery_at
    ; ownership
    ; delivery_cancellation
    }
  in
  let%map () = validate value in
  value
;;

let validate_transition ~previous next =
  let conflict message =
    Error (Protocol_error.create Conflict ~message ~retryable:false ())
  in
  let open Result.Let_syntax in
  let%bind () = validate next in
  match previous with
  | None ->
    (match next.ownership, next.status with
     | None, _ | Some { subscription = None; _ }, Scheduled -> Ok ()
     | _ -> conflict "owned schedule must start scheduled and unbound")
  | Some previous ->
    let%bind () = validate previous in
    (match previous.ownership, next.ownership with
     | None, None -> Ok ()
     | Some before, Some after ->
       let immutable =
         Id.Schedule.equal previous.id next.id
         && Id.Session.equal previous.session_id next.session_id
         && Int.equal previous.generation next.generation
         && Jsonaf.exactly_equal previous.payload next.payload
         && Timestamp.equal previous.created_at next.created_at
         && Timestamp.equal previous.next_due_at next.next_due_at
         && equal_misfire previous.misfire next.misfire
         && Invocation.equal_observer before.source after.source
         && Job.equal_launch_owner before.creator after.creator
       in
       let%bind () =
         match immutable with
         | true -> Ok ()
         | false ->
           conflict "owned schedule identity, source, creator and timing are immutable"
       in
       let%bind () =
         match before.subscription, after.subscription, previous.status, next.status with
         | before, after, _, _
           when Option.equal
                  (fun (id, epoch) (other, other_epoch) ->
                     Id.Subscription.equal id other && Int.equal epoch other_epoch)
                  before
                  after -> Ok ()
         | None, Some _, Scheduled, Scheduled -> Ok ()
         | _ -> conflict "schedule subscription binding cannot be replaced"
       in
       let%bind () =
         match previous.delivery_cancellation, next.delivery_cancellation with
         | None, None -> Ok ()
         | None, Some _ ->
           (match previous.status, next.status with
            | Delivered, Delivered -> Ok ()
            | _ -> conflict "delivery cancellation requires an already-enqueued timer")
         | Some before, Some after when String.equal before after -> Ok ()
         | Some _, _ ->
           conflict "timer delivery cancellation cannot be replaced or removed"
       in
       (match previous.status, next.status with
        | Scheduled, (Scheduled | Delivering | Delivered | Cancelled | Failed _)
        | Delivering, (Scheduled | Delivering | Delivered | Cancelled | Failed _) -> Ok ()
        | Delivered, Delivered
          when Option.is_none previous.delivery_cancellation
               && Option.is_some next.delivery_cancellation
               && Jsonaf.exactly_equal
                    (to_json previous)
                    (to_json { next with delivery_cancellation = None }) -> Ok ()
        | (Delivered | Cancelled | Failed _), _
          when Jsonaf.exactly_equal (to_json previous) (to_json next) -> Ok ()
        | _ -> conflict "terminal schedule cannot change")
     | _ -> conflict "schedule ownership cannot be attached or removed")
;;

module List_request = struct
  type nonrec t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; status : string option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      ("session_id", Id.Session.to_json t.session_id) :: Page.Request.to_fields t.page
    in
    match t.status with
    | None -> `Object fields
    | Some status -> `Object (fields @ [ "status", `String status ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind page = Page.Request.of_fields fields in
    let%map status = Json_codec.optional_as fields "status" Json_codec.string in
    { session_id; page; status }
  ;;
end

module Get_request = struct
  type t =
    { session_id : Id.Session.t
    ; schedule_id : Id.Schedule.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "schedule_id", Id.Schedule.to_json t.schedule_id
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%map schedule_id =
      Json_codec.required_as fields "schedule_id" Id.Schedule.of_json
    in
    { session_id; schedule_id }
  ;;
end

module Create_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; payload : Jsonaf.t
    ; due : due
    ; misfire : misfire
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "payload", t.payload
      ; "due", due_to_json t.due
      ; "misfire", `String (misfire_to_string t.misfire)
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
    let%bind payload = Json_codec.required fields "payload" in
    let%bind due = Json_codec.required_as fields "due" due_of_json in
    let%bind misfire = Json_codec.required_as fields "misfire" misfire_of_json in
    let%map idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    { session_id; attachment_id; payload; due; misfire; idempotency_key }
  ;;
end

module Cancel_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; schedule_id : Id.Schedule.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "schedule_id", Id.Schedule.to_json t.schedule_id
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
    let%bind schedule_id =
      Json_codec.required_as fields "schedule_id" Id.Schedule.of_json
    in
    let%map idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    { session_id; attachment_id; schedule_id; idempotency_key }
  ;;
end

module Mutation_response = struct
  type nonrec t =
    { schedule : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object (("schedule", to_json t.schedule) :: Mutation_result.to_fields t.mutation)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind schedule = Json_codec.required_as fields "schedule" of_json in
    let%map mutation = Mutation_result.of_fields fields in
    { schedule; mutation }
  ;;
end
