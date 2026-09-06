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
  `Object fields
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
  let%map last_delivery_at =
    Json_codec.optional_as fields "last_delivery_at" Timestamp.of_json
  in
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
  }
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
