open Core

type kind =
  | Invocation
  | Subscription
  | Delivery
  | Moderator_execution
[@@deriving compare, equal, sexp]

type t =
  { kind : kind
  ; id : string
  ; generation : int
  ; state : string
  }
[@@deriving equal, sexp]

let outcome = function
  | Invocation.Complete _ -> "complete"
  | Pending _ -> "pending"
  | Fail _ -> "failed"
  | Cancelled _ -> "cancelled"
;;

let invocation (value : Invocation.t) =
  let state =
    match value.status with
    | Admitted -> "admitted"
    | Dispatching -> "dispatching"
    | Resolved result -> "resolved." ^ outcome result
    | Published result -> "published." ^ outcome result
  in
  { kind = Invocation
  ; id = Id.Invocation.to_string value.context.id
  ; generation = value.context.generation
  ; state
  }
;;

let subscription (value : Subscription.t) =
  let state =
    match value.result with
    | None -> "active"
    | Some (Succeeded _) -> "succeeded"
    | Some (Failed _) -> "failed"
    | Some (Cancelled _) -> "cancelled"
    | Some Expired -> "expired"
  in
  { kind = Subscription
  ; id = Id.Subscription.to_string value.context.id
  ; generation = value.context.generation
  ; state
  }
;;

let delivery (value : Delivery.t) =
  { kind = Delivery
  ; id = Id.Delivery.to_string value.context.id
  ; generation = value.context.generation
  ; state =
      (match value.status with
       | Pending -> "pending"
       | Committed _ -> "committed"
       | Failed _ -> "failed")
  }
;;

let moderator_execution (value : Moderator_execution.t) =
  { kind = Moderator_execution
  ; id = Id.Moderator_execution.to_string value.context.id
  ; generation = value.context.generation
  ; state =
      (match value.status, value.intent with
       | Running, _ -> "running"
       | Failed _, _ -> "failed"
       | Interrupted _, _ -> "interrupted"
       | Completed _, None -> "completed"
       | Completed _, Some Pending -> "completed.pending"
       | Completed _, Some (Waiting_compaction _) -> "completed.waiting_compaction"
       | Completed _, Some Applied -> "completed.applied"
       | Completed _, Some (Discarded _) -> "completed.discarded")
  }
;;

let kind_values =
  [ "invocation", Invocation
  ; "subscription", Subscription
  ; "delivery", Delivery
  ; "moderator_execution", Moderator_execution
  ]
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; ( "kind"
      , `String
          (List.find_exn kind_values ~f:(fun (_, kind) -> equal_kind kind t.kind) |> fst)
      )
    ; "id", `String t.id
    ; "generation", `Number (Int.to_string t.generation)
    ; "state", `String t.state
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_json ~max_bytes:1024 ~max_depth:2 json in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed fields [ "version"; "kind"; "id"; "generation"; "state" ]
  in
  let%bind _ =
    Json_codec.required_as fields "version" (Json_codec.bounded_int ~min:1 ~max:1)
  in
  let%bind kind =
    Json_codec.required_as
      fields
      "kind"
      (Json_codec.enum ~name:"extension kind" kind_values)
  in
  let decode_id =
    match kind with
    | Invocation ->
      fun json -> Result.map (Id.Invocation.of_json json) ~f:Id.Invocation.to_string
    | Subscription ->
      fun json -> Result.map (Id.Subscription.of_json json) ~f:Id.Subscription.to_string
    | Delivery ->
      fun json -> Result.map (Id.Delivery.of_json json) ~f:Id.Delivery.to_string
    | Moderator_execution ->
      fun json ->
        Result.map
          (Id.Moderator_execution.of_json json)
          ~f:Id.Moderator_execution.to_string
  in
  let%bind id = Json_codec.required_as fields "id" decode_id in
  let%bind generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let states =
    match kind with
    | Invocation ->
      [ "admitted"
      ; "dispatching"
      ; "resolved.complete"
      ; "resolved.pending"
      ; "resolved.failed"
      ; "resolved.cancelled"
      ; "published.complete"
      ; "published.pending"
      ; "published.failed"
      ; "published.cancelled"
      ]
    | Subscription -> [ "active"; "succeeded"; "failed"; "cancelled"; "expired" ]
    | Delivery -> [ "pending"; "committed"; "failed" ]
    | Moderator_execution ->
      [ "running"
      ; "failed"
      ; "interrupted"
      ; "completed"
      ; "completed.pending"
      ; "completed.waiting_compaction"
      ; "completed.applied"
      ; "completed.discarded"
      ]
  in
  let%map state =
    Json_codec.required_as
      fields
      "state"
      (Json_codec.enum ~name:"extension state" (List.map states ~f:(fun s -> s, s)))
  in
  { kind; id; generation; state }
;;

let list_of_json json =
  let open Result.Let_syntax in
  let%bind values = Json_codec.list of_json json in
  match
    List.find_a_dup (List.map values ~f:(fun value -> value.id)) ~compare:String.compare
  with
  | None -> Ok values
  | Some _ -> Error (Protocol_error.invalid_request "duplicate extension status identity")
;;
