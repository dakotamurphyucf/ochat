open! Core
module J = Json_codec

type t =
  | Event of Id.Moderator_execution.t
  | Invocation of Id.Invocation.t
  | Invocation_at_source of
      { id : Id.Invocation.t
      ; source : Run_source.t
      }
  | Job_completion of
      { id : Id.Job.t
      ; generation : int
      ; attempt : int
      }
  | Schedule_delivery of
      { id : Id.Schedule.t
      ; generation : int
      ; delivery_count : int
      }
  | Ingress_delivery of
      { registration_id : Id.Capability.t
      ; event_id : Id.Ingress_event.t
      }
  | Source_setup of { basis_revision : int64 }
[@@deriving equal, sexp]

let invalid message = Error (Protocol_error.invalid_request message)

let validate = function
  | Event id ->
    Id.Moderator_execution.of_string (Id.Moderator_execution.to_string id)
    |> Result.map ~f:ignore
  | Invocation id ->
    Id.Invocation.of_string (Id.Invocation.to_string id) |> Result.map ~f:ignore
  | Invocation_at_source { id; source } ->
    let%bind.Result _ = Id.Invocation.of_string (Id.Invocation.to_string id) in
    Run_source.validate source
  | Job_completion { id; generation; attempt } ->
    let%bind.Result _ = Id.Job.of_string (Id.Job.to_string id) in
    if generation >= 0 && attempt >= 0
    then Ok ()
    else invalid "invalid queued Job producer counters"
  | Schedule_delivery { id; generation; delivery_count } ->
    let%bind.Result _ = Id.Schedule.of_string (Id.Schedule.to_string id) in
    if generation >= 0 && delivery_count > 0
    then Ok ()
    else invalid "invalid queued Schedule producer counters"
  | Ingress_delivery { registration_id; event_id } ->
    let%bind.Result _ =
      Id.Capability.of_string (Id.Capability.to_string registration_id)
    in
    Id.Ingress_event.of_string (Id.Ingress_event.to_string event_id)
    |> Result.map ~f:ignore
  | Source_setup { basis_revision } ->
    if Int64.(basis_revision >= 0L)
    then Ok ()
    else invalid "negative source setup revision"
;;

let to_jsonaf = function
  | Event id ->
    `Object [ "kind", `String "event"; "id", Id.Moderator_execution.to_json id ]
  | Invocation id ->
    `Object [ "kind", `String "invocation"; "id", Id.Invocation.to_json id ]
  | Invocation_at_source { id; source } ->
    `Object
      [ "kind", `String "invocation_at_source"
      ; "id", Id.Invocation.to_json id
      ; "source", Run_source.to_json source
      ]
  | Job_completion { id; generation; attempt } ->
    `Object
      [ "kind", `String "job_completion"
      ; "id", Id.Job.to_json id
      ; "generation", `Number (Int.to_string generation)
      ; "attempt", `Number (Int.to_string attempt)
      ]
  | Schedule_delivery { id; generation; delivery_count } ->
    `Object
      [ "kind", `String "schedule_delivery"
      ; "id", Id.Schedule.to_json id
      ; "generation", `Number (Int.to_string generation)
      ; "delivery_count", `Number (Int.to_string delivery_count)
      ]
  | Ingress_delivery { registration_id; event_id } ->
    `Object
      [ "kind", `String "ingress_delivery"
      ; "registration_id", Id.Capability.to_json registration_id
      ; "event_id", Id.Ingress_event.to_json event_id
      ]
  | Source_setup { basis_revision } ->
    `Object
      [ "kind", `String "source_setup"
      ; "basis_revision", `String (Int64.to_string basis_revision)
      ]
;;

let setup_revision_of_json = function
  | `String encoded when String.length encoded <= 19 ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value >= 0L) && String.equal encoded (Int64.to_string value)
       -> Ok value
     | Some _ | None -> invalid "invalid source setup revision")
  | _ -> invalid "source setup revision must be a canonical decimal string"
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind limits =
    Document_schema.Limits.create
      ~max_bytes:4096
      ~max_depth:8
      ~max_fields:32
      ~max_nodes:64
    |> Result.map_error ~f:(fun error ->
      Protocol_error.invalid_request
        (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%bind () =
    Document_schema.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      Protocol_error.invalid_request
        (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%bind fields = J.fields json in
  let get name decode = J.required_as fields name decode in
  let%bind kind = get "kind" J.string in
  let%bind t =
    match kind with
    | "event" ->
      get "id" Id.Moderator_execution.of_json |> Result.map ~f:(fun id -> Event id)
    | "invocation" ->
      get "id" Id.Invocation.of_json |> Result.map ~f:(fun id -> Invocation id)
    | "invocation_at_source" ->
      let%bind id = get "id" Id.Invocation.of_json in
      let%map source = get "source" Run_source.of_json in
      Invocation_at_source { id; source }
    | "job_completion" ->
      let%bind id = get "id" Id.Job.of_json in
      let%bind generation = get "generation" (J.bounded_int ~min:0 ~max:Int.max_value) in
      let%map attempt = get "attempt" (J.bounded_int ~min:0 ~max:Int.max_value) in
      Job_completion { id; generation; attempt }
    | "schedule_delivery" ->
      let%bind id = get "id" Id.Schedule.of_json in
      let%bind generation = get "generation" (J.bounded_int ~min:0 ~max:Int.max_value) in
      let%map delivery_count =
        get "delivery_count" (J.bounded_int ~min:0 ~max:Int.max_value)
      in
      Schedule_delivery { id; generation; delivery_count }
    | "ingress_delivery" ->
      let%bind registration_id = get "registration_id" Id.Capability.of_json in
      let%map event_id = get "event_id" Id.Ingress_event.of_json in
      Ingress_delivery { registration_id; event_id }
    | "source_setup" ->
      get "basis_revision" setup_revision_of_json
      |> Result.map ~f:(fun basis_revision -> Source_setup { basis_revision })
    | _ -> invalid "unknown queued callback producer"
  in
  let%map () = validate t in
  t
;;

let generated_t_of_sexp = t_of_sexp

let t_of_sexp sexp =
  let t = generated_t_of_sexp sexp in
  match validate t with
  | Ok () -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
;;

let producer_fields names =
  Document_schema.Shape.object_
    (List.map names ~f:(fun name -> name, Document_schema.Shape.value))
  |> function
  | Ok shape -> shape
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let invocation_at_source_shape =
  Document_schema.Shape.object_
    [ "kind", Document_schema.Shape.value
    ; "id", Document_schema.Shape.value
    ; ( "source"
      , producer_fields
          [ "script_id"; "source_sha256"; "generation"; "installation_epoch" ] )
    ]
  |> function
  | Ok shape -> shape
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let shape =
  Document_schema.Shape.tagged_object
    ~discriminator:"kind"
    [ "event", producer_fields [ "kind"; "id" ]
    ; "invocation", producer_fields [ "kind"; "id" ]
    ; "invocation_at_source", invocation_at_source_shape
    ; "job_completion", producer_fields [ "kind"; "id"; "generation"; "attempt" ]
    ; ( "schedule_delivery"
      , producer_fields [ "kind"; "id"; "generation"; "delivery_count" ] )
    ; "ingress_delivery", producer_fields [ "kind"; "registration_id"; "event_id" ]
    ; "source_setup", producer_fields [ "kind"; "basis_revision" ]
    ]
  |> function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;
