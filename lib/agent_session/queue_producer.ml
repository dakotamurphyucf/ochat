open! Core
module P = Agent_protocol
module J = P.Json_codec

type t =
  | Event of P.Id.Moderator_execution.t
  | Invocation of P.Id.Invocation.t
  | Job_completion of
      { id : P.Id.Job.t
      ; generation : int
      ; attempt : int
      }
  | Schedule_delivery of
      { id : P.Id.Schedule.t
      ; generation : int
      ; delivery_count : int
      }
  | Source_setup of { basis_revision : int64 }
[@@deriving equal, sexp]

let invalid message = Error (P.Error.invalid_request message)

let validate = function
  | Event id ->
    P.Id.Moderator_execution.of_string (P.Id.Moderator_execution.to_string id)
    |> Result.map ~f:ignore
  | Invocation id ->
    P.Id.Invocation.of_string (P.Id.Invocation.to_string id) |> Result.map ~f:ignore
  | Job_completion { id; generation; attempt } ->
    let%bind.Result _ = P.Id.Job.of_string (P.Id.Job.to_string id) in
    if generation >= 0 && attempt > 0
    then Ok ()
    else invalid "invalid queued Job producer counters"
  | Schedule_delivery { id; generation; delivery_count } ->
    let%bind.Result _ = P.Id.Schedule.of_string (P.Id.Schedule.to_string id) in
    if generation >= 0 && delivery_count > 0
    then Ok ()
    else invalid "invalid queued Schedule producer counters"
  | Source_setup { basis_revision } ->
    if Int64.(basis_revision >= 0L)
    then Ok ()
    else invalid "negative source setup revision"
;;

let to_jsonaf = function
  | Event id ->
    `Object [ "kind", `String "event"; "id", P.Id.Moderator_execution.to_json id ]
  | Invocation id ->
    `Object [ "kind", `String "invocation"; "id", P.Id.Invocation.to_json id ]
  | Job_completion { id; generation; attempt } ->
    `Object
      [ "kind", `String "job_completion"
      ; "id", P.Id.Job.to_json id
      ; "generation", `Number (Int.to_string generation)
      ; "attempt", `Number (Int.to_string attempt)
      ]
  | Schedule_delivery { id; generation; delivery_count } ->
    `Object
      [ "kind", `String "schedule_delivery"
      ; "id", P.Id.Schedule.to_json id
      ; "generation", `Number (Int.to_string generation)
      ; "delivery_count", `Number (Int.to_string delivery_count)
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
      P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%bind () =
    Document_schema.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%bind fields = J.fields json in
  let get name decode = J.required_as fields name decode in
  let%bind kind = get "kind" J.string in
  let%bind t =
    match kind with
    | "event" ->
      get "id" P.Id.Moderator_execution.of_json |> Result.map ~f:(fun id -> Event id)
    | "invocation" ->
      get "id" P.Id.Invocation.of_json |> Result.map ~f:(fun id -> Invocation id)
    | "job_completion" ->
      let%bind id = get "id" P.Id.Job.of_json in
      let%bind generation = get "generation" (J.bounded_int ~min:0 ~max:Int.max_value) in
      let%map attempt = get "attempt" (J.bounded_int ~min:0 ~max:Int.max_value) in
      Job_completion { id; generation; attempt }
    | "schedule_delivery" ->
      let%bind id = get "id" P.Id.Schedule.of_json in
      let%bind generation = get "generation" (J.bounded_int ~min:0 ~max:Int.max_value) in
      let%map delivery_count =
        get "delivery_count" (J.bounded_int ~min:0 ~max:Int.max_value)
      in
      Schedule_delivery { id; generation; delivery_count }
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
  | Error error -> Sexplib.Conv.of_sexp_error error.P.Error.message sexp
;;

let producer_fields names =
  Document_schema.Shape.object_
    (List.map names ~f:(fun name -> name, Document_schema.Shape.value))
  |> function
  | Ok shape -> shape
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let shape =
  Document_schema.Shape.tagged_object
    ~discriminator:"kind"
    [ "event", producer_fields [ "kind"; "id" ]
    ; "invocation", producer_fields [ "kind"; "id" ]
    ; "job_completion", producer_fields [ "kind"; "id"; "generation"; "attempt" ]
    ; ( "schedule_delivery"
      , producer_fields [ "kind"; "id"; "generation"; "delivery_count" ] )
    ; "source_setup", producer_fields [ "kind"; "basis_revision" ]
    ]
  |> function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;
