open! Core
module P = Agent_protocol
module J = P.Json_codec
module X = Persistence_codec
module Frame = Chat_response.Background_delivery

module Retirement = struct
  type t =
    | Source_change
    | Run_terminal
    | Recovery
    | Authorization_lost
  [@@deriving equal, sexp]

  let to_json = function
    | Source_change -> `String "source_change"
    | Run_terminal -> `String "run_terminal"
    | Recovery -> `String "recovery"
    | Authorization_lost -> `String "authorization_lost"
  ;;

  let of_json = function
    | `String "source_change" -> Ok Source_change
    | `String "run_terminal" -> Ok Run_terminal
    | `String "recovery" -> Ok Recovery
    | `String "authorization_lost" -> Ok Authorization_lost
    | _ -> Error (P.Error.invalid_request "unknown run job delivery retirement")
  ;;
end

module Disposition = struct
  type t =
    | Pending
    | Enqueued of P.Timestamp.t
    | Claimed of
        { enqueued_at : P.Timestamp.t
        ; execution_id : P.Id.Moderator_execution.t
        }
    | Retired of
        { enqueued_at : P.Timestamp.t option
        ; execution_id : P.Id.Moderator_execution.t option
        ; reason : Retirement.t
        }
  [@@deriving equal, sexp]
end

module Key = struct
  module T = struct
    type t =
      { run_id : P.Id.Run.t
      ; job_id : P.Id.Job.t
      ; generation : int
      ; attempt : int
      }
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let of_wake (wake : P.Run_wake.t) =
    match wake.occurrence with
    | Job_completion { job_id; attempt } ->
      Some { run_id = wake.run_id; job_id; generation = wake.source.generation; attempt }
    | Delivered_timer _ | Subscription_delivery _ -> None
  ;;
end

let max_encoded_bytes = 8192
let reservation_bytes = 16384

type t =
  { run_id : P.Id.Run.t
  ; source : P.Run_source.t
  ; frame : Frame.t
  ; frame_encoding : string
  ; disposition : Disposition.t
  }

let key t =
  { Key.run_id = t.run_id
  ; job_id = t.frame.job_id
  ; generation = t.frame.generation
  ; attempt = t.frame.attempt
  }
;;

let identity t =
  String.concat
    ~sep:":"
    [ P.Id.Run.to_string t.run_id
    ; P.Id.Job.to_string t.frame.job_id
    ; Int.to_string t.frame.generation
    ; Int.to_string t.frame.attempt
    ]
;;

let run_id t = t.run_id
let source t = t.source
let frame t = t.frame
let disposition t = t.disposition

let disposition_reserve_bytes t =
  (* Timestamp JSON is at most 32 bytes and IDs are at most 96 ASCII bytes.
     Enqueue adds under 64 encoded bytes; claim adds under 128; retirement
     preserves both and adds under 64 for its closed reason and field names. *)
  match t.disposition with
  | Pending -> 256
  | Enqueued _ -> 192
  | Claimed _ -> 64
  | Retired _ -> 0
;;

let invalid message = Error (P.Error.invalid_request message)

let equal a b =
  P.Id.Run.equal a.run_id b.run_id
  && P.Run_source.equal a.source b.source
  && String.equal a.frame_encoding b.frame_encoding
  && Disposition.equal a.disposition b.disposition
;;

let disposition_json = function
  | Disposition.Pending -> `Object [ "kind", `String "pending" ]
  | Enqueued at ->
    `Object [ "kind", `String "enqueued"; "enqueued_at", P.Timestamp.to_json at ]
  | Claimed { enqueued_at; execution_id } ->
    `Object
      [ "kind", `String "claimed"
      ; "enqueued_at", P.Timestamp.to_json enqueued_at
      ; "execution_id", P.Id.Moderator_execution.to_json execution_id
      ]
  | Retired { enqueued_at; execution_id; reason } ->
    `Object
      [ "kind", `String "retired"
      ; "enqueued_at", X.option_json P.Timestamp.to_json enqueued_at
      ; "execution_id", X.option_json P.Id.Moderator_execution.to_json execution_id
      ; "reason", Retirement.to_json reason
      ]
;;

let to_jsonaf t =
  `Object
    [ "id", `String (identity t)
    ; "run_id", P.Id.Run.to_json t.run_id
    ; "source", P.Run_source.to_json t.source
    ; "frame", `String t.frame_encoding
    ; "disposition", disposition_json t.disposition
    ]
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    J.validate_limits
      ~max_bytes:(max_encoded_bytes - disposition_reserve_bytes t)
      ~max_depth:P.Run_limits.max_depth
      (to_jsonaf t)
  in
  let enqueued_at, execution_id =
    match t.disposition with
    | Pending -> None, None
    | Enqueued at -> Some at, None
    | Claimed { enqueued_at; execution_id } -> Some enqueued_at, Some execution_id
    | Retired { enqueued_at; execution_id; reason = _ } -> enqueued_at, execution_id
  in
  let%bind () =
    match execution_id with
    | None -> Ok ()
    | Some id ->
      P.Id.Moderator_execution.of_string (P.Id.Moderator_execution.to_string id)
      |> Result.map ~f:ignore
  in
  if
    Int.equal t.frame.generation t.source.generation
    && P.Invocation.equal_observer t.frame.source t.source.observer
    && Option.value_map enqueued_at ~default:true ~f:(fun at ->
      P.Timestamp.compare at t.frame.completed_at >= 0)
    && (Option.is_none execution_id || Option.is_some enqueued_at)
  then Ok ()
  else invalid "run job delivery provenance or disposition is inconsistent"
;;

let capture (run : P.Run.t) ~(frame : Frame.t) =
  let open Result.Let_syntax in
  let owns =
    List.exists run.owned_work ~f:(fun work ->
      Int.equal work.P.Run_work.generation frame.generation
      && P.Run_work.Key.equal
           work.key
           (Retained (Job { id = frame.job_id; attempt = frame.attempt })))
  in
  let waiting =
    match run.lifecycle with
    | Waiting wake ->
      P.Id.Run.equal wake.run_id run.id
      && P.Run_source.equal wake.source run.source
      &&
        (match wake.occurrence with
        | Job_completion { job_id; attempt } ->
          P.Id.Job.equal job_id frame.job_id && Int.equal attempt frame.attempt
        | Delivered_timer _ | Subscription_delivery _ -> false)
    | Admitted | Active | Terminal _ -> false
  in
  if
    not
      (owns
       && waiting
       && P.Id.Session.equal frame.session_id (P.Session_ref.session_id run.session)
       && Int.equal frame.generation run.source.generation
       && P.Invocation.equal_observer frame.source run.source.observer)
  then invalid "terminal job frame does not match its exact owned run wait"
  else (
    let json = Frame.to_json frame in
    let%bind () = J.validate_limits ~max_bytes:max_encoded_bytes ~max_depth:132 json in
    let t =
      { run_id = run.id
      ; source = run.source
      ; frame
      ; frame_encoding = Jsonaf.to_string json
      ; disposition = Pending
      }
    in
    let%map () = validate t in
    t)
;;

let enqueue t ~at =
  match t.disposition with
  | Pending ->
    let next = { t with disposition = Enqueued at } in
    Result.map (validate next) ~f:(fun () -> next)
  | Enqueued _ | Claimed _ | Retired _ ->
    invalid "run job occurrence is not pending enqueue"
;;

let claim t ~execution_id =
  match t.disposition with
  | Enqueued enqueued_at ->
    let next = { t with disposition = Claimed { enqueued_at; execution_id } } in
    Result.map (validate next) ~f:(fun () -> next)
  | Pending | Claimed _ | Retired _ ->
    invalid "run job occurrence is not enqueued for claim"
;;

let retire t ~reason =
  let enqueued_at, execution_id =
    match t.disposition with
    | Pending -> None, None
    | Enqueued at -> Some at, None
    | Claimed { enqueued_at; execution_id } -> Some enqueued_at, Some execution_id
    | Retired _ -> None, None
  in
  match t.disposition with
  | Retired _ -> t
  | Pending | Enqueued _ | Claimed _ ->
    { t with disposition = Retired { enqueued_at; execution_id; reason } }
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () =
    J.validate_limits
      ~max_bytes:P.Run_limits.max_document_bytes
      ~max_depth:P.Run_limits.max_depth
      json
  in
  let%bind fields = J.fields json in
  let%bind run_id = J.required_as fields "run_id" P.Id.Run.of_json in
  let%bind source = J.required_as fields "source" P.Run_source.of_json in
  let%bind frame_encoding = J.required_as fields "frame" J.string in
  let%bind () =
    if String.length frame_encoding <= max_encoded_bytes
    then Ok ()
    else invalid "retained terminal frame exceeds its admitted encoded bound"
  in
  let%bind frame_json =
    Chatmd_shell_spec.Tool_schema.parse_json frame_encoding
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "invalid retained terminal frame JSON")
  in
  let%bind frame =
    Frame.of_json frame_json |> Result.map_error ~f:P.Error.invalid_request
  in
  let%bind disposition =
    J.required_as fields "disposition" (fun json ->
      let%bind fields = J.fields json in
      let%bind kind = J.required_as fields "kind" J.string in
      match kind with
      | "pending" -> Ok Disposition.Pending
      | "enqueued" ->
        let%map at = J.required_as fields "enqueued_at" P.Timestamp.of_json in
        Disposition.Enqueued at
      | "claimed" ->
        let%bind enqueued_at = J.required_as fields "enqueued_at" P.Timestamp.of_json in
        let%map execution_id =
          J.required_as fields "execution_id" P.Id.Moderator_execution.of_json
        in
        Disposition.Claimed { enqueued_at; execution_id }
      | "retired" ->
        let%bind enqueued_at =
          J.required_as fields "enqueued_at" (X.nullable P.Timestamp.of_json)
        in
        let%bind execution_id =
          J.required_as
            fields
            "execution_id"
            (X.nullable P.Id.Moderator_execution.of_json)
        in
        let%map reason = J.required_as fields "reason" Retirement.of_json in
        Disposition.Retired { enqueued_at; execution_id; reason }
      | _ -> invalid "unknown run job delivery disposition")
  in
  let t = { run_id; source; frame; frame_encoding; disposition } in
  let%bind id = J.required_as fields "id" J.string in
  let%bind () =
    if String.equal id (identity t)
    then Ok ()
    else invalid "run job delivery identity differs from its exact occurrence"
  in
  let%map () = validate t in
  t
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_jsonaf t)

let t_of_sexp sexp =
  match of_jsonaf (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> invalid_arg error.P.Error.message
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = validate next in
  if
    not
      (P.Id.Run.equal previous.run_id next.run_id
       && P.Run_source.equal previous.source next.source
       && String.equal previous.frame_encoding next.frame_encoding)
  then invalid "run job delivery immutable occurrence changed"
  else if Disposition.equal previous.disposition next.disposition
  then Ok ()
  else (
    match previous.disposition, next.disposition with
    | Pending, Enqueued _ -> Ok ()
    | Enqueued at, Claimed { enqueued_at; execution_id = _ }
      when P.Timestamp.equal at enqueued_at -> Ok ()
    | (Pending | Enqueued _ | Claimed _), Retired { reason; _ } ->
      if equal (retire previous ~reason) next
      then Ok ()
      else invalid "retirement erased run job claim provenance"
    | Pending, (Pending | Claimed _)
    | Enqueued _, (Pending | Enqueued _ | Claimed _)
    | Claimed _, (Pending | Enqueued _ | Claimed _)
    | Retired _, (Pending | Enqueued _ | Claimed _ | Retired _) ->
      invalid "run job delivery disposition cannot replay or regress")
;;

let shape =
  X.shape_exn
    [ "id", Document_schema.Shape.value
    ; "run_id", Document_schema.Shape.value
    ; "source", Run_record_shapes.source
    ; "frame", Document_schema.Shape.value
    ; "disposition", X.fields_shape [ "kind"; "enqueued_at"; "execution_id"; "reason" ]
    ]
;;

let validate_owner t ~(run : P.Run.t) ~proof ~pending_wait ~current_source =
  let expected =
    P.Run_work.Key.Retained (Job { id = t.frame.job_id; attempt = t.frame.attempt })
  in
  let proof_matches =
    Option.exists proof ~f:(fun (proof : P.Run_work.Terminal.t) ->
      P.Run_work.Key.equal proof.work.key expected
      && Int.equal proof.work.generation t.frame.generation
      &&
      match P.Stored_completion.outcome t.frame.result, proof.outcome with
      | Succeeded, Succeeded
      | Failed, (Failed | Limited | Interrupted)
      | Cancelled, (Cancelled | Interrupted)
      | Expired, Limited -> true
      | Succeeded, (Failed | Cancelled | Limited | Interrupted | Unconfirmed)
      | Failed, (Succeeded | Cancelled | Unconfirmed)
      | Cancelled, (Succeeded | Failed | Limited | Unconfirmed)
      | Expired, (Succeeded | Failed | Cancelled | Interrupted | Unconfirmed) -> false)
  in
  let wait_matches =
    Option.exists pending_wait ~f:(fun (wake : P.Run_wake.t) ->
      P.Id.Run.equal wake.run_id t.run_id
      && P.Run_source.equal wake.source t.source
      &&
      match wake.occurrence with
      | Job_completion { job_id; attempt } ->
        P.Id.Job.equal job_id t.frame.job_id && Int.equal attempt t.frame.attempt
      | Delivered_timer _ | Subscription_delivery _ -> false)
  in
  let disposition_matches =
    match t.disposition, run.lifecycle with
    | (Pending | Enqueued _), Waiting wake ->
      current_source
      && wait_matches
      && Option.exists pending_wait ~f:(P.Run_wake.equal wake)
    | Claimed _, (Active | Waiting _) -> current_source
    | Retired _, Terminal _ -> true
    | (Pending | Enqueued _), (Admitted | Active | Terminal _)
    | Claimed _, (Admitted | Terminal _)
    | Retired _, (Admitted | Active | Waiting _) -> false
  in
  if
    P.Id.Run.equal run.id t.run_id
    && P.Run_source.equal run.source t.source
    && P.Id.Session.equal (P.Session_ref.session_id run.session) t.frame.session_id
    && proof_matches
    && disposition_matches
  then Ok ()
  else invalid "run job delivery differs from its immutable owner evidence"
;;
