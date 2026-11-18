open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Cause = Policy_cause_data
module PC = Policy_canonical_usage
module Occurrence = Goal_descendant_occurrence

let invalid message = Error (P.Error.invalid_request message)

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let add a b =
  let open Result.Let_syntax in
  let convert =
    Result.map_error ~f:(fun (_ : Goal_accounting_counter.error) ->
      P.Error.invalid_request "causal receipt counter exhausted")
  in
  let%bind a = Goal_accounting_counter.of_int64 a |> convert in
  let%bind b = Goal_accounting_counter.of_int64 b |> convert in
  let%map value = Goal_accounting_counter.add a b |> convert in
  Goal_accounting_counter.to_int64 value
;;

let pending_limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:80 ~max_fields:16384 ~max_nodes:32768
  |> domain
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

module Pending = struct
  type t =
    { owner : P.Session_ref.t
    ; goal_id : P.Id.Goal.t
    ; stream_id : string
    ; nonce : int64
    ; next_prefix : int64
    ; occurrence : Occurrence.t
    ; cause : Cause.Key.t
    ; work : Cause.Allocation_work.t
    ; parent_event : P.Id.Moderator_execution.t
    ; caller_origin : P.Inference_origin.t option
    ; child_invocation : P.Id.Invocation.t
    ; admission_sha256 : string
    ; forwarding_sha256 : string option
    }
  [@@deriving equal, sexp_of]

  let forwarding_sha256 t = t.forwarding_sha256
  let owner t = t.owner
  let goal_id t = t.goal_id
  let stream_id t = t.stream_id
  let nonce t = t.nonce
  let next_prefix t = t.next_prefix
  let occurrence t = t.occurrence
  let cause t = t.cause
  let work t = t.work

  let to_json t =
    `Object
      ([ "owner", P.Session_ref.to_json t.owner
       ; "goal_id", P.Id.Goal.to_json t.goal_id
       ; "stream_id", `String t.stream_id
       ; "nonce", X.int64_json t.nonce
       ; "next_prefix", X.int64_json t.next_prefix
       ; "occurrence", Occurrence.to_json t.occurrence
       ; "cause", Cause.Key.to_json t.cause
       ; "work", Cause.Allocation_work.to_json t.work
       ; "parent_event", P.Id.Moderator_execution.to_json t.parent_event
       ; "caller_origin", X.option_json P.Inference_origin.to_json t.caller_origin
       ; "child_invocation", P.Id.Invocation.to_json t.child_invocation
       ; "admission_sha256", `String t.admission_sha256
       ]
       @ Option.to_list
           (Option.map t.forwarding_sha256 ~f:(fun hash ->
              "forwarding_sha256", `String hash)))
  ;;

  let shape =
    X.shape_exn
      [ "owner", D.Shape.value
      ; "goal_id", D.Shape.value
      ; "stream_id", D.Shape.value
      ; "nonce", D.Shape.value
      ; "next_prefix", D.Shape.value
      ; "occurrence", Occurrence.shape
      ; "cause", D.Shape.value
      ; "work", Cause.Allocation_work.shape
      ; "parent_event", D.Shape.value
      ; "caller_origin", D.Shape.value
      ; "child_invocation", D.Shape.value
      ; "admission_sha256", D.Shape.value
      ; "forwarding_sha256", D.Shape.value
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate raw ~limits:pending_limits |> domain in
    let%bind f = X.object_ raw in
    let%bind owner = X.required f "owner" P.Session_ref.of_json in
    let%bind goal_id = X.required f "goal_id" P.Id.Goal.of_json in
    let%bind stream_id =
      X.required f "stream_id" Cause.Key.of_json |> Result.map ~f:Cause.Key.to_string
    in
    let%bind nonce = X.required f "nonce" X.nonnegative_int64 in
    let%bind next_prefix = X.required f "next_prefix" X.nonnegative_int64 in
    let%bind occurrence = X.required f "occurrence" Occurrence.of_json in
    let%bind cause = X.required f "cause" Cause.Key.of_json in
    let%bind work = X.required f "work" Cause.Allocation_work.of_json in
    let%bind parent_event =
      X.required f "parent_event" P.Id.Moderator_execution.of_json
    in
    let%bind caller_origin =
      X.required f "caller_origin" (X.nullable P.Inference_origin.of_json)
    in
    let%bind child_invocation = X.required f "child_invocation" P.Id.Invocation.of_json in
    let%bind admission_sha256 =
      X.required f "admission_sha256" Cause.Key.of_json
      |> Result.map ~f:Cause.Key.to_string
    in
    let%bind forwarding_sha256 =
      P.Json_codec.optional_as f "forwarding_sha256" (fun raw ->
        Cause.Key.of_json raw |> Result.map ~f:Cause.Key.to_string)
    in
    let%bind () =
      match Occurrence.kind occurrence with
      | Attempt _ when Int64.(next_prefix > 0L && nonce >= next_prefix) -> Ok ()
      | Attempt _ | Host_turn _ ->
        invalid "causal pending requires positive Attempt prefix <= nonce"
    in
    Ok
      { owner
      ; goal_id
      ; stream_id
      ; nonce
      ; next_prefix
      ; occurrence
      ; cause
      ; work
      ; parent_event
      ; caller_origin
      ; child_invocation
      ; admission_sha256
      ; forwarding_sha256
      }
  ;;

  let capacity_projection t =
    let max_id = `String (String.make 96 'x') in
    let replace fields key value =
      List.map fields ~f:(fun (name, previous) ->
        name, if String.equal name key then value else previous)
    in
    match to_json t with
    | `Object fields ->
      `Object
        (fields
         |> fun f ->
         replace f "nonce" (X.int64_json Int64.max_value)
         |> fun f ->
         replace f "next_prefix" (X.int64_json Int64.max_value)
         |> fun f ->
         replace f "occurrence" (Occurrence.capacity_projection_for t.occurrence)
         |> fun f ->
         replace f "parent_event" max_id
         |> fun f ->
         replace f "child_invocation" max_id
         |> fun f ->
         replace
           f
           "caller_origin"
           (`Object
               [ "schema_version", `Number "1"; "type", `String "run"; "run_id", max_id ])
         |> fun f ->
         (* Async and Model attempts share this maximum encoded child size. *)
         replace
           f
           "work"
           (`Object
               [ "kind", `String "model_job_attempt"
               ; "id", max_id
               ; "attempt", X.host_counter_to_json Int.max_value
               ]))
    | _ -> assert false
  ;;
end

let validate_pending (pending : Pending.t) ~stream =
  let open Result.Let_syntax in
  let frame = Occurrence.frame pending.occurrence in
  let%bind (_ : Cause.Member.t) =
    Cause.Allocation_work.member
      pending.work
      ~generation:(Occurrence.Frame.generation frame)
  in
  let cause_key =
    Digestif.SHA256.(
      to_hex
        (digest_string
           (Jsonaf.to_string
              (`Object
                  [ "event", P.Id.Moderator_execution.to_json pending.parent_event
                  ; "callee", P.Session_ref.to_json (PC.Stream.callee stream)
                  ; ( "generation"
                    , X.host_counter_to_json (PC.Stream.callee_source stream).generation )
                  ]))))
  in
  if
    P.Session_ref.equal pending.owner (Cause.Goal_target.owner (PC.Stream.target stream))
    && P.Id.Goal.equal
         pending.goal_id
         (Cause.Goal_target.goal_id (PC.Stream.target stream))
    && String.equal pending.stream_id (PC.Stream.id stream)
    && P.Session_ref.equal (Occurrence.Frame.session frame) (PC.Stream.callee stream)
    && Int.equal
         (Occurrence.Frame.generation frame)
         (PC.Stream.callee_source stream).generation
    && String.equal cause_key (Cause.Key.to_string pending.cause)
    && Bool.equal
         (Option.is_some pending.forwarding_sha256)
         (Option.is_some (PC.Stream.forwarding_route stream))
  then Ok ()
  else invalid "causal pending differs from its exact stream or original event"
;;

module Request = struct
  let limits =
    D.Limits.create
      ~max_bytes:8388608
      ~max_depth:112
      ~max_fields:600000
      ~max_nodes:1200000
    |> domain
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> Result.ok_or_failwith
  ;;

  type t =
    { cause : Cause.t
    ; stream : PC.Stream.t
    ; work : Cause.Allocation_work.t
    ; occurrence : Occurrence.t
    }

  let stream t = t.stream
  let cause t = t.cause
  let work t = t.work
  let occurrence t = t.occurrence

  let pending t ~nonce ~next_prefix =
    let open Result.Let_syntax in
    let%bind forwarding_sha256 =
      match PC.Stream.forwarding_route t.stream, Cause.forwarding t.cause with
      | None, _ -> Ok None
      | Some route, Some forwarded ->
        let%bind witness =
          Cause.Forwarding.for_target forwarded ~target:(PC.Stream.target t.stream)
        in
        if Cause.Forwarding.Route.equal route (Cause.Forwarding.Witness.route witness)
        then Ok (Some (Cause.Forwarding.Witness.known_sha256 witness))
        else invalid "pending forwarding witness route differs"
      | Some _, None -> invalid "pending forwarding cause is absent"
    in
    let pending : Pending.t =
      { owner = Cause.Goal_target.owner (PC.Stream.target t.stream)
      ; goal_id = Cause.Goal_target.goal_id (PC.Stream.target t.stream)
      ; stream_id = PC.Stream.id t.stream
      ; nonce
      ; next_prefix
      ; occurrence = t.occurrence
      ; cause = Cause.key t.cause
      ; work = t.work
      ; parent_event = Cause.parent_event t.cause
      ; caller_origin = Cause.caller_origin t.cause
      ; child_invocation = Cause.child_invocation t.cause
      ; admission_sha256 = Cause.admission_sha256 t.cause
      ; forwarding_sha256
      }
    in
    let%bind.Result parsed = Pending.of_json (Pending.to_json pending) in
    let%map.Result () = validate_pending parsed ~stream:t.stream in
    parsed
  ;;

  let create cause ~target ~work ~occurrence =
    let%bind.Result stream = PC.Stream.for_target cause ~target in
    let t = { cause; stream; work; occurrence } in
    let%map.Result (_ : Pending.t) = pending t ~nonce:1L ~next_prefix:1L in
    t
  ;;

  let matches t receipt =
    match pending t ~nonce:receipt.Pending.nonce ~next_prefix:receipt.next_prefix with
    | Ok expected -> Pending.equal expected receipt
    | Error _ -> false
  ;;

  let to_json t =
    `Object
      [ "cause", Cause.to_json t.cause
      ; "target", Cause.Goal_target.to_json (PC.Stream.target t.stream)
      ; "work", Cause.Allocation_work.to_json t.work
      ; "occurrence", Occurrence.to_json t.occurrence
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate raw ~limits |> domain in
    let%bind f = X.object_ raw in
    let%bind cause = X.required f "cause" Cause.of_json in
    let%bind target = X.required f "target" Cause.Goal_target.of_json in
    let%bind work = X.required f "work" Cause.Allocation_work.of_json in
    let%bind occurrence = X.required f "occurrence" Occurrence.of_json in
    create cause ~target ~work ~occurrence
  ;;

  let shape =
    X.shape_exn
      [ "cause", Cause.shape
      ; "target", Cause.Goal_target.shape
      ; "work", Cause.Allocation_work.shape
      ; "occurrence", Occurrence.shape
      ]
  ;;
end

let confirms pending ~projection =
  let open Result.Let_syntax in
  let%bind () = validate_pending pending ~stream:(PC.Projection.stream projection) in
  match PC.Projection.head projection with
  | None -> invalid "causal confirmation has no canonical head"
  | Some head ->
    let witness = PC.Head.witness head in
    if
      Int64.equal pending.Pending.next_prefix (PC.Projection.admitted_attempts projection)
      && Occurrence.equal
           pending.occurrence
           (Goal_descendants.Canonical.Fact.occurrence (PC.Head.canonical head))
      && Cause.Key.equal pending.cause (PC.Head.cause head)
      && Cause.Allocation_work.equal pending.work (PC.Head.work head)
      && Cause.Key.equal pending.cause (PC.Head.Witness.cause witness)
      && Option.equal
           P.Inference_origin.equal
           pending.caller_origin
           (PC.Head.Witness.caller_origin witness)
      && P.Id.Invocation.equal
           pending.child_invocation
           (PC.Head.Witness.child_invocation witness)
      && String.equal pending.admission_sha256 (PC.Head.Witness.admission_sha256 witness)
      && Option.equal
           String.equal
           pending.forwarding_sha256
           (Option.map
              (PC.Head.Witness.forwarding witness)
              ~f:Cause.Forwarding.Witness.known_sha256)
    then Ok ()
    else
      invalid
        "causal confirmation does not match its exact pending occurrence and original"
;;
