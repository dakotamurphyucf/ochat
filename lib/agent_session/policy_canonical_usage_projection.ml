open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Policy_canonical_usage_data
module Cause = Policy_cause_data
module Member = Cause.Member
module Work = Cause.Allocation_work
module Target = Cause.Goal_target
module Coverage = Cause.Coverage
module Context = Cause.Context
module L = Inference_ledger
module F = Canonical_usage_fold
module Canonical = Goal_descendants.Canonical
module Occurrence = Goal_descendant_occurrence
module U = Goal_usage
open Policy_canonical_usage_carrier

type t = Policy_canonical_usage_carrier.t

module Stream = Policy_canonical_usage_data.Stream

module Head = struct
  module Witness = struct
    type t =
      { cause : Cause.Key.t
      ; parent_event : P.Id.Moderator_execution.t
      ; caller_origin : P.Inference_origin.t option
      ; child_invocation : P.Id.Invocation.t
      ; admission_sha256 : string
      ; forwarding : Cause.Forwarding.Witness.t option
      }

    let forwarding t = t.forwarding
    let cause t = t.cause
    let caller_origin t = t.caller_origin
    let child_invocation t = t.child_invocation
    let admission_sha256 t = t.admission_sha256

    let of_json raw =
      let open Result.Let_syntax in
      let%bind fields = X.object_ raw in
      let%bind cause = X.required fields "cause" Cause.Key.of_json in
      let%bind parent_event =
        X.required fields "parent_event" P.Id.Moderator_execution.of_json
      in
      let%bind caller_origin =
        X.required fields "caller_origin" (X.nullable P.Inference_origin.of_json)
      in
      let%bind child_invocation =
        X.required fields "child_invocation" P.Id.Invocation.of_json
      in
      let%bind admission_sha256 =
        X.required fields "admission_sha256" Cause.Key.of_json
        |> Result.map ~f:Cause.Key.to_string
      in
      let%map forwarding =
        P.Json_codec.optional_as fields "forwarding" Cause.Forwarding.Witness.of_json
      in
      { cause
      ; parent_event
      ; caller_origin
      ; child_invocation
      ; admission_sha256
      ; forwarding
      }
    ;;

    let known_json t =
      `Object
        ([ "cause", Cause.Key.to_json t.cause
         ; "parent_event", P.Id.Moderator_execution.to_json t.parent_event
         ; "caller_origin", X.option_json P.Inference_origin.to_json t.caller_origin
         ; "child_invocation", P.Id.Invocation.to_json t.child_invocation
         ; "admission_sha256", `String t.admission_sha256
         ]
         @ Option.to_list
             (Option.map t.forwarding ~f:(fun forwarding ->
                "forwarding", Cause.Forwarding.Witness.known_json forwarding)))
    ;;

    let shape = witness_shape
  end

  type t =
    { witness : Witness.t
    ; value : Data.Head.t
    }

  let witness t = t.witness
  let canonical t = t.value.canonical
  let cause t = t.value.cause
  let work t = t.value.work

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind value = Data.Head.of_json raw in
    let%bind witness = X.required fields "witness" Witness.of_json in
    if Cause.Key.equal witness.cause value.cause
    then Ok { witness; value }
    else invalid "policy projection witness cause differs"
  ;;

  let known_json t =
    match Data.Head.to_json t.value with
    | `Object fields -> `Object (fields @ [ "witness", Witness.known_json t.witness ])
    | _ -> assert false
  ;;

  let shape = head_shape
end

module Projection = struct
  type t =
    { session_revision : int64
    ; stream : Stream.t
    ; admitted_attempts : int64
    ; usage : U.t
    ; head : Head.t
    ; raw : Jsonaf.t
    }

  let session_revision t = t.session_revision
  let stream t = t.stream
  let admitted_attempts t = t.admitted_attempts
  let usage t = t.usage
  let head t = Some t.head
  let to_json t = t.raw

  let known_json t =
    `Object
      [ "session_revision", X.int64_json t.session_revision
      ; "stream", Stream.known_json t.stream
      ; "admitted_attempts", X.int64_json t.admitted_attempts
      ; "usage", U.Persistence.to_json t.usage
      ; "head", Head.known_json t.head
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate raw ~limits:Data.projection_limits |> domain in
    let%bind fields = X.object_ raw in
    let%bind session_revision =
      X.required fields "session_revision" X.nonnegative_int64
    in
    let%bind stream = X.required fields "stream" Stream.of_json in
    let%bind admitted_attempts =
      X.required fields "admitted_attempts" X.nonnegative_int64
    in
    let%bind usage = X.required fields "usage" U.Persistence.of_json in
    let%bind head = X.required fields "head" Head.of_json in
    let fact = Head.canonical head in
    let frame = Occurrence.frame (Canonical.Fact.occurrence fact) in
    let witness = Head.witness head in
    let%bind () =
      match Stream.forwarding_route stream, Head.Witness.forwarding witness with
      | None, None -> Ok ()
      | Some route, Some forwarded ->
        if Cause.Forwarding.Route.equal route (Cause.Forwarding.Witness.route forwarded)
        then Ok ()
        else invalid "policy projection forwarding route/witness differs"
      | None, Some _ | Some _, None ->
        invalid "policy projection forwarding witness presence differs"
    in
    let expected_key =
      Data.digest
        (`Object
            [ "event", P.Id.Moderator_execution.to_json witness.parent_event
            ; "callee", P.Session_ref.to_json stream.callee
            ; "generation", X.host_counter_to_json stream.callee_source.generation
            ])
    in
    let%bind (_ : int64) = Data.ordinal fact in
    if
      Int64.(session_revision > 0L)
      && Int64.(Occurrence.Frame.candidate_session_revision frame <= session_revision)
      && P.Session_ref.equal (Occurrence.Frame.session frame) stream.callee
      && Int.equal (Occurrence.Frame.generation frame) stream.callee_source.generation
      && String.equal expected_key (Cause.Key.to_string witness.cause)
      && Int64.(admitted_attempts > 0L)
      && Int64.equal admitted_attempts (U.attempts usage)
    then Ok { session_revision; stream; admitted_attempts; usage; head; raw }
    else invalid "policy projection stream/head/count correspondence differs"
  ;;

  let shape = projection_shape
end

let projections accounting ~ledger ~session_revision =
  let open Result.Let_syntax in
  let%bind document = to_document accounting in
  let data = value accounting in
  let%bind actual_marks = marks ledger in
  let%bind () =
    if
      Int64.equal data.ledger_revision (L.revision ledger)
      && F.Marks.equal data.marks actual_marks
    then Ok ()
    else invalid "policy projection canonical frontier differs"
  in
  List.map data.streams ~f:(fun partition ->
    let%bind.Result usage =
      List.fold_result
        data.bindings
        ~init:partition.Data.Partition.settled
        ~f:(fun usage binding ->
          if not (Data.selected data binding partition.stream)
          then Ok usage
          else (
            let%bind row =
              Result.of_option
                (L.find ledger ~ordinal:binding.ordinal)
                ~error:
                  (P.Error.invalid_request
                     "policy projection selected canonical row absent")
            in
            U.add_record usage (L.Row.record row) |> Data.counter))
    in
    let%bind.Result raw =
      projection_raw
        accounting
        document
        partition
        ~session_revision
        ~usage:(U.Persistence.to_json usage)
    in
    Projection.of_json raw)
  |> Result.all
;;
