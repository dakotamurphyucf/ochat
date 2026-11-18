open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Goal_policy_contributions_data
module R = Goal_policy_receipt
module PC = Policy_canonical_usage
module U = Goal_usage

let domain = R.domain

let replace raw key replacement =
  match raw with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) ->
         name, if String.equal name key then replacement else value))
  | _ -> assert false
;;

let field raw name =
  match D.Json.field raw ~name with
  | Value value -> value
  | Absent | Null -> `Null
;;

let max_id = `String (String.make 96 'x')

(* Model and Async attempt tags have the same encoded length and child shape. *)
let maximum_work =
  `Object
    [ "kind", `String "model_job_attempt"
    ; "id", max_id
    ; "attempt", X.host_counter_to_json Int.max_value
    ]
;;

let work_future work =
  let raw = Policy_cause_data.Allocation_work.to_json work in
  let raw = replace raw "id" max_id in
  match work with
  | Event _ | Invocation _ -> raw
  | Model_job_attempt _ | Async_job_attempt _ ->
    replace raw "attempt" (X.host_counter_to_json Int.max_value)
;;

let pending_future pending =
  R.Pending.capacity_projection pending
  |> fun raw -> replace raw "work" (work_future (R.Pending.work pending))
;;

let maximum_projection stream pending =
  let witness =
    `Object
      [ "cause", Policy_cause_data.Key.to_json (R.Pending.cause pending)
      ; "parent_event", max_id
      ; ( "caller_origin"
        , `Object
            [ "schema_version", `Number "1"; "type", `String "run"; "run_id", max_id ] )
      ; "child_invocation", max_id
      ; "admission_sha256", `String (String.make 64 'f')
      ]
  in
  `Object
    [ "session_revision", X.int64_json Int64.max_value
    ; "stream", PC.Stream.known_json stream
    ; "admitted_attempts", X.int64_json Int64.max_value
    ; "usage", U.Persistence.capacity_projection U.empty
    ; ( "head"
      , `Object
          [ "cause", Policy_cause_data.Key.to_json (R.Pending.cause pending)
          ; "work", maximum_work
          ; ( "canonical"
            , `Object
                [ ( "occurrence"
                  , Goal_descendant_occurrence.capacity_projection_for
                      (R.Pending.occurrence pending) )
                ; "tracking", `String "untracked_protected_future_data"
                ; "operation", `Null
                ] )
          ; "witness", witness
          ] )
    ]
;;

let projection_future projection =
  let raw = PC.Projection.known_json projection in
  let head = Option.value_exn (PC.Projection.head projection) in
  let head_raw = field raw "head" in
  let canonical = field head_raw "canonical" in
  let canonical =
    replace
      canonical
      "occurrence"
      (Goal_descendant_occurrence.capacity_projection_for
         (Goal_descendants.Canonical.Fact.occurrence (PC.Head.canonical head)))
  in
  let canonical =
    replace canonical "tracking" (`String "untracked_protected_future_data")
  in
  let witness = field head_raw "witness" in
  let witness =
    replace witness "parent_event" max_id
    |> fun raw ->
    replace raw "child_invocation" max_id
    |> fun raw ->
    replace
      raw
      "caller_origin"
      (`Object [ "schema_version", `Number "1"; "type", `String "run"; "run_id", max_id ])
  in
  let head_raw =
    replace head_raw "canonical" canonical
    |> fun raw ->
    replace raw "work" (work_future (PC.Head.work head))
    |> fun raw -> replace raw "witness" witness
  in
  replace raw "session_revision" (X.int64_json Int64.max_value)
  |> fun raw ->
  replace raw "admitted_attempts" (X.int64_json Int64.max_value)
  |> fun raw ->
  replace raw "usage" (U.Persistence.capacity_projection (PC.Projection.usage projection))
  |> fun raw -> replace raw "head" head_raw
;;

let slot_future (slot : Data.Slot.t) =
  let pending =
    match slot.pending, slot.last with
    | Some pending, _ -> pending
    | None, Some last -> Data.Last.pending last
    | None, None -> assert false
  in
  let last_pending = Option.value_map slot.last ~default:pending ~f:Data.Last.pending in
  let raw = Data.Slot.to_json slot in
  replace raw "confirmed_prefix" (X.int64_json Int64.max_value)
  |> fun raw ->
  replace raw "last_nonce" (X.int64_json Int64.max_value)
  |> fun raw ->
  replace raw "pending" (pending_future pending)
  |> fun raw ->
  replace
    raw
    "last"
    (`Object
        [ "kind", `String "no_writer_closed"; "pending", pending_future last_pending ])
  |> fun raw ->
  replace
    raw
    "projection"
    (Option.value_map
       slot.projection
       ~default:(maximum_projection slot.stream pending)
       ~f:projection_future)
;;

let probe_codec =
  X.codec_exn
    ~version:1
    ~limits:Data.limits
    ~kind:"ochat.goal_policy_contributions"
    ~shape:Data.shape
    ~decode:Result.return
    ~encode:Result.return
;;

(* Pending carries only an exact occurrence digest, not the full future frame
   vector. A forwarded reservation therefore holds the entire EXISTING projection
   profile until confirmed/closed, including either Caller_source envelope and
   its actual nesting. Source identities are fixed by Stream/Frame identity;
   changing a source is not unchecked growth of a retained child. Subtract only
   the actual preserved projection already included in the future document,
   never a lossy native reconstruction. *)
let forwarded_projection_reservation (value : Data.t) future =
  let open Result.Let_syntax in
  let projection_limits = Policy_canonical_usage_data.projection_limits in
  let%bind maximum =
    Capacity_growth.create
      ~bytes:(D.Limits.max_bytes projection_limits)
      ~fields:(D.Limits.max_fields projection_limits)
      ~nodes:(D.Limits.max_nodes projection_limits)
  in
  let%bind fields = X.object_ (D.Document.payload future) in
  let%bind books = X.required fields "books" (X.list X.raw) in
  let%bind raw_slots =
    List.map books ~f:(fun book ->
      let%bind fields = X.object_ book in
      X.required fields "slots" (X.list X.raw))
    |> Result.all
    |> Result.map ~f:List.concat
  in
  let%bind growth, has_pending =
    List.fold_result value.books ~init:(Capacity_growth.zero, false) ~f:(fun state book ->
      List.fold_result
        book.Data.Book.slots
        ~init:state
        ~f:(fun (growth, has_pending) slot ->
          match slot.Data.Slot.pending with
          | Some pending when Option.is_some (R.Pending.forwarding_sha256 pending) ->
            let%bind raw =
              List.find raw_slots ~f:(fun raw ->
                match D.Json.field raw ~name:"id" with
                | Value (`String id) -> String.equal id (PC.Stream.id slot.stream)
                | Absent | Null | Value _ -> false)
              |> Result.of_option
                   ~error:
                     (Agent_protocol.Error.invalid_request
                        "forwarded capacity slot is absent")
            in
            let projection = field raw "projection" in
            let%bind accounted =
              Capacity_growth.measure projection ~limits:projection_limits
            in
            let extra = Capacity_growth.positive_difference maximum ~previous:accounted in
            let%bind remaining =
              Capacity_growth.remaining_limits extra Data.slot_limits
            in
            let%bind () = D.Json.validate raw ~limits:remaining |> domain in
            let%bind () =
              Capacity_depth.validate
                (D.Limits.max_depth projection_limits)
                ~ancestors:1
                ~limits:Data.slot_limits
            in
            let%map growth = Capacity_growth.add growth extra in
            growth, true
          | None | Some _ -> Ok (growth, has_pending)))
  in
  (* Document.payload.books[].slots[].projection has six enclosing edges. *)
  let depth = if has_pending then D.Limits.max_depth projection_limits + 6 else 0 in
  let%map () =
    if has_pending
    then
      Capacity_depth.validate
        (D.Limits.max_depth projection_limits)
        ~ancestors:6
        ~limits:Data.limits
    else Ok ()
  in
  growth, depth
;;

let capture (value : Data.t) ~current =
  let open Result.Let_syntax in
  let%bind carrier = D.Domain_codec.decode probe_codec current |> domain in
  let raw = Data.to_json value in
  let books =
    List.map value.books ~f:(fun book ->
      replace
        (Data.Book.to_json book)
        "slots"
        (`Array (List.map book.Data.Book.slots ~f:slot_future)))
  in
  let future = replace raw "books" (`Array books) in
  let%bind future =
    D.Domain_codec.encode probe_codec (C.with_value carrier future) |> domain
  in
  let%bind () = Data.validate_original_children (D.Document.payload future) in
  let%bind before =
    Capacity_growth.measure (D.Document.json current) ~limits:Data.limits
  in
  let%bind after = Capacity_growth.measure (D.Document.json future) ~limits:Data.limits in
  let%bind depth = Capacity_depth.measure (D.Document.json future) ~limits:Data.limits in
  let growth = Capacity_growth.positive_difference after ~previous:before in
  let%bind forwarded_growth, forwarded_depth =
    forwarded_projection_reservation value future
  in
  let%bind growth = Capacity_growth.add growth forwarded_growth in
  let depth = Int.max depth forwarded_depth in
  let%bind maximum = Capacity_growth.measure maximum_work ~limits:Data.limits in
  (* Three independently changing work coordinates: future Pending, last receipt
     and latest projection head. Extra known work form growth is additive; raw
     unknowns remain in their original semantic shape and actual edits still
     reject a protected alternative instead of dropping it. *)
  let%map growth =
    List.fold_result value.books ~init:growth ~f:(fun growth book ->
      List.fold_result book.Data.Book.slots ~init:growth ~f:(fun growth _slot ->
        List.fold_result [ (); (); () ] ~init:growth ~f:(fun growth () ->
          Capacity_growth.add growth maximum)))
  in
  growth, depth
;;
