open! Core
module P = Agent_protocol
module CO = Agent_session.Close_observation
module D = Agent_session.Cleanup_diagnostic
module R = Runtime_owner.Cleanup_recovery
module Original = Original_cleanup_report
module Observed = Native_cleanup_observation

type observation = (unit, P.Error.t) result CO.t

let unavailable message = P.Error.create Conflict ~message ~retryable:true ()
let current_domain () = (Stdlib.Domain.self () :> int)

module Control_role = struct
  type t =
    | Preparation
    | Cleanup
    | Unknown
end

module Own_post_callback_check = struct
  type t = { diagnostic : D.t }

  let diagnostic t = t.diagnostic

  (* This is producer metadata, not a new cancellation policy. Only the actual
     post-callback Fiber.check can issue this receipt. Its one selected control
     must not be a forwarded callback alias or a coarse multi-failure group. *)
  let of_unwind ~stage ~primary ~callback ~observed =
    match stage, primary, callback with
    | ( Some Runtime_owner.Lease_outcome.Primary_stage.After_callback
      , Runtime_owner.Scope_outcome.Raised (Eio.Cancel.Cancelled _, _)
      , Some callback ) ->
      (match CO.exit callback, CO.primary observed with
       | Returned (Ok ()), Some diagnostic ->
         (match D.failures diagnostic with
          | [ Agent_session.Callback_cleanup.Failure.Raised (Eio.Cancel.Cancelled _, _) ]
            when not (List.mem (CO.diagnostics callback) diagnostic ~equal:D.equal) ->
            Some { diagnostic }
          | _ -> None)
       | (Returned (Error _) | Raised _), _ | Returned (Ok ()), None -> None)
    | _ -> None
  ;;

  let eligible t ~selected ~excluded =
    D.equal t.diagnostic selected && not (List.mem excluded t.diagnostic ~equal:D.equal)
  ;;
end

module Control = struct
  type t =
    { role : Control_role.t
    ; observation : observation
    ; post_callback_check : Own_post_callback_check.t option
    }
end

module Snapshot = struct
  type t =
    { cleanup : (observation, P.Error.t) result
    ; fixed_controls : (observation list, P.Error.t) result
    ; cleanup_controls : (observation list, P.Error.t) result
    ; original_view : Cleanup_original_view.t
    ; custody_joined : bool
    ; own_post_callback_checks : Own_post_callback_check.t list
    }

  let cleanup t = t.cleanup
  let fixed_controls t = t.fixed_controls
  let cleanup_controls t = t.cleanup_controls
  let original_view t = t.original_view
  let custody_joined t = t.custody_joined
  let own_post_callback_checks t = t.own_post_callback_checks
end

module Retry = struct
  type t =
    | Not_started of P.Error.t
    | Completed of Snapshot.t
    | Stopped of
        { refusal : P.Error.t
        ; current : Snapshot.t
        }
end

module Edge = struct
  type t =
    { owner : Runtime_owner.t
    ; recovery : R.t
    ; mutable control : (Control.t option, P.Error.t) result option
    }
end

type phase =
  | Fresh
  | Original_running
  | Complete of Snapshot.t
  | Retrying of Snapshot.t

type t =
  { domain : int
  ; graph : Independent_cleanup.t
  ; mutable edges : Edge.t list
  ; mutable controls : (Control.t option, P.Error.t) result list
  ; mutable selections : (Control.t option, P.Error.t) result list
  ; mutable phase : phase
  }

let create_with_graph graph =
  { domain = current_domain ()
  ; graph
  ; edges = []
  ; controls = []
  ; selections = []
  ; phase = Fresh
  }
;;

let create () = create_with_graph (Independent_cleanup.create ())

let check_domain t =
  if Int.equal t.domain (current_domain ())
  then Ok ()
  else Error (P.Error.invalid_request "independent cleanup belongs to another domain")
;;

let check_domain_exn t =
  if not (Int.equal t.domain (current_domain ()))
  then invalid_arg "Ancestry_cleanup: foreign creating domain"
;;

let check_open t =
  check_domain_exn t;
  match t.phase with
  | Original_running -> ()
  | Fresh | Complete _ | Retrying _ ->
    invalid_arg "Ancestry_cleanup: original registration closed"
;;

let graph t =
  check_domain_exn t;
  t.graph
;;

let snapshot t =
  Result.map (check_domain t) ~f:(fun () ->
    match t.phase with
    | Fresh | Original_running -> None
    | Complete snapshot | Retrying snapshot -> Some snapshot)
;;

let begin_original t =
  check_domain_exn t;
  match t.phase with
  | Fresh -> t.phase <- Original_running
  | Original_running | Complete _ | Retrying _ ->
    invalid_arg "Ancestry_cleanup: original bracket already entered"
;;

let capture_lease t ~owner recovery =
  check_open t;
  let edge = { Edge.owner; recovery; control = None } in
  t.edges <- edge :: t.edges;
  edge
;;

let finish_capture t ~owner ~captured ~actual =
  check_open t;
  match captured with
  | None -> capture_lease t ~owner actual
  | Some edge ->
    if
      (not (List.exists t.edges ~f:(phys_equal edge)))
      || (not (phys_equal edge.Edge.owner owner))
      || not (phys_equal edge.recovery actual)
    then invalid_arg "Ancestry_cleanup: returned lease differs from original capture";
    edge
;;

let check_edge t edge =
  check_open t;
  if not (List.exists t.edges ~f:(phys_equal edge))
  then invalid_arg "Ancestry_cleanup: foreign lease edge";
  if Option.is_some edge.Edge.control
  then invalid_arg "Ancestry_cleanup: lease edge recorded twice"
;;

let meaningful_primary observed =
  match CO.primary observed with
  | Some primary when not (List.is_empty (D.failures primary)) -> Ok primary
  | None | Some _ ->
    Error (unavailable "independent scope control has no selected origin")
;;

let retain_control t control =
  (* Encounter order belongs to the original view. Selection priority is a
     separate structural stack: only a newly selected actual own control
     overrides the callback it replaced at this very unwind boundary. *)
  t.controls <- t.controls @ [ control ];
  match control with
  | Ok None -> ()
  | Ok (Some actual) when not (Observed.is_negative actual.Control.observation) ->
    t.selections <- t.selections @ [ control ]
  | Ok (Some _) | Error _ -> t.selections <- control :: t.selections
;;

let publish_edge t edge control =
  edge.Edge.control <- Some control;
  retain_control t control
;;

let lease_control_role outcome =
  let module Stage = Runtime_owner.Lease_outcome.Primary_stage in
  match Runtime_owner.Lease_outcome.primary_stage outcome with
  | Some (Stage.Admission | Before_callback | Scope_exit_before_entry) ->
    Control_role.Preparation
  | Some (Admission_batch | After_callback) -> Cleanup
  | Some (Callback | Scope_exit) | None -> Unknown
;;

let record_lease t edge ~callback ~outcome =
  check_edge t edge;
  let observed = Runtime_owner.Lease_outcome.observation outcome in
  let primary =
    Runtime_owner.Lease_outcome.outcome outcome |> Runtime_owner.Scope_outcome.primary
  in
  let callback_primary =
    match callback with
    | Some observed when Observed.is_negative observed ->
      Result.map (meaningful_primary observed) ~f:Option.some
    | None | Some _ -> Ok None
  in
  let control =
    match primary with
    | Returned (Ok ()) -> Ok None
    | Returned (Error _) | Raised _ ->
      let open Result.Let_syntax in
      let%bind callback_primary = callback_primary in
      let%map selected = meaningful_primary observed in
      (match callback_primary with
       | Some callback_selected when D.equal selected callback_selected -> None
       | None | Some _ ->
         Some
           { Control.role = lease_control_role outcome
           ; observation = observed
           ; post_callback_check =
               Own_post_callback_check.of_unwind
                 ~stage:(Runtime_owner.Lease_outcome.primary_stage outcome)
                 ~primary
                 ~callback
                 ~observed
           })
  in
  publish_edge t edge control
;;

let record_legacy_lease t edge ~outcome =
  check_edge t edge;
  let control =
    match
      Runtime_owner.Lease_outcome.outcome outcome |> Runtime_owner.Scope_outcome.primary
    with
    | Returned (Ok _) -> Ok None
    | Returned (Error _) | Raised _ ->
      Error (unavailable "legacy independent scope has no observed callback partition")
  in
  publish_edge t edge control
;;

let record_control t ~role observed =
  check_open t;
  let control =
    if Observed.is_negative observed
    then
      Result.map (meaningful_primary observed) ~f:(fun _ ->
        Some { Control.role; observation = observed; post_callback_check = None })
    else Ok (Some { Control.role; observation = observed; post_callback_check = None })
  in
  retain_control t control
;;

let make_snapshot t =
  let open Result.Let_syntax in
  let graph_report = Independent_cleanup.report t.graph in
  let graph_cleanup =
    match graph_report with
    | None -> Error (unavailable "independent graph original scope is unfinished")
    | Some report -> Independent_cleanup.Report.observation report
  in
  let cleanup =
    let%bind initial = graph_cleanup in
    List.fold_result t.edges ~init:initial ~f:(fun collected edge ->
      let%bind latest = R.snapshot edge.Edge.recovery in
      let%bind latest =
        Result.of_option
          latest
          ~error:(unavailable "independent ancestor original scope is unfinished")
      in
      let%map observed = R.Snapshot.observation latest in
      Observed.combine collected observed)
  in
  let controls ~include_preparation =
    let%bind () =
      if List.for_all t.edges ~f:(fun edge -> Option.is_some edge.Edge.control)
      then Ok ()
      else Error (unavailable "independent ancestor unwind partition is unfinished")
    in
    List.fold_result t.selections ~init:[] ~f:(fun collected control ->
      let%map control = control in
      match control with
      | None -> collected
      | Some control ->
        let retain =
          match control.Control.role with
          | Preparation -> include_preparation
          | Cleanup | Unknown -> true
        in
        if retain then collected @ [ control.observation ] else collected)
  in
  let fixed_controls = controls ~include_preparation:true in
  let cleanup_controls = controls ~include_preparation:false in
  let graph_joined =
    Option.exists graph_report ~f:(fun report ->
      match Independent_cleanup.Report.status report with
      | Never_entered | Joined -> true
      | Failed | Incomplete -> false)
  in
  let custody_joined =
    graph_joined
    && List.for_all t.edges ~f:(fun edge ->
      match R.snapshot edge.Edge.recovery with
      | Ok (Some snapshot) ->
        R.Snapshot.equal_progress (R.Snapshot.progress snapshot) Joined
      | Ok None | Error _ -> false)
  in
  let graph_original = Independent_cleanup.original_report t.graph in
  let original_pieces =
    Option.value_map graph_original ~default:[] ~f:(fun report ->
      List.map (Independent_cleanup.Report.diagnostics report) ~f:(fun diagnostic ->
        Original.Piece.Own diagnostic))
  in
  let graph_original_complete =
    Option.exists graph_original ~f:(fun report ->
      match Independent_cleanup.Report.release_report report with
      | None ->
        Independent_cleanup.Report.equal_status
          (Independent_cleanup.Report.status report)
          Never_entered
      | Some release ->
        (match Eio.Switch.Release_report.status release with
         | Complete -> true
         | Not_entered | Incomplete -> false))
  in
  let original_pieces, originals_complete =
    List.fold
      t.edges
      ~init:(original_pieces, graph_original_complete)
      ~f:(fun (pieces, complete) edge ->
        match R.original_view edge.Edge.recovery with
        | Ok (Some child) -> pieces @ [ Original.Piece.Child child ], complete
        | Ok None | Error _ -> pieces, false)
  in
  let original_pieces =
    List.fold t.controls ~init:original_pieces ~f:(fun pieces -> function
      | Ok (Some control) ->
        pieces
        @ List.map (CO.diagnostics control.Control.observation) ~f:(fun diagnostic ->
          Original.Piece.Dependency_copy diagnostic)
      | Ok None | Error _ -> pieces)
  in
  { Snapshot.cleanup
  ; fixed_controls
  ; cleanup_controls
  ; custody_joined
  ; own_post_callback_checks =
      List.filter_map t.controls ~f:(function
        | Ok (Some control) -> control.Control.post_callback_check
        | Ok None | Error _ -> None)
  ; original_view =
      Original.create
        ~diagnostics_complete:
          (originals_complete
           && List.for_all t.controls ~f:Result.is_ok
           && List.for_all t.edges ~f:(fun edge -> Option.is_some edge.Edge.control))
        original_pieces
  }
;;

let seal_observed t =
  check_open t;
  let finished =
    Observed.capture (fun () ->
      Independent_cleanup.finish_without_entry t.graph;
      Ok ())
  in
  if Observed.is_negative finished then record_control t ~role:Cleanup finished;
  t.phase <- Complete (make_snapshot t);
  finished
;;

let seal t = ignore (CO.resume (seal_observed t) : (unit, P.Error.t) result)

let finish_without_entry t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase with
  | Complete snapshot -> Ok snapshot
  | Original_running | Retrying _ ->
    Error (unavailable "independent original or retry is running")
  | Fresh ->
    (* This changes only the private registration gate. The actual graph's
       finish_without_entry still validates its own no-entry state; no entered
       Switch or lease receipt is fabricated. Both paths use the same seal. *)
    t.phase <- Original_running;
    ignore (seal_observed t : observation);
    (match t.phase with
     | Complete snapshot -> Ok snapshot
     | Fresh | Original_running | Retrying _ ->
       invalid_arg "Ancestry_cleanup: completed no-entry seal was not published")
;;

let retry_observed t =
  match check_domain t with
  | Error error -> Retry.Not_started error
  | Ok () ->
    (match t.phase with
     | Fresh | Original_running ->
       Retry.Not_started (unavailable "independent original scope is running")
     | Retrying _ -> Retry.Not_started (unavailable "independent cleanup retry is busy")
     | Complete previous ->
       t.phase <- Retrying previous;
       let progressed = ref false in
       let publish () =
         let current = make_snapshot t in
         t.phase <- Complete current;
         current
       in
       let execute () =
         let open Result.Let_syntax in
         let%bind () =
           match Independent_cleanup.retry_status t.graph with
           | Original_running | Busy ->
             Error (unavailable "independent graph retry is unavailable")
           | Joined | Fixed -> Ok ()
           | Retryable ->
             let%map _ = Independent_cleanup.retry_observed t.graph in
             progressed := true
         in
         List.fold_result t.edges ~init:() ~f:(fun () edge ->
           let%bind latest = R.snapshot edge.Edge.recovery in
           let%bind latest =
             Result.of_option
               latest
               ~error:(unavailable "independent ancestor original scope is running")
           in
           match R.Snapshot.progress latest with
           | Joined | Blocked | Terminal -> Ok ()
           | Retryable ->
             let%map _ = R.retry edge.recovery ~owner:edge.owner ~after:latest in
             progressed := true)
       in
       (match execute () with
        | Ok () -> Retry.Completed (publish ())
        | Error refusal ->
          let current = publish () in
          if !progressed then Retry.Stopped { refusal; current } else Not_started refusal
        | exception exn ->
          let trace = Stdlib.Printexc.get_raw_backtrace () in
          ignore (publish () : Snapshot.t);
          Exn.raise_with_original_backtrace exn trace))
;;

let%expect_test "normalized own-check delivery requires singleton original provenance" =
  let module F = Agent_session.Callback_cleanup.Failure in
  let module Stage = Runtime_owner.Lease_outcome.Primary_stage in
  let recording = Stdlib.Printexc.backtrace_status () in
  let cancellation, trace =
    Exn.protect
      ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
      ~f:(fun () ->
        Stdlib.Printexc.record_backtrace true;
        try raise (Eio.Cancel.Cancelled Exit) with
        | exn -> exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  assert (not (String.is_empty (Stdlib.Printexc.raw_backtrace_to_string trace)));
  let payload = [ F.Raised (cancellation, trace) ] in
  let primary = Runtime_owner.Scope_outcome.Raised (cancellation, trace) in
  let positive groups = CO.create (Returned (Ok ())) ~diagnostics:groups () in
  let negative group =
    CO.create (Raised (cancellation, trace)) ~diagnostics:[ group ] ~primary:group ()
  in
  let classify ?(stage = Some Stage.After_callback) ?(callback_groups = []) group =
    Own_post_callback_check.of_unwind
      ~stage
      ~primary
      ~callback:(Some (positive callback_groups))
      ~observed:(negative group)
  in
  let require_pending group =
    match D.take_legacy_diagnostic_exn [ group ] with
    | Some (actual, _) -> assert (D.equal group actual)
    | None -> failwith "metadata inspection consumed a rejected control"
  in
  let selected = D.create payload in
  let independent = D.create payload in
  let receipt = classify selected |> Option.value_exn in
  assert (Own_post_callback_check.eligible receipt ~selected ~excluded:[]);
  assert (
    not (Own_post_callback_check.eligible receipt ~selected:independent ~excluded:[]));
  assert (D.equal selected (Own_post_callback_check.diagnostic receipt));
  ignore (D.take_legacy_diagnostic_exn [ selected ] : (D.t * F.t) option);
  assert (Option.is_none (D.take_legacy_diagnostic_exn [ selected ]));
  require_pending independent;
  (match D.failures selected with
   | [ Raised (actual, original_trace) ] ->
     assert (phys_equal cancellation actual && phys_equal trace original_trace)
   | _ -> failwith "acknowledgment changed the immutable original diagnostic");
  let missing_stage = D.create payload in
  assert (Option.is_none (classify ~stage:None missing_stage));
  require_pending missing_stage;
  let callback_alias = D.create payload in
  assert (Option.is_none (classify ~callback_groups:[ callback_alias ] callback_alias));
  require_pending callback_alias;
  let local_alias = D.create payload in
  let receipt = classify local_alias |> Option.value_exn in
  assert (
    not
      (Own_post_callback_check.eligible
         receipt
         ~selected:local_alias
         ~excluded:[ local_alias ]));
  require_pending local_alias;
  let mixed = D.create (payload @ [ F.Protocol (P.Error.invalid_request "hook") ]) in
  assert (Option.is_none (classify mixed));
  require_pending mixed;
  let other_exception = D.create [ F.Raised (Exit, trace) ] in
  assert (Option.is_none (classify other_exception));
  require_pending other_exception;
  print_endline
    "singleton own check only; aliases, mixed groups and missing stage stay pending";
  [%expect
    {| singleton own check only; aliases, mixed groups and missing stage stay pending |}]
;;
