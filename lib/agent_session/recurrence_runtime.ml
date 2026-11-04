open! Core
module P = Agent_protocol
module E = Recurrence_entry
module F = Recurrence_definition
module H = Recurrence_frontier
module Phase = Recurrence_phase
module Status = Recurrence_planning_status

module Run_id = struct
  include P.Id.Run
  include Comparator.Make (P.Id.Run)
end

module Completion = struct
  type sample =
    { wall : P.Timestamp.t
    ; monotonic : Mtime.t
    }

  type timing =
    | Valid of sample
    | Invalid of P.Error.t

  type t =
    { origin_sha256 : string
    ; terminal : Recurrence_run_reference.t
    ; timing : timing
    }

  type plan =
    { owner : unit ref
    ; bindings : t Map.M(Run_id).t
    }

  let sample ~wall ~monotonic = { wall; monotonic }

  let timing_error t =
    match t.timing with
    | Valid _ -> None
    | Invalid error -> Some error
  ;;
end

type anchor =
  | Pending_adoption of Phase.Snapshot.t
  | Live of Phase.t

type binding =
  { definition_sha256 : string
  ; control_revision : int64
  ; cursor : Recurrence_cursor.t
  ; anchor : anchor
  }

module Completion_phase = struct
  type source =
    | Live_completion
    | Recovery_adoption

  type t =
    { owner : unit ref
    ; bindings : binding String.Map.t
    ; source : source
    }

  type preparation =
    | Prepared of t
    | Timing_unavailable of P.Error.t

  let source t = t.source
end

type grace_binding =
  { definition_sha256 : string
  ; source : P.Run_source.t
  ; birth : P.Session_lifecycle.Revision.t
  ; control_revision : int64
  ; anchor : Recurrence_selection_grace.t
  }

type t =
  { owner : unit ref
  ; origin : Mtime.t
  ; mutable host : Recurrence_planning_host.t option
  ; mutable bindings : binding String.Map.t
  ; mutable active : unit ref option
  ; mutable grace : grace_binding list
  ; mutable completions : Completion.t Map.M(Run_id).t
  }

type preparation =
  { owner : unit ref
  ; nonce : unit ref
  ; controller : P.Session_ref.t
  ; recurrence_id : P.Id.Recurrence.t
  ; definition : F.t
  ; control_revision : int64
  ; cursor : Recurrence_cursor.t
  ; frontier : H.t
  ; host : Recurrence_planning_host.t
  ; actor : Operator_authorization.t
  ; phase : Phase.t option
  ; snapshot : Phase.Snapshot.t
  }

type prepared =
  { preparation : preparation
  ; decision : Recurrence_due_plan.decision
  }

type validation =
  { prepared : prepared
  ; current : Recurrence_controller.observation
  ; delta : Session_delta.t option
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let create ~origin =
  { owner = ref ()
  ; origin
  ; host = None
  ; bindings = String.Map.empty
  ; active = None
  ; grace = []
  ; completions = Map.empty (module Run_id)
  }
;;

let set_host (t : t) host =
  match t.host with
  | Some _ -> conflict "recurrence planning host is already installed"
  | None ->
    t.host <- Some host;
    Ok ()
;;

let snapshot (t : t) ~wall ~monotonic =
  if Mtime.compare monotonic t.origin < 0
  then conflict "recurrence monotonic clock moved before its process origin"
  else (
    let elapsed_ns = Mtime.span t.origin monotonic |> Mtime.Span.to_uint64_ns in
    Phase.Snapshot.create ~wall:(P.Timestamp.to_time_ns wall) ~elapsed_ns)
;;

let terminal_receipts index =
  Run_state.receipts index
  |> List.filter ~f:(fun receipt -> P.Run_receipt.Kind.equal receipt.kind Terminal)
  |> List.map ~f:(fun receipt -> receipt.P.Run_receipt.run_id, receipt)
  |> Map.of_alist_multi (module Run_id)
;;

let retained_completions bindings (state : Session_state.t) =
  match Map.is_empty bindings, state.run_state with
  | true, _ -> bindings
  | false, None -> Map.empty (module Run_id)
  | false, Some index ->
    let receipts = terminal_receipts index in
    Map.filteri bindings ~f:(fun ~key ~data ->
      match
        ( Run_state.find index key
        , Run_state.recurrence_origin index key
        , Map.find receipts key )
      with
      | Some run, Some origin, Some [ receipt ] ->
        Int64.equal run.revision receipt.run_revision
        && String.equal
             data.Completion.origin_sha256
             (Recurrence_run_origin.digest origin)
        && P.Run_receipt.equal receipt (Recurrence_run_reference.receipt data.terminal)
      | _, _, _ -> false)
;;

let prepare_completions (t : t) ~(sample : Completion.sample) ~previous ~current =
  let open Result.Let_syntax in
  let timing =
    (* Truthful terminal facts still commit when a live clock is invalid. Keep
       that presence explicit so a later importer cannot choose wall recovery. *)
    if Mtime.compare sample.monotonic t.origin < 0
    then
      Completion.Invalid
        (P.Error.invalid_request "live completion clock precedes process origin")
    else (
      let elapsed = Mtime.span t.origin sample.monotonic |> Mtime.Span.to_uint64_ns in
      if Int64.(elapsed < 0L)
      then
        Completion.Invalid
          (P.Error.invalid_request "live completion clock exceeds signed range")
      else Completion.Valid sample)
  in
  let%map added =
    match current.Session_state.run_state with
    | None -> Ok []
    | Some index when List.is_empty (Run_state.recurrence_origins index) -> Ok []
    | Some index ->
      let receipts = terminal_receipts index in
      List.filter_map (Run_state.recurrence_origins index) ~f:(fun origin ->
        let id = Recurrence_run_origin.run_id origin in
        match Run_state.find index id with
        | None -> Some (conflict "recurrence completion lost its actual Run")
        | Some run ->
          (match run.lifecycle with
           | Admitted | Active | Waiting _ | Terminal Interrupted -> None
           | Terminal (Completed _ | Failed _ | Cancelled | Limited) ->
             let prior =
               Option.bind previous.Session_state.run_state ~f:(fun old ->
                 Run_state.find old id)
             in
             (match prior with
              | Some { lifecycle = Terminal _; _ } -> None
              | None | Some _ ->
                Some
                  (let%bind old_index =
                     Result.of_option
                       previous.run_state
                       ~error:
                         (P.Error.invalid_request "completion has no prior Run index")
                   in
                   let%bind old_run =
                     Result.of_option
                       prior
                       ~error:(P.Error.invalid_request "completion has no prior Run")
                   in
                   let%bind old_origin =
                     Result.of_option
                       (Run_state.recurrence_origin old_index id)
                       ~error:
                         (P.Error.invalid_request
                            "completion has no prior reciprocal origin")
                   in
                   let%bind () =
                     if Recurrence_run_origin.equal origin old_origin
                     then Ok ()
                     else conflict "completion changed its reciprocal origin"
                   in
                   let admission = Recurrence_run_origin.admission origin in
                   let%bind () =
                     Recurrence_run_origin.validate_run origin ~run:old_run ~admission
                   in
                   let%bind actual_admission =
                     Run_state.receipt
                       index
                       ~principal_id:admission.principal_id
                       ~key:admission.key
                       ~request_sha256:admission.request_sha256
                   in
                   let%bind () =
                     if Option.equal P.Run_receipt.equal actual_admission (Some admission)
                     then Ok ()
                     else conflict "completion lost its actual Admission receipt"
                   in
                   let%bind receipt =
                     match Map.find receipts id with
                     | Some [ receipt ] -> Ok receipt
                     | None | Some ([] | _ :: _ :: _) ->
                       conflict "completion requires one actual Terminal receipt"
                   in
                   let%bind terminal =
                     Recurrence_run_reference.create ~origin ~run ~receipt
                   in
                   let%map () =
                     if P.Timestamp.equal receipt.committed_at sample.wall
                     then Ok ()
                     else conflict "completion receipt differs from producer clock pair"
                   in
                   Completion.
                     { origin_sha256 = Recurrence_run_origin.digest origin
                     ; terminal
                     ; timing
                     }))))
      |> Result.all
  in
  let bindings =
    List.fold
      added
      ~init:(retained_completions t.completions current)
      ~f:(fun bindings binding ->
        Map.set
          bindings
          ~key:(Recurrence_run_reference.run_id binding.Completion.terminal)
          ~data:binding)
  in
  Completion.{ owner = t.owner; bindings }
;;

let install_completions (t : t) (plan : Completion.plan) =
  if not (phys_equal t.owner plan.owner)
  then invalid_arg "completion plan belongs to another runtime";
  t.completions <- plan.bindings
;;

let completion_matches binding ~origin ~terminal =
  let reference = binding.Completion.terminal in
  String.equal binding.origin_sha256 (Recurrence_run_origin.digest origin)
  && String.equal
       (Recurrence_run_reference.origin_sha256 reference)
       (Recurrence_run_reference.origin_sha256 terminal)
  && P.Run_receipt.equal
       (Recurrence_run_reference.receipt reference)
       (Recurrence_run_reference.receipt terminal)
  && Recurrence_run_reference.Outcome.equal
       (Recurrence_run_reference.outcome reference)
       (Recurrence_run_reference.outcome terminal)
;;

let completion (t : t) ~origin ~terminal =
  match Map.find t.completions (Recurrence_run_origin.run_id origin) with
  | None -> Ok None
  | Some binding ->
    (* The importer authenticates complete saved carriers against the actual
         Run first. Compatible reference annotations must not erase a live pair. *)
    if completion_matches binding ~origin ~terminal
    then Ok (Some binding)
    else conflict "present live completion differs from exact terminal evidence"
;;

let catalog (state : Session_state.t) =
  Option.value state.recurrence_catalog ~default:Recurrence_catalog.empty
;;

let ids state =
  Recurrence_catalog.entries (catalog state)
  |> List.filter_map ~f:(fun entry ->
    match E.control entry with
    | Enabled -> Some (E.id entry)
    | Paused _ | Retired _ -> None)
;;

let source = Recurrence_source.installed

let binding_matches (binding : binding) entry =
  E.equal_control (E.control entry) Enabled
  && Int64.equal binding.control_revision (E.control_revision entry)
  && String.equal binding.definition_sha256 (F.digest (E.current entry))
  && Recurrence_cursor.equal binding.cursor (E.cursor entry)
;;

let grace_matches (binding : grace_binding) entry =
  let definition = E.current entry in
  E.equal_control (E.control entry) Enabled
  && Int64.equal binding.control_revision (E.control_revision entry)
  && String.equal binding.definition_sha256 (F.digest definition)
  && P.Run_source.equal binding.source (F.source definition)
  && P.Session_lifecycle.Revision.equal
       binding.birth
       (F.birth_lifecycle_revision definition)
  &&
  match E.frontier entry with
  | Error _ -> false
  | Ok frontier ->
    let key = Recurrence_selection_grace.occurrence binding.anchor in
    List.exists (H.ready frontier) ~f:(fun nominal ->
      Recurrence_identity.Occurrence.equal key (Recurrence_nominal.key nominal))
    || List.exists (H.protected_children frontier) ~f:(fun row ->
      Recurrence_identity.Occurrence.equal
        key
        (Recurrence_child_reservation.occurrence row))
    || List.exists (H.executions frontier) ~f:(fun row ->
      Recurrence_execution.is_pending row
      && Recurrence_identity.Occurrence.equal
           key
           (Recurrence_execution_intent.occurrence (Recurrence_execution.intent row)))
;;

let retained_bindings bindings state =
  let installed = source state |> Result.ok in
  Map.filteri bindings ~f:(fun ~key ~data ->
    match
      List.find
        (Recurrence_catalog.entries (catalog state))
        ~f:(fun entry -> String.equal key (P.Id.Recurrence.to_string (E.id entry)))
    with
    | Some entry ->
      binding_matches data entry
      && Option.exists installed ~f:(P.Run_source.equal (F.source (E.current entry)))
    | None -> false)
;;

let retain_committed (t : t) state =
  t.completions <- retained_completions t.completions state;
  let installed = source state |> Result.ok in
  t.grace
  <- List.filter t.grace ~f:(fun binding ->
       List.exists
         (Recurrence_catalog.entries (catalog state))
         ~f:(fun entry ->
           grace_matches binding entry
           && Option.exists installed ~f:(P.Run_source.equal binding.source)));
  t.bindings <- retained_bindings t.bindings state
;;

let bind (t : t) entry anchor =
  t.bindings
  <- Map.set
       t.bindings
       ~key:(P.Id.Recurrence.to_string (E.id entry))
       ~data:
         { definition_sha256 = F.digest (E.current entry)
         ; control_revision = E.control_revision entry
         ; cursor = E.cursor entry
         ; anchor
         }
;;

let completion_candidate ~basis ~entry (state : Session_state.t) =
  let open Result.Let_syntax in
  let definition = E.current entry in
  let%bind () =
    match Recurrence_catalog.find (catalog state) (E.id entry) with
    | None -> conflict "completion phase entry is not retained"
    | Some retained ->
      (match E.to_document retained, E.to_document entry with
       | Ok retained, Ok incoming
         when Document_schema.Json.equal
                (Document_schema.Document.json retained)
                (Document_schema.Document.json incoming) -> Ok ()
       | Ok _, Ok _ | Error _, _ | _, Error _ ->
         conflict "completion phase differs from the exact candidate entry")
  in
  let%bind () =
    if
      P.Id.Session.equal
        state.identity.session_id
        (P.Session_ref.session_id (E.controller entry))
      && P.Session.equal_desired_state state.lifecycle.desired Running
      && (not state.halted)
      && Option.is_none state.failure
      && E.equal_control (E.control entry) Enabled
    then Ok ()
    else conflict "completion phase requires the enabled running controller"
  in
  let%bind installed = source state in
  let%bind () =
    if P.Run_source.equal installed (F.source definition)
    then Ok ()
    else conflict "completion phase source differs from the candidate definition"
  in
  let%bind frontier = E.frontier entry in
  let%bind () =
    match H.delay_basis frontier with
    | Some retained when Recurrence_delay_basis.equal retained basis -> Ok ()
    | Some _ | None -> conflict "completion phase requires the exact retained basis"
  in
  let%bind () = Recurrence_delay_basis.validate basis ~definition in
  let%bind expected = Recurrence_delay_basis.next_interval basis ~definition in
  match Recurrence_cursor.position (E.cursor entry) with
  | Interval cursor when Recurrence_rules.Cursor.Interval.equal expected cursor ->
    Ok cursor
  | Interval _ | Calendar _ | Awaiting_completion _ | Exhausted ->
    conflict "completion phase cursor differs from the receipt-derived successor"
;;

let completion_offset (t : t) (sample : Completion.sample) ~monotonic =
  if Mtime.compare sample.monotonic monotonic > 0
  then conflict "completion monotonic sample is later than the importing clock"
  else (
    let after_origin = Mtime.compare sample.monotonic t.origin >= 0 in
    let distance = Mtime.span t.origin sample.monotonic |> Mtime.Span.to_uint64_ns in
    if Int64.(distance < 0L)
    then conflict "completion monotonic distance exceeds the signed clock range"
    else Ok (if after_origin then distance else Int64.neg distance))
;;

let prepare_completion_phase (t : t) ~completion ~basis ~entry ~state ~wall ~monotonic =
  let open Result.Let_syntax in
  (* Reject contradictory facts before classifying any clock error as holdable.
     A process-local sample is never a substitute for the importer authenticating
     the saved complete reference against the actual target. *)
  let%bind cursor = completion_candidate ~basis ~entry state in
  let%bind () =
    match completion with
    | None -> Ok ()
    | Some binding ->
      if
        completion_matches
          binding
          ~origin:(Recurrence_delay_basis.origin basis)
          ~terminal:(Recurrence_delay_basis.terminal basis)
      then (
        match binding.timing with
        | Invalid _ -> Ok ()
        | Valid sample ->
          if P.Timestamp.equal sample.wall (Recurrence_delay_basis.completed_at basis)
          then Ok ()
          else conflict "completion sample wall differs from the terminal receipt")
      else conflict "completion sample differs from the exact successor evidence"
  in
  let prepare anchor (phase_source : Completion_phase.source) =
    let bindings =
      Map.set
        (retained_bindings t.bindings state)
        ~key:(P.Id.Recurrence.to_string (E.id entry))
        ~data:
          { definition_sha256 = F.digest (E.current entry)
          ; control_revision = E.control_revision entry
          ; cursor = E.cursor entry
          ; anchor
          }
    in
    Ok (Completion_phase.Prepared { owner = t.owner; bindings; source = phase_source })
  in
  match snapshot t ~wall ~monotonic with
  | Error error -> Ok (Completion_phase.Timing_unavailable error)
  | Ok snapshot ->
    (match completion with
     | None -> prepare (Pending_adoption snapshot) Recovery_adoption
     | Some { timing = Invalid error; _ } ->
       Ok (Completion_phase.Timing_unavailable error)
     | Some { timing = Valid sample; _ } ->
       (match completion_offset t sample ~monotonic with
        | Error error -> Ok (Completion_phase.Timing_unavailable error)
        | Ok completion_elapsed_ns ->
          (* The candidate facts and checked offset are now independent. Any
             remaining constructor contradiction is a rejection, not recovery. *)
          let%bind phase =
            Phase.from_completion
              (E.current entry)
              ~cursor
              ~completed_at:(P.Timestamp.to_time_ns sample.wall)
              ~completion_elapsed_ns
          in
          (match Phase.lateness_ns phase ~snapshot with
           | Error error -> Ok (Completion_phase.Timing_unavailable error)
           | Ok _ -> prepare (Live phase) Live_completion)))
;;

let install_completion_phase (t : t) (plan : Completion_phase.t) =
  if not (phys_equal t.owner plan.owner)
  then invalid_arg "completion phase plan belongs to another runtime";
  t.bindings <- plan.bindings
;;

let newly_enabled ~previous entry =
  match Recurrence_catalog.find (catalog previous) (E.id entry), E.control entry with
  | Some old, Enabled ->
    (match E.control old, F.schedule (E.current entry) with
     | Paused _, Interval _ -> true
     | (Enabled | Retired _), (Interval _ | Calendar _) | Paused _, Calendar _ -> false)
  | (Some _ | None), (Paused _ | Retired _) | None, Enabled -> false
;;

let needs_adoption ~previous ~current =
  List.exists (Recurrence_catalog.entries (catalog current)) ~f:(newly_enabled ~previous)
;;

let adopt_control t ~previous ~current ~snapshot =
  List.iter
    (Recurrence_catalog.entries (catalog current))
    ~f:(fun entry ->
      if newly_enabled ~previous entry then bind t entry (Pending_adoption snapshot))
;;

let proof_current actor =
  Operator_authorization.is_current actor
  && P.Principal.has_scope (Operator_authorization.principal actor) Send_messages
;;

let running (state : Session_state.t) =
  match state.lifecycle.desired with
  | Running -> true
  | Stopped -> false
;;

let host_authority (t : t) ~controller =
  match t.host with
  | None -> Error Status.Reason.Not_configured
  | Some host ->
    Recurrence_planning_host.authorize
      host
      ~controller:(Recurrence_controller.session controller)
    |> Result.map ~f:(fun actor -> host, actor)
;;

let status t ~state ~controller =
  let open Result.Let_syntax in
  match host_authority t ~controller with
  | Error reason -> Ok (Status.Unavailable reason)
  | Ok (_, actor) ->
    if not (running state)
    then Ok (Status.Unavailable Status.Reason.Controller_not_running)
    else (
      let%bind current = Recurrence_controller.observe controller state in
      if not (proof_current actor)
      then Ok (Status.Unavailable Status.Reason.Not_granted)
      else if not (Recurrence_controller.is_current current)
      then conflict "recurrence status lifecycle observation changed"
      else if not (Recurrence_controller.automatic current)
      then Ok (Status.Unavailable Status.Reason.Lifecycle_not_automatic)
      else (
        match source state with
        | Error _ -> Ok (Status.Unavailable Status.Reason.Source_unavailable)
        | Ok _ -> Ok Status.Available))
;;

let begin_due (t : t) ~state ~controller ~recurrence_id ~snapshot =
  let open Result.Let_syntax in
  match host_authority t ~controller with
  | Error _ -> Ok None
  | Ok (_, _) when not (running state) -> Ok None
  | Ok (host, actor) ->
    if Option.is_some t.active
    then conflict "recurrence sweep preparation is already owned"
    else (
      match Recurrence_catalog.find (catalog state) recurrence_id with
      | None -> Ok None
      | Some entry ->
        (match E.control entry with
         | Paused _ | Retired _ -> Ok None
         | Enabled ->
           let definition = E.current entry in
           let%bind installed = source state in
           let%bind current = Recurrence_controller.observe controller state in
           let%bind () =
             if
               proof_current actor
               && Recurrence_controller.is_current current
               && Recurrence_controller.automatic current
               && P.Session_ref.equal
                    (E.controller entry)
                    (Recurrence_controller.session controller)
               && P.Run_source.equal installed (F.source definition)
               && P.Session_lifecycle.Revision.equal
                    (Recurrence_controller.revision current)
                    (F.birth_lifecycle_revision definition)
             then Ok ()
             else
               conflict "recurrence current source, lifecycle or original grant changed"
           in
           let%bind frontier = E.frontier entry in
           let phase, snapshot =
             match Map.find t.bindings (P.Id.Recurrence.to_string recurrence_id) with
             | Some binding when binding_matches binding entry ->
               (match binding.anchor with
                | Pending_adoption adopted -> None, adopted
                | Live phase -> Some phase, snapshot)
             | Some _ | None -> None, snapshot
           in
           let nonce = ref () in
           let preparation =
             { owner = t.owner
             ; nonce
             ; controller = Recurrence_controller.session controller
             ; recurrence_id
             ; definition
             ; control_revision = E.control_revision entry
             ; cursor = E.cursor entry
             ; frontier
             ; host
             ; actor
             ; phase
             ; snapshot
             }
           in
           t.active <- Some nonce;
           Ok (Some preparation)))
;;

let prepare (preparation : preparation) =
  let open Result.Let_syntax in
  let zone =
    match F.schedule preparation.definition with
    | Interval _ -> None
    | Calendar rule ->
      (* Expected finite loader failure means honest missing data, never UTC.
         Cancellation/checkpoint exceptions are deliberately not caught. *)
      Recurrence_planning_host.load_zone
        preparation.host
        ~name:(Recurrence_rules.Rule.Calendar.zone rule)
      |> Result.ok
      |> Option.join
  in
  let%map decision =
    Recurrence_due_plan.prepare
      ~definition:preparation.definition
      ~cursor:preparation.cursor
      ~frontier:preparation.frontier
      ~phase:preparation.phase
      ~snapshot:preparation.snapshot
      ~zone
  in
  { preparation; decision }
;;

let owns (t : t) (preparation : preparation) =
  phys_equal t.owner preparation.owner
  && Option.exists t.active ~f:(phys_equal preparation.nonce)
;;

let basis_matches (preparation : preparation) state =
  match
    Recurrence_catalog.find (catalog state) preparation.recurrence_id, source state
  with
  | Some entry, Ok installed ->
    E.equal_control (E.control entry) Enabled
    && Int64.equal (E.control_revision entry) preparation.control_revision
    && String.equal (F.digest (E.current entry)) (F.digest preparation.definition)
    && P.Run_source.equal installed (F.source preparation.definition)
    && P.Session_ref.equal (E.controller entry) preparation.controller
    && Recurrence_cursor.equal (E.cursor entry) preparation.cursor
    &&
      (match E.frontier entry, H.to_document preparation.frontier with
      | Ok frontier, Ok original ->
        (match H.to_document frontier with
         | Ok current ->
           Document_schema.Json.equal
             (Document_schema.Document.json current)
             (Document_schema.Document.json original)
         | Error _ -> false)
      | Error _, _ | _, Error _ -> false)
  | None, _ | Some _, Error _ -> false
;;

let check_preparation t preparation state =
  if
    owns t preparation
    && running state
    && proof_current preparation.actor
    && basis_matches preparation state
  then Ok ()
  else conflict "recurrence prepared basis or original host authority changed"
;;

let validate t (prepared : prepared) ~state ~controller =
  let open Result.Let_syntax in
  let preparation = prepared.preparation in
  let%bind () = check_preparation t preparation state in
  let%bind current = Recurrence_controller.observe controller state in
  let%bind () = check_preparation t preparation state in
  let%bind () =
    if
      Recurrence_controller.is_current current
      && Recurrence_controller.automatic current
      && P.Session_ref.equal
           preparation.controller
           (Recurrence_controller.session controller)
      && P.Session_lifecycle.Revision.equal
           (Recurrence_controller.revision current)
           (F.birth_lifecycle_revision preparation.definition)
    then Ok ()
    else conflict "recurrence lifecycle changed during due preparation"
  in
  let%map delta =
    match prepared.decision with
    | Unchanged _ -> Ok None
    | Candidate plan ->
      Recurrence_catalog.apply_due
        (catalog state)
        ~recurrence_id:preparation.recurrence_id
        plan
      |> Result.map ~f:(fun catalog ->
        Some (Session_delta.Recurrence_catalog_changed catalog))
  in
  { prepared; current; delta }
;;

let delta (t : validation) = t.delta

let check_current t (validation : validation) ~state =
  let open Result.Let_syntax in
  let%bind () = check_preparation t validation.prepared.preparation state in
  if Recurrence_controller.is_current validation.current
  then Ok ()
  else conflict "recurrence lifecycle changed before due commit"
;;

let install_phase t (validation : validation) ~state =
  let prepared = validation.prepared in
  let id = prepared.preparation.recurrence_id in
  (match prepared.decision, Recurrence_catalog.find (catalog state) id with
   | Candidate plan, Some entry ->
     let definition = E.current entry in
     let added =
       List.map (Recurrence_due_plan.selection_grace plan) ~f:(fun anchor ->
         { definition_sha256 = F.digest definition
         ; source = F.source definition
         ; birth = F.birth_lifecycle_revision definition
         ; control_revision = E.control_revision entry
         ; anchor
         })
     in
     t.grace <- t.grace @ added
   | Unchanged _, _ | Candidate _, None -> ());
  let phase =
    match prepared.decision with
    | Unchanged phase -> phase
    | Candidate plan -> Recurrence_due_plan.next_phase plan
  in
  match phase, Recurrence_catalog.find (catalog state) id with
  | Some phase, Some entry -> bind t entry (Live phase)
  | None, _ | Some _, None ->
    t.bindings <- Map.remove t.bindings (P.Id.Recurrence.to_string id)
;;

let release t preparation = if owns t preparation then t.active <- None

let selection_lateness (t : t) ~entry ~nominal ~snapshot =
  match
    List.find t.grace ~f:(fun binding ->
      grace_matches binding entry
      && Recurrence_identity.Occurrence.equal
           (Recurrence_selection_grace.occurrence binding.anchor)
           (Recurrence_nominal.key nominal))
  with
  | Some binding ->
    Recurrence_selection_grace.lateness_ns binding.anchor ~snapshot
    |> Result.map ~f:Option.some
  | None ->
    (* No process-local selected anchor survives restart. The admitted immutable
       wall nominal is the explicit reconciliation basis, never fake elapsed time. *)
    let now = Phase.Snapshot.wall snapshot in
    let due = Recurrence_nominal.at nominal in
    if Time_ns.compare now due < 0
    then Ok None
    else
      Ok
        (Some
           Int64.(
             (Time_ns.to_int63_ns_since_epoch now |> Int63.to_int64)
             - (Time_ns.to_int63_ns_since_epoch due |> Int63.to_int64)))
;;
