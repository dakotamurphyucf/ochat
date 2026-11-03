open! Core
module P = Agent_protocol
module E = Recurrence_entry
module F = Recurrence_definition
module H = Recurrence_frontier
module Phase = Recurrence_phase
module Status = Recurrence_planning_status

type anchor =
  | Pending_adoption of Phase.Snapshot.t
  | Live of Phase.t

type binding =
  { definition_sha256 : string
  ; control_revision : int64
  ; cursor : Recurrence_cursor.t
  ; anchor : anchor
  }

type t =
  { owner : unit ref
  ; origin : Mtime.t
  ; mutable host : Recurrence_planning_host.t option
  ; mutable bindings : binding String.Map.t
  ; mutable active : unit ref option
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
  { owner = ref (); origin; host = None; bindings = String.Map.empty; active = None }
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

let source (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind observer = Moderator_checkpoint.observer state.moderator in
  match state.run_state, observer with
  | Some index, Some observer ->
    let installed = Run_state.installation index in
    (match installed.source with
     | Some current when P.Invocation.equal_observer current observer ->
       Run_source_installation.captured installed ~generation:state.identity.generation
     | Some _ | None -> conflict "recurrence source installation is unavailable")
  | None, _ | Some _, None -> conflict "recurrence source installation is unavailable"
;;

let binding_matches (binding : binding) entry =
  E.equal_control (E.control entry) Enabled
  && Int64.equal binding.control_revision (E.control_revision entry)
  && String.equal binding.definition_sha256 (F.digest (E.current entry))
  && Recurrence_cursor.equal binding.cursor (E.cursor entry)
;;

let retain_committed (t : t) state =
  let installed = source state |> Result.ok in
  t.bindings
  <- Map.filteri t.bindings ~f:(fun ~key ~data ->
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
