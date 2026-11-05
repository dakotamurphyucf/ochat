open! Core
module P = Agent_protocol
module A = Agent_session
module Actor = A.Session_actor
module Host = Recurrence_execution_host
module Service = Recurrence_run_service
module Occurrence = A.Recurrence_identity.Occurrence
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Frontier = A.Recurrence_frontier
module Entry = A.Recurrence_entry

module Live = struct
  type t =
    { intent : Intent.t
    ; attempt : Attempt.t
    ; result : Service.Attempt.t
    }
end

module Cursor = struct
  type t =
    { entry : Session_registry.entry
    ; mutable after : Occurrence.t option
    ; mutable live : Live.t list
    }
end

type t =
  { factory : Session_factory.t
  ; registry : Session_registry.t
  ; host : Host.t
  ; domain : int
  ; mutable cursors : Cursor.t list
  ; mutable in_flight : P.Session_ref.t list
  }

let create ~factory ~registry ~host =
  { factory
  ; registry
  ; host
  ; domain = (Stdlib.Domain.self () :> int)
  ; cursors = []
  ; in_flight = []
  }
;;

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let cursor t entry =
  t.cursors
  <- List.filter t.cursors ~f:(fun cursor ->
       List.mem
         (Session_registry.entries t.registry)
         cursor.Cursor.entry
         ~equal:phys_equal);
  match List.find t.cursors ~f:(fun cursor -> phys_equal cursor.Cursor.entry entry) with
  | Some cursor -> cursor
  | None ->
    let cursor = { Cursor.entry; after = None; live = [] } in
    t.cursors <- cursor :: t.cursors;
    cursor
;;

let close t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Recurrence_dispatcher.close: foreign domain";
  t.cursors <- []
;;

let find_live (cursor : Cursor.t) occurrence =
  List.find cursor.Cursor.live ~f:(fun live ->
    Occurrence.equal (Intent.occurrence live.Live.intent) occurrence)
;;

let remove_live (cursor : Cursor.t) occurrence =
  cursor.Cursor.live
  <- List.filter cursor.live ~f:(fun live ->
       not (Occurrence.equal (Intent.occurrence live.Live.intent) occurrence))
;;

let target_entry t session =
  Session_registry.find t.registry (P.Session_ref.session_id session)
  |> Result.of_option
       ~error:
         (P.Error.create
            Conflict
            ~message:"acquired recurrence target is no longer registered"
            ~retryable:false
            ())
;;

let acquire t controller intent =
  match Intent.child intent with
  | None -> Host.acquire_existing_target controller ~session:(Intent.target intent)
  | Some _ ->
    Host.acquire_captured_target controller ~occurrence:(Intent.occurrence intent)
;;

let live_job (job : P.Job.t) =
  match job.status with
  | Queued | Running | Waiting_permission _ | Waiting_completion _ -> true
  | Succeeded | Failed _ | Cancelled | Interrupted _ -> false
;;

let unfinished_run (run : P.Run.t) =
  match run.lifecycle with
  | Admitted | Active | Waiting _ -> true
  | Terminal _ -> false
;;

let preflight ?expected_revision t occupancy ~occurrence ~session target =
  let open Result.Let_syntax in
  let%bind entry = target_entry t session in
  let%bind state = Actor.state entry.actor in
  let%bind () = Host.Target.authorize target state in
  let%bind () =
    match expected_revision with
    | None -> Ok ()
    | Some revision ->
      if Int64.equal revision state.counters.revision
      then Ok ()
      else conflict "recurrence target changed after source preview"
  in
  let occupied =
    List.exists occupancy.Host.Dispatch_occupancy.known ~f:(fun known ->
      P.Session_ref.equal known.target session
      && not (Occurrence.equal known.occurrence occurrence))
  in
  if
    occupied
    || (not (List.is_empty occupancy.unknown))
    || Option.is_some state.active_operation
    || List.exists state.jobs ~f:live_job
    || Option.exists state.run_state ~f:(fun index ->
      List.exists (A.Run_state.runs index) ~f:unfinished_run)
    || not (Runtime_owner.source_lane_idle entry.runtime)
  then conflict "recurrence target is occupied or its occupancy is unknown"
  else Ok ()
;;

let with_target t session f =
  (* Same-domain, no yield between this deny-only check and insertion. Lower
     Factory/Actor admission is still the sole permission and durable owner. *)
  if List.mem t.in_flight session ~equal:P.Session_ref.equal
  then conflict "another recurrence preparation owns this target"
  else (
    t.in_flight <- session :: t.in_flight;
    Exn.protect ~f ~finally:(fun () ->
      t.in_flight
      <- List.filter t.in_flight ~f:(fun current ->
           not (P.Session_ref.equal current session))))
;;

let with_fresh_target t session f =
  with_target t session (fun () ->
    let open Result.Let_syntax in
    let%bind occupancy = Host.pending_occupancy t.host in
    match occupancy.unknown with
    | unknown :: _ -> Error unknown.error
    | [] -> f occupancy)
;;

let result_of_attempt cursor ~intent ~attempt result =
  remove_live cursor (Intent.occurrence intent);
  (* Even an envelope without retry evidence must remain: forgetting an
     uncertain/failed cleanup before recording would turn the same Ready row
     into a fresh request on the next pass. Retention denies; it grants nothing. *)
  cursor.Cursor.live <- { Live.intent; attempt; result } :: cursor.live;
  match Service.Attempt.result result with
  | Ok (Activated _) ->
    remove_live cursor (Intent.occurrence intent);
    Ok ()
  | Ok (Retained retained) ->
    remove_live cursor (Intent.occurrence intent);
    Result.map retained.reconciliation ~f:(fun _ -> ())
  | Ok (Admission_uncertain error | Activation_failed { error; _ }) -> Error error
  | Error error -> Error error
;;

let reconcile t controller intent attempt =
  let open Result.Let_syntax in
  let%bind target = acquire t controller intent in
  let%bind execution = Host.Target.execution target ~intent in
  Host.Execution.reconcile execution ~attempt
;;

let retry_live t controller cursor ~action (live : Live.t) =
  with_fresh_target t (Intent.target live.intent) (fun occupancy ->
    let open Result.Let_syntax in
    let%bind target = acquire t controller live.intent in
    let%bind execution = Host.Target.execution target ~intent:live.intent in
    let session = Intent.target live.intent in
    let%bind () =
      preflight t occupancy ~occurrence:(Intent.occurrence live.intent) ~session target
    in
    match Service.Attempt.retry live.result, Service.Attempt.no_effects live.result with
    | Some retry, _ ->
      let%bind result =
        action := true;
        Host.Execution.resume_retry execution ~retry
      in
      let%map () =
        result_of_attempt cursor ~intent:live.intent ~attempt:live.attempt result
      in
      true
    | None, Some observation ->
      if Attempt.number live.attempt + 1 >= Attempt.maximum_count
      then Ok false
      else (
        let%bind preview = Host.preview_target_source target in
        let%bind () =
          if
            P.Run_source.equal
              (Intent.source live.intent)
              (Session_factory.Recurrence_source_preview.source preview)
          then Ok ()
          else conflict "retry source changed from the frozen recurrence intent"
        in
        let%bind () =
          preflight
            t
            occupancy
            ~occurrence:(Intent.occurrence live.intent)
            ~session
            target
            ~expected_revision:
              (Session_factory.Recurrence_source_preview.expected_revision preview)
        in
        let%bind key =
          P.Idempotency_key.of_string
            (P.Id.Transaction.create () |> P.Id.Transaction.to_string)
        in
        let%bind attempt =
          Attempt.create
            ~intent:live.intent
            ~number:(Attempt.number live.attempt + 1)
            ~principal_id:(Attempt.principal_id live.attempt)
            ~expected_revision:
              (Session_factory.Recurrence_source_preview.expected_revision preview)
            ~key
        in
        let%bind request = Attempt.request attempt ~intent:live.intent in
        let%bind result =
          action := true;
          Host.Execution.retry execution ~observation ~previous:live.attempt ~request
        in
        let%map () = result_of_attempt cursor ~intent:live.intent ~attempt result in
        true)
    | None, None -> Ok false)
;;

let reenter_initial t controller cursor ~action (live : Live.t) =
  with_fresh_target t (Intent.target live.intent) (fun occupancy ->
    let open Result.Let_syntax in
    let%bind observation =
      Result.of_option
        (Service.Attempt.no_effects live.result)
        ~error:
          (P.Error.create
             Conflict
             ~message:"initial source retry has no joined NoEffects owner"
             ~retryable:false
             ())
    in
    let%bind () =
      if Int.equal (Attempt.number live.attempt) 0
      then Ok ()
      else conflict "a later retry cannot re-enter as an initial request"
    in
    let%bind target = acquire t controller live.intent in
    let%bind preview = Host.preview_target_source target in
    let%bind () =
      if
        P.Run_source.equal
          (Intent.source live.intent)
          (Session_factory.Recurrence_source_preview.source preview)
      then Ok ()
      else conflict "initial retry source changed from the frozen intent"
    in
    let%bind () =
      preflight
        t
        occupancy
        ~occurrence:(Intent.occurrence live.intent)
        ~session:(Intent.target live.intent)
        target
        ~expected_revision:(Attempt.expected_revision live.attempt)
    in
    let%bind state = Actor.state cursor.Cursor.entry.actor in
    let%bind () = Host.Controller.authorize controller state in
    let%bind () =
      let entries =
        Option.value_map
          state.recurrence_catalog
          ~default:[]
          ~f:A.Recurrence_catalog.entries
      in
      List.fold_result entries ~init:() ~f:(fun () entry ->
        let%bind frontier = Entry.frontier entry in
        if
          Option.is_none
            (Frontier.find_execution frontier (Intent.occurrence live.intent))
        then Ok ()
        else conflict "initial request now has an acknowledged controller row")
    in
    let%bind () = Actor.No_effects_observation.check_current observation in
    let%bind execution = Host.Target.execution target ~intent:live.intent in
    let%bind result =
      action := true;
      Host.Execution.activate execution ~attempt:live.attempt
    in
    match result with
    | Retained retained ->
      remove_live cursor (Intent.occurrence live.intent);
      Result.map retained.reconciliation ~f:(fun _ -> ())
    | Attempted result ->
      result_of_attempt cursor ~intent:live.intent ~attempt:live.attempt result)
;;

let with_ready_target
      t
      controller
      controller_entry
      recurrence_entry
      frontier
      definition
      occurrence
      f
  =
  let open Result.Let_syntax in
  match A.Recurrence_target.view (A.Recurrence_definition.target definition) with
  | Existing_session { session; _ } ->
    with_fresh_target t session (fun occupancy ->
      let%bind target = Host.acquire_existing_target controller ~session in
      f occupancy target)
  | Fresh_child _ ->
    let linked =
      List.exists (Frontier.child_associations frontier) ~f:(fun association ->
        Occurrence.equal
          occurrence
          (A.Recurrence_child_association.occurrence association))
    in
    let%bind reservation, permit =
      match
        List.find (Frontier.protected_children frontier) ~f:(fun row ->
          Occurrence.equal occurrence (A.Recurrence_child_reservation.occurrence row))
      with
      | Some reservation when linked -> Ok (reservation, None)
      | Some _ | None ->
        let%map permit =
          Actor.reserve_recurrence_child
            controller_entry.Session_registry.actor
            ~actor:(Host.Controller.actor controller)
            ~authorize:(Host.Controller.authorize_creation controller)
            ~occurrence
            ~expected_control_revision:(Entry.control_revision recurrence_entry)
        in
        Actor.Recurrence_creation.reservation permit, Some permit
    in
    let session =
      P.Session_ref.create
        ~server_id:(P.Session_ref.server_id (Entry.controller recurrence_entry))
        ~session_id:(A.Recurrence_child_reservation.child_session_id reservation)
    in
    with_fresh_target t session (fun occupancy ->
      let%bind target =
        match permit with
        | None -> Host.acquire_captured_target controller ~occurrence
        | Some permit ->
          let%bind child =
            Session_factory.create_recurrence_child
              t.factory
              ~parent:controller_entry
              ~permit
          in
          let%bind target =
            target_entry t (Session_factory.Recurrence_child.session child)
          in
          Host.captured_target controller ~child ~target
      in
      f occupancy target)
;;

let start_ready t controller cursor ~action recurrence_entry frontier occurrence =
  let open Result.Let_syntax in
  let%bind () =
    match Entry.control recurrence_entry with
    | Enabled -> Ok ()
    | Paused _ | Retired _ -> conflict "recurrence is not enabled for new work"
  in
  let%bind definition =
    List.find (Entry.definitions recurrence_entry) ~f:(fun definition ->
      A.Recurrence_identity.Definition.equal
        (A.Recurrence_definition.key definition)
        (Occurrence.definition occurrence))
    |> Result.of_option
         ~error:
           (P.Error.create
              Conflict
              ~message:"recurrence candidate lost its retained definition"
              ~retryable:false
              ())
  in
  let reservation =
    List.find (Frontier.protected_children frontier) ~f:(fun row ->
      Occurrence.equal occurrence (A.Recurrence_child_reservation.occurrence row))
  in
  let%bind nominal_json =
    match reservation with
    | Some row -> Ok (A.Recurrence_child_reservation.nominal_json row)
    | None ->
      Frontier.find_ready_json frontier occurrence
      |> Result.of_option
           ~error:
             (P.Error.create
                Conflict
                ~message:"recurrence candidate is not Ready or protected"
                ~retryable:false
                ())
  in
  (* Captured children have distinct IDs, so target exclusion alone cannot
     implement the recurrence's serial contract. An unresolved sibling holds it. *)
  let sibling =
    List.exists (Frontier.executions frontier) ~f:(fun execution ->
      A.Recurrence_execution.is_pending execution
      && not
           (Occurrence.equal
              occurrence
              (Intent.occurrence (A.Recurrence_execution.intent execution))))
    || List.exists (Frontier.protected_children frontier) ~f:(fun row ->
      let key = A.Recurrence_child_reservation.occurrence row in
      (not (Occurrence.equal occurrence key))
      && not
           (Option.exists (Frontier.find_execution frontier key) ~f:(fun execution ->
              not (A.Recurrence_execution.is_pending execution))))
  in
  let%bind () =
    if sibling then conflict "an earlier occurrence retains serial occupancy" else Ok ()
  in
  with_ready_target
    t
    controller
    cursor.Cursor.entry
    recurrence_entry
    frontier
    definition
    occurrence
    (fun occupancy target ->
       let%bind child, session, source =
         match A.Recurrence_target.view (A.Recurrence_definition.target definition) with
         | Existing_session { session; source; _ } -> Ok (None, session, Some source)
         | Fresh_child _ ->
           (* Creation/link may have advanced the controller. Read its actual rows;
         never manufacture an association from the proposed child ID. *)
           let%bind state = Actor.state cursor.entry.actor in
           let%bind () = Host.Controller.authorize controller state in
           let%bind catalog =
             Result.of_option
               state.recurrence_catalog
               ~error:
                 (P.Error.invalid_request
                    "captured controller lost its recurrence catalog")
           in
           let%bind entry =
             Result.of_option
               (A.Recurrence_catalog.find catalog (Entry.id recurrence_entry))
               ~error:(P.Error.invalid_request "captured recurrence is not retained")
           in
           let%bind frontier = Entry.frontier entry in
           let%bind reservation =
             List.find (Frontier.protected_children frontier) ~f:(fun row ->
               Occurrence.equal occurrence (A.Recurrence_child_reservation.occurrence row))
             |> Result.of_option
                  ~error:(P.Error.invalid_request "captured reservation is absent")
           in
           let%bind association =
             List.find (Frontier.child_associations frontier) ~f:(fun row ->
               Occurrence.equal occurrence (A.Recurrence_child_association.occurrence row))
             |> Result.of_option
                  ~error:(P.Error.invalid_request "captured association is absent")
           in
           let%map child = Intent.Child.create ~reservation ~association in
           ( Some child
           , P.Session_ref.create
               ~server_id:(P.Session_ref.server_id (Entry.controller entry))
               ~session_id:(A.Recurrence_child_reservation.child_session_id reservation)
           , None )
       in
       let%bind () = preflight t occupancy ~occurrence ~session target in
       let%bind preview = Host.preview_target_source target in
       let proposed_source = Session_factory.Recurrence_source_preview.source preview in
       let%bind () =
         match source with
         | None -> Ok ()
         | Some source ->
           if P.Run_source.equal source proposed_source
           then Ok ()
           else
             conflict
               "existing recurrence target source changed from its frozen definition"
       in
       let expected_revision =
         Session_factory.Recurrence_source_preview.expected_revision preview
       in
       let%bind () =
         preflight t occupancy ~occurrence ~session target ~expected_revision
       in
       let%bind intent =
         Intent.create
           ~definition
           ~control_revision:(Entry.control_revision recurrence_entry)
           ~nominal_json
           ~target:session
           ~source:proposed_source
           ~child
       in
       let%bind key =
         P.Idempotency_key.of_string
           (P.Id.Transaction.create () |> P.Id.Transaction.to_string)
       in
       let%bind attempt =
         Attempt.create
           ~intent
           ~number:0
           ~principal_id:(Operator_authorization.principal (Host.Target.actor target)).id
           ~expected_revision
           ~key
       in
       let%bind execution = Host.Target.execution target ~intent in
       let%bind result =
         action := true;
         Host.Execution.activate execution ~attempt
       in
       match result with
       | Retained retained -> retained.reconciliation |> Result.map ~f:(fun _ -> ())
       | Attempted result -> result_of_attempt cursor ~intent ~attempt result)
;;

module Candidate = struct
  type t =
    { occurrence : Occurrence.t
    ; entry : Entry.t
    ; frontier : Frontier.t
    }
end

let candidates state =
  let open Result.Let_syntax in
  let entries =
    Option.value_map
      state.A.Session_state.recurrence_catalog
      ~default:[]
      ~f:A.Recurrence_catalog.entries
  in
  List.fold_result entries ~init:[] ~f:(fun candidates entry ->
    let%map frontier = Entry.frontier entry in
    let keys =
      List.map (Frontier.ready frontier) ~f:A.Recurrence_nominal.key
      @ List.map
          (Frontier.protected_children frontier)
          ~f:A.Recurrence_child_reservation.occurrence
      @ (Frontier.executions frontier
         |> List.filter ~f:A.Recurrence_execution.is_pending
         |> List.map ~f:(fun execution ->
           Intent.occurrence (A.Recurrence_execution.intent execution)))
    in
    List.map keys ~f:(fun occurrence -> { Candidate.occurrence; entry; frontier })
    @ candidates)
;;

let process_entry t entry =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Recurrence_dispatcher.process_entry: foreign domain";
  let open Result.Let_syntax in
  let%bind controller = Host.controller t.host entry in
  let cursor = cursor t entry in
  let%bind state = Actor.state entry.Session_registry.actor in
  let%bind () = Host.Controller.authorize controller state in
  let%bind snapshot = candidates state in
  (* A live envelope is retained only while the actual bounded catalog retains
     that occurrence, and never reconstructed from its durable request DATA. *)
  cursor.live
  <- List.filter cursor.live ~f:(fun live ->
       List.exists snapshot ~f:(fun candidate ->
         Occurrence.equal
           candidate.Candidate.occurrence
           (Intent.occurrence live.Live.intent)));
  let snapshot =
    List.dedup_and_sort snapshot ~compare:(fun left right ->
      Occurrence.compare left.Candidate.occurrence right.Candidate.occurrence)
  in
  let before, after =
    match cursor.after with
    | None -> [], snapshot
    | Some key ->
      List.partition_tf snapshot ~f:(fun candidate ->
        Occurrence.compare candidate.Candidate.occurrence key <= 0)
  in
  let ordered = List.take (after @ before) 32 in
  let%bind occupancy = Host.pending_occupancy t.host in
  let action = ref false in
  let first_error =
    ref (List.hd occupancy.unknown |> Option.map ~f:(fun row -> row.error))
  in
  let visit candidate =
    let occurrence = candidate.Candidate.occurrence in
    cursor.after <- Some occurrence;
    let live = find_live cursor occurrence in
    let execution = Frontier.find_execution candidate.frontier occurrence in
    let%bind retained =
      match execution with
      | None -> Ok false
      | Some execution ->
        let intent = A.Recurrence_execution.intent execution in
        let attempt =
          A.Recurrence_execution.current_attempt execution
          |> A.Recurrence_execution.Attempt.request
        in
        let%map observed = reconcile t controller intent attempt in
        (match observed with
         | Retained retained ->
           if Option.is_some live then remove_live cursor occurrence;
           (match retained.reconciliation with
            | Ok _ -> true
            | Error error ->
              if Option.is_none !first_error then first_error := Some error;
              true)
         | Absent -> false)
    in
    if retained
    then Ok ()
    else (
      match live, execution with
      | Some live, None -> reenter_initial t controller cursor ~action live
      | Some live, Some _ ->
        retry_live t controller cursor ~action live |> Result.map ~f:(fun _ -> ())
      | None, Some _ -> Ok () (* Recovered DATA cannot restore retry custody. *)
      | None, None ->
        start_ready
          t
          controller
          cursor
          ~action
          candidate.entry
          candidate.frontier
          occurrence)
  in
  let rec scan = function
    | [] -> Option.value_map !first_error ~default:(Ok ()) ~f:(fun error -> Error error)
    | candidate :: rest ->
      let result = visit candidate in
      if !action
      then (
        match result with
        | Error _ -> result
        | Ok () ->
          Option.value_map !first_error ~default:(Ok ()) ~f:(fun error -> Error error))
      else (
        (match result with
         | Ok () -> ()
         | Error error -> if Option.is_none !first_error then first_error := Some error);
        scan rest)
  in
  scan ordered
;;
