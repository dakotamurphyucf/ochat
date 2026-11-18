open! Core
module A = Agent_session
module Actor = A.Session_actor
module D = Actor.Descendant_admission
module Scope = Agent_store.Delegation_goal_origin.Scope
module Store = Agent_store.Session_store
module P = Agent_protocol

type host =
  { registry : Session_registry.t
  ; store : Store.t
  ; runtime_source_owner : A.Runtime_source_owner.t
  ; check_registered_owner :
      Session_registry.entry -> session:P.Session_ref.t -> (unit, P.Error.t) result
  ; check_registered_factual_owner :
      Session_registry.entry -> session:P.Session_ref.t -> (unit, P.Error.t) result
  }

type source =
  { session : P.Session_ref.t
  ; handle : Store.Handle.t
  ; actor_ref : Actor.t option ref
  ; check_ancestry :
      (unit -> (A.Delegation_authority.Checked_ancestry.t, P.Error.t) result) option
  }

let unavailable code message = P.Error.create code ~message ~retryable:false ()

let check_domain t =
  ignore
    (A.Runtime_source_owner.identity t.runtime_source_owner
     : A.Runtime_source_owner.Identity.t)
;;

let descendant_registered_owner t ~session =
  let open Result.Let_syntax in
  let%bind entry =
    Session_registry.find t.registry (Agent_protocol.Session_ref.session_id session)
    |> Result.of_option
         ~error:(unavailable Conflict "descendant accounting owner is not registered")
  in
  let%map () = t.check_registered_owner entry ~session in
  entry
;;

let descendant_registered_factual_owner t ~session =
  let open Result.Let_syntax in
  let%bind entry =
    Session_registry.find t.registry (Agent_protocol.Session_ref.session_id session)
    |> Result.of_option
         ~error:(unavailable Conflict "descendant accounting owner is not registered")
  in
  let%map () = t.check_registered_factual_owner entry ~session in
  entry
;;

let source_actor_for_session t source session ~factual =
  let open Result.Let_syntax in
  let { session = source_session; handle; actor_ref; check_ancestry = _ } = source in
  let%bind () =
    if P.Session_ref.equal source_session session
    then Ok ()
    else Error (unavailable Conflict "descendant preparation has another source session")
  in
  match
    factual, Session_registry.find t.registry (P.Session_ref.session_id source_session)
  with
  | true, Some entry ->
    let%map () = t.check_registered_factual_owner entry ~session:source_session in
    entry.actor
  | false, _ | true, None ->
    let%bind actor =
      Result.of_option
        !actor_ref
        ~error:(unavailable Conflict "descendant source Actor is unavailable")
    in
    if
      Store.owns_handle t.store handle
      && (not (Session_registry.is_closing t.registry))
      && (not
            (Session_registry.has_pending_cleanup
               t.registry
               (Store.Handle.session_id handle)))
      && Option.exists
           (Actor.runtime_source_owner_identity actor)
           ~f:(A.Runtime_source_owner.matches t.runtime_source_owner)
    then Ok actor
    else Error (unavailable Conflict "descendant source lost its actual Factory owner")
;;

let unconfirmed () =
  Error (unavailable Conflict "descendant root writer outcome remains unconfirmed")
;;

let physical t source =
  let open Result.Let_syntax in
  let source_actor preparation ~factual =
    source_actor_for_session
      t
      source
      (A.Goal_descendant_occurrence.Frame.session
         (A.Goal_descendant_occurrence.frame (D.Preparation.occurrence preparation)))
      ~factual
  in
  let check_source_actor preparation actor ~factual =
    let%bind actual = source_actor preparation ~factual in
    if phys_equal actual actor
    then Ok ()
    else Error (unavailable Conflict "descendant actual source Actor was displaced")
  in
  let owner scope = descendant_registered_owner t ~session:(Scope.owner scope) in
  let blocked = function
    | A.Goal_descendants.Availability.Exhausted ->
      Error (unavailable Resource_limit "descendant root Goal count budget is exhausted")
    | Reserved ->
      Error
        (P.Error.create
           Conflict
           ~message:"descendant root capacity is reserved by an unconfirmed occurrence"
           ~retryable:true
           ())
    | Unknown ->
      Error
        (P.Error.create
           Conflict
           ~message:"descendant root capacity requires confirmed accounting"
           ~retryable:true
           ())
    | Available -> Error (unavailable Invalid_state "descendant root blocked Available")
  in
  let check_original_source () =
    match source.check_ancestry with
    | Some check -> check ()
    | None ->
      Error
        (unavailable
           Permission_denied
           "descendant governed source has no actual delegated ancestry guard")
  in
  let prepare preparation reservations =
    check_domain t;
    let%bind () =
      if phys_equal (D.Reservation_set.preparation reservations) preparation
      then Ok ()
      else Error (unavailable Conflict "descendant host received another reservation set")
    in
    let%bind source = source_actor preparation ~factual:false in
    let ancestry = check_original_source () in
    let%bind () = check_source_actor preparation source ~factual:false in
    let%bind ancestry = ancestry in
    let scopes = D.Preparation.scopes preparation in
    let owners =
      List.map scopes ~f:Scope.owner |> List.dedup_and_sort ~compare:P.Session_ref.compare
    in
    let%bind routes =
      List.fold_result owners ~init:[] ~f:(fun routes session ->
        let%bind entry = descendant_registered_owner t ~session in
        let result =
          D.bind_root_routes
            entry.actor
            ~source_owner:t.runtime_source_owner
            ~ancestry
            ~preparation
        in
        let%bind () = t.check_registered_owner entry ~session in
        let%map selected = result in
        routes @ selected)
    in
    let routes =
      List.sort routes ~compare:(fun left right ->
        Scope.compare_root (D.Root_route.scope left) (D.Root_route.scope right))
    in
    let existing = D.Reservation_set.attempts reservations in
    let%bind () =
      if
        List.equal Scope.equal scopes (List.map routes ~f:D.Root_route.scope)
        && List.equal
             Scope.equal
             (List.map existing ~f:D.Reservation_attempt.scope)
             (List.take scopes (List.length existing))
        && List.length existing <= List.length scopes
      then Ok ()
      else
        Error
          (unavailable Conflict "descendant routes do not cover the exact frozen vector")
    in
    let%bind admitted =
      List.mapi routes ~f:(fun index route -> index, route)
      |> List.fold_result ~init:[] ~f:(fun admitted (index, route) ->
        let scope = D.Root_route.scope route in
        let%bind entry = owner scope in
        let%bind attempt =
          match List.nth existing index with
          | Some attempt -> Ok attempt
          | None ->
            (* Finite Begin retains ownership in SAME Set before its reply,
               including when cancellation prevents the caller seeing that reply. *)
            let result = D.begin_reservation entry.actor ~route ~reservations in
            let%bind () = t.check_registered_owner entry ~session:(Scope.owner scope) in
            result
        in
        let result = D.commit_reservation entry.actor ~attempt in
        let%bind () = t.check_registered_owner entry ~session:(Scope.owner scope) in
        let%bind result = result in
        match result with
        | Reserved reservation -> Ok (admitted @ [ scope, reservation ])
        | Blocked availability -> blocked availability
        | Attempt_unconfirmed _ -> unconfirmed ())
    in
    let%bind views =
      List.fold_result admitted ~init:[] ~f:(fun views (scope, reservation) ->
        let%bind entry = owner scope in
        let result = D.refresh_reserved entry.actor ~reservation in
        let%bind () = t.check_registered_owner entry ~session:(Scope.owner scope) in
        let%map view = result in
        views @ [ view ])
    in
    (* Root RPCs may yield after the original delegated permission check.
       Repeat the same source guard, then check every root together without
       another wait. Source lifetime remains owned by the actual runtime lease. *)
    let%bind _ = check_original_source () in
    D.prepare_before_writer ~preparation ~reservations ~views
  in
  let close reservations closed =
    check_domain t;
    List.fold_result
      (D.Reservation_set.attempts reservations)
      ~init:()
      ~f:(fun () attempt ->
        match D.Reservation_attempt.outcome attempt with
        | Not_attempted -> Ok ()
        | Acknowledged reservation ->
          let scope = D.Reservation_attempt.scope attempt in
          let%bind entry =
            descendant_registered_factual_owner t ~session:(Scope.owner scope)
          in
          let result = D.close_before_writer entry.actor ~reservation ~closed in
          let%bind () =
            t.check_registered_factual_owner entry ~session:(Scope.owner scope)
          in
          result
        | Attempt_unconfirmed ->
          let scope = D.Reservation_attempt.scope attempt in
          let%bind entry =
            descendant_registered_factual_owner t ~session:(Scope.owner scope)
          in
          let result = D.reconcile_reservation entry.actor ~attempt in
          let%bind () =
            t.check_registered_factual_owner entry ~session:(Scope.owner scope)
          in
          let%bind result = result in
          (match result with
           | Reserved reservation ->
             let result = D.close_before_writer entry.actor ~reservation ~closed in
             let%bind () =
               t.check_registered_factual_owner entry ~session:(Scope.owner scope)
             in
             result
           | Attempt_unconfirmed _ -> unconfirmed ()
           | Blocked availability -> blocked availability))
  in
  let confirm acknowledgement =
    check_domain t;
    let reservations = D.Acknowledgement.reservations acknowledgement in
    let preparation = D.Reservation_set.preparation reservations in
    let%bind child = source_actor preparation ~factual:true in
    let state = Actor.state child in
    let%bind () = check_source_actor preparation child ~factual:true in
    let%bind state = state in
    let%bind accounting =
      Result.of_option
        state.goal_descendants
        ~error:(unavailable Conflict "factual descendant source accounting is absent")
    in
    (* Birth captured the actual host depth. Today's smaller host ceiling must
       not block facts about the older already-admitted private ancestry. *)
    let max_depth =
      A.Goal_descendants.Profile.max_scopes (A.Goal_descendants.profile accounting)
    in
    let remaining =
      List.map (D.Preparation.scopes preparation) ~f:Scope.owner
      |> List.dedup_and_sort ~compare:P.Session_ref.compare
    in
    let%bind reference =
      Result.of_option
        (D.Preparation.delegation preparation)
        ~error:(unavailable Conflict "governed child has no actual own delegation")
    in
    let observation =
      D.observe_acknowledged_usage
        child
        ~source_owner:t.runtime_source_owner
        ~acknowledgement
    in
    let%bind () = check_source_actor preparation child ~factual:true in
    let%bind observation = observation in
    let rec propagate ~depth ~visited ~remaining ~imports reference observation =
      if depth >= max_depth
      then
        Error
          (unavailable Conflict "factual descendant ancestry exceeds its captured depth")
      else (
        let session =
          P.Session_ref.create
            ~server_id:(Store.server_id t.store)
            ~session_id:
              reference.Agent_store.Delegation_store.Reference.key.parent_session_id
        in
        if List.mem visited session ~equal:P.Session_ref.equal
        then
          Error
            (unavailable Conflict "factual descendant ancestry repeats a source session")
        else (
          let%bind entry = descendant_registered_factual_owner t ~session in
          let result =
            D.import_usage entry.actor ~source_owner:t.runtime_source_owner ~observation
          in
          let%bind () = t.check_registered_factual_owner entry ~session in
          let%bind imported = result in
          let imports = imports @ [ imported ] in
          let remaining =
            List.filter remaining ~f:(fun owner ->
              not (P.Session_ref.equal owner session))
          in
          if List.is_empty remaining
          then D.confirmation acknowledgement ~imports
          else (
            let%bind reference =
              Result.of_option
                (D.Import_acknowledgement.delegation imported)
                ~error:
                  (unavailable
                     Conflict
                     "factual ancestry does not reach every frozen root owner")
            in
            let result =
              D.observe_imported_usage
                entry.actor
                ~source_owner:t.runtime_source_owner
                ~acknowledgement:imported
            in
            let%bind () = t.check_registered_factual_owner entry ~session in
            let%bind observation = result in
            propagate
              ~depth:(depth + 1)
              ~visited:(session :: visited)
              ~remaining
              ~imports
              reference
              observation)))
    in
    propagate
      ~depth:0
      ~visited:[ source.session ]
      ~remaining
      ~imports:[]
      reference
      observation
  in
  let refresh_admitted acknowledgement confirmation =
    check_domain t;
    let reservations = D.Acknowledgement.reservations acknowledgement in
    let%bind views =
      List.fold_result
        (D.Reservation_set.attempts reservations)
        ~init:[]
        ~f:(fun views attempt ->
          match D.Reservation_attempt.outcome attempt with
          | Not_attempted | Attempt_unconfirmed -> unconfirmed ()
          | Acknowledged reservation ->
            let scope = D.Reservation_attempt.scope attempt in
            let%bind entry = owner scope in
            let result =
              D.refresh_admitted_root
                entry.actor
                ~reservation
                ~acknowledgement
                ~confirmation
            in
            let%bind () = t.check_registered_owner entry ~session:(Scope.owner scope) in
            let%map view = result in
            views @ [ view ])
    in
    (* Admission ACK does not waive original source/private permission checks.
       This does not repeat fresh Goal count or token admission. *)
    let%bind _ = check_original_source () in
    D.admitted_views ~acknowledgement ~confirmation ~views
  in
  Actor.Descendant_host.{ prepare; close; confirm; refresh_admitted }
;;

let causal t source =
  let open Result.Let_syntax in
  let { session = source_session; handle; actor_ref; check_ancestry } = source in
  let module C = Actor.Causal_admission in
  let module PC = A.Policy_canonical_usage in
  let module Target = A.Policy_cause_data.Goal_target in
  let module Request = A.Goal_policy_contributions.Request in
  let module Cleanup_failure = struct
    type t =
      | Protocol of P.Error.t
      | Raised of exn * Stdlib.Printexc.raw_backtrace

    let capture f =
      try Result.map_error (f ()) ~f:(fun failure -> Protocol failure) with
      | exn -> Error (Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
    ;;

    let resume = function
      | None -> Ok ()
      | Some (Protocol failure) -> Error failure
      | Some (Raised (exn, backtrace)) -> Exn.raise_with_original_backtrace exn backtrace
    ;;
  end
  in
  let attempt_factual_leg first f =
    let run () = Cleanup_failure.capture f in
    let outcome =
      match !first with
      | None -> run ()
      | Some _ -> Eio.Cancel.protect run
    in
    match outcome with
    | Ok value -> Some value
    | Error failure ->
      if Option.is_none !first then first := Some failure;
      None
  in
  let captured_actor = !actor_ref in
  let source_actor preparation ~factual =
    let session = C.Preparation.session preparation in
    if not (P.Session_ref.equal source_session session)
    then Error (unavailable Conflict "causal preparation has another source session")
    else (
      match
        factual, Session_registry.find t.registry (P.Session_ref.session_id session)
      with
      | true, registered ->
        (* A factual retry retains this exact source, including provisional
           cleanup. A current registry locator cannot replace its Actor/Handle. *)
        let%bind actor =
          Result.of_option
            captured_actor
            ~error:(unavailable Conflict "causal factual source Actor is unavailable")
        in
        let%bind () =
          if
            Store.owns_handle t.store handle
            && Option.exists !actor_ref ~f:(phys_equal actor)
            && Option.exists
                 (Actor.runtime_source_owner_identity actor)
                 ~f:(A.Runtime_source_owner.matches t.runtime_source_owner)
          then Ok ()
          else Error (unavailable Conflict "causal factual source lost its actual owner")
        in
        (match registered with
         | None -> Ok actor
         | Some entry ->
           let%bind () =
             if
               phys_equal entry.actor actor
               && Option.exists entry.store_handle ~f:(phys_equal handle)
             then Ok ()
             else Error (unavailable Conflict "causal factual source tuple was displaced")
           in
           let%map () = t.check_registered_factual_owner entry ~session in
           actor)
      | false, _ -> source_actor_for_session t source session ~factual)
  in
  let check_source preparation actor ~factual =
    let%bind actual = source_actor preparation ~factual in
    if phys_equal actual actor
    then Ok ()
    else Error (unavailable Conflict "causal actual source Actor was displaced")
  in
  let ancestry () =
    match check_ancestry with
    | None -> Ok None
    | Some check -> Result.map (check ()) ~f:Option.some
  in
  let owner session ~factual =
    if factual
    then descendant_registered_factual_owner t ~session
    else descendant_registered_owner t ~session
  in
  let check_owner entry session ~factual =
    if factual
    then t.check_registered_factual_owner entry ~session
    else t.check_registered_owner entry ~session
  in
  let blocked = function
    | A.Goal_admission_policy.Count_availability.Exhausted ->
      Error (unavailable Resource_limit "causal root Goal count budget is exhausted")
    | Reserved | Unknown ->
      Error
        (P.Error.create
           Conflict
           ~message:"causal root capacity requires confirmed accounting"
           ~retryable:true
           ())
    | Available -> Error (unavailable Invalid_state "causal root blocked Available")
  in
  let prepare preparation reservations =
    check_domain t;
    let%bind () =
      if phys_equal (C.Reservation_set.preparation reservations) preparation
      then Ok ()
      else Error (unavailable Conflict "causal host received another reservation set")
    in
    let%bind source = source_actor preparation ~factual:false in
    let ancestry_result = ancestry () in
    let%bind () = check_source preparation source ~factual:false in
    let%bind callee_ancestry = ancestry_result in
    let result =
      C.observe_routes
        source
        ~source_owner:t.runtime_source_owner
        ~preparation
        ~reservations
        ~callee_ancestry
    in
    let%bind () = check_source preparation source ~factual:false in
    let%bind observation = result in
    (* Authenticate every actual target owner, including covered/inline targets,
       before selecting the remote Request vector. No self reserve is created. *)
    let%bind routes =
      List.fold_result
        (C.Route_observation.owners observation)
        ~init:[]
        ~f:(fun routes session ->
          let%bind entry = owner session ~factual:false in
          let result =
            C.bind_root_routes
              entry.actor
              ~source_owner:t.runtime_source_owner
              ~observation
          in
          let%bind () = check_owner entry session ~factual:false in
          let%bind () = check_source preparation source ~factual:false in
          let%map selected = result in
          routes @ selected)
    in
    let%bind requests = C.Reservation_set.requests reservations in
    let targets =
      List.map requests ~f:(fun request -> PC.Stream.target (Request.stream request))
    in
    let%bind routes =
      let%bind () =
        if Int.equal (List.length routes) (List.length targets)
        then Ok ()
        else Error (unavailable Conflict "causal routes differ from the frozen requests")
      in
      List.map targets ~f:(fun target ->
        match
          List.filter routes ~f:(fun route ->
            Target.equal target (C.Root_route.target route))
        with
        | [ route ] -> Ok route
        | [] | _ :: _ ->
          Error (unavailable Conflict "causal request has no unique authenticated route"))
      |> Result.all
    in
    let existing = C.Reservation_set.attempts reservations in
    let%bind () =
      if
        List.length existing <= List.length routes
        && List.equal
             P.Session_ref.equal
             (List.map existing ~f:C.Reservation_attempt.owner)
             (List.take (List.map routes ~f:C.Root_route.owner) (List.length existing))
      then Ok ()
      else
        Error (unavailable Conflict "causal attempts differ from the retained Set order")
    in
    let%bind admitted =
      List.mapi routes ~f:(fun index route -> index, route)
      |> List.fold_result ~init:[] ~f:(fun admitted (index, route) ->
        let session = C.Root_route.owner route in
        let%bind entry = owner session ~factual:false in
        let%bind attempt =
          match List.nth existing index with
          | Some attempt -> Ok attempt
          | None ->
            let result = C.begin_reservation entry.actor ~route ~reservations in
            let%bind () = check_owner entry session ~factual:false in
            let%bind () = check_source preparation source ~factual:false in
            result
        in
        let result = C.commit_reservation entry.actor ~attempt in
        let%bind () = check_owner entry session ~factual:false in
        let%bind () = check_source preparation source ~factual:false in
        let%bind result = result in
        match result with
        | Reserved reservation -> Ok (admitted @ [ session, reservation ])
        | Blocked availability -> blocked availability
        | Attempt_unconfirmed _ -> unconfirmed ())
    in
    let%bind views =
      List.fold_result admitted ~init:[] ~f:(fun views (session, reservation) ->
        let%bind entry = owner session ~factual:false in
        let result = C.refresh_reserved entry.actor ~reservation in
        let%bind () = check_owner entry session ~factual:false in
        let%bind () = check_source preparation source ~factual:false in
        let%map view = result in
        views @ [ view ])
    in
    let final_ancestry = ancestry () in
    let%bind () = check_source preparation source ~factual:false in
    let%bind _ = final_ancestry in
    C.prepare_before_writer ~preparation ~reservations ~views
  in
  let close reservations closed =
    check_domain t;
    let first = ref None in
    List.iter (C.Reservation_set.attempts reservations) ~f:(fun attempt ->
      ignore
        (attempt_factual_leg first (fun () ->
           let session = C.Reservation_attempt.owner attempt in
           let close reservation =
             let%bind entry = owner session ~factual:true in
             let%bind () = C.close_before_writer entry.actor ~reservation ~closed in
             check_owner entry session ~factual:true
           in
           match C.Reservation_attempt.outcome attempt with
           | Not_attempted | Known_not_reserved -> Ok ()
           | Acknowledged reservation -> close reservation
           | Attempt_unconfirmed ->
             let%bind entry = owner session ~factual:true in
             let%bind result = C.reconcile_reservation entry.actor ~attempt in
             let%bind () = check_owner entry session ~factual:true in
             (match result with
              | Reserved reservation -> close reservation
              | Blocked _ -> Ok ()
              | Attempt_unconfirmed _ -> unconfirmed ()))
         : unit option));
    Cleanup_failure.resume !first
  in
  let confirm acknowledgement =
    check_domain t;
    let reservations = C.Acknowledgement.reservations acknowledgement in
    let preparation = C.Reservation_set.preparation reservations in
    let%bind source = source_actor preparation ~factual:true in
    let%bind retained_confirmation = C.initial_confirmation acknowledgement in
    match retained_confirmation with
    | Some confirmation -> Ok confirmation
    | None ->
      let result =
        C.initial_usage source ~source_owner:t.runtime_source_owner ~acknowledgement
      in
      let%bind () = check_source preparation source ~factual:true in
      let%bind observation = result in
      let%bind retained = C.initial_imports acknowledgement in
      let attempts = C.Reservation_set.attempts reservations in
      let%bind () =
        if List.length retained <= List.length attempts
        then Ok ()
        else Error (unavailable Conflict "causal initial prefix exceeds its actual Set")
      in
      let%bind imports =
        List.mapi attempts ~f:(fun index attempt -> index, attempt)
        |> List.fold_result ~init:[] ~f:(fun imports (index, attempt) ->
          let%bind reservation =
            match C.Reservation_attempt.outcome attempt with
            | Acknowledged reservation -> Ok reservation
            | Not_attempted | Known_not_reserved | Attempt_unconfirmed -> unconfirmed ()
          in
          match List.nth retained index with
          | Some imported ->
            if phys_equal (C.Import_acknowledgement.reservation imported) reservation
            then Ok (imports @ [ imported ])
            else
              Error (unavailable Conflict "causal initial prefix has another reservation")
          | None ->
            let session = C.Reservation.owner reservation in
            let%bind entry = owner session ~factual:true in
            (* import_usage itself reconciles an already installed initial receipt;
               it cannot repeat an unknown/entered kernel or replace its observation. *)
            let result = C.import_usage entry.actor ~reservation ~observation in
            let%bind () = check_owner entry session ~factual:true in
            let%bind () = check_source preparation source ~factual:true in
            let%bind imported = result in
            let%map () = C.retain_initial_import acknowledgement ~observation ~imported in
            imports @ [ imported ])
      in
      let%bind confirmation = C.confirmation acknowledgement ~imports in
      let%map () = C.retain_initial_confirmation acknowledgement confirmation in
      confirmation
  in
  let check_confirmation acknowledgement confirmation =
    let%bind retained = C.initial_confirmation acknowledgement in
    match retained with
    | Some actual when phys_equal actual confirmation -> Ok ()
    | Some _ | None ->
      Error (unavailable Conflict "causal host has another initial confirmation")
  in
  let refresh_admitted acknowledgement confirmation =
    check_domain t;
    let%bind () = check_confirmation acknowledgement confirmation in
    let reservations = C.Acknowledgement.reservations acknowledgement in
    let preparation = C.Reservation_set.preparation reservations in
    let%bind source = source_actor preparation ~factual:false in
    let%bind views =
      List.fold_result
        (C.Confirmation.imports confirmation)
        ~init:[]
        ~f:(fun views imported ->
          let reservation = C.Import_acknowledgement.reservation imported in
          let session = C.Reservation.owner reservation in
          let%bind entry = owner session ~factual:false in
          let result =
            C.refresh_admitted_root
              entry.actor
              ~reservation
              ~acknowledgement
              ~confirmation
          in
          let%bind () = check_owner entry session ~factual:false in
          let%bind () = check_source preparation source ~factual:false in
          let%map view = result in
          views @ [ view ])
    in
    let result = ancestry () in
    let%bind () = check_source preparation source ~factual:false in
    let%bind _ = result in
    C.admitted_views ~acknowledgement ~confirmation ~views
  in
  let correct_usage acknowledgement confirmation =
    check_domain t;
    let%bind () = check_confirmation acknowledgement confirmation in
    let preparation =
      C.Reservation_set.preparation (C.Acknowledgement.reservations acknowledgement)
    in
    let%bind source = source_actor preparation ~factual:true in
    let first = ref None in
    let awaiting = ref false in
    List.iter (C.Confirmation.imports confirmation) ~f:(fun imported ->
      match
        attempt_factual_leg first (fun () ->
          let%bind () = check_source preparation source ~factual:true in
          let%bind result =
            C.correct_imported_usage
              source
              ~source_owner:t.runtime_source_owner
              ~acknowledgement
              ~imported
          in
          let%map () = check_source preparation source ~factual:true in
          result)
      with
      | Some A.Inference_graph_owner.Factual_usage.Correction_outcome.Settled -> ()
      | Some Awaiting_initial_confirmation -> awaiting := true
      | None -> ());
    let%map () = Cleanup_failure.resume !first in
    if !awaiting
    then
      A.Inference_graph_owner.Factual_usage.Correction_outcome
      .Awaiting_initial_confirmation
    else Settled
  in
  Actor.Descendant_host.Causal_driver.
    { prepare; close; confirm; refresh_admitted; correct_usage }
;;

let create t ~handle ~actor_ref ~check_ancestry =
  let session =
    P.Session_ref.create
      ~server_id:(Store.server_id t.store)
      ~session_id:(Store.Handle.session_id handle)
  in
  let source = { session; handle; actor_ref; check_ancestry } in
  let physical = physical t source in
  let causal = causal t source in
  physical, causal
;;
