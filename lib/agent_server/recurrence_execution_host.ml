open! Core
module P = Agent_protocol
module A = Agent_session
module Actor = A.Session_actor
module Configuration = Recurrence_execution_configuration
module Intent = A.Recurrence_execution_intent
module Service = Recurrence_run_service

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

type t =
  { configuration : Configuration.t
  ; factory : Session_factory.t
  ; server_id : P.Id.Server.t
  ; sw : Eio.Switch.t
  ; now : unit -> P.Timestamp.t
  ; domain : int
  ; mutable closed : bool
  }

type host = t

let check_domain t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Recurrence_execution_host: foreign domain"
;;

let close t =
  check_domain t;
  t.closed <- true
;;

let create configuration ~factory ~server_id ~sw ~now =
  let open Result.Let_syntax in
  let%bind () = Configuration.validate configuration in
  let t =
    { configuration
    ; factory
    ; server_id
    ; sw
    ; now
    ; domain = (Stdlib.Domain.self () :> int)
    ; closed = false
    }
  in
  Eio.Switch.on_release sw (fun () -> close t);
  Ok t
;;

let current t =
  check_domain t;
  if t.closed || Option.is_some (Eio.Switch.get_error t.sw)
  then denied "recurrence execution host is closing"
  else if
    Option.exists t.configuration.expires_at ~f:(fun expiry ->
      P.Timestamp.compare (t.now ()) expiry >= 0)
  then denied "recurrence execution grant expired"
  else Ok ()
;;

let session t (entry : Session_registry.entry) =
  match entry.store_handle with
  | None -> conflict "recurrence execution requires an actual Factory entry"
  | Some handle ->
    Ok
      (P.Session_ref.create
         ~server_id:t.server_id
         ~session_id:(Agent_store.Session_store.Handle.session_id handle))
;;

let check_entry t entry ~session =
  let open Result.Let_syntax in
  let%bind () = current t in
  Session_factory.check_registered_entry t.factory entry ~session
;;

let check_owner t entry ~session =
  let open Result.Let_syntax in
  let%bind () = current t in
  Session_factory.check_registered_owner t.factory entry ~session
;;

let check_state session (state : A.Session_state.t) =
  if P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
  then Ok ()
  else conflict "recurrence execution policy received another target state"
;;

let principal t ~scopes =
  P.Principal.create
    ~id:t.configuration.principal_id
    ~authentication_kind:"host.recurrence.execution"
    ~scopes:(P.Scope.Set.of_list scopes)
    ~attributes:[]
;;

module Controller = struct
  type t =
    { host : host
    ; entry : Session_registry.entry
    ; session : P.Session_ref.t
    ; grant : Configuration.Controller.t
    ; actor : Operator_authorization.t
    }

  let actor t = t.actor

  let basis t =
    let open Result.Let_syntax in
    let%bind () = current t.host in
    Session_factory.check_execution_selected_entry
      t.host.factory
      t.entry
      ~session:t.session
  ;;

  let authority_basis t =
    let open Result.Let_syntax in
    let%bind () = current t.host in
    Session_factory.check_execution_selected_owner
      t.host.factory
      t.entry
      ~session:t.session
  ;;

  let authorize t state =
    let open Result.Let_syntax in
    let%bind () = authority_basis t in
    check_state t.session state
  ;;

  let check_definition t definition =
    let selected =
      A.Recurrence_definition.key definition
      |> A.Recurrence_identity.Definition.controller
    in
    if P.Session_ref.equal t.session selected
    then Ok ()
    else denied "recurrence definition belongs to another configured controller"
  ;;

  let authorize_creation t state definition =
    let open Result.Let_syntax in
    let%bind () = basis t in
    let%bind () = authorize t state in
    let%bind () = check_definition t definition in
    match A.Recurrence_target.view (A.Recurrence_definition.target definition) with
    | Fresh_child _ when t.grant.captured_children -> Ok ()
    | Fresh_child _ -> denied "captured recurrence launch is not configured"
    | Existing_session _ -> denied "recurrence creation requires a captured target"
  ;;

  let automatic_policy t state definition =
    let open Result.Let_syntax in
    let%bind () = basis t in
    let%bind () = authorize t state in
    let%bind () = check_definition t definition in
    match A.Recurrence_target.view (A.Recurrence_definition.target definition) with
    | Fresh_child _ ->
      if t.grant.captured_children
      then Ok true
      else denied "captured recurrence launch is not configured"
    | Existing_session { session; _ } ->
      if
        P.Id.Server.equal t.host.server_id (P.Session_ref.server_id session)
        && List.mem
             t.grant.existing_targets
             (P.Session_ref.session_id session)
             ~equal:P.Id.Session.equal
      then Ok true
      else denied "existing recurrence target is not configured"
  ;;
end

let controller host entry =
  let open Result.Let_syntax in
  let%bind () = current host in
  let%bind session = session host entry in
  let%bind grant =
    List.find host.configuration.controllers ~f:(fun grant ->
      P.Id.Session.equal
        grant.Configuration.Controller.session_id
        (P.Session_ref.session_id session))
    |> Result.of_option
         ~error:
           (P.Error.create
              Permission_denied
              ~message:"recurrence controller is not configured for execution"
              ~retryable:false
              ())
  in
  let basis () =
    let%bind () = current host in
    Session_factory.check_execution_selected_entry host.factory entry ~session
  in
  let authority_basis () =
    let%bind () = current host in
    Session_factory.check_execution_selected_owner host.factory entry ~session
  in
  let%bind () = basis () in
  let scopes = P.Scope.[ Send_messages; View_session_transcript ] in
  let scopes =
    if grant.captured_children then P.Scope.Create_sessions :: scopes else scopes
  in
  let%map principal = principal host ~scopes in
  let actor =
    Operator_authorization.guarded ~principal ~is_current:(fun () ->
      Result.is_ok (authority_basis ()))
  in
  { Controller.host; entry; session; grant; actor }
;;

module Captured = struct
  type t =
    { child : Session_factory.Recurrence_child.t
    ; reservation : A.Recurrence_child_reservation.t
    ; association : A.Recurrence_child_association.t
    ; definition : A.Recurrence_definition.t
    }

  let check_target t (state : A.Session_state.t) =
    if
      Option.exists
        state.A.Session_state.spec.delegation
        ~f:
          (Agent_store.Delegation_store.Reference.equal
             (Session_factory.Recurrence_child.reference t.child))
    then Ok ()
    else denied "captured recurrence target lost its actual delegation association"
  ;;
end

module Selected_target = struct
  type family =
    | Existing
    | Captured of Captured.t

  type t =
    { controller : Controller.t
    ; entry : Session_registry.entry
    ; session : P.Session_ref.t
    ; actor : Operator_authorization.t
    ; family : family
    }

  let basis t =
    let open Result.Let_syntax in
    let%bind () = Controller.basis t.controller in
    check_entry t.controller.host t.entry ~session:t.session
  ;;

  let authority_basis t =
    let open Result.Let_syntax in
    let%bind () = Controller.authority_basis t.controller in
    check_owner t.controller.host t.entry ~session:t.session
  ;;

  let authorize t state =
    let open Result.Let_syntax in
    let%bind () = authority_basis t in
    let%bind () = check_state t.session state in
    match t.family with
    | Existing -> Ok ()
    | Captured captured -> Captured.check_target captured state
  ;;

  let create controller entry session family =
    let open Result.Let_syntax in
    let basis () =
      let%bind () = Controller.basis controller in
      check_entry controller.host entry ~session
    in
    let authority_basis () =
      let%bind () = Controller.authority_basis controller in
      check_owner controller.host entry ~session
    in
    let%bind () = basis () in
    let%map principal =
      principal controller.host ~scopes:P.Scope.[ Send_messages; View_session_transcript ]
    in
    let actor =
      Operator_authorization.guarded ~principal ~is_current:(fun () ->
        Result.is_ok (authority_basis ()))
    in
    { controller; entry; session; actor; family }
  ;;
end

module Execution = struct
  type t =
    { target : Selected_target.t
    ; intent : Intent.t
    ; service : Service.t
    }

  let activate t ~attempt =
    Service.activate
      t.service
      ~controller:t.target.controller.entry
      ~controller_actor:t.target.controller.actor
      ~target:t.target.entry
      ~target_actor:t.target.actor
      ~intent:t.intent
      ~attempt
  ;;

  let retry t ~observation ~previous ~request =
    Service.retry
      t.service
      ~controller:t.target.controller.entry
      ~controller_actor:t.target.controller.actor
      ~target:t.target.entry
      ~target_actor:t.target.actor
      ~observation
      ~intent:t.intent
      ~previous
      ~request
  ;;

  let resume_retry t ~retry =
    if not (Intent.equal t.intent (Actor.Recurrence_retry.intent retry))
    then conflict "recurrence retry belongs to another selected execution Intent"
    else
      Service.resume_retry
        t.service
        ~controller:t.target.controller.entry
        ~controller_actor:t.target.controller.actor
        ~target:t.target.entry
        ~target_actor:t.target.actor
        ~retry
  ;;

  let reconcile t ~attempt =
    Service.reconcile
      t.service
      ~controller:t.target.controller.entry
      ~controller_actor:t.target.controller.actor
      ~target:t.target.entry
      ~target_actor:t.target.actor
      ~intent:t.intent
      ~attempt
  ;;
end

module Target = struct
  type t = Selected_target.t

  let actor (t : t) = t.actor
  let authorize = Selected_target.authorize

  let execution (target : t) ~intent =
    let open Result.Let_syntax in
    let%bind () = Selected_target.basis target in
    let controller = target.controller in
    let%bind () =
      if
        P.Session_ref.equal (Intent.target intent) target.session
        && P.Session_ref.equal
             (Intent.occurrence intent
              |> A.Recurrence_identity.Occurrence.definition
              |> A.Recurrence_identity.Definition.controller)
             controller.session
      then Ok ()
      else conflict "recurrence Intent belongs to another selected controller or target"
    in
    let%bind () =
      match target.family, Intent.child intent, Intent.input intent with
      | Existing, None, User_submission _
        when P.Run.Mode.equal (Intent.mode intent) Single_turn -> Ok ()
      | Captured captured, Some association, Authored_start
        when A.Recurrence_child_association.equal association captured.association ->
        Intent.validate
          intent
          ~definition:captured.definition
          ~reservation:(Some captured.reservation)
      | (Existing | Captured _), _, _ ->
        denied "recurrence Intent does not match its selected target family"
    in
    let authorize actor state =
      if phys_equal actor controller.actor
      then Controller.authorize controller state
      else if phys_equal actor target.actor
      then Selected_target.authorize target state
      else denied "recurrence policy requires its exact original selected proof"
    in
    let automatic_policy state definition =
      let%bind () = Selected_target.basis target in
      Controller.automatic_policy controller state definition
    in
    let service =
      Service.create ~factory:controller.host.factory ~authorize ~automatic_policy
    in
    Ok { Execution.target; intent; service }
  ;;
end

let existing_target (controller : Controller.t) ~target =
  let open Result.Let_syntax in
  let%bind () = Controller.basis controller in
  let%bind session = session controller.host target in
  if
    not
      (List.mem
         controller.grant.existing_targets
         (P.Session_ref.session_id session)
         ~equal:P.Id.Session.equal)
  then denied "existing recurrence target is not configured"
  else Selected_target.create controller target session Existing
;;

let captured_target (controller : Controller.t) ~child ~target =
  let open Result.Let_syntax in
  let%bind () = Controller.basis controller in
  let%bind () =
    if controller.grant.captured_children
    then Ok ()
    else denied "captured recurrence launch is not configured"
  in
  let%bind session = session controller.host target in
  let%bind () =
    if P.Session_ref.equal session (Session_factory.Recurrence_child.session child)
    then check_entry controller.host target ~session
    else denied "captured recurrence child names another actual target"
  in
  let%bind parent_state = Actor.state controller.entry.actor in
  let%bind () = Controller.authorize controller parent_state in
  let reference = Session_factory.Recurrence_child.reference child in
  let linked =
    Option.value_map parent_state.recurrence_catalog ~default:[] ~f:(fun catalog ->
      A.Recurrence_catalog.entries catalog
      |> List.concat_map ~f:(fun entry ->
        match A.Recurrence_entry.frontier entry with
        | Error _ -> []
        | Ok frontier ->
          A.Recurrence_frontier.child_associations frontier
          |> List.filter_map ~f:(fun association ->
            if
              not
                (Agent_store.Delegation_store.Reference.equal
                   reference
                   (A.Recurrence_child_association.reference association))
            then None
            else (
              let occurrence = A.Recurrence_child_association.occurrence association in
              Option.bind
                (List.find
                   (A.Recurrence_frontier.protected_children frontier)
                   ~f:(fun reservation ->
                     A.Recurrence_identity.Occurrence.equal
                       occurrence
                       (A.Recurrence_child_reservation.occurrence reservation)))
                ~f:(fun reservation ->
                  List.find (A.Recurrence_entry.definitions entry) ~f:(fun definition ->
                    A.Recurrence_identity.Definition.equal
                      (A.Recurrence_definition.key definition)
                      (A.Recurrence_identity.Occurrence.definition occurrence))
                  |> Option.map ~f:(fun definition ->
                    { Captured.child; reservation; association; definition }))))))
  in
  let%bind captured =
    match linked with
    | [ captured ] -> Ok captured
    | [] | _ :: _ :: _ ->
      denied "captured target has no unique actual controller association"
  in
  let%bind () =
    Controller.authorize_creation controller parent_state captured.definition
  in
  let%bind () =
    A.Recurrence_child_association.validate
      captured.association
      ~reservation:captured.reservation
  in
  let%bind target_state = Actor.state target.actor in
  let%bind () = Controller.basis controller in
  let%bind () = check_entry controller.host target ~session in
  let%bind () = check_state session target_state in
  let%bind () = Captured.check_target captured target_state in
  Selected_target.create controller target session (Captured captured)
;;

let acquire_existing_target (controller : Controller.t) ~session =
  let open Result.Let_syntax in
  let%bind () = Controller.basis controller in
  let%bind () =
    if
      P.Id.Server.equal (P.Session_ref.server_id session) controller.host.server_id
      && List.mem
           controller.grant.existing_targets
           (P.Session_ref.session_id session)
           ~equal:P.Id.Session.equal
    then Ok ()
    else denied "existing recurrence target is not configured on this host"
  in
  let%bind principal =
    principal controller.host ~scopes:P.Scope.[ Send_messages; View_session_transcript ]
  in
  let actor =
    Operator_authorization.guarded ~principal ~is_current:(fun () ->
      Result.is_ok (Controller.basis controller))
  in
  let authorize state =
    let%bind () = Controller.basis controller in
    check_state session state
  in
  let%bind target =
    Session_factory.acquire_recurrence_target
      controller.host.factory
      ~session
      ~actor
      ~authorize
  in
  existing_target controller ~target
;;

let preview_target_source (target : Target.t) =
  Session_factory.preview_recurrence_source
    target.controller.host.factory
    target.entry
    ~actor:target.actor
    ~authorize:(Target.authorize target)
;;

module Dispatch_occupancy = struct
  module Known = struct
    type t =
      { occurrence : A.Recurrence_identity.Occurrence.t
      ; target : P.Session_ref.t
      }
  end

  module Unknown = struct
    type t =
      { controller : P.Session_ref.t
      ; error : P.Error.t
      }
  end

  type t =
    { known : Known.t list
    ; unknown : Unknown.t list
    }
end

let pending_occupancy host =
  let module O = Dispatch_occupancy in
  let open Result.Let_syntax in
  let%bind () = current host in
  let%bind principal =
    principal host ~scopes:P.Scope.[ Send_messages; View_session_transcript ]
  in
  let actor =
    Operator_authorization.guarded ~principal ~is_current:(fun () ->
      Result.is_ok (current host))
  in
  let inspect (grant : Configuration.Controller.t) =
    let controller =
      P.Session_ref.create ~server_id:host.server_id ~session_id:grant.session_id
    in
    let read () =
      let%bind state =
        Session_factory.read_recurrence_state
          host.factory
          ~session:controller
          ~actor
          ~authorize:(fun summary ->
            let%bind () = current host in
            if P.Id.Session.equal summary.P.Session.id grant.session_id
            then Ok ()
            else conflict "recurrence occupancy read returned another controller")
      in
      let entries =
        Option.value_map
          state.recurrence_catalog
          ~default:[]
          ~f:A.Recurrence_catalog.entries
      in
      List.fold_result entries ~init:[] ~f:(fun known entry ->
        let%map frontier = A.Recurrence_entry.frontier entry in
        let executions =
          A.Recurrence_frontier.executions frontier
          |> List.filter ~f:A.Recurrence_execution.is_pending
          |> List.map ~f:(fun execution ->
            let intent = A.Recurrence_execution.intent execution in
            { O.Known.occurrence = Intent.occurrence intent
            ; target = Intent.target intent
            })
        in
        let protected =
          A.Recurrence_frontier.protected_children frontier
          |> List.filter_map ~f:(fun reservation ->
            let occurrence = A.Recurrence_child_reservation.occurrence reservation in
            let terminal =
              Option.exists
                (A.Recurrence_frontier.find_execution frontier occurrence)
                ~f:(fun execution -> not (A.Recurrence_execution.is_pending execution))
            in
            if terminal
            then None
            else
              Some
                { O.Known.occurrence
                ; target =
                    P.Session_ref.create
                      ~server_id:host.server_id
                      ~session_id:
                        (A.Recurrence_child_reservation.child_session_id reservation)
                })
        in
        known @ executions @ protected)
    in
    match read () with
    | Ok known -> known, None
    | Error error ->
      (* The unreadable durable catalog may retain targets removed from this
         restart's configuration. Its uncertainty therefore covers every local
         target; current grant selectors cannot prove historical nonoverlap. *)
      [], Some { O.Unknown.controller; error }
  in
  let known, unknown =
    List.fold
      host.configuration.controllers
      ~init:([], [])
      ~f:(fun (known, unknown) grant ->
        let next, failure = inspect grant in
        known @ next, Option.to_list failure @ unknown)
  in
  let%map () = current host in
  { O.known; unknown = List.rev unknown }
;;

let acquire_captured_target (controller : Controller.t) ~occurrence =
  let open Result.Let_syntax in
  let%bind () = Controller.basis controller in
  let%bind () =
    if controller.grant.captured_children
    then Ok ()
    else denied "captured recurrence recovery is not configured"
  in
  let%bind child, target =
    Session_factory.acquire_recurrence_child
      controller.host.factory
      ~parent:controller.entry
      ~actor:controller.actor
      ~authorize:(Controller.authorize_creation controller)
      ~occurrence
  in
  captured_target controller ~child ~target
;;
