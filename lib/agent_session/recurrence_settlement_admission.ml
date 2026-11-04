open! Core
module P = Agent_protocol
module C = Recurrence_catalog
module E = Recurrence_entry
module F = Recurrence_definition
module H = Recurrence_frontier
module I = Recurrence_identity
module Execution = Recurrence_execution
module Origin = Recurrence_run_origin
module Reference = Recurrence_run_reference
module Settlement = Recurrence_settlement

type t =
  { previous_catalog : C.t
  ; entry : E.t
  ; definition : F.t
  ; catalog : C.t
  ; settlement : Settlement.t
  ; origin : Origin.t
  ; terminal : Reference.t option
  ; automatic : Settlement.Automatic.t
  ; actor : Operator_authorization.t
  ; authorize : Session_state.t -> F.t -> (unit, P.Error.t) result
  ; automatic_policy : Session_state.t -> F.t -> (bool, P.Error.t) result
  ; current : Recurrence_controller.observation
  ; session_id : P.Id.Session.t
  ; generation : int
  ; revision : int64
  }

let definition t = t.definition
let catalog t = t.catalog
let settlement t = t.settlement
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let find_entry (state : Session_state.t) occurrence =
  Option.bind state.recurrence_catalog ~f:(fun catalog ->
    C.find catalog (I.Definition.recurrence_id (I.Occurrence.definition occurrence)))
;;

let same_entry previous incoming =
  match E.to_document previous, E.to_document incoming with
  | Ok previous, Ok incoming ->
    Document_schema.Json.equal
      (Document_schema.Document.json previous)
      (Document_schema.Document.json incoming)
  | Error _, _ | _, Error _ -> false
;;

let check_original actor ~authorize state definition =
  Recurrence_command_admission.check_write
    actor
    ~authorize:(fun state -> authorize state definition)
    state
;;

let automatic_eligible (state : Session_state.t) current entry definition =
  P.Session.equal_desired_state state.lifecycle.desired Running
  && (not state.halted)
  && Option.is_none state.failure
  && E.equal_control (E.control entry) Enabled
  && String.equal (F.digest definition) (F.digest (E.current entry))
  && Recurrence_controller.automatic current
  && P.Session_lifecycle.Revision.equal
       (Recurrence_controller.revision current)
       (F.birth_lifecycle_revision definition)
  && Option.exists
       (Recurrence_source.installed state |> Result.ok)
       ~f:(fun installed -> P.Run_source.equal installed (F.source definition))
;;

let definitive_delay definition terminal =
  match F.schedule definition, terminal with
  | Interval { rule; _ }, Some terminal ->
    Recurrence_rules.Rule.Interval.equal_mode
      (Recurrence_rules.Rule.Interval.mode rule)
      Fixed_delay
    &&
      (match Reference.outcome terminal with
      | Completed | Failed _ | Cancelled | Limited -> true
      | Interrupted -> false)
  | Calendar _, _ | Interval _, None -> false
;;

let changed_evidence entry ~origin ~terminal =
  let open Result.Let_syntax in
  let%bind frontier = E.frontier entry in
  match
    List.find (H.executions frontier) ~f:(fun execution ->
      I.Occurrence.equal
        (Recurrence_execution_intent.occurrence (Execution.intent execution))
        (Origin.occurrence origin))
  with
  | None -> Ok false
  | Some previous ->
    let%bind incoming = Execution.associate previous origin in
    let%map incoming =
      match terminal with
      | None -> Ok incoming
      | Some terminal -> Execution.settle incoming terminal
    in
    not (Execution.equal previous incoming)
;;

let check_basis t (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () =
    if
      Operator_authorization.is_current t.actor
      && P.Principal.has_scope (Operator_authorization.principal t.actor) Send_messages
    then Ok ()
    else
      Error
        (P.Error.create
           Permission_denied
           ~message:"recurrence original authority expired, was revoked or lacks scope"
           ~retryable:false
           ())
  in
  let%bind () =
    if
      Recurrence_controller.is_current t.current
      && P.Id.Session.equal t.session_id state.identity.session_id
      && Int.equal t.generation state.identity.generation
      && Int64.equal t.revision state.counters.revision
      && Option.exists state.recurrence_catalog ~f:(fun catalog ->
        Int64.equal (C.revision t.previous_catalog) (C.revision catalog))
    then Ok ()
    else conflict "recurrence settlement controller observation changed"
  in
  let%bind () =
    match find_entry state (Origin.occurrence t.origin) with
    | Some entry when same_entry t.entry entry -> Ok ()
    | Some _ | None -> conflict "recurrence settlement exact retained basis changed"
  in
  match t.automatic with
  | Historical_only -> Ok ()
  | Current_policy_allows ->
    if automatic_eligible state t.current t.entry t.definition
    then Ok ()
    else conflict "recurrence settlement automatic eligibility changed"
;;

let check_current t (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () = check_original t.actor ~authorize:t.authorize state t.definition in
  let%bind () = check_basis t state in
  let%bind () =
    match t.automatic with
    | Historical_only -> Ok ()
    | Current_policy_allows ->
      let%bind allowed = t.automatic_policy state t.definition in
      if allowed then Ok () else conflict "recurrence settlement automatic policy changed"
  in
  let%bind () = check_original t.actor ~authorize:t.authorize state t.definition in
  check_basis t state
;;

let prepare
      (state : Session_state.t)
      ~controller
      ~actor
      ~authorize
      ~automatic_policy
      ~origin
      ~terminal
  =
  let open Result.Let_syntax in
  let%bind () =
    Recurrence_command_admission.check_write actor ~authorize:(fun _ -> Ok ()) state
  in
  let key = I.Occurrence.definition (Origin.occurrence origin) in
  let%bind () =
    if
      P.Session_ref.equal
        (I.Definition.controller key)
        (Recurrence_controller.session controller)
      && P.Id.Session.equal
           state.identity.session_id
           (P.Session_ref.session_id (Recurrence_controller.session controller))
    then Ok ()
    else conflict "recurrence settlement belongs to another controller"
  in
  let%bind entry =
    Result.of_option
      (find_entry state (Origin.occurrence origin))
      ~error:(P.Error.invalid_request "settlement recurrence identity is not retained")
  in
  let%bind definition =
    Result.of_option
      (List.find (E.definitions entry) ~f:(fun definition ->
         I.Definition.equal key (F.key definition)))
      ~error:(P.Error.invalid_request "settlement definition is not retained")
  in
  let%bind () = check_original actor ~authorize state definition in
  (* Lower strict row classification has no control candidate. An unselected
     historical hold must not reject a valid automatic successor alternative. *)
  let%bind changed = changed_evidence entry ~origin ~terminal in
  let%bind current = Recurrence_controller.observe controller state in
  let%bind () = check_original actor ~authorize state definition in
  let%bind () =
    if Recurrence_controller.is_current current
    then Ok ()
    else conflict "recurrence settlement lifecycle changed during observation"
  in
  let%bind allowed =
    if
      changed
      && definitive_delay definition terminal
      && automatic_eligible state current entry definition
    then automatic_policy state definition
    else Ok false
  in
  let automatic : Settlement.Automatic.t =
    if allowed then Current_policy_allows else Historical_only
  in
  let previous_catalog =
    Option.value state.recurrence_catalog ~default:Recurrence_catalog.empty
  in
  let%bind catalog, settlement =
    C.import_execution previous_catalog ~origin ~terminal ~automatic
  in
  let t =
    { previous_catalog
    ; entry
    ; definition
    ; catalog
    ; settlement
    ; origin
    ; terminal
    ; automatic
    ; actor
    ; authorize
    ; automatic_policy
    ; current
    ; session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; revision = state.counters.revision
    }
  in
  let%map () = check_current t state in
  t
;;

let hold t =
  let open Result.Let_syntax in
  match t.settlement with
  | Imported { next = Successor _; _ } ->
    let%map catalog, settlement =
      C.import_execution
        t.previous_catalog
        ~origin:t.origin
        ~terminal:t.terminal
        ~automatic:Historical_only
    in
    { t with catalog; settlement; automatic = Historical_only }
  | Imported { next = Evidence_only | Review_hold _; _ }
  | Retained_exact _ | Detail_not_retained _ ->
    Error (P.Error.invalid_request "settlement has no proposed successor to hold")
;;
