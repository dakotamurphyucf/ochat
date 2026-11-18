open! Core
module P = Agent_protocol
module E = Recurrence_entry
module F = Recurrence_definition
module I = Recurrence_identity
module Intent = Recurrence_execution_intent
module Attempt = Recurrence_execution_attempt

type decision =
  | Retained of Recurrence_execution.t
  | New of
      { catalog : Recurrence_catalog.t
      ; execution : Recurrence_execution.t
      }

type t =
  { entry : E.t
  ; definition : F.t
  ; intent : Intent.t
  ; attempt : Attempt.t
  ; decision : decision
  ; actor : Operator_authorization.t
  ; authorize : Session_state.t -> F.t -> (unit, P.Error.t) result
  ; current : Recurrence_controller.observation
  ; session_id : P.Id.Session.t
  ; generation : int
  ; revision : int64
  }

let decision t = t.decision
let entry t = t.entry
let nominal t = Intent.nominal t.intent
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let check_original actor ~authorize state definition =
  Recurrence_command_admission.check_write
    actor
    ~authorize:(fun state -> authorize state definition)
    state
;;

let find_entry (state : Session_state.t) occurrence =
  Option.bind state.recurrence_catalog ~f:(fun catalog ->
    Recurrence_catalog.find
      catalog
      (I.Definition.recurrence_id (I.Occurrence.definition occurrence)))
;;

let same_entry a b =
  match E.to_document a, E.to_document b with
  | Ok a, Ok b ->
    Document_schema.Json.equal
      (Document_schema.Document.json a)
      (Document_schema.Document.json b)
  | Error _, _ | _, Error _ -> false
;;

let check_new t state =
  match t.decision with
  | Retained _ -> Ok ()
  | New _ ->
    let open Result.Let_syntax in
    let%bind installed = Recurrence_source.installed state in
    if
      Recurrence_controller.automatic t.current
      && P.Session_lifecycle.Revision.equal
           (Recurrence_controller.revision t.current)
           (F.birth_lifecycle_revision t.definition)
      && P.Run_source.equal installed (F.source t.definition)
      && E.equal_control (E.control t.entry) Enabled
      && String.equal (F.digest (E.current t.entry)) (F.digest t.definition)
      && Int64.equal (Intent.control_revision t.intent) (E.control_revision t.entry)
      && P.Id.Principal.equal
           (Attempt.principal_id t.attempt)
           (Operator_authorization.principal t.actor).id
    then Ok ()
    else conflict "new recurrence execution source, lifecycle, control or caller changed"
;;

let check_basis t (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () =
    if
      Recurrence_controller.is_current t.current
      && P.Id.Session.equal t.session_id state.identity.session_id
      && Int.equal t.generation state.identity.generation
      && Int64.equal t.revision state.counters.revision
    then Ok ()
    else conflict "recurrence execution controller observation changed"
  in
  let%bind () =
    match find_entry state (Intent.occurrence t.intent) with
    | Some entry when same_entry t.entry entry -> Ok ()
    | Some _ | None -> conflict "recurrence execution exact retained basis changed"
  in
  check_new t state
;;

let check_current t state =
  let open Result.Let_syntax in
  let%bind () = check_original t.actor ~authorize:t.authorize state t.definition in
  check_basis t state
;;

let prepare (state : Session_state.t) ~controller ~actor ~authorize ~intent ~attempt =
  let open Result.Let_syntax in
  (* Scope is checked before resolving immutable request data. The exact policy
     follows once its retained historical definition is known. *)
  let%bind () =
    Recurrence_command_admission.check_write actor ~authorize:(fun _ -> Ok ()) state
  in
  let occurrence = Intent.occurrence intent in
  let definition_key = I.Occurrence.definition occurrence in
  let%bind () =
    if
      P.Session_ref.equal
        (I.Definition.controller definition_key)
        (Recurrence_controller.session controller)
      && P.Id.Session.equal
           state.identity.session_id
           (P.Session_ref.session_id (Recurrence_controller.session controller))
    then Ok ()
    else conflict "recurrence execution belongs to another controller"
  in
  let%bind entry =
    Result.of_option
      (find_entry state occurrence)
      ~error:(P.Error.invalid_request "execution recurrence identity is not retained")
  in
  let%bind definition =
    Result.of_option
      (List.find (E.definitions entry) ~f:(fun definition ->
         I.Definition.equal definition_key (F.key definition)))
      ~error:(P.Error.invalid_request "execution definition is not retained")
  in
  let%bind () = check_original actor ~authorize state definition in
  let catalog = Option.value state.recurrence_catalog ~default:Recurrence_catalog.empty in
  (* The pure owner compares the complete original intent and attempt zero on
     replay, even when the current attempt has since advanced. Classify before
     imposing any new-work source/control/principal/grace requirements. *)
  let%bind next, execution =
    Recurrence_catalog.record_execution catalog ~intent ~attempt
  in
  let decision =
    if
      Int64.equal (Recurrence_catalog.revision next) (Recurrence_catalog.revision catalog)
    then Retained execution
    else New { catalog = next; execution }
  in
  let%bind current = Recurrence_controller.observe controller state in
  let t =
    { entry
    ; definition
    ; intent
    ; attempt
    ; decision
    ; actor
    ; authorize
    ; current
    ; session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; revision = state.counters.revision
    }
  in
  let%map () = check_current t state in
  t
;;
