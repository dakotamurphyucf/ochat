open! Core
module P = Agent_protocol
module E = Recurrence_entry
module F = Recurrence_definition
module I = Recurrence_identity

type decision =
  | Retained of Recurrence_execution.t
  | New of
      { catalog : Recurrence_catalog.t
      ; execution : Recurrence_execution.t
      }

type t =
  { entry : E.t
  ; intent : Recurrence_execution_intent.t
  ; next : Recurrence_execution_attempt.t
  ; definition : F.t
  ; actor : Operator_authorization.t
  ; authorize : Session_state.t -> F.t -> (unit, P.Error.t) result
  ; current : Recurrence_controller.observation
  ; session_id : P.Id.Session.t
  ; generation : int
  ; revision : int64
  ; decision : decision
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let decision t = t.decision
let entry t = t.entry
let nominal t = Recurrence_execution_intent.nominal t.intent

let check_policy t state =
  Recurrence_command_admission.check_write
    t.actor
    ~authorize:(fun state -> t.authorize state t.definition)
    state
;;

let same_entry previous current =
  match E.to_document previous, E.to_document current with
  | Ok previous, Ok current ->
    Document_schema.Json.equal
      (Document_schema.Document.json previous)
      (Document_schema.Document.json current)
  | Error _, _ | _, Error _ -> false
;;

let check_basis t (state : Session_state.t) =
  let open Result.Let_syntax in
  let retained =
    Option.bind state.recurrence_catalog ~f:(fun catalog ->
      Recurrence_catalog.find catalog (E.id t.entry))
  in
  let%bind () =
    if
      P.Id.Session.equal state.identity.session_id t.session_id
      && Int.equal state.identity.generation t.generation
      && Int64.equal state.counters.revision t.revision
      && Option.exists retained ~f:(same_entry t.entry)
      && Recurrence_controller.is_current t.current
    then Ok ()
    else conflict "retry controller exact observation changed"
  in
  match t.decision with
  | Retained _ -> Ok ()
  | New _ ->
    let%bind installed = Recurrence_source.installed state in
    if
      Recurrence_controller.automatic t.current
      && P.Session_lifecycle.Revision.equal
           (Recurrence_controller.revision t.current)
           (F.birth_lifecycle_revision t.definition)
      && P.Run_source.equal installed (F.source t.definition)
      && E.equal_control (E.control t.entry) Enabled
      && String.equal (F.digest (E.current t.entry)) (F.digest t.definition)
      && Int64.equal
           (Recurrence_execution_intent.control_revision t.intent)
           (E.control_revision t.entry)
      && P.Id.Principal.equal
           (Recurrence_execution_attempt.principal_id t.next)
           (Operator_authorization.principal t.actor).id
    then Ok ()
    else conflict "retry controller source, lifecycle, control or caller changed"
;;

let prepare state ~controller ~actor ~authorize ~intent ~previous ~resolution ~next =
  let open Result.Let_syntax in
  let%bind () =
    Recurrence_command_admission.check_write actor ~authorize:(fun _ -> Ok ()) state
  in
  let occurrence = Recurrence_execution_intent.occurrence intent in
  let definition_key = I.Occurrence.definition occurrence in
  let%bind () =
    if
      P.Session_ref.equal
        (I.Definition.controller definition_key)
        (Recurrence_controller.session controller)
      && P.Id.Session.equal
           state.Session_state.identity.session_id
           (P.Session_ref.session_id (Recurrence_controller.session controller))
    then Ok ()
    else conflict "retry belongs to another controller"
  in
  let catalog = Option.value state.recurrence_catalog ~default:Recurrence_catalog.empty in
  let%bind entry =
    Result.of_option
      (Recurrence_catalog.find catalog (I.Definition.recurrence_id definition_key))
      ~error:(P.Error.invalid_request "retry recurrence is not retained")
  in
  let%bind definition =
    Result.of_option
      (List.find (E.definitions entry) ~f:(fun definition ->
         I.Definition.equal definition_key (F.key definition)))
      ~error:(P.Error.invalid_request "retry definition is not retained")
  in
  let%bind () =
    Recurrence_command_admission.check_write
      actor
      ~authorize:(fun state -> authorize state definition)
      state
  in
  let%bind updated, execution =
    Recurrence_catalog.rotate_execution catalog ~intent ~previous ~resolution ~next
  in
  let decision =
    if
      Int64.equal
        (Recurrence_catalog.revision catalog)
        (Recurrence_catalog.revision updated)
    then Retained execution
    else New { catalog = updated; execution }
  in
  let%bind current = Recurrence_controller.observe controller state in
  let t =
    { entry
    ; intent
    ; next
    ; definition
    ; actor
    ; authorize
    ; current
    ; session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; revision = state.counters.revision
    ; decision
    }
  in
  let%bind () = check_policy t state in
  let%map () = check_basis t state in
  t
;;
