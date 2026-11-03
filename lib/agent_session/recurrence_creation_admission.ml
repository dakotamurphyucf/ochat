open! Core
module P = Agent_protocol
module E = Recurrence_entry
module F = Recurrence_definition
module H = Recurrence_frontier
module I = Recurrence_identity
module D = Document_schema

type t =
  { entry : E.t
  ; frontier : H.t
  ; nominal : Recurrence_nominal.t
  ; reservation : Recurrence_child_reservation.t option
  ; actor : Operator_authorization.t
  ; current : Recurrence_controller.observation
  }

let entry t = t.entry
let nominal t = t.nominal
let reservation t = t.reservation
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let original_current actor =
  Operator_authorization.is_current actor
  && P.Principal.has_scope (Operator_authorization.principal actor) Create_sessions
;;

let check_original actor =
  if original_current actor
  then Ok ()
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:
           "recurrence creation original authority expired, was revoked or lacks \
            Create_sessions"
         ~retryable:false
         ())
;;

let find_entry state id =
  Option.bind state.Session_state.recurrence_catalog ~f:(fun catalog ->
    Recurrence_catalog.find catalog id)
;;

let same_frontier a b =
  match H.to_document a, H.to_document b with
  | Ok a, Ok b -> D.Json.equal (D.Document.json a) (D.Document.json b)
  | Error _, _ | _, Error _ -> false
;;

let check_current t (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () = check_original t.actor in
  let%bind installed = Recurrence_source.installed state in
  let definition = E.current t.entry in
  let%bind () =
    if
      Recurrence_controller.is_current t.current
      && Recurrence_controller.automatic t.current
      && P.Session_lifecycle.Revision.equal
           (Recurrence_controller.revision t.current)
           (F.birth_lifecycle_revision definition)
      && P.Run_source.equal installed (F.source definition)
    then Ok ()
    else conflict "recurrence creation source or lifecycle changed"
  in
  match find_entry state (E.id t.entry) with
  | None -> conflict "recurrence creation entry was removed"
  | Some current ->
    (match E.frontier current with
     | Error _ -> conflict "recurrence creation frontier is unavailable"
     | Ok frontier ->
       if
         E.equal_control (E.control current) Enabled
         && Int64.equal (E.control_revision current) (E.control_revision t.entry)
         && String.equal (F.digest (E.current current)) (F.digest definition)
         && Recurrence_cursor.equal (E.cursor current) (E.cursor t.entry)
         && same_frontier frontier t.frontier
       then Ok ()
       else conflict "recurrence creation exact basis changed")
;;

let prepare
      (state : Session_state.t)
      ~controller
      ~actor
      ~authorize
      ~occurrence
      ~expected_control_revision
  =
  let open Result.Let_syntax in
  let%bind () = check_original actor in
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
    else conflict "recurrence creation belongs to another controller"
  in
  let%bind entry =
    Result.of_option
      (find_entry state (I.Definition.recurrence_id definition_key))
      ~error:(P.Error.invalid_request "recurrence creation identity is unavailable")
  in
  let definition = E.current entry in
  let%bind () =
    if
      E.equal_control (E.control entry) Enabled
      && Int64.equal expected_control_revision (E.control_revision entry)
      && I.Definition.equal definition_key (F.key definition)
    then Ok ()
    else conflict "recurrence creation definition or control is obsolete"
  in
  let%bind () =
    match Recurrence_target.view (F.target definition) with
    | Fresh_child _ -> Ok ()
    | Existing_session _ ->
      Error (P.Error.invalid_request "creation requires fresh-child target")
  in
  let%bind frontier = E.frontier entry in
  let reservation =
    List.find (H.protected_children frontier) ~f:(fun row ->
      I.Occurrence.equal occurrence (Recurrence_child_reservation.occurrence row))
  in
  let%bind nominal =
    match reservation with
    | Some row ->
      let%bind () = Recurrence_child_reservation.validate row ~definition in
      if
        Int64.equal
          (Recurrence_child_reservation.control_revision row)
          expected_control_revision
      then Ok (Recurrence_child_reservation.nominal row)
      else conflict "creation reservation belongs to an earlier control frontier"
    | None ->
      Result.of_option
        (List.find (H.ready frontier) ~f:(fun nominal ->
           I.Occurrence.equal occurrence (Recurrence_nominal.key nominal)))
        ~error:(P.Error.invalid_request "creation occurrence is not Ready or protected")
  in
  let%bind () = authorize state definition in
  let%bind () = check_original actor in
  let%bind current = Recurrence_controller.observe controller state in
  let%bind () = authorize state definition in
  let t = { entry; frontier; nominal; reservation; actor; current } in
  let%map () = check_current t state in
  t
;;
