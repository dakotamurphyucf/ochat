open! Core
module P = Agent_protocol
module Admission = Recurrence_native_admission
module Request = Recurrence_native_request
module Command = Recurrence_command
module Receipt = Recurrence_command_receipt

type t =
  { state : unit -> (Session_state.t, P.Error.t) result
  ; query :
      admission:Admission.t
      -> request:Recurrence_query.request
      -> (Jsonaf.t, P.Error.t) result
  ; capture_target :
      admission:Admission.t
      -> target:Request.Target.t
      -> (Recurrence_target.t, P.Error.t) result
  ; apply : admission:Admission.t -> request:Command.t -> (Receipt.t, P.Error.t) result
  }

let create ~state ~query ~capture_target ~apply = { state; query; capture_target; apply }

let conflict () =
  Error
    (P.Error.create
       Idempotency_conflict
       ~message:"recurrence key does not match its complete original request"
       ~retryable:false
       ())
;;

let check t ~admission ~effect_kind =
  let open Result.Let_syntax in
  let%bind state = t.state () in
  let%map () = Admission.check_now admission state ~effect_kind in
  state
;;

let selector_matches (selector : Request.Target.t) retained =
  match selector, Recurrence_target.view retained with
  | Existing_session selected, Existing_session retained ->
    P.Session_ref.equal selected.session retained.session
    && Document_schema.Json.equal
         (P.Session.Message_content.to_json selected.input)
         (P.Session.Message_content.to_json retained.input)
  | Authored_child selected, Fresh_child retained ->
    (match retained.source_kind, retained.mode with
     | Authored_tool tool, Some mode ->
       String.equal selected.name tool.name && P.Run.Mode.equal selected.mode mode
     | Artifact, _ | Authored_tool _, None -> false)
  | Existing_session _, Fresh_child _ | Authored_child _, Existing_session _ -> false
;;

let retained_target catalog ~(receipt : Receipt.t) ~id ~kind ~selector =
  let open Result.Let_syntax in
  if
    not
      (P.Id.Recurrence.equal id receipt.recurrence_id
       && Receipt.equal_kind kind receipt.kind)
  then conflict ()
  else (
    let%bind entry =
      Result.of_option
        (Recurrence_catalog.find catalog id)
        ~error:
          (P.Error.create
             Idempotency_conflict
             ~message:"acknowledged recurrence definition is no longer retained"
             ~retryable:false
             ())
    in
    let definition =
      List.find (Recurrence_entry.definitions entry) ~f:(fun definition ->
        Int64.equal
          receipt.definition_revision
          (Recurrence_identity.Definition.revision (Recurrence_definition.key definition)))
    in
    match definition with
    | None -> conflict ()
    | Some definition ->
      let target = Recurrence_definition.target definition in
      if selector_matches selector target then Ok target else conflict ())
;;

let command (state : Session_state.t) (basis : Request.Basis.t) mutation =
  Command.create
    ~session_id:state.identity.session_id
    ~generation:basis.generation
    ~source:basis.source
    ~expected_lifecycle_revision:basis.expected_lifecycle_revision
    ~expected_catalog_revision:basis.expected_catalog_revision
    ~expected_control_revision:basis.expected_control_revision
    ~key:basis.key
    ~mutation
;;

module Action = struct
  type t =
    | Create of Request.Capture.t
    | Edit of Request.Capture.t
    | Enable
    | Pause
    | Retire

  let kind = function
    | Create _ -> Receipt.Create
    | Edit _ -> Edit
    | Enable -> Enable
    | Pause -> Pause
    | Retire -> Retire
  ;;
end

let write t ~admission ~effect_kind ~id ~(basis : Request.Basis.t) ~action =
  let open Result.Let_syntax in
  let%bind state = check t ~admission ~effect_kind in
  let catalog = Option.value state.recurrence_catalog ~default:Recurrence_catalog.empty in
  let principal_id = (Operator_authorization.principal (Admission.actor admission)).id in
  let retained =
    List.find (Recurrence_catalog.receipts catalog) ~f:(fun receipt ->
      P.Id.Principal.equal principal_id receipt.Receipt.principal_id
      && P.Idempotency_key.equal basis.key receipt.key)
  in
  let capture (capture : Request.Capture.t) =
    let%map target =
      match retained with
      | Some receipt ->
        retained_target
          catalog
          ~receipt
          ~id
          ~kind:(Action.kind action)
          ~selector:capture.target
      | None -> t.capture_target ~admission ~target:capture.target
    in
    Command.Capture.{ schedule = capture.schedule; target; policy = capture.policy }
  in
  let%bind mutation =
    match action with
    | Action.Create requested ->
      let%map capture = capture requested in
      Command.Mutation.Create (id, capture)
    | Edit requested ->
      let%map capture = capture requested in
      Command.Mutation.Edit (id, capture)
    | Enable -> Ok (Command.Mutation.Enable id)
    | Pause -> Ok (Command.Mutation.Pause id)
    | Retire -> Ok (Command.Mutation.Retire id)
  in
  let%bind request = command state basis mutation in
  (* This check compares every echoed CAS field, including generation, before
     applying a retained key. It never derives current expectations for a retry. *)
  let%bind () =
    match retained with
    | None -> Ok ()
    | Some receipt ->
      let%bind request_sha256 = Command.digest request in
      if String.equal receipt.request_sha256 request_sha256 then Ok () else conflict ()
  in
  let%bind _ = check t ~admission ~effect_kind in
  let%map receipt = t.apply ~admission ~request in
  (* The actual Actor ACK remains success even if authority changes afterwards. *)
  `Object [ "version", `Number "1"; "receipt", Receipt.to_json receipt ]
;;

let run t ~admission ~request =
  let read effect_kind request =
    let open Result.Let_syntax in
    let%bind _ = check t ~admission ~effect_kind in
    t.query ~admission ~request
  in
  match request with
  | Request.List -> read List Recurrence_query.List
  | Get id -> read Get (Get id)
  | Status id -> read Status (Status id)
  | History id -> read History (History id)
  | Create { id; basis; capture } ->
    write t ~admission ~effect_kind:Create ~id ~basis ~action:(Action.Create capture)
  | Edit { id; basis; capture } ->
    write t ~admission ~effect_kind:Edit ~id ~basis ~action:(Action.Edit capture)
  | Pause { id; basis } ->
    write t ~admission ~effect_kind:Pause ~id ~basis ~action:Action.Pause
  | Resume { id; basis } ->
    write t ~admission ~effect_kind:Resume ~id ~basis ~action:Action.Enable
  | Cancel { id; basis } ->
    write t ~admission ~effect_kind:Cancel ~id ~basis ~action:Action.Retire
;;
