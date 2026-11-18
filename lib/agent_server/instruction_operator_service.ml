open! Core
module P = Agent_protocol
module A = Agent_session
module W = P.Instruction_control
module I = Instruction_content
module Request = A.Instruction_source_request
module Service = A.Instruction_service

type t =
  { registry : Session_registry.t
  ; authoring : Authoring_installation.t option
  }

let create ~registry ~authoring = { registry; authoring }
let error code message = Error (P.Error.create code ~message ~retryable:false ())

let current actor scope =
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope (Operator_authorization.principal actor) scope
  then Ok ()
  else error Permission_denied "instruction operator authority denied or expired"
;;

let visible actor summary =
  if Authorization.session_visible_to (Operator_authorization.principal actor) summary
  then Ok ()
  else error Permission_denied "instruction session is not visible to this operator"
;;

let authorize actor scope summary =
  let open Result.Let_syntax in
  let%bind () = current actor scope in
  let%bind () = visible actor summary in
  current actor scope
;;

let sources t () =
  Option.bind t.authoring ~f:Authoring_installation.instruction_source_port
;;

let read_state t actor session_id =
  Session_registry.read_state
    t.registry
    ~authorize:(authorize actor View_session_transcript)
    session_id
;;

let read_service t actor session_id =
  Service.Shared.create
    ~state:(fun () -> read_state t actor session_id)
    ~apply:(fun ~admission:_ ~source:_ ~request:_ ->
      error Permission_denied "read-only instruction composition has no writer")
    ~sources:(sources t)
      (* This nonactivating reader has no physical agent execution epoch. It is
       never a provider-guidance service, and cannot grant one accidentally. *)
    ~selection_current:(fun _ -> false)
;;

let read_admission actor (state : A.Session_state.t) effect_kind =
  A.Instruction_admission.of_operator
    actor
    ~session_id:state.identity.session_id
    ~generation:state.identity.generation
    ~effect_kind
    ~attachment_id:None
    ~authorize:(fun state -> visible actor (A.Session_state.summary state))
;;

let with_reader t actor session_id effect_kind f =
  let open Result.Let_syntax in
  let%bind state = read_state t actor session_id in
  let%bind admission = read_admission actor state effect_kind in
  f (read_service t actor session_id) admission
;;

let owned_attachment context ~session_id ~attachment_id =
  if Connection_context.owns_attachment context ~session_id ~attachment_id
  then Ok ()
  else error Permission_denied "instruction attachment is not owned by this connection"
;;

let with_writer t actor context ~session_id ~attachment_id ~generation ~effect_kind f =
  let open Result.Let_syntax in
  let%bind () = current actor Send_messages in
  let%bind () = owned_attachment context ~session_id ~attachment_id in
  let%bind _ =
    Session_registry.read_state
      t.registry
      ~authorize:(authorize actor Send_messages)
      session_id
  in
  Session_registry.with_loaded_read t.registry session_id (fun entry ->
    let%bind admission =
      A.Instruction_admission.of_operator
        actor
        ~session_id
        ~generation
        ~effect_kind
        ~attachment_id:(Some attachment_id)
        ~authorize:(fun state -> visible actor (A.Session_state.summary state))
    in
    let service =
      Service.Shared.create
        ~state:(fun () -> A.Session_actor.state entry.actor)
        ~apply:(fun ~admission ~source ~request ->
          A.Session_actor.instruction_command_admitted
            entry.actor
            ~admission
            ~source
            ~request)
        ~sources:(sources t)
        ~selection_current:(A.Session_actor.instruction_selection_current entry.actor)
    in
    f service admission)
;;

let wire_root root = W.Root_id.of_string (I.Root_id.to_string root)

let domain_root root =
  I.Root_id.of_string (W.Root_id.to_string root)
  |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid instruction root ID")
;;

let wire_member observed =
  let id = I.Observed.id observed in
  let%map.Result root = wire_root (I.Id.root id) in
  W.Member.create ~root ~member:(I.Id.member id) ~digest:(I.Observed.digest observed)
;;

let domain_member (member : W.Member.t) =
  let%map.Result root = domain_root member.root in
  Request.Member.create ~root ~member:member.member ~digest:member.digest
;;

let domain_selection = function
  | W.Selection.Project { root; scope } ->
    let%map.Result root = domain_root root in
    Request.Selection.Project
      { root; scope = Option.value_map scope ~default:I.Scope.root ~f:I.Scope.of_path }
  | Skills roots ->
    let%map.Result roots = List.map roots ~f:domain_root |> Result.all in
    Request.Selection.Skills roots
;;

let domain_request source =
  let open Result.Let_syntax in
  let%bind selection = domain_selection (W.Source_request.selection source) in
  let%bind members =
    List.map (W.Source_request.members source) ~f:domain_member |> Result.all
  in
  Request.create ~selection ~members
;;

let wire_project capture =
  let open Result.Let_syntax in
  let%bind root =
    match A.Instruction_capture.root capture with
    | Some root -> wire_root root
    | None -> error Invalid_state "selected project capture has no root"
  in
  let selection =
    W.Selection.Project
      { root; scope = I.Scope.path (A.Instruction_capture.scope capture) }
  in
  let%bind members =
    List.map (A.Instruction_capture.loaded capture) ~f:(fun loaded ->
      wire_member (I.Loaded.observed loaded))
    |> Result.all
  in
  W.Source_request.create ~selection ~members
;;

let wire_receipt (receipt : A.Instruction_command.Receipt.t) =
  W.Receipt.create
    ~principal_id:receipt.principal_id
    ~key:receipt.key
    ~request_sha256:receipt.request_sha256
    ~session_id:receipt.session_id
    ~generation:receipt.generation
    ~revision:receipt.revision
    ~accepted_at:receipt.accepted_at
;;

let permitted_actions t actor context (request : W.Request.Inspect.t) =
  let reads = [ W.Action.Inspect; Refresh; Capture ] in
  if not (P.Principal.has_scope (Operator_authorization.principal actor) Send_messages)
  then Ok reads
  else (
    match request.attachment_id with
    | None -> Ok reads
    | Some attachment_id ->
      let open Result.Let_syntax in
      let%bind () =
        owned_attachment context ~session_id:request.session_id ~attachment_id
      in
      Session_registry.with_loaded_read t.registry request.session_id (fun entry ->
        match A.Session_actor.authorize_writer entry.actor ~attachment_id with
        | Ok () -> Ok (reads @ [ Activate; Remove ])
        | Error failure ->
          (match failure.P.Error.code with
           | Invalid_request | Permission_denied | Lease_stale -> Ok reads
           | _ -> Error failure)))
;;

let inspect t actor context request =
  let open Result.Let_syntax in
  let%bind _ = read_state t actor request.W.Request.Inspect.session_id in
  (* Writer checks can yield. Complete them before the final source/current
     disclosure bracket; no Actor RPC occurs after that bracket returns. *)
  let%bind permitted_actions = permitted_actions t actor context request in
  with_reader t actor request.session_id Status (fun service admission ->
    let%bind inspection = Service.Shared.inspect service ~admission in
    let%bind project =
      match inspection.project with
      | None -> Ok None
      | Some capture -> Result.map (wire_project capture) ~f:Option.some
    in
    let%bind skills =
      List.map inspection.skills ~f:(fun loaded -> wire_member (I.Loaded.observed loaded))
      |> Result.all
    in
    let%map view =
      W.View.create
        ~session_id:inspection.session_id
        ~generation:inspection.generation
        ~summary:inspection.summary
        ~project
        ~skills
        ~permitted_actions
    in
    W.Result.Inspect view)
;;

let refresh t actor (request : W.Request.Refresh.t) =
  let open Result.Let_syntax in
  let%bind selection =
    match request.selection with
    | None -> Ok None
    | Some selection -> Result.map (domain_selection selection) ~f:Option.some
  in
  with_reader t actor request.session_id Refresh (fun service admission ->
    let%bind result = Service.Shared.discover service ~admission ~selection in
    let%bind roots = List.map result.roots ~f:wire_root |> Result.all in
    let%bind members =
      Option.value_map result.discovery ~default:[] ~f:Request.Discovery.observations
      |> List.map ~f:wire_member
      |> Result.all
    in
    let%bind issues =
      Option.value_map result.discovery ~default:[] ~f:Request.Discovery.issues
      |> List.map ~f:(fun (root, member, _) ->
        let%map.Result root = wire_root root in
        W.Discovery.Issue.{ root; member; code = Skipped })
      |> Result.all
    in
    let%map discovery = W.Discovery.create ~roots ~members ~issues in
    W.Result.Refresh discovery)
;;

let capture t actor (request : W.Request.Capture.t) =
  let open Result.Let_syntax in
  let%bind source = domain_request request.source in
  with_reader t actor request.session_id Read (fun service admission ->
    let%bind captured = Service.Shared.capture service ~admission ~request:source in
    let%map capture_id =
      Chatml_source.Digest.of_hex (A.Instruction_capture.id captured)
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid instruction capture ID")
    in
    W.Result.Capture (W.Capture.create ~source:request.source ~capture_id))
;;

let activate t actor context (request : W.Request.Activate.t) =
  let open Result.Let_syntax in
  let%bind source = domain_request request.source in
  with_writer
    t
    actor
    context
    ~session_id:request.session_id
    ~attachment_id:request.attachment_id
    ~generation:request.expected_generation
    ~effect_kind:Activate
    (fun service admission ->
       let%bind receipt =
         Service.Shared.activate
           service
           ~admission
           ~request:source
           ~capture_id:(Chatml_source.Digest.to_hex request.capture_id)
           ~expected_revision:request.expected_revision
           ~key:request.key
       in
       let%map receipt = wire_receipt receipt in
       W.Result.Activate receipt)
;;

let remove t actor context (request : W.Request.Remove.t) =
  with_writer
    t
    actor
    context
    ~session_id:request.session_id
    ~attachment_id:request.attachment_id
    ~generation:request.expected_generation
    ~effect_kind:Deactivate
    (fun service admission ->
       let open Result.Let_syntax in
       let deactivate change =
         Service.Shared.deactivate
           service
           ~admission
           ~change
           ~expected_revision:request.expected_revision
           ~key:request.key
       in
       let%bind receipt =
         match request.change with
         | W.Request.Remove.All -> deactivate A.Instruction_command.Change.Clear
         | Project -> deactivate A.Instruction_command.Change.Remove_project
         | Skills members ->
           let%bind members = List.map members ~f:domain_member |> Result.all in
           Service.Shared.remove_skills
             service
             ~admission
             ~members
             ~expected_revision:request.expected_revision
             ~key:request.key
       in
       let%map receipt = wire_receipt receipt in
       W.Result.Remove receipt)
;;

let execute t ~actor ~context request =
  let open Result.Let_syntax in
  let%bind actor = Connection_context.request_actor context (Some actor) in
  let scope =
    match request with
    | W.Request.Inspect _ | Refresh _ | Capture _ -> P.Scope.View_session_transcript
    | Activate _ | Remove _ -> Send_messages
  in
  let%bind () = current actor scope in
  match request with
  | W.Request.Inspect request -> inspect t actor context request
  | Refresh request -> refresh t actor request
  | Capture request -> capture t actor request
  | Activate request -> activate t actor context request
  | Remove request -> remove t actor context request
;;
