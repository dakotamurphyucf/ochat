open Core
module CM = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module Programs = Chatml_prompt_programs
module Startup = Chat_response.Startup_compilation
module Driver = Chat_response.Driver
module Port = Chat_response.Tool.Agent_source_runner
module Preparation = Runtime_script_preparation
module Portable_path = Source.Portable_path

module Binding_issuer = struct
  type t = unit ref

  let create () = ref ()
end

let numeric_identity host_tag policy =
  [%sexp (Chatml_source.Digest.to_hex host_tag : string), (policy : Sexp.t)]
  |> Sexp.to_string_mach
  |> fun contents -> Chatml_source.Digest.of_contents contents ~checkpoint:Eio.Fiber.check
;;

let content_policy_identity limits =
  Option.sexp_of_t
    (fun limits ->
       let module L = Chatmd_prompt_content.Limits in
       let document = L.document limits in
       let module D = Document_schema.Limits in
       [%sexp
         (L.occurrences limits : int)
       , (L.parser_inputs limits : int)
       , (L.input_bytes limits : int)
       , (L.output_bytes limits : int)
       , (L.work_steps limits : int)
       , (L.nodes limits : int)
       , (L.depth limits : int)
       , (L.payloads limits : int)
       , (L.payload_bytes limits : int)
       , (D.max_bytes document : int)
       , (D.max_depth document : int)
       , (D.max_fields document : int)
       , (D.max_nodes document : int)])
    limits
;;

module Source = struct
  type t =
    { policy_revision : Chatml_source.Digest.t
    ; compilation : Chatml_compilation.limits
    ; parser_limits : CM.Limits.t
    ; max_work_steps : int
    ; preprocess : string -> string
    ; prepare_programs :
        Startup.t
        -> tool_contract:Chatml_compilation.Tool_contract.t
        -> invocation:Preparation.Invocation.t
        -> Prompt_source_owner.Scope.t
        -> Prompt_source_owner.Capture.t
        -> (Programs.t, Prompt_revision_builder.Diagnostic.t list) Result.t
    }

  let create
        ~policy_revision
        ~compilation
        ~parser_limits
        ~max_work_steps
        ~preprocess
        ~prepare_programs
    =
    let open Or_error.Let_syntax in
    let%bind () =
      Chatml_compilation.validate_limits compilation
      |> Result.map_error ~f:(fun error ->
        Error.of_string error.Chatml_compilation.message)
    in
    if max_work_steps <= 0
    then Or_error.error_string "Source work allowance must be positive"
    else (
      let policy_revision =
        numeric_identity
          policy_revision
          [%sexp
            ("source.v1" : string)
          , (compilation.wall_seconds : float)
          , (compilation.max_source_bytes : int)
          , (CM.Limits.policy_identity parser_limits : Sexp.t)
          , (max_work_steps : int)]
      in
      Ok
        { policy_revision
        ; compilation
        ; parser_limits
        ; max_work_steps
        ; preprocess
        ; prepare_programs
        })
  ;;

  let policy_identity t = t.policy_revision
end

module Installed = struct
  type t =
    { source : Source.t
    ; artifact_store : Agent_store.Prompt_artifact_store.t
    ; source_lease : Prompt_source_lease.t
    ; env : Eio_unix.Stdenv.base
    ; binding_issuer : Binding_issuer.t option
    }
end

type t =
  { domain_id : int
  ; script_preparation : Preparation.t
  ; tool_contract : Chatml_compilation.Tool_contract.t
  ; developer_literal_child : bool
  ; content_limits : Chatmd_prompt_content.Limits.t option
  ; policy_revision : Chatml_source.Digest.t
  ; manifest_authorizer : Shell_runtime.Manifest_authorizer.t
  ; approval_provider : Shell_runtime.Approval_broker.provider
  ; response_dir : Eio.Fs.dir_ty Eio.Path.t
  }

let runtime_policy_identity
      ~script_preparation
      ~tool_contract
      ~content_limits
      ~policy_revision
  =
  numeric_identity
    policy_revision
    [%sexp
      ("runtime.v1" : string)
    , (Preparation.policy_identity script_preparation : Sexp.t)
    , (tool_contract : Chatml_compilation.Tool_contract.t)
    , (content_policy_identity content_limits : Sexp.t)]
;;

let create
      ~script_preparation
      ~tool_contract
      ~content_limits
      ~policy_revision
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
  =
  let policy_revision =
    runtime_policy_identity
      ~script_preparation
      ~tool_contract
      ~content_limits
      ~policy_revision
  in
  { domain_id = (Stdlib.Domain.self () :> int)
  ; script_preparation
  ; tool_contract
  ; developer_literal_child = false
  ; content_limits
  ; policy_revision
  ; manifest_authorizer
  ; approval_provider
  ; response_dir
  }
;;

let policy_identity t = t.policy_revision

let failure message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let check_domain t =
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then Error (failure "captured agent runner belongs to another Eio domain")
  else Ok ()
;;

let bounded t ~checkpoint ~f =
  let module Abort = struct
    exception Limit
  end
  in
  let remaining = ref (Preparation.max_custody_work_steps t.script_preparation) in
  let poll () =
    checkpoint ();
    if !remaining <= 0 then raise Abort.Limit;
    decr remaining
  in
  try
    poll ();
    let result = f poll in
    poll ();
    result
  with
  | Abort.Limit -> Error (failure "captured agent lookup exceeds host work allowance")
;;

let admit_developer_literal_child t revision parsed ~checkpoint =
  if not t.developer_literal_child
  then Ok ()
  else
    bounded t ~checkpoint ~f:(fun checkpoint ->
      Developer_child_preflight.admit_literal ~revision ~parsed ~checkpoint)
;;

let compilation_error (error : Chatml_compilation.error) =
  failure (error.code ^ ": " ^ error.message)
;;

let admit_revision t revision ~checkpoint =
  bounded t ~checkpoint ~f:(fun checkpoint ->
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    match Prompt_revision.parser_context revision, Prompt_revision.programs revision with
    | Some parsed, Some custody ->
      let artifact = Prompt_revision.artifact revision in
      let root = CM.Parsed.root_input parsed in
      let same_root =
        String.equal
          (Input.identity_digest root ~checkpoint)
          (Input.identity_digest (Programs.root_input custody) ~checkpoint)
      in
      if
        (not same_root)
        || (not (Input.Role.equal (Input.role root) Root))
        || (not (String.equal (Input.path root) artifact.root_relative_path))
        || not (Input.authored_matches root artifact.root_chatmd ~checkpoint)
      then Error (failure "captured agent revision has inconsistent root custody")
      else (
        let projection = Prompt_revision.document_projection revision in
        if
          Option.exists projection ~f:(fun projection ->
            not (phys_equal parsed (CM.Document_projection.parsed projection)))
          || (Option.is_some artifact.content_bundle && Option.is_none projection)
          || Option.is_none artifact.program_bundle
        then Error (failure "captured agent revision has inconsistent input custody")
        else (
          let%map manifest =
            Chatml_source.Digest.of_hex artifact.manifest_sha256
            |> Result.map_error ~f:(fun _ -> failure "invalid captured manifest digest")
          in
          let revision =
            Chatml_source.Digest.of_contents
              ("captured-agent-runner-v1\000"
               ^ Chatml_source.Digest.to_hex manifest
               ^ "\000"
               ^ Chatml_source.Digest.to_hex t.policy_revision)
              ~checkpoint
          in
          parsed, projection, revision))
    | None, None | None, Some _ | Some _, None ->
      Error (failure "captured agent runner requires owned parser and program custody"))
;;

let tool_reference t parsed declaration ~checkpoint =
  bounded t ~checkpoint ~f:(fun checkpoint ->
    let rec find found = function
      | [] ->
        (match found with
         | None -> Error (failure "agent tool is not owned by the defining parser")
         | Some reference -> Ok reference)
      | reference :: rest ->
        checkpoint ();
        let matches =
          match CM.Agent_reference.invocation reference with
          | Tool actual -> phys_equal actual declaration
          | Persistent_tool _ | Content _ -> false
        in
        if matches
        then (
          match found with
          | None -> find (Some reference) rest
          | Some _ -> Error (failure "agent tool has ambiguous parser ownership"))
        else find found rest
    in
    find None (CM.Parsed.agent_references parsed))
;;

let runner_result result =
  Result.map_error result ~f:(fun error -> error.Agent_protocol.Error.message)
  |> Result.ok_or_failwith
;;

type invocation_target =
  | Tool_target of Ochat_function.t option
  | Content_target

let select_installed_child
      installed
      ~script_preparation
      ~tool_contract
      ~content_limits
      ~parent
      ~reference
      ~(invocation : Preparation.Invocation.t)
      ~recorded
      ~checkpoint
  =
  match recorded with
  | Some recorded ->
    Prompt_child_source.restore
      ~artifact_store:installed.Installed.artifact_store
      ~definition:(Prompt_revision.definition parent)
      ~content_limits
      recorded
      ~checkpoint
  | None ->
    (match CM.Agent_reference.source_mode reference with
     | Captured ->
       Prompt_child_source.captured
         ~parent
         ~reference
         ~artifact_store:installed.artifact_store
         ~policy:script_preparation
         ~tool_contract
         ~invocation
         ~content_limits
         ~env:installed.env
         ~checkpoint
     | File ->
       let source = installed.source in
       let created_at =
         Eio.Time.now (Eio.Stdenv.clock installed.env)
         |> Time_ns.Span.of_sec
         |> Time_ns.of_span_since_epoch
         |> Agent_protocol.Timestamp.of_time_ns
       in
       Prompt_child_source.file
         ~parent
         ~reference
         ~source_lease:installed.source_lease
         ~artifact_store:installed.artifact_store
         ~compilation:source.compilation
         ~tool_contract
         ~parser_limits:source.parser_limits
         ~content_limits
         ~max_work_steps:source.max_work_steps
         ~preprocess:source.preprocess
         ~prepare_programs:(fun startup ~tool_contract scope capture ->
           source.prepare_programs startup ~tool_contract ~invocation scope capture)
         ~env:installed.env
         ~created_at
         ~checkpoint)
;;

let select_child t installed ~parent ~reference ~invocation ~recorded ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  select_installed_child
    installed
    ~script_preparation:t.script_preparation
    ~tool_contract:t.tool_contract
    ~content_limits:t.content_limits
    ~parent
    ~reference
    ~invocation
    ~recorded
    ~checkpoint
;;

module Claimed_developer = struct
  module B = Chat_response.Background_request
  module C = Chat_response.Tool_capability
  module P = Agent_protocol
  module J = Developer_job_scope
  module N = Native_tool_invocation
  module D = Developer_invocation_owner.Selected

  type t =
    { active : J.active
    ; parent : J.Parent.t
    ; artifact : B.Agent_artifact.t
    ; binding : C.binding
    ; invocation : P.Host_invocation.t
    ; check_current : unit -> (unit, P.Error.t) result
    ; domain_id : int
    ; live : bool Atomic.t
    }

  let key : t Eio.Fiber.key = Eio.Fiber.create_key ()
  let invalid message = Error (P.Error.invalid_request message)

  let live t =
    Atomic.get t.live
    && Int.equal t.domain_id (Stdlib.Domain.self () :> int)
    && J.Parent.same_active t.parent t.active
    && Option.value_map (Eio.Fiber.get key) ~default:false ~f:(phys_equal t)
  ;;

  let with_saved_artifact
        ~active
        ~parent
        ~request
        ~policy
        ~prepared
        ~binding
        ~agent_runtime
        ~max_registrations
        ~checkpoint
        ~invocation
        ~check_current
        f
    =
    let open Result.Let_syntax in
    let claim = J.claim active in
    let job = Agent_store.Developer_run_store.Claim.job claim in
    let%bind stored = B.of_json ~policy (P.Developer_job.payload job) in
    let%bind artifact =
      B.agent_artifact stored
      |> Result.of_option
           ~error:(P.Error.invalid_request "claimed Tool has no saved Agent artifact")
    in
    let%bind () =
      if
        J.Parent.same_active parent active
        && B.Target_kind.equal (B.target_kind stored) B.Target_kind.Tool
        && String.equal (B.fingerprint stored) (B.fingerprint request)
      then Ok ()
      else invalid "saved Agent artifact is foreign to the entered Worker Claim"
    in
    let%bind () =
      match prepared with
      | B.Script _ -> invalid "saved Agent artifact requires a Tool execution"
      | Tool { capabilities; reference; _ } ->
        let actual = C.reference binding in
        let context = P.Host_invocation.context invocation in
        if
          P.Id.Capability.equal reference.id actual.id
          && String.equal reference.fingerprint actual.fingerprint
          && String.equal reference.name context.tool_name
          && String.equal
               reference.implementation_revision
               context.implementation_revision
          && String.equal (C.fingerprint capabilities) context.capability_fingerprint
          && Option.equal
               P.Id.Job.equal
               context.parent_job
               (Some (P.Developer_job.id job))
          && Option.is_some (C.native_implementation binding)
        then B.validate_capabilities stored ~capabilities
        else invalid "saved Agent artifact has a foreign selected native binding"
    in
    let%bind () =
      Chat_response.Agent_runtime.require_saved_agent_source
        agent_runtime
        ~binding
        ~max_registrations
        ~checkpoint
    in
    let%bind () = check_current () in
    let scoped =
      { active
      ; parent
      ; artifact
      ; binding
      ; invocation
      ; check_current
      ; domain_id = (Stdlib.Domain.self () :> int)
      ; live = Atomic.make true
      }
    in
    Exn.protect
      ~finally:(fun () -> Atomic.set scoped.live false)
      ~f:(fun () -> Eio.Fiber.with_binding key scoped f)
  ;;

  let selected ~borrowed ~implementation =
    let open Result.Let_syntax in
    match Eio.Fiber.get key with
    | None -> Ok None
    | Some scoped ->
      let expected = C.native_implementation scoped.binding in
      (match expected with
       | None -> invalid "claimed Agent source binding is no longer native"
       | Some expected when not (phys_equal expected implementation) -> Ok None
       | Some _ ->
         let%bind () =
           if live scoped then Ok () else invalid "claimed Agent source scope has expired"
         in
         let actual = N.selected_borrowed_invocation borrowed in
         let expected_context = P.Host_invocation.context scoped.invocation in
         let actual_context = P.Host_invocation.context actual in
         if not (P.Id.Invocation.equal expected_context.id actual_context.id)
         then invalid "saved Agent source belongs to another native invocation"
         else (
           let issuer = N.selected_borrowed_issuer borrowed in
           let%bind inherited =
             D.inherited_job_parent issuer (N.selected_borrowed_parent borrowed)
           in
           let%bind () =
             match inherited with
             | Some inherited when J.Parent.same_claim scoped.parent inherited -> Ok ()
             | Some _ | None ->
               invalid "saved Agent source belongs to another Worker Claim"
           in
           let%bind () = N.check_selected_source borrowed ~implementation in
           let%bind () = scoped.check_current () in
           let%bind () = N.check_selected_source borrowed ~implementation in
           if live scoped
           then Ok (Some scoped.artifact)
           else invalid "saved Agent scope ended"))
  ;;

  module Selected = struct
    type nonrec t =
      { scope : t
      ; borrowed : N.selected_borrowed
      ; implementation : Ochat_function.t
      ; live : bool Atomic.t
      }

    type selected = t

    let key : t Eio.Fiber.key = Eio.Fiber.create_key ()
    let active t = t.scope.active
    let invocation t = t.scope.invocation
    let matches_borrowed t borrowed = phys_equal t.borrowed borrowed

    let check_now t =
      let open Result.Let_syntax in
      let%bind current = N.borrow_selected () in
      if
        (not (Atomic.get t.live))
        || (not (phys_equal current t.borrowed))
        || (not (live t.scope))
        || (not (J.Parent.same_active t.scope.parent t.scope.active))
        ||
        match C.native_implementation t.scope.binding with
        | Some implementation -> not (phys_equal implementation t.implementation)
        | None -> true
      then invalid "saved Agent source callback is no longer selected"
      else (
        let%map _ = N.selected_borrowed_capabilities t.borrowed in
        ())
    ;;

    let check t =
      let open Result.Let_syntax in
      let%bind () = check_now t in
      let%bind artifact =
        selected ~borrowed:t.borrowed ~implementation:t.implementation
      in
      match artifact with
      | Some artifact when phys_equal artifact t.scope.artifact -> check_now t
      | Some _ | None -> invalid "saved Agent source changed after selected admission"
    ;;

    let check_descendant_now t ~child =
      let open Result.Let_syntax in
      if
        (not (Atomic.get t.live))
        || (not (live t.scope))
        || (not (J.Parent.same_active t.scope.parent t.scope.active))
        ||
        match C.native_implementation t.scope.binding with
        | Some implementation -> not (phys_equal implementation t.implementation)
        | None -> true
      then invalid "saved Agent source callback is no longer selected"
      else (
        let%bind () = N.check_selected_ancestor ~ancestor:t.borrowed ~child in
        let%map _ = N.selected_borrowed_capabilities t.borrowed in
        ())
    ;;

    module Target = struct
      type t =
        { saved : selected
        ; source : Prompt_child_source.t
        ; parent : Prompt_revision.t
        ; caller : Port.t
        ; binding_issuer : Binding_issuer.t option
        }

      let revision t = Prompt_child_source.revision t.source
      let caller_revision t = t.parent
      let saved t = t.saved

      let same_reference left right =
        P.Id.Prompt_revision.equal
          (B.Agent_artifact.revision_id left)
          (B.Agent_artifact.revision_id right)
        && Chatml_source.Digest.equal
             (B.Agent_artifact.manifest_sha256 left)
             (B.Agent_artifact.manifest_sha256 right)
        && Portable_path.equal
             (B.Agent_artifact.root_relative_path left)
             (B.Agent_artifact.root_relative_path right)
      ;;

      let check_reference t =
        if same_reference (Prompt_child_source.reference t.source) t.saved.scope.artifact
        then Ok ()
        else invalid "saved Agent target differs from its selected artifact"
      ;;

      let check t =
        let open Result.Let_syntax in
        let%bind () = check t.saved in
        let%bind () = check_reference t in
        check t.saved
      ;;

      let check_now t =
        let open Result.Let_syntax in
        let%bind () = check_now t.saved in
        check_reference t
      ;;

      let check_descendant_now t ~child =
        let open Result.Let_syntax in
        let%bind () = check_descendant_now t.saved ~child in
        check_reference t
      ;;
    end

    let target_for_current source ~parent ~caller ~binding_issuer =
      match Eio.Fiber.get key with
      | None -> invalid "saved Agent target has no selected source callback"
      | Some selected ->
        let target =
          Target.{ saved = selected; source; parent; caller; binding_issuer }
        in
        Result.map (Target.check target) ~f:(fun () -> target)
    ;;
  end

  let with_selected ~borrowed ~implementation f =
    let open Result.Let_syntax in
    let%bind artifact = selected ~borrowed ~implementation in
    match Eio.Fiber.get key, artifact with
    | Some scope, Some artifact when phys_equal artifact scope.artifact ->
      let selected =
        Selected.{ scope; borrowed; implementation; live = Atomic.make true }
      in
      let%bind () = Selected.check_now selected in
      Exn.protect
        ~finally:(fun () -> Atomic.set selected.live false)
        ~f:(fun () -> Eio.Fiber.with_binding Selected.key selected (fun () -> f selected))
    | Some _, (None | Some _) | None, _ ->
      invalid "saved Agent source was not selected by this native callback"
  ;;
end

module Selected_child_target = struct
  module Saved_target = Claimed_developer.Selected.Target
  module N = Native_tool_invocation

  module Node = struct
    type t =
      { saved : Saved_target.t
      ; parent : t option
      ; borrowed : N.selected_borrowed
      ; live : bool Atomic.t
      ; binding_issuer : Binding_issuer.t option
      ; identity : unit ref
      }

    let create ~saved ~parent ~borrowed ~live ~binding_issuer =
      { saved; parent; borrowed; live; binding_issuer; identity = ref () }
    ;;

    let invalid message = Error (Agent_protocol.Error.invalid_request message)

    let check_saved_now t ~current =
      if Claimed_developer.Selected.matches_borrowed (Saved_target.saved t.saved) current
      then Saved_target.check_now t.saved
      else Saved_target.check_descendant_now t.saved ~child:current
    ;;

    let rec check_descendant_now t ~child =
      let open Result.Let_syntax in
      if not (Atomic.get t.live)
      then invalid "selected child target has expired"
      else (
        let%bind () = N.check_selected_ancestor ~ancestor:t.borrowed ~child in
        match t.parent with
        | None -> check_saved_now t ~current:child
        | Some parent ->
          if phys_equal parent.borrowed child
          then check_now parent
          else check_descendant_now parent ~child)

    and check_now t =
      let open Result.Let_syntax in
      let%bind current = N.borrow_selected () in
      if (not (Atomic.get t.live)) || not (phys_equal current t.borrowed)
      then invalid "selected child target is not the current native callback"
      else (
        match t.parent with
        | None -> check_saved_now t ~current
        | Some parent ->
          if phys_equal parent.borrowed current
          then check_now parent
          else check_descendant_now parent ~child:current)
    ;;

    let check_current_or_descendant_now t =
      let open Result.Let_syntax in
      let%bind current = N.borrow_selected () in
      if phys_equal current t.borrowed
      then check_now t
      else check_descendant_now t ~child:current
    ;;

    let check t =
      let open Result.Let_syntax in
      let%bind () = check_now t in
      let%bind () = N.check_selected_content_context t.borrowed in
      check_now t
    ;;
  end

  module Model = struct
    type parent_identity =
      | Saved_parent of Saved_target.t
      | Child_parent of unit ref

    type t =
      { node : Node.t
      ; parent_identity : parent_identity
      ; source : Prompt_child_source.t
      ; caller_revision : Prompt_revision.t
      ; caller : Port.t
      }
  end

  module Tool = struct
    type parent_identity =
      | Saved_parent of Saved_target.t
      | Child_parent of unit ref

    type t =
      { node : Node.t
      ; parent_identity : parent_identity
      ; source : Prompt_child_source.t
      ; caller_revision : Prompt_revision.t
      ; caller : Port.t
      ; implementation : Ochat_function.t
      ; declaration : CM.agent_tool
      ; reference : CM.Agent_reference.t
      }
  end
end

module Selected_content_target = struct
  module Saved_target = Claimed_developer.Selected.Target
  module Node = Selected_child_target.Node

  type t =
    { parent : parent
    ; node : Node.t
    ; source : Prompt_child_source.t
    ; caller_revision : Prompt_revision.t
    ; caller : Port.t
    }

  and parent =
    | Saved of Saved_target.t
    | Content of t
    | Model of Selected_child_target.Model.t
    | Tool of Selected_child_target.Tool.t

  let revision (t : t) = Prompt_child_source.revision t.source
  let caller_revision (t : t) = t.caller_revision
  let saved_ancestor (t : t) = t.node.saved

  let matches_parent t parent =
    match t.parent, parent with
    | Saved actual, Saved expected -> phys_equal actual expected
    | Content actual, Content expected -> phys_equal actual expected
    | Model actual, Model expected -> phys_equal actual expected
    | Tool actual, Tool expected -> phys_equal actual expected
    | Saved _, (Content _ | Model _ | Tool _)
    | Content _, (Saved _ | Model _ | Tool _)
    | Model _, (Saved _ | Content _ | Tool _)
    | Tool _, (Saved _ | Content _ | Model _) -> false
  ;;

  let check_descendant_now t ~child = Node.check_descendant_now t.node ~child
  let check_now t = Node.check_now t.node
  let check_current_or_descendant_now t = Node.check_current_or_descendant_now t.node
  let check t = Node.check t.node
end

module Selected_model_target = struct
  module Parent = Selected_child_target.Model
  open Parent

  type t = Selected_child_target.Model.t

  let revision (t : t) = Prompt_child_source.revision t.source
  let caller_revision (t : t) = t.caller_revision
  let saved_ancestor (t : t) = t.node.saved

  let matches_parent t parent =
    match t.parent_identity, parent with
    | Parent.Saved_parent actual, Selected_content_target.Saved expected ->
      phys_equal actual expected
    | Parent.Child_parent actual, Selected_content_target.Content expected ->
      phys_equal actual expected.node.identity
    | Parent.Child_parent actual, Selected_content_target.Model expected ->
      phys_equal actual expected.node.identity
    | Parent.Child_parent actual, Selected_content_target.Tool expected ->
      phys_equal actual expected.node.identity
    | ( Parent.Saved_parent _
      , ( Selected_content_target.Content _
        | Selected_content_target.Model _
        | Selected_content_target.Tool _ ) )
    | Parent.Child_parent _, Selected_content_target.Saved _ -> false
  ;;

  let check_descendant_now t ~child =
    Selected_child_target.Node.check_descendant_now t.node ~child
  ;;

  let check_now t = Selected_child_target.Node.check_now t.node

  let check_current_or_descendant_now t =
    Selected_child_target.Node.check_current_or_descendant_now t.node
  ;;

  let check t = Selected_child_target.Node.check t.node
end

module Selected_tool_target = struct
  module Parent = Selected_child_target.Tool
  open Parent

  type t = Parent.t

  let revision (t : t) = Prompt_child_source.revision t.source
  let caller_revision (t : t) = t.caller_revision
  let saved_ancestor (t : t) = t.node.saved

  let matches_parent t parent =
    match t.parent_identity, parent with
    | Parent.Saved_parent actual, Selected_content_target.Saved expected ->
      phys_equal actual expected
    | Parent.Child_parent actual, Selected_content_target.Content expected ->
      phys_equal actual expected.node.identity
    | Parent.Child_parent actual, Selected_content_target.Model expected ->
      phys_equal actual expected.node.identity
    | Parent.Child_parent actual, Selected_content_target.Tool expected ->
      phys_equal actual expected.node.identity
    | ( Parent.Saved_parent _
      , ( Selected_content_target.Content _
        | Selected_content_target.Model _
        | Selected_content_target.Tool _ ) )
    | Parent.Child_parent _, Selected_content_target.Saved _ -> false
  ;;

  let check_now t = Selected_child_target.Node.check_now t.node

  let check_current_or_descendant_now t =
    Selected_child_target.Node.check_current_or_descendant_now t.node
  ;;

  let check_descendant_now t ~child =
    Selected_child_target.Node.check_descendant_now t.node ~child
  ;;

  let check t =
    let open Result.Let_syntax in
    let%bind () = check_now t in
    let%bind () =
      Native_tool_invocation.check_selected_source
        t.node.borrowed
        ~implementation:t.implementation
    in
    check_now t
  ;;
end

let child_node_of_parent parent ~borrowed ~live ~binding_issuer =
  let module Node = Selected_child_target.Node in
  let saved, parent_node =
    match parent with
    | Selected_content_target.Saved target -> target, None
    | Content target -> target.node.saved, Some target.node
    | Model target -> target.node.saved, Some target.node
    | Tool target -> target.node.saved, Some target.node
  in
  Node.create ~saved ~parent:parent_node ~borrowed ~live ~binding_issuer
;;

type selected_content_runner =
  target:Selected_content_target.t
  -> startup:Chat_response.Startup_compilation.t
  -> compilation_limits:Chatml_compilation.limits
  -> programs:Chatml_prompt_programs.Restored.t option
  -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> input:Prompt.Chat_markdown.content_item list
  -> source:string
  -> (string, Agent_protocol.Error.t) result

type selected_tool_runner =
  target:Selected_tool_target.t
  -> startup:Chat_response.Startup_compilation.t
  -> compilation_limits:Chatml_compilation.limits
  -> programs:Chatml_prompt_programs.Restored.t option
  -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> input:Prompt.Chat_markdown.content_item list
  -> observer:Chat_response.Driver.agent_observer option
  -> source:string
  -> (string, Agent_protocol.Error.t) result

type selected_saved_runner =
  target:Claimed_developer.Selected.Target.t
  -> startup:Chat_response.Startup_compilation.t
  -> compilation_limits:Chatml_compilation.limits
  -> programs:Chatml_prompt_programs.Restored.t option
  -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> input:Prompt.Chat_markdown.content_item list
  -> observer:Chat_response.Driver.agent_observer option
  -> source:string
  -> (string, Agent_protocol.Error.t) result

type child_handler =
  | Literal_child
  | Saved_child of selected_saved_runner
  | Tool_child of selected_tool_runner * Selected_tool_target.t
  | Content_child of selected_content_runner * Selected_content_target.t

exception Selected_content_denied of Agent_protocol.Error.t

let check_selected_content_parent = function
  | Selected_content_target.Saved target -> Claimed_developer.Selected.Target.check target
  | Content target -> Selected_content_target.check target
  | Model target -> Selected_model_target.check target
  | Tool target -> Selected_tool_target.check target
;;

let check_selected_parent_descendant_now parent ~child =
  match parent with
  | Selected_content_target.Saved target ->
    if
      Claimed_developer.Selected.matches_borrowed
        (Claimed_developer.Selected.Target.saved target)
        child
    then Claimed_developer.Selected.Target.check_now target
    else Claimed_developer.Selected.Target.check_descendant_now target ~child
  | Content target -> Selected_content_target.check_descendant_now target ~child
  | Model target -> Selected_model_target.check_descendant_now target ~child
  | Tool target -> Selected_tool_target.check_descendant_now target ~child
;;

module Content_selection = struct
  type t =
    { parent : Selected_content_target.parent
    ; run : selected_content_runner
    ; binding_issuer : Binding_issuer.t
    ; live : bool Atomic.t
    }

  let key : t Eio.Fiber.key = Eio.Fiber.create_key ()
end

module Tool_selection = struct
  type t =
    { parent : Selected_content_target.parent
    ; run : selected_tool_runner
    ; binding_issuer : Binding_issuer.t
    ; implementation : Ochat_function.t
    ; declaration : CM.agent_tool
    ; reference : CM.Agent_reference.t
    ; port : Port.t
    ; borrowed : Native_tool_invocation.selected_borrowed
    ; live : bool Atomic.t
    ; domain_id : int
    }

  let key : t Eio.Fiber.key = Eio.Fiber.create_key ()
  let invalid message = Error (Agent_protocol.Error.invalid_request message)

  let check_now t ~implementation ~port =
    let open Result.Let_syntax in
    let%bind current = Native_tool_invocation.borrow_selected () in
    if
      Atomic.get t.live
      && Int.equal t.domain_id (Stdlib.Domain.self () :> int)
      && Option.value_map (Eio.Fiber.get key) ~default:false ~f:(phys_equal t)
      && phys_equal current t.borrowed
      && phys_equal implementation t.implementation
      && phys_equal port t.port
    then check_selected_parent_descendant_now t.parent ~child:current
    else invalid "selected child Tool source is foreign or expired"
  ;;

  let check t ~implementation ~port =
    let open Result.Let_syntax in
    let%bind () = check_now t ~implementation ~port in
    let%bind () =
      Native_tool_invocation.check_selected_source t.borrowed ~implementation
    in
    check_now t ~implementation ~port
  ;;
end

type selected_source =
  | No_selected_source
  | Selected_tool_source of Native_tool_invocation.selected_borrowed * Ochat_function.t
  | Selected_content_context of Native_tool_invocation.selected_borrowed
  | Selected_child_tool_source of Tool_selection.t

let rec bind_with_checkpoint ?installed ?run_selected_target t ~revision ~sw ~checkpoint =
  let open Result.Let_syntax in
  let%map parsed, projection, identity = admit_revision t revision ~checkpoint in
  let identity =
    match installed with
    | None -> identity
    | Some installed ->
      Chatml_source.Digest.of_contents
        ("installed-agent-runner-v1\000"
         ^ Chatml_source.Digest.to_hex identity
         ^ "\000"
         ^ Chatml_source.Digest.to_hex installed.Installed.source.policy_revision)
        ~checkpoint
  in
  let activity = Runtime_activity.create ~sw in
  let rec port : Port.t =
    { revision = identity
    ; run =
        (fun ~implementation ~declaration ~ctx ~source ~observer items ->
          invoke
            t
            ?installed
            ?run_selected_target
            ~activity
            ~parent:revision
            ~caller:port
            ~target:(Tool_target implementation)
            ~ctx
            ~source
            ~observer
            ~reference:(fun ~checkpoint ->
              tool_reference t parsed declaration ~checkpoint)
            items)
    ; prepare =
        Option.map installed ~f:(fun installed ~declaration ~launch:_ ~checkpoint ->
          Runtime_activity.run activity (fun () ->
            let open Result.Let_syntax in
            let%bind reference = tool_reference t parsed declaration ~checkpoint in
            let%map selected =
              select_child
                t
                installed
                ~parent:revision
                ~reference
                ~invocation:Headless
                ~recorded:None
                ~checkpoint
            in
            Prompt_child_source.reference selected))
    ; run_content =
        (fun ~declaration ~ctx ->
          invoke
            t
            ?installed
            ?run_selected_target
            ~activity
            ~parent:revision
            ~caller:port
            ~target:Content_target
            ~ctx
            ~source:"captured-content-agent"
            ~observer:None
            ~reference:(fun ~checkpoint ->
              CM.content_agent_reference
                parsed
                ?projection
                ~max_work_steps:(Preparation.max_custody_work_steps t.script_preparation)
                ~checkpoint
                declaration
              |> Result.map_error ~f:(fun error -> failure (Error.to_string_hum error)))
            declaration.CM.items)
    }
  in
  port

and invoke
      ?installed
      ?run_selected_target
      t
      ~activity
      ~parent
      ~caller
      ~target
      ~ctx
      ~source
      ~observer
      ~reference
      items
  =
  check_domain t |> runner_result;
  let selected_content =
    match target with
    | Content_target -> Eio.Fiber.get Content_selection.key
    | Tool_target _ -> None
  in
  Runtime_activity.with_switch activity (fun ~sw ->
    let env = Chat_response.Ctx.env ctx in
    let checkpoint = Eio.Fiber.check in
    let run
          ?(check_selected = fun () -> Ok ())
          caller
          startup
          ~revision
          ~compilation_limits
          ~programs
      =
      let poll () = Startup.checkpoint startup in
      let open Result.Let_syntax in
      let%bind child =
        Startup.within startup ~map_error:compilation_error ~f:(fun () ->
          bind_with_checkpoint ?installed t ~revision ~sw ~checkpoint:poll)
      in
      let%bind parsed =
        match Prompt_revision.parser_context revision with
        | Some parsed -> Ok parsed
        | None -> Error (failure "selected child has no owned parser")
      in
      let child_dir =
        Eio.Path.(
          Prompt_revision.materialized_tree revision
          / Filename.dirname (Prompt_revision.root_relative_path revision))
      in
      let%bind () = check_selected () in
      Ok
        (Driver.run_prepared_agent
           parsed
           ~agent_source_runner:child
           ~input_agent_source_runner:caller
           ~startup
           ~compilation_limits
           ~checkpoint:poll
           ?programs
           ?document_projection:(Prompt_revision.document_projection revision)
           ~prompt_dir:child_dir
           ~response_dir:t.response_dir
           ?observer
           ~source
           ~shell_manifest_authorizer:t.manifest_authorizer
           ~shell_approval_provider:t.approval_provider
           ~ctx
           items)
    in
    let open Result.Let_syntax in
    let result =
      match installed with
      | None ->
        Startup.with_context
          ~limits:(Preparation.compilation t.script_preparation)
          ~env
          ~checkpoint
          ~map_error:compilation_error
          ~f:(fun startup ->
            let poll () = Startup.checkpoint startup in
            let%bind reference =
              Startup.within startup ~map_error:compilation_error ~f:(fun () ->
                reference ~checkpoint:poll)
            in
            Preparation.with_captured_child
              t.script_preparation
              ~startup
              ~tool_contract:t.tool_contract
              parent
              ~reference
              ~invocation:Headless
              ~content_limits:t.content_limits
              ~env
              ~checkpoint:poll
              ~f:(run caller))
          ()
      | Some installed ->
        (* A recorded selection belongs only to this actual tool invocation.
           Content occurrences keep their own defining revision and must never
           consume the enclosing queued target. Both paths reject expired/foreign
           services before source selection. *)
        let%bind direct_tool_scope =
          match Eio.Fiber.get Tool_selection.key with
          | Some scope ->
            (match Native_tool_invocation.borrow_selected () with
             | Ok current when phys_equal current scope.borrowed ->
               (match target with
                | Tool_target (Some implementation)
                  when phys_equal caller scope.port
                       && phys_equal implementation scope.implementation ->
                  Ok (Some scope)
                | Tool_target _ ->
                  Error (failure "selected Tool has a foreign port or implementation")
                | Content_target -> Ok None)
             | Ok _ | Error _ -> Ok None)
          | None -> Ok None
        in
        let%bind recorded, selected_source =
          match direct_tool_scope, Script_tool_calls.native_services_scope () with
          | Some scope, Unbound ->
            (match target with
             | Tool_target (Some implementation) ->
               let%bind () =
                 match installed.binding_issuer with
                 | Some issuer when phys_equal issuer scope.binding_issuer -> Ok ()
                 | None | Some _ ->
                   Error (failure "selected Tool has a foreign source binding")
               in
               let%map () = Tool_selection.check scope ~implementation ~port:caller in
               None, Selected_child_tool_source scope
             | Content_target | Tool_target None ->
               Error (failure "selected child Tool loan has another source callback"))
          | Some _, (Active _ | Expired) ->
            Error (failure "selected child Tool source overlaps another native owner")
          | None, Active services ->
            (match target with
             | Tool_target implementation ->
               Script_tool_calls.agent_artifact_for_implementation
                 services
                 ~implementation
               |> Result.map ~f:(fun recorded -> recorded, No_selected_source)
             | Content_target ->
               Script_tool_calls.agent_artifact services
               |> Result.map ~f:(fun _ -> None, No_selected_source))
          | None, Expired -> Error (failure "selected child native services have expired")
          | None, Unbound ->
            (match Native_tool_invocation.borrow_selected () with
             | Ok borrowed ->
               (match target with
                | Content_target ->
                  let%map () =
                    Native_tool_invocation.check_selected_content_context borrowed
                  in
                  None, Selected_content_context borrowed
                | Tool_target None ->
                  Error (failure "developer Agent source requires its exact native tool")
                | Tool_target (Some implementation) ->
                  let%bind () =
                    Native_tool_invocation.check_selected_source borrowed ~implementation
                  in
                  let%map recorded =
                    Claimed_developer.selected ~borrowed ~implementation
                  in
                  recorded, Selected_tool_source (borrowed, implementation))
             | Error _ ->
               (match Native_tool_invocation.current_scope () with
                | Expired ->
                  Error (failure "selected child native invocation has expired")
                | Unbound | Active _ -> Ok (None, No_selected_source)))
        in
        let%bind context =
          match recorded, target, Script_tool_calls.native_services_scope () with
          | None, Tool_target (Some implementation), Active services ->
            Script_tool_calls.agent_source_context
              services
              ~defining_revision:parent
              ~implementation
          | Some _, _, _
          | None, (Content_target | Tool_target None), _
          | None, Tool_target (Some _), (Expired | Unbound) -> Ok None
        in
        let%bind parent, caller, reference =
          match context with
          | None ->
            let%map reference = reference ~checkpoint in
            parent, caller, reference
          | Some context ->
            let parent = Agent_source_code_view.Context.revision context in
            let%map caller =
              bind_with_checkpoint ~installed t ~revision:parent ~sw ~checkpoint
            in
            parent, caller, Agent_source_code_view.Context.reference context
        in
        let%bind () =
          match selected_source with
          | Selected_child_tool_source scope ->
            if phys_equal reference scope.reference
            then
              Tool_selection.check scope ~implementation:scope.implementation ~port:caller
            else Error (failure "selected Tool parser declaration changed")
          | No_selected_source | Selected_tool_source _ | Selected_content_context _ ->
            Ok ()
        in
        let%bind selected =
          select_child
            t
            installed
            ~parent
            ~reference
            ~invocation:Headless
            ~recorded
            ~checkpoint
        in
        let check_selected () =
          match selected_source with
          | No_selected_source -> Ok ()
          | Selected_content_context borrowed ->
            Native_tool_invocation.check_selected_content_context borrowed
          | Selected_tool_source (borrowed, implementation) ->
            let open Result.Let_syntax in
            let%bind () =
              Native_tool_invocation.check_selected_source borrowed ~implementation
            in
            let%bind current = Claimed_developer.selected ~borrowed ~implementation in
            let same_reference left right =
              let module Artifact = Chat_response.Background_request.Agent_artifact in
              Agent_protocol.Id.Prompt_revision.equal
                (Artifact.revision_id left)
                (Artifact.revision_id right)
              && Chatml_source.Digest.equal
                   (Artifact.manifest_sha256 left)
                   (Artifact.manifest_sha256 right)
              && Portable_path.equal
                   (Artifact.root_relative_path left)
                   (Artifact.root_relative_path right)
            in
            if Option.equal same_reference recorded current
            then Ok ()
            else Error (failure "selected Agent source changed during restoration")
          | Selected_child_tool_source scope ->
            (match target with
             | Tool_target (Some implementation) ->
               Tool_selection.check scope ~implementation ~port:caller
             | Content_target | Tool_target None ->
               Error (failure "selected Tool source changed during restoration"))
        in
        let%bind () = check_selected () in
        let revision = Prompt_child_source.revision selected in
        let%bind parsed =
          Prompt_revision.parser_context revision
          |> Result.of_option
               ~error:(failure "selected developer child has no owned parser")
        in
        let%bind handler =
          match selected_source, selected_content, target, run_selected_target with
          | Selected_child_tool_source scope, None, Tool_target (Some _), _ ->
            let%bind () =
              Tool_selection.check scope ~implementation:scope.implementation ~port:caller
            in
            let live = Atomic.make true in
            let parent_identity =
              match scope.parent with
              | Saved saved -> Selected_child_target.Tool.Saved_parent saved
              | Content target -> Child_parent target.node.identity
              | Model target -> Child_parent target.node.identity
              | Tool target -> Child_parent target.node.identity
            in
            let tool_target =
              Selected_child_target.Tool.
                { node =
                    child_node_of_parent
                      scope.parent
                      ~borrowed:scope.borrowed
                      ~live
                      ~binding_issuer:(Some scope.binding_issuer)
                ; parent_identity
                ; source = selected
                ; caller_revision = parent
                ; caller
                ; implementation = scope.implementation
                ; declaration = scope.declaration
                ; reference = scope.reference
                }
            in
            let%map () = Selected_tool_target.check tool_target in
            Tool_child (scope.run, tool_target)
          | Selected_tool_source _, None, Tool_target (Some _), Some run ->
            Ok (Saved_child run)
          | ( (No_selected_source | Selected_content_context _)
            , Some scope
            , Content_target
            , _ ) ->
            let%bind () =
              if Atomic.get scope.live
              then Ok ()
              else Error (failure "selected Content child port has expired")
            in
            let%bind () =
              match installed.binding_issuer with
              | Some issuer when phys_equal issuer scope.binding_issuer -> Ok ()
              | None | Some _ ->
                Error (failure "selected Content child has a foreign source binding")
            in
            let%bind () = check_selected_content_parent scope.parent in
            let%bind borrowed = Native_tool_invocation.borrow_selected () in
            let live = Atomic.make true in
            let child =
              Selected_content_target.
                { parent = scope.parent
                ; node =
                    child_node_of_parent
                      scope.parent
                      ~borrowed
                      ~live
                      ~binding_issuer:installed.binding_issuer
                ; source = selected
                ; caller_revision = parent
                ; caller
                }
            in
            let%map () = Selected_content_target.check child in
            Content_child (scope.run, child)
          | ( (No_selected_source | Selected_tool_source _ | Selected_content_context _)
            , None
            , _
            , _ ) ->
            let%map () = admit_developer_literal_child t revision parsed ~checkpoint in
            Literal_child
          | _ -> Error (failure "selected child handlers cannot overlap")
        in
        Exn.protect
          ~finally:(fun () ->
            match handler with
            | Tool_child (_, target) ->
              Atomic.set target.Selected_child_target.Tool.node.live false
            | Content_child (_, target) ->
              Atomic.set target.Selected_content_target.node.live false
            | Literal_child | Saved_child _ -> ())
          ~f:(fun () ->
            Preparation.with_revision
              (Some t.script_preparation)
              ~tool_contract:t.tool_contract
              revision
              ~env
              ~invocation:Headless
              ~checkpoint
              ~f:(fun startup ~compilation_limits ~programs ->
                match handler with
                | Literal_child ->
                  run
                    ~check_selected
                    caller
                    startup
                    ~revision
                    ~compilation_limits
                    ~programs
                | Saved_child run_selected_target ->
                  let%bind target =
                    Claimed_developer.Selected.target_for_current
                      selected
                      ~parent
                      ~caller
                      ~binding_issuer:installed.binding_issuer
                  in
                  let%bind () = check_selected () in
                  run_selected_target
                    ~target
                    ~startup
                    ~compilation_limits
                    ~programs
                    ~ctx
                    ~input:items
                    ~observer
                    ~source
                | Tool_child (run_selected_tool, target) ->
                  let%bind () = check_selected () in
                  let%bind () = Selected_tool_target.check target in
                  run_selected_tool
                    ~target
                    ~startup
                    ~compilation_limits
                    ~programs
                    ~ctx
                    ~input:items
                    ~observer
                    ~source
                | Content_child (run_selected_content, target) ->
                  let%bind () = check_selected () in
                  let%bind () = Selected_content_target.check target in
                  run_selected_content
                    ~target
                    ~startup
                    ~compilation_limits
                    ~programs
                    ~ctx
                    ~input:items
                    ~source))
    in
    match selected_content, result with
    | Some _, Error error -> raise (Selected_content_denied error)
    | Some _, Ok value -> value
    | None, _ -> runner_result result)
;;

let bind t ~revision ~sw =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  bind_with_checkpoint t ~revision ~sw ~checkpoint:Eio.Fiber.check
;;

let bind_installed_with_selected
      ?run_selected_target
      ?binding_issuer
      t
      ~source
      ~artifact_store
      ~source_lease
      ~env
      ~revision
      ~sw
  =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let installed =
    Installed.
      { source
      ; artifact_store
      ; source_lease = Prompt_source_lease.borrow source_lease
      ; env
      ; binding_issuer
      }
  in
  bind_with_checkpoint
    ~installed
    ?run_selected_target
    t
    ~revision
    ~sw
    ~checkpoint:Eio.Fiber.check
;;

let bind_installed t ~source ~artifact_store ~source_lease ~env ~revision ~sw =
  bind_installed_with_selected t ~source ~artifact_store ~source_lease ~env ~revision ~sw
;;

let create_runner = create

module Source_binding = struct
  module P = Agent_protocol

  type t =
    { domain_id : int
    ; issuer : Binding_issuer.t
    ; source : Source.t
    ; tool_contract : Chatml_compilation.Tool_contract.t
    ; runtime_policy_revision : Chatml_source.Digest.t
    ; content_limits : Chatmd_prompt_content.Limits.t option
    ; artifact_store : Agent_store.Prompt_artifact_store.t
    ; source_lease : Prompt_source_lease.t
    }

  let create
        ~source
        ~tool_contract
        ~runtime_policy_revision
        ~content_limits
        ~artifact_store
        ~source_lease
    =
    { domain_id = (Stdlib.Domain.self () :> int)
    ; issuer = Binding_issuer.create ()
    ; source
    ; tool_contract
    ; runtime_policy_revision
    ; content_limits
    ; artifact_store
    ; source_lease = Prompt_source_lease.borrow source_lease
    }
  ;;

  module Policy_identity = struct
    type t =
      { source : Chatml_source.Digest.t
      ; runtime : Chatml_source.Digest.t
      }

    let source t = t.source
    let runtime t = t.runtime
  end

  let policy_identity t ~script_preparation =
    let runtime_digest =
      runtime_policy_identity
        ~script_preparation
        ~tool_contract:t.tool_contract
        ~content_limits:t.content_limits
        ~policy_revision:t.runtime_policy_revision
      |> fun base -> numeric_identity base [%sexp ("developer-literal-child.v1" : string)]
    in
    Policy_identity.{ source = Source.policy_identity t.source; runtime = runtime_digest }
  ;;

  module Claimed_source = struct
    type binding = t

    type t =
      { binding : binding
      ; active : Developer_job_scope.active
      ; parent : Developer_job_scope.Parent.t
      ; request : Chat_response.Background_request.t
      ; issuer_source : Chat_response.Background_request.Issuer_source.t
      ; revision : Prompt_revision.t
      ; owner : P.Execution_owner.developer
      ; capture_fingerprint : Chatml_source.Digest.t
      ; domain_id : int
      ; live : bool Atomic.t
      }

    let key : t Eio.Fiber.key = Eio.Fiber.create_key ()
    let revision t = t.revision
    let tool_contract t = t.binding.tool_contract
    let request t = t.request
    let issuer_source t = t.issuer_source
    let same_parent t parent = Developer_job_scope.Parent.same_claim t.parent parent

    let check_now t =
      if
        Atomic.get t.live
        && Int.equal t.domain_id (Stdlib.Domain.self () :> int)
        && Option.value_map (Eio.Fiber.get key) ~default:false ~f:(phys_equal t)
        && Developer_job_scope.Parent.same_active t.parent t.active
      then
        Developer_job_scope.Parent.check_now
          t.parent
          ~owner:t.owner
          ~capture_fingerprint:t.capture_fingerprint
      else Error (P.Error.invalid_request "claimed issuing source has expired")
    ;;
  end

  let with_claimed_source
        t
        ~active
        ~parent
        ~(definition : Prompt_definition.t)
        ~policy
        ~request
        ~checkpoint
        f
    =
    let module B = Chat_response.Background_request in
    let module J = Developer_job_scope in
    let open Result.Let_syntax in
    let invalid message = Error (P.Error.invalid_request message) in
    let claim = J.claim active in
    let job = Agent_store.Developer_run_store.Claim.job claim in
    let owner = Agent_store.Developer_run_store.Claim.owner claim in
    let capture_fingerprint = P.Developer_job.capture_fingerprint job in
    let%bind () =
      if
        Int.equal t.domain_id (Stdlib.Domain.self () :> int)
        && J.Parent.same_active parent active
      then J.Parent.check parent ~owner ~capture_fingerprint
      else invalid "claimed issuing source has a foreign Worker parent"
    in
    let%bind stored = B.of_json ~policy (P.Developer_job.payload job) in
    let%bind issuer =
      B.issuer_source stored
      |> Result.of_option
           ~error:(P.Error.invalid_request "claimed job has no saved issuing source")
    in
    let%bind () =
      if String.equal (B.fingerprint stored) (B.fingerprint request)
      then Ok ()
      else invalid "saved issuing source is foreign to the entered Worker Claim"
    in
    let%bind () =
      if
        P.Id.Prompt_definition.equal (B.Issuer_source.prompt issuer) definition.id
        && Chatml_compilation.Tool_contract.equal
             (B.Issuer_source.tool_contract issuer)
             t.tool_contract
        && Chatml_source.Digest.equal
             (B.Issuer_source.source_policy issuer)
             (Source.policy_identity t.source)
      then Ok ()
      else invalid "saved issuing source differs from its installed source policy"
    in
    let%bind () = J.Parent.check parent ~owner ~capture_fingerprint in
    let%bind restored =
      Prompt_child_source.restore
        ~artifact_store:t.artifact_store
        ~definition
        ~content_limits:t.content_limits
        (B.Issuer_source.artifact issuer)
        ~checkpoint
    in
    let%bind () = J.Parent.check parent ~owner ~capture_fingerprint in
    let scoped =
      Claimed_source.
        { binding = t
        ; active
        ; parent
        ; request = stored
        ; issuer_source = issuer
        ; revision = Prompt_child_source.revision restored
        ; owner
        ; capture_fingerprint
        ; domain_id = (Stdlib.Domain.self () :> int)
        ; live = Atomic.make true
        }
    in
    Exn.protect
      ~finally:(fun () -> Atomic.set scoped.live false)
      ~f:(fun () ->
        Eio.Fiber.with_binding Claimed_source.key scoped (fun () ->
          let%bind () = Claimed_source.check_now scoped in
          let%bind result = f scoped in
          let%map () = Claimed_source.check_now scoped in
          result))
  ;;

  let content_source_error failure =
    let module E = Prompt_source_owner.Error in
    let open P.Error in
    let code, retryable =
      match E.code failure with
      | Busy -> P.Error.Conflict, true
      | Revoked | Closed | Missing_live_root -> Lease_stale, false
      | Lease_limit -> Resource_limit, false
      | Invalid_binding
      | Foreign_receipt
      | Foreign_capture
      | Filesystem
      | Capture
      | Project_preparation
      | Document_capture -> Invalid_state, false
    in
    P.Error.create code ~message:"selected child source is unavailable" ~retryable ()
  ;;

  module Content_source = struct
    type binding = t

    type target =
      | Saved of Claimed_developer.Selected.Target.t
      | Content of Selected_content_target.t
      | Model of Selected_model_target.t
      | Tool of Selected_tool_target.t

    type t =
      { binding : binding
      ; target : target
      }

    type side =
      | Declaring
      | Caller

    module Selected_member = struct
      type t =
        { source : binding * target
        ; side : side
        ; parent : Prompt_revision.t
        ; member : Portable_path.t
        ; mode : P.Model_spawn_request.Source_mode.t
        }

      let mode t = t.mode
    end

    let check t =
      let open Result.Let_syntax in
      let%bind () =
        if Int.equal t.binding.domain_id (Stdlib.Domain.self () :> int)
        then Ok ()
        else Error (failure "selected child source belongs to another Eio domain")
      in
      match t.target with
      | Saved target ->
        let%bind () =
          match target.Claimed_developer.Selected.Target.binding_issuer with
          | Some issuer when phys_equal issuer t.binding.issuer -> Ok ()
          | None | Some _ ->
            Error (failure "selected child belongs to another source binding")
        in
        Claimed_developer.Selected.Target.check target
      | Content target ->
        let%bind () =
          match target.Selected_content_target.node.binding_issuer with
          | Some issuer when phys_equal issuer t.binding.issuer -> Ok ()
          | None | Some _ ->
            Error (failure "selected content belongs to another source binding")
        in
        Selected_content_target.check target
      | Model target ->
        let%bind () =
          match target.Selected_child_target.Model.node.binding_issuer with
          | Some issuer when phys_equal issuer t.binding.issuer -> Ok ()
          | None | Some _ ->
            Error (failure "selected Model belongs to another source binding")
        in
        Selected_model_target.check target
      | Tool target ->
        let%bind () =
          match target.Selected_child_target.Tool.node.binding_issuer with
          | Some issuer when phys_equal issuer t.binding.issuer -> Ok ()
          | None | Some _ ->
            Error (failure "selected Tool belongs to another source binding")
        in
        Selected_tool_target.check target
    ;;

    let revision t = function
      | Declaring ->
        (match t.target with
         | Saved target -> Claimed_developer.Selected.Target.revision target
         | Content target -> Selected_content_target.revision target
         | Model target -> Selected_model_target.revision target
         | Tool target -> Selected_tool_target.revision target)
      | Caller ->
        (match t.target with
         | Saved target -> Claimed_developer.Selected.Target.caller_revision target
         | Content target -> Selected_content_target.caller_revision target
         | Model target -> Selected_model_target.caller_revision target
         | Tool target -> Selected_tool_target.caller_revision target)
    ;;

    let matches_revision t ~side selected = phys_equal (revision t side) selected

    let select_local_member t ~side ~reference ~mode:selected_mode ~checkpoint =
      let open Result.Let_syntax in
      let%bind () = check t in
      let parent = revision t side in
      let%bind root =
        Portable_path.create (Prompt_revision.root_relative_path parent)
        |> Result.map_error ~f:(fun _ ->
          failure "selected Model parent root is not portable")
      in
      let%bind member =
        Source_loader.resolve_member root ~reference ~max_reference_bytes:1024 ~checkpoint
        |> Result.map_error ~f:(fun error -> failure (Error.to_string_hum error))
      in
      let%map () = check t in
      Selected_member.
        { source = t.binding, t.target; side; parent; member; mode = selected_mode }
    ;;

    let check_selected_member t member =
      let open Result.Let_syntax in
      let%bind () = check t in
      let binding, target = member.Selected_member.source in
      if
        phys_equal binding t.binding
        && phys_equal target t.target
        && phys_equal member.parent (revision t member.side)
      then check t
      else Error (failure "selected Model member belongs to another source port")
    ;;

    let with_read t ~side ~f =
      let module Owner = Prompt_source_owner in
      let open Result.Let_syntax in
      let%bind () = check t in
      let%bind read =
        Prompt_source_lease.with_scope t.binding.source_lease ~f:(fun scope ->
          let open Result.Let_syntax in
          let%bind content =
            Owner.Scope.bind_content_revision scope ~revision:(revision t side)
          in
          let%bind value = f scope content in
          let%map () = Owner.Scope.check scope in
          value)
        |> Result.map_error ~f:content_source_error
      in
      let%map () = check t in
      read
    ;;

    let check_dynamic t ~side = with_read t ~side ~f:(fun _ _ -> Ok ())

    let read_local t ~side ~item ~max_bytes ~max_work_steps ~checkpoint ~on_acquire =
      with_read t ~side ~f:(fun scope content ->
        Prompt_source_owner.Scope.read_dynamic_content
          scope
          ~content
          ~item
          ~max_bytes
          ~max_work_steps
          ~checkpoint
          ~on_acquire)
    ;;

    let remote_origin t ~side ~item ~max_work_steps ~checkpoint =
      with_read t ~side ~f:(fun scope content ->
        Prompt_source_owner.Scope.content_origin
          scope
          ~content
          ~item
          ~max_work_steps
          ~checkpoint)
    ;;
  end

  let selected_content_source t ~target =
    let port : Content_source.t = { binding = t; target = Saved target } in
    Result.map (Content_source.check port) ~f:(fun () -> port)
  ;;

  let selected_descendant_content_source t ~target =
    let port : Content_source.t = { binding = t; target = Content target } in
    Result.map (Content_source.check port) ~f:(fun () -> port)
  ;;

  let selected_model_content_source t ~target =
    let port : Content_source.t = { binding = t; target = Model target } in
    Result.map (Content_source.check port) ~f:(fun () -> port)
  ;;

  let selected_tool_content_source t ~target =
    let port : Content_source.t = { binding = t; target = Tool target } in
    Result.map (Content_source.check port) ~f:(fun () -> port)
  ;;

  module Selected_model_source = struct
    type binding = t

    type t =
      { binding : binding
      ; port : Content_source.t
      ; member : Content_source.Selected_member.t
      ; selection : Prompt_child_source.Model_selection.t
      ; input_bytes : int
      ; work_steps : int
      }

    let input_bytes t = t.input_bytes
    let work_steps t = t.work_steps
  end

  let selected_parent_revision = function
    | Selected_content_target.Saved target ->
      Claimed_developer.Selected.Target.revision target
    | Content target -> Selected_content_target.revision target
    | Model target -> Selected_model_target.revision target
    | Tool target -> Selected_tool_target.revision target
  ;;

  let selected_parent_caller = function
    | Selected_content_target.Saved target -> target.caller
    | Content target -> target.caller
    | Model target -> target.Selected_child_target.Model.caller
    | Tool target -> target.Selected_child_target.Tool.caller
  ;;

  let selected_parent_issuer = function
    | Selected_content_target.Saved target -> target.binding_issuer
    | Content target -> target.node.binding_issuer
    | Model target -> target.node.binding_issuer
    | Tool target -> target.node.binding_issuer
  ;;

  module Content_ports = struct
    type binding = t

    type t =
      { binding : binding
      ; parent : Selected_content_target.parent
      ; declaring : Port.t
      ; caller : Port.t
      ; run_selected_content : selected_content_runner
      ; live : bool Atomic.t
      }

    let run t ~side ~declaration ~ctx =
      let open Result.Let_syntax in
      if
        (not (Atomic.get t.live))
        || not (Int.equal t.binding.domain_id (Stdlib.Domain.self () :> int))
      then Error (failure "selected child Agent ports have expired")
      else (
        let%bind () = check_selected_content_parent t.parent in
        let port =
          match side with
          | Content_source.Declaring -> t.declaring
          | Caller -> t.caller
        in
        let selected =
          Content_selection.
            { parent = t.parent
            ; run = t.run_selected_content
            ; binding_issuer = t.binding.issuer
            ; live = t.live
            }
        in
        let value =
          try
            Ok
              (Eio.Fiber.with_binding Content_selection.key selected (fun () ->
                 port.run_content ~declaration ~ctx))
          with
          | Selected_content_denied error -> Error error
        in
        let%bind value = value in
        let%map () = check_selected_content_parent t.parent in
        value)
    ;;

    let with_selected_model_target t ~source ~member ~selected ~f =
      let open Result.Let_syntax in
      if not (Atomic.get t.live)
      then Error (failure "selected child Agent ports have expired")
      else (
        let%bind () = check_selected_content_parent t.parent in
        let%bind () = Content_source.check_selected_member source member in
        let%bind () =
          if
            phys_equal source.binding t.binding
            && phys_equal selected.Selected_model_source.binding t.binding
            && phys_equal selected.port source
            && phys_equal selected.member member
          then Ok ()
          else Error (failure "selected Model child belongs to another source port")
        in
        let expected_target =
          match t.parent with
          | Saved target ->
            (match source.target with
             | Saved actual -> phys_equal actual target
             | Content _ | Model _ | Tool _ -> false)
          | Content target ->
            (match source.target with
             | Content actual -> phys_equal actual target
             | Saved _ | Model _ | Tool _ -> false)
          | Model target ->
            (match source.target with
             | Model actual -> phys_equal actual target
             | Saved _ | Content _ | Tool _ -> false)
          | Tool target ->
            (match source.target with
             | Tool actual -> phys_equal actual target
             | Saved _ | Content _ | Model _ -> false)
        in
        let%bind () =
          if expected_target
          then Ok ()
          else Error (failure "selected Model child has another logical parent")
        in
        let caller =
          match member.Content_source.Selected_member.side with
          | Declaring -> t.declaring
          | Caller -> t.caller
        in
        let%bind borrowed = Native_tool_invocation.borrow_selected () in
        let live = Atomic.make true in
        let parent_identity =
          match t.parent with
          | Saved saved -> Selected_child_target.Model.Saved_parent saved
          | Content target -> Child_parent target.node.identity
          | Model target -> Child_parent target.node.identity
          | Tool target -> Child_parent target.node.identity
        in
        let target =
          Selected_child_target.Model.
            { node =
                child_node_of_parent
                  t.parent
                  ~borrowed
                  ~live
                  ~binding_issuer:(Some t.binding.issuer)
            ; parent_identity
            ; source = Prompt_child_source.Model_selection.source selected.selection
            ; caller_revision = member.parent
            ; caller
            }
        in
        Exn.protect
          ~finally:(fun () -> Atomic.set live false)
          ~f:(fun () ->
            let%bind () = Selected_model_target.check target in
            let%bind value = f target in
            let%map () = Selected_model_target.check target in
            value))
    ;;
  end

  let bind_with_literal_policy
        ?run_selected_target
        t
        ~developer_literal_child
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
    =
    if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
    then Error (failure "source binding belongs to another Eio domain")
    else (
      let runner =
        create_runner
          ~script_preparation
          ~tool_contract:t.tool_contract
          ~content_limits:t.content_limits
          ~policy_revision:t.runtime_policy_revision
          ~manifest_authorizer
          ~approval_provider
          ~response_dir
      in
      let runner =
        if developer_literal_child
        then
          { runner with
            developer_literal_child = true
          ; policy_revision =
              numeric_identity
                runner.policy_revision
                [%sexp
                  (if Option.is_some run_selected_target
                   then "developer-selected-child.v1"
                   else "developer-literal-child.v1"
                   : string)]
          }
        else runner
      in
      bind_installed_with_selected
        ?run_selected_target
        ~binding_issuer:t.issuer
        runner
        ~source:t.source
        ~artifact_store:t.artifact_store
        ~source_lease:t.source_lease
        ~env
        ~revision
        ~sw)
  ;;

  let bind
        t
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
    =
    bind_with_literal_policy
      t
      ~developer_literal_child:false
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~env
      ~revision
      ~sw
  ;;

  let bind_developer_literal
        ?run_selected_target
        t
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
    =
    bind_with_literal_policy
      ?run_selected_target
      t
      ~developer_literal_child:true
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~env
      ~revision
      ~sw
  ;;

  let bind_selected_child
        t
        ~parent
        ~run_selected_content
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
    =
    let open Result.Let_syntax in
    let%bind () =
      if phys_equal revision (selected_parent_revision parent)
      then Ok ()
      else Error (failure "selected content parser differs from its child target")
    in
    let%bind () =
      match selected_parent_issuer parent with
      | Some issuer when phys_equal issuer t.issuer -> Ok ()
      | None | Some _ ->
        Error (failure "selected content belongs to another source binding")
    in
    let%bind () = check_selected_content_parent parent in
    let%bind declaring =
      bind_developer_literal
        t
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
    in
    let%bind () = check_selected_content_parent parent in
    let live = Atomic.make true in
    Eio.Switch.on_release sw (fun () -> Atomic.set live false);
    Ok
      Content_ports.
        { binding = t
        ; parent
        ; declaring
        ; caller = selected_parent_caller parent
        ; run_selected_content
        ; live
        }
  ;;

  let select_child
        t
        ~script_preparation
        ~env
        ~parent
        ~reference
        ~invocation
        ~recorded
        ~checkpoint
    =
    if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
    then Error (failure "source binding belongs to another Eio domain")
    else (
      let installed =
        Installed.
          { source = t.source
          ; artifact_store = t.artifact_store
          ; source_lease = Prompt_source_lease.borrow t.source_lease
          ; env
          ; binding_issuer = Some t.issuer
          }
      in
      select_installed_child
        installed
        ~script_preparation
        ~tool_contract:t.tool_contract
        ~content_limits:t.content_limits
        ~parent
        ~reference
        ~invocation
        ~recorded
        ~checkpoint)
  ;;

  let select_model_member
        t
        ~source
        ~member
        ~script_preparation
        ~env
        ~max_input_bytes
        ~max_work_steps
        ~checkpoint
        ~on_acquire
    =
    let module P = Agent_protocol in
    let module Abort = struct
      exception Work_limit
    end
    in
    let resource message = P.Error.create Resource_limit ~message ~retryable:false () in
    if max_input_bytes <= 0 || max_work_steps <= 0
    then Error (resource "selected Model source allowance exhausted")
    else (
      let used = ref 0 in
      let poll () =
        if !used >= max_work_steps then raise Abort.Work_limit;
        incr used;
        checkpoint ()
      in
      try
        let open Result.Let_syntax in
        let%bind () = Content_source.check_selected_member source member in
        let%bind () =
          if phys_equal source.binding t
          then Ok ()
          else Error (failure "selected Model source belongs to another binding")
        in
        let parent = member.Content_source.Selected_member.parent in
        let selected_member = member.member in
        let%bind selection =
          match Content_source.Selected_member.mode member with
          | P.Model_spawn_request.Source_mode.File ->
            let%bind entry =
              Prompt_source_entry.of_selected_member
                ~parent
                ~member:selected_member
                ~max_work_steps:(max_work_steps - !used)
                ~checkpoint:poll
              |> Result.map_error ~f:(fun error -> failure (Error.to_string_hum error))
            in
            let created_at =
              Eio.Time.now (Eio.Stdenv.clock env)
              |> Time_ns.Span.of_sec
              |> Time_ns.of_span_since_epoch
              |> P.Timestamp.of_time_ns
            in
            Prompt_child_source.file_selected_member
              ~parent
              ~entry
              ~source_lease:t.source_lease
              ~artifact_store:t.artifact_store
              ~compilation:t.source.compilation
              ~tool_contract:t.tool_contract
              ~parser_limits:t.source.parser_limits
              ~content_limits:t.content_limits
              ~max_work_steps:(max_work_steps - !used)
              ~max_input_bytes
              ~preprocess:t.source.preprocess
              ~prepare_programs:(fun startup ~tool_contract scope capture ->
                t.source.prepare_programs
                  startup
                  ~tool_contract
                  ~invocation:Headless
                  scope
                  capture)
              ~env
              ~created_at
              ~checkpoint:poll
              ~on_acquire
          | Captured ->
            Prompt_child_source.captured_selected_member
              ~parent
              ~member:selected_member
              ~artifact_store:t.artifact_store
              ~policy:script_preparation
              ~tool_contract:t.tool_contract
              ~invocation:Headless
              ~content_limits:t.content_limits
              ~max_input_bytes
              ~max_work_steps:(max_work_steps - !used)
              ~env
              ~checkpoint:poll
              ~on_acquire
        in
        let%map () = Content_source.check_selected_member source member in
        Selected_model_source.
          { binding = t
          ; port = source
          ; member
          ; input_bytes = Prompt_child_source.Model_selection.input_bytes selection
          ; work_steps = !used
          ; selection
          }
      with
      | Abort.Work_limit ->
        Error (resource "selected Model source work allowance exhausted"))
  ;;
end

module Deferred_developer = struct
  type t =
    { registrations : Chat_response.Agent_runtime.native_registration list
    ; port : Port.t
    }

  let registrations t = t.registrations

  let check_limits ~max_agents ~max_work_steps =
    if max_agents <= 0 || max_agents > 1024 || max_work_steps <= 0
    then Error (failure "deferred Agent registration limits are invalid")
    else Ok ()
  ;;

  let prepare_registered
        (source : Source_binding.t)
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~revision
        ~port
        ~max_agents
        ~max_work_steps
        ~checkpoint
        ~strict_child_tools
        ~context_for
    =
    let module Abort = struct
      exception Work_limit
    end
    in
    match check_limits ~max_agents ~max_work_steps with
    | Error _ as error -> error
    | Ok () ->
      let remaining = ref max_work_steps in
      let poll () =
        if !remaining <= 0 then raise Abort.Work_limit;
        decr remaining;
        checkpoint ();
        Eio.Fiber.check ()
      in
      (try
         let open Result.Let_syntax in
         let%bind parsed =
           match Prompt_revision.parser_context revision with
           | Some parsed -> Ok parsed
           | None -> Error (failure "deferred Agent revision has no owned parser")
         in
         let%bind prepare =
           match port.Port.prepare with
           | Some prepare -> Ok prepare
           | None -> Error (failure "deferred Agent source preparer is unavailable")
         in
         let membership_runner =
           create_runner
             ~script_preparation
             ~tool_contract:source.tool_contract
             ~content_limits:source.content_limits
             ~policy_revision:source.runtime_policy_revision
             ~manifest_authorizer
             ~approval_provider
             ~response_dir
         in
         let%map _, _, reversed =
           List.fold_result
             (CM.Parsed.elements parsed)
             ~init:(0, String.Set.empty, [])
             ~f:(fun (count, names, reversed) element ->
               poll ();
               match element with
               | CM.Tool (Agent declaration) ->
                 if count >= max_agents
                 then Error (failure "deferred Agent registration count exhausted")
                 else if Set.mem names declaration.name
                 then Error (failure "deferred Agent name is duplicated")
                 else (
                   let%bind reference =
                     tool_reference membership_runner parsed declaration ~checkpoint:poll
                   in
                   let expected_implementation = ref None in
                   let with_context =
                     context_for ~port ~declaration ~reference ~expected_implementation
                   in
                   let deferred =
                     Chat_response.Tool.Deferred_agent.create
                       ~declaration
                       ~runner:port
                       ~with_context
                   in
                   let implementation =
                     Chat_response.Tool.Deferred_agent.implementation deferred
                   in
                   expected_implementation := Some implementation;
                   let source_requirement =
                     Chat_response.Native_tool_pool.Source_requirement.Source
                       (Some
                          (Chat_response.Native_source_registration.create
                             ~implementation
                             ~prepare:(fun ~launch ~request:_ ~checkpoint ->
                               match
                                 Chat_response.Source_launch.developer_owner launch
                               with
                               | None ->
                                 Error
                                   (Agent_protocol.Error.invalid_request
                                      "deferred Agent requires a developer source parent")
                               | Some _ ->
                                 prepare ~declaration ~launch ~checkpoint
                                 |> Result.map ~f:Option.some)))
                   in
                   let registration : Chat_response.Agent_runtime.native_registration =
                     { implementation
                     ; implementation_revision =
                         [%sexp
                           ("deferred-agent.v1" : string)
                         , (Chatml_source.Digest.to_hex port.revision : string)
                         , (declaration : CM.agent_tool)]
                         |> Sexp.to_string_mach
                         |> Chatmd_shell_spec.Source_ref.digest
                     ; result_contract = Native_output
                     ; authoring_metadata = None
                     ; source_requirement
                     }
                   in
                   Ok (count + 1, Set.add names declaration.name, registration :: reversed))
               | CM.Tool (Persistent_agent _ | Inherited _) when strict_child_tools ->
                 Error (failure "selected child has unsupported Agent Tool declaration")
               | CM.Msg _
               | Developer _
               | System _
               | User _
               | Assistant _
               | Tool_call _
               | Tool_response _
               | Config _
               | Reasoning _
               | Tool _
               | Shell_runtime _
               | Moderator_runtime _
               | Script _
               | Shell_script _
               | Extension_script _
               | Authoring_context _
               | Authoring_help _ -> Ok (count, names, reversed))
         in
         { registrations = List.rev reversed; port }
       with
       | Abort.Work_limit -> Error (failure "deferred Agent work allowance exhausted"))
  ;;

  let prepare
        ?run_selected_target
        source
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
        ~max_agents
        ~max_work_steps
        ~checkpoint
        ~with_context
    =
    let open Result.Let_syntax in
    let%bind () = check_limits ~max_agents ~max_work_steps in
    let%bind port =
      Source_binding.bind_developer_literal
        ?run_selected_target
        source
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
    in
    prepare_registered
      source
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~revision
      ~port
      ~max_agents
      ~max_work_steps
      ~checkpoint
      ~strict_child_tools:false
      ~context_for:(fun ~port:_ ~declaration:_ ~reference:_ ~expected_implementation:_ ->
        with_context)
  ;;

  let prepare_selected_child
        (source : Source_binding.t)
        ~parent
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
        ~max_agents
        ~max_work_steps
        ~checkpoint
        ~borrow_child_context
        ~run_selected_tool
    =
    let open Result.Let_syntax in
    let%bind () = check_limits ~max_agents ~max_work_steps in
    let%bind () =
      if phys_equal revision (Source_binding.selected_parent_revision parent)
      then Ok ()
      else Error (failure "selected Tool parser differs from its actual parent")
    in
    let%bind () =
      match Source_binding.selected_parent_issuer parent with
      | Some issuer when phys_equal issuer source.issuer -> Ok ()
      | None | Some _ -> Error (failure "selected Tool belongs to another source binding")
    in
    let%bind () = check_selected_content_parent parent in
    let%bind port =
      Source_binding.bind_developer_literal
        source
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~env
        ~revision
        ~sw
    in
    let%bind () = check_selected_content_parent parent in
    let%bind prepared =
      prepare_registered
        source
        ~script_preparation
        ~manifest_authorizer
        ~approval_provider
        ~response_dir
        ~revision
        ~port
        ~max_agents
        ~max_work_steps
        ~checkpoint
        ~strict_child_tools:true
        ~context_for:(fun ~port ~declaration ~reference ~expected_implementation ->
          fun ~implementation ~invocation f ->
          let open Result.Let_syntax in
          let borrowed = Native_tool_invocation.borrow_selected () |> runner_result in
          let live = Atomic.make true in
          let scope =
            Tool_selection.
              { parent
              ; run = run_selected_tool
              ; binding_issuer = source.issuer
              ; implementation
              ; declaration
              ; reference
              ; port
              ; borrowed
              ; live
              ; domain_id = (Stdlib.Domain.self () :> int)
              }
          in
          Exn.protect
            ~finally:(fun () -> Atomic.set live false)
            ~f:(fun () ->
              Eio.Fiber.with_binding Tool_selection.key scope (fun () ->
                let%bind () =
                  match !expected_implementation with
                  | Some expected when phys_equal expected implementation -> Ok ()
                  | None | Some _ ->
                    Error (failure "selected Tool implementation changed")
                in
                let%bind () = Tool_selection.check scope ~implementation ~port in
                let value = borrow_child_context ~implementation ~invocation f in
                let%map () = Tool_selection.check scope ~implementation ~port in
                value)
              |> runner_result))
    in
    let%map () = check_selected_content_parent parent in
    prepared
  ;;
end
