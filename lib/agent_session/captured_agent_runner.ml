open Core
module CM = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module Programs = Chatml_prompt_programs
module Startup = Chat_response.Startup_compilation
module Driver = Chat_response.Driver
module Port = Chat_response.Tool.Agent_source_runner
module Preparation = Runtime_script_preparation
module Portable_path = Source.Portable_path

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
    numeric_identity
      policy_revision
      [%sexp
        ("runtime.v1" : string)
      , (Preparation.policy_identity script_preparation : Sexp.t)
      , (tool_contract : Chatml_compilation.Tool_contract.t)
      , (content_policy_identity content_limits : Sexp.t)]
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
      let open Result.Let_syntax in
      let denied () =
        Error (failure "developer child effect needs a selected child owner")
      in
      let elements =
        match Prompt_revision.document_projection revision with
        | None -> CM.Parsed.elements parsed
        | Some projection ->
          if not (phys_equal parsed (CM.Document_projection.parsed projection))
          then invalid_arg "developer child projection belongs to another parser";
          CM.Document_projection.elements projection
      in
      let admit_content = function
        | CM.Basic basic ->
          if
            String.equal basic.type_ "text"
            && Option.is_none basic.image_url
            && Option.is_none basic.document_url
            && (not basic.is_local)
            && (not basic.cleanup_html)
            && not basic.markdown
          then Ok ()
          else denied ()
        | CM.Agent _ -> denied ()
      in
      let admit_message (message : CM.msg) =
        if
          Option.is_some message.function_call
          || Option.is_some message.tool_call
          || Option.is_some message.tool_call_id
        then denied ()
        else (
          match message.content with
          | None | Some (Text _) -> Ok ()
          | Some (Items items) ->
            List.fold_result items ~init:() ~f:(fun () item ->
              checkpoint ();
              admit_content item))
      in
      List.fold_result elements ~init:() ~f:(fun () element ->
        checkpoint ();
        match element with
        | CM.Msg message
        | Developer message
        | System message
        | User message
        | Assistant message -> admit_message message
        | Config _ | Reasoning _ -> Ok ()
        | Tool_call _
        | Tool_response _
        | Tool _
        | Shell_runtime _
        | Moderator_runtime _
        | Script _
        | Shell_script _
        | Extension_script _
        | Authoring_context _
        | Authoring_help _ -> denied ()))
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

type selected_source =
  | No_selected_source
  | Selected_tool_source of Native_tool_invocation.selected_borrowed * Ochat_function.t
  | Selected_content_context of Native_tool_invocation.selected_borrowed

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

    let active t = t.scope.active
    let invocation t = t.scope.invocation

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
        ~f:(fun () -> f selected)
    | Some _, (None | Some _) | None, _ ->
      invalid "saved Agent source was not selected by this native callback"
  ;;
end

let rec bind_with_checkpoint ?installed t ~revision ~sw ~checkpoint =
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
        let%bind recorded, selected_source =
          match Script_tool_calls.native_services_scope () with
          | Active services ->
            (match target with
             | Tool_target implementation ->
               Script_tool_calls.agent_artifact_for_implementation
                 services
                 ~implementation
               |> Result.map ~f:(fun recorded -> recorded, No_selected_source)
             | Content_target ->
               Script_tool_calls.agent_artifact services
               |> Result.map ~f:(fun _ -> None, No_selected_source))
          | Expired -> Error (failure "selected child native services have expired")
          | Unbound ->
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
        in
        let%bind () = check_selected () in
        let revision = Prompt_child_source.revision selected in
        let%bind parsed =
          Prompt_revision.parser_context revision
          |> Result.of_option
               ~error:(failure "selected developer child has no owned parser")
        in
        let%bind () = admit_developer_literal_child t revision parsed ~checkpoint in
        Preparation.with_revision
          (Some t.script_preparation)
          ~tool_contract:t.tool_contract
          revision
          ~env
          ~invocation:Headless
          ~checkpoint
          ~f:(run ~check_selected caller ~revision)
    in
    runner_result result)
;;

let bind t ~revision ~sw =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  bind_with_checkpoint t ~revision ~sw ~checkpoint:Eio.Fiber.check
;;

let bind_installed t ~source ~artifact_store ~source_lease ~env ~revision ~sw =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let installed =
    Installed.
      { source
      ; artifact_store
      ; source_lease = Prompt_source_lease.borrow source_lease
      ; env
      }
  in
  bind_with_checkpoint ~installed t ~revision ~sw ~checkpoint:Eio.Fiber.check
;;

let create_runner = create

module Source_binding = struct
  type t =
    { domain_id : int
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
    ; source
    ; tool_contract
    ; runtime_policy_revision
    ; content_limits
    ; artifact_store
    ; source_lease = Prompt_source_lease.borrow source_lease
    }
  ;;

  let bind_with_literal_policy
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
                [%sexp ("developer-literal-child.v1" : string)]
          }
        else runner
      in
      bind_installed
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
      ~developer_literal_child:true
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~env
      ~revision
      ~sw
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
end

module Deferred_developer = struct
  type t =
    { registrations : Chat_response.Agent_runtime.native_registration list
    ; port : Port.t
    }

  let registrations t = t.registrations

  let prepare
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
    let module Abort = struct
      exception Work_limit
    end
    in
    if max_agents <= 0 || max_agents > 1024 || max_work_steps <= 0
    then Error (failure "deferred Agent registration limits are invalid")
    else (
      let remaining = ref max_work_steps in
      let poll () =
        if !remaining <= 0 then raise Abort.Work_limit;
        decr remaining;
        checkpoint ();
        Eio.Fiber.check ()
      in
      try
        let open Result.Let_syntax in
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
                  let%bind (_ : CM.Agent_reference.t) =
                    tool_reference membership_runner parsed declaration ~checkpoint:poll
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
end
