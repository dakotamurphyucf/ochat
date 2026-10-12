open Core
module CM = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module Programs = Chatml_prompt_programs
module Startup = Chat_response.Startup_compilation
module Driver = Chat_response.Driver
module Port = Chat_response.Tool.Agent_source_runner
module Preparation = Runtime_script_preparation

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
  ; content_limits : Chatmd_prompt_content.Limits.t option
  ; policy_revision : Chatml_source.Digest.t
  ; manifest_authorizer : Shell_runtime.Manifest_authorizer.t
  ; approval_provider : Shell_runtime.Approval_broker.provider
  ; response_dir : Eio.Fs.dir_ty Eio.Path.t
  }

let create
      ~script_preparation
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
      , (content_policy_identity content_limits : Sexp.t)]
  in
  { domain_id = (Stdlib.Domain.self () :> int)
  ; script_preparation
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

let compilation_error (error : Chatml_compilation.error) =
  failure (error.code ^ ": " ^ error.message)
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
        Option.map installed ~f:(fun installed ~declaration ~owner:_ ~checkpoint ->
          Runtime_activity.run activity (fun () ->
            let open Result.Let_syntax in
            let%bind reference = tool_reference t parsed declaration ~checkpoint in
            let%map selected =
              select_child
                t
                installed
                ~parent:revision
                ~reference
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

and select_child t installed ~parent ~reference ~recorded ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match recorded with
  | Some recorded ->
    Prompt_child_source.restore
      ~artifact_store:installed.Installed.artifact_store
      ~definition:(Prompt_revision.definition parent)
      ~content_limits:t.content_limits
      recorded
      ~checkpoint
  | None ->
    (match CM.Agent_reference.source_mode reference with
     | Captured ->
       Prompt_child_source.captured
         ~parent
         ~reference
         ~artifact_store:installed.artifact_store
         ~policy:t.script_preparation
         ~invocation:Headless
         ~content_limits:t.content_limits
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
         ~parser_limits:source.parser_limits
         ~content_limits:t.content_limits
         ~max_work_steps:source.max_work_steps
         ~preprocess:source.preprocess
         ~prepare_programs:(fun startup scope capture ->
           source.prepare_programs startup ~invocation:Headless scope capture)
         ~env:installed.env
         ~created_at
         ~checkpoint)

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
    let run startup ~revision ~compilation_limits ~programs =
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
              parent
              ~reference
              ~invocation:Headless
              ~content_limits:t.content_limits
              ~env
              ~checkpoint:poll
              ~f:run)
          ()
      | Some installed ->
        (* A recorded selection belongs only to this actual tool invocation.
           Content occurrences keep their own defining revision and must never
           consume the enclosing queued target. Both paths reject expired/foreign
           services before source selection. *)
        let%bind recorded =
          match Script_tool_calls.native_services_scope () with
          | Active services ->
            (match target with
             | Tool_target implementation ->
               Script_tool_calls.agent_artifact_for_implementation
                 services
                 ~implementation
             | Content_target ->
               Script_tool_calls.agent_artifact services |> Result.map ~f:(fun _ -> None))
          | Expired -> Error (failure "selected child native services have expired")
          | Unbound ->
            (match Native_tool_invocation.current_scope () with
             | Expired -> Error (failure "selected child native invocation has expired")
             | Unbound | Active _ -> Ok None)
        in
        let%bind reference = reference ~checkpoint in
        let%bind selected =
          select_child t installed ~parent ~reference ~recorded ~checkpoint
        in
        let revision = Prompt_child_source.revision selected in
        Preparation.with_revision
          (Some t.script_preparation)
          revision
          ~env
          ~invocation:Headless
          ~checkpoint
          ~f:(run ~revision)
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
    ; runtime_policy_revision : Chatml_source.Digest.t
    ; content_limits : Chatmd_prompt_content.Limits.t option
    ; artifact_store : Agent_store.Prompt_artifact_store.t
    ; source_lease : Prompt_source_lease.t
    }

  let create
        ~source
        ~runtime_policy_revision
        ~content_limits
        ~artifact_store
        ~source_lease
    =
    { domain_id = (Stdlib.Domain.self () :> int)
    ; source
    ; runtime_policy_revision
    ; content_limits
    ; artifact_store
    ; source_lease = Prompt_source_lease.borrow source_lease
    }
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
    if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
    then Error (failure "source binding belongs to another Eio domain")
    else (
      let runner =
        create_runner
          ~script_preparation
          ~content_limits:t.content_limits
          ~policy_revision:t.runtime_policy_revision
          ~manifest_authorizer
          ~approval_provider
          ~response_dir
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
end
