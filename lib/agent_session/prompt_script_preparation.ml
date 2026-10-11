open Core
module Plan = Chat_response.Script_contract_plan
module Startup = Chat_response.Startup_compilation
module Runtime = Chat_response.Agent_runtime
module Compiler = Chatml_compilation
module Sources = Prompt_script_sources
module Builder = Prompt_revision_builder
module Owner = Prompt_source_owner
module Roles = Prompt_script_roles

module Root_role = struct
  type t =
    | Ui
    | Headless
  [@@deriving equal]
end

module Source_policy = struct
  type t =
    { root_limits : Chatml_local_roots.Limits.t
    ; max_grants : int
    ; max_grant_bytes : int
    ; max_request_bytes : int
    ; max_path_components : int
    ; max_input_bytes : int
    ; max_work_steps : int
    ; policy : Chatml_project_prepare.Policy.t
    ; limits : Chatml_project_prepare.Limits.t
    ; custody_limits : Chatml_prompt_programs.Limits.t
    }

  let create
        ~root_limits
        ~max_grants
        ~max_grant_bytes
        ~max_request_bytes
        ~max_path_components
        ~max_input_bytes
        ~max_work_steps
        ~policy
        ~limits
        ~custody_limits
    =
    if
      max_grants < 0
      || max_grant_bytes < 0
      || max_request_bytes <= 0
      || max_path_components <= 0
      || max_input_bytes <= 0
      || max_work_steps <= 0
    then Or_error.error_string "Invalid script source policy ceilings"
    else
      Ok
        { root_limits
        ; max_grants
        ; max_grant_bytes
        ; max_request_bytes
        ; max_path_components
        ; max_input_bytes
        ; max_work_steps
        ; policy
        ; limits
        ; custody_limits
        }
  ;;
end

module Policy = struct
  type t =
    { compilation : Compiler.limits
    ; planning : Plan.Limits.t
    ; sources : Source_policy.t
    ; max_context_roles : int
    ; max_reference_visits : int
    ; max_work_steps : int
    }

  let create
        ~compilation
        ~planning
        ~sources
        ~max_context_roles
        ~max_reference_visits
        ~max_work_steps
    =
    let open Or_error.Let_syntax in
    let%bind () =
      Compiler.validate_limits compilation
      |> Result.map_error ~f:(fun error -> Core.Error.of_string error.Compiler.message)
    in
    if max_context_roles <= 0 || max_reference_visits < 0 || max_work_steps <= 0
    then Or_error.error_string "Invalid script role preparation ceilings"
    else
      Ok
        { compilation
        ; planning
        ; sources
        ; max_context_roles
        ; max_reference_visits
        ; max_work_steps
        }
  ;;
end

module Host = struct
  type t =
    { additional_anchors : Chatml_local_roots.Anchor.t list
    ; git_store : Chatml_git_host.Store.t option
    ; git_grants : Chatml_git_host.Grant.t list
    }
end

module Prepared = struct
  type t =
    { programs : Chatml_prompt_programs.t
    ; plans : Plan.t list
    }

  let programs t = t.programs
  let plans t = t.plans
end

let diagnostic ?source code message = Builder.Diagnostic.{ code; message; source }
let compilation_error (error : Compiler.error) = [ diagnostic error.code error.message ]

let owner_error error =
  match Owner.Error.capture_diagnostics error with
  | Some diagnostics -> diagnostics
  | None ->
    [ diagnostic "prompt.script_source_owner" "Original source admission is unavailable" ]
;;

let shell_errors diagnostics =
  List.map diagnostics ~f:(fun (error : Chatmd_shell_spec.Diagnostic.t) ->
    diagnostic
      ?source:
        (Option.map error.source ~f:(fun source ->
           source.Chatmd_shell_spec.Source_ref.file))
      error.code
      error.message)
;;

let runtime_errors diagnostics =
  List.map diagnostics ~f:(fun (error : Runtime.diagnostic) ->
    diagnostic
      ?source:
        (Option.map error.source ~f:(fun source ->
           source.Chatmd_shell_spec.Source_ref.file))
      error.code
      error.message)
;;

let capture
      policy
      ?startup
      ~root_role
      ~host
      ~scope
      ~capture
      ~operation
      ~provided
      ~env
      ~checkpoint
      ()
  =
  let module Abort = struct
    exception Stop of Builder.Diagnostic.t list
  end
  in
  Startup.with_context
    ?startup
    ~limits:policy.Policy.compilation
    ~env
    ~checkpoint
    ~map_error:compilation_error
    ~f:(fun startup ->
      Startup.within startup ~map_error:compilation_error ~f:(fun () ->
        let remaining = ref policy.max_work_steps in
        let check result =
          match result with
          | Ok () -> ()
          | Error error -> raise (Abort.Stop (owner_error error))
        in
        let poll () =
          Startup.checkpoint startup;
          check (Owner.Scope.check_capture scope capture);
          if !remaining <= 0
          then
            raise
              (Abort.Stop
                 [ diagnostic
                     "prompt.script_preparation_limit"
                     "Whole script preparation work allowance exhausted"
                 ]);
          decr remaining
        in
        try
          poll ();
          let open Result.Let_syntax in
          let%bind roles =
            Roles.collect
              (Owner.Capture.value capture)
              ~root_role:
                (match root_role with
                 | Root_role.Ui -> Roles.Role.Ui
                 | Headless -> Roles.Role.Headless)
              ~max_context_roles:policy.max_context_roles
              ~max_reference_visits:policy.max_reference_visits
              ~checkpoint:poll
          in
          let shell = ref [] in
          let material parsed =
            poll ();
            match
              List.find !shell ~f:(fun (previous, _) ->
                poll ();
                phys_equal previous parsed)
            with
            | Some (_, material) -> Ok material
            | None ->
              let%map material =
                Runtime.prepare_shell_manifest
                  ~platform:(Runtime.platform ())
                  ~prompt_elements:(Prompt.Chat_markdown.Parsed.elements parsed)
                |> Result.map_error ~f:runtime_errors
              in
              poll ();
              shell := (parsed, material) :: !shell;
              material
          in
          let plans =
            List.fold_result roles ~init:[] ~f:(fun plans role ->
              poll ();
              let parsed = Roles.parsed role in
              let%bind shell = material parsed in
              let moderator_invocation, moderator_surface, delegated_moderator =
                match Roles.role role with
                | Ui ->
                  ( Plan.Program.Role.Moderator_invocation.Ui
                  , Chatml.Chatml_builtin_surface.ui_moderator_surface
                  , false )
                | Headless ->
                  Headless, Chatml.Chatml_builtin_surface.moderator_surface, false
                | Delegated_ui ->
                  Ui, Chatml.Chatml_builtin_surface.ui_moderator_surface, true
              in
              let%map plan =
                Plan.create
                  parsed
                  ~moderator_invocation
                  ~moderator_surface
                  ~delegated_moderator
                  ~shell:(Some shell)
                  ~limits:policy.planning
                  ~checkpoint:poll
                |> Result.map_error ~f:shell_errors
              in
              poll ();
              plan :: plans)
            |> Result.map ~f:List.rev
          in
          let%bind plans = plans in
          match plans with
          | [] ->
            Error
              [ diagnostic "prompt.script_role_owner" "Current Root role plan is missing"
              ]
          | root_plan :: role_plans ->
            let source = policy.sources in
            let%map programs =
              Sources.capture
                scope
                ~capture
                ~root_plan
                ~role_plans
                ~operation
                ~provided
                ~additional_anchors:host.Host.additional_anchors
                ~git_store:host.git_store
                ~git_grants:host.git_grants
                ~root_limits:source.root_limits
                ~max_grants:source.max_grants
                ~max_grant_bytes:source.max_grant_bytes
                ~max_request_bytes:source.max_request_bytes
                ~max_path_components:source.max_path_components
                ~max_input_bytes:source.max_input_bytes
                ~max_work_steps:source.max_work_steps
                ~policy:source.policy
                ~limits:source.limits
                ~custody_limits:source.custody_limits
                ~clock:(Eio.Stdenv.mono_clock env)
                ~checkpoint:poll
            in
            { Prepared.programs; plans }
        with
        | Abort.Stop errors -> Error errors))
    ()
;;
