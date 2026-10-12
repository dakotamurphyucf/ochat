open Core
module Runtime = Chat_response.Agent_runtime
module Compiler = Chatml_compilation
module Startup = Chat_response.Startup_compilation
module Plan = Chat_response.Script_contract_plan

type t =
  { compilation : Compiler.limits
  ; planning : Plan.Limits.t
  ; restoration : Chatml_prompt_programs.Limits.t
  ; max_custody_work_steps : int
  }

module Invocation = struct
  type t =
    | Ui
    | Headless
    | Delegated_ui
  [@@deriving equal, sexp_of]

  let of_root : Plan.Program.Role.Moderator_invocation.t -> t = function
    | Ui -> Ui
    | Headless -> Headless
  ;;

  let moderator_surface = function
    | Ui | Delegated_ui -> Chatml.Chatml_builtin_surface.ui_moderator_surface
    | Headless -> Chatml.Chatml_builtin_surface.moderator_surface
  ;;
end

let create ~compilation ~planning ~restoration ~max_custody_work_steps =
  let open Or_error.Let_syntax in
  let%bind () =
    Compiler.validate_limits compilation
    |> Result.map_error ~f:(fun error -> Error.of_string error.Compiler.message)
  in
  if max_custody_work_steps <= 0
  then
    Or_error.error_string "retained script selection requires a positive work allowance"
  else Ok { compilation; planning; restoration; max_custody_work_steps }
;;

let compilation t = t.compilation
let restoration t = t.restoration
let max_custody_work_steps t = t.max_custody_work_steps

let failure message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let compilation_error (error : Compiler.error) =
  failure (error.code ^ ": " ^ error.message)
;;

let map_diagnostics result ~to_string =
  Result.map_error result ~f:(fun diagnostics ->
    List.map diagnostics ~f:to_string |> String.concat ~sep:"\n" |> failure)
;;

let plan t ~startup ~parsed ~invocation =
  Startup.within startup ~map_error:compilation_error ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind shell =
      Runtime.prepare_shell_manifest
        ~platform:(Runtime.platform ())
        ~prompt_elements:(Prompt.Chat_markdown.Parsed.elements parsed)
      |> map_diagnostics ~to_string:Runtime.diagnostic_to_string
    in
    let moderator_invocation, delegated_moderator =
      match invocation with
      | Invocation.Ui -> Plan.Program.Role.Moderator_invocation.Ui, false
      | Headless -> Headless, false
      | Delegated_ui -> Ui, true
    in
    let moderator_surface = Invocation.moderator_surface invocation in
    Plan.create
      parsed
      ~moderator_invocation
      ~moderator_surface
      ~delegated_moderator
      ~shell:(Some shell)
      ~limits:t.planning
      ~checkpoint:(fun () -> Startup.checkpoint startup)
    |> map_diagnostics ~to_string:Chatmd_shell_spec.Diagnostic.to_string)
;;

let restore_programs t ~startup ~plan ~custody =
  let module Abort = struct
    exception Limit
  end
  in
  let remaining = ref t.max_custody_work_steps in
  let checkpoint () =
    Startup.checkpoint startup;
    if !remaining <= 0 then raise Abort.Limit;
    decr remaining
  in
  try
    Chat_response.Restored_programs.restore
      plan
      ~custody
      ~limits:t.restoration
      ~checkpoint
    |> map_diagnostics ~to_string:Chatmd_shell_spec.Diagnostic.to_string
  with
  | Abort.Limit ->
    Error (failure "script custody restoration exceeds host work allowance")
;;

let restore policy ~startup ~revision ~invocation =
  let artifact = Prompt_revision.artifact revision in
  if
    Option.is_some artifact.content_bundle
    && Option.is_none (Prompt_revision.document_projection revision)
  then Error (failure "stored document custody is missing its validated projection")
  else (
    match Prompt_revision.parser_context revision, Prompt_revision.programs revision with
    | None, None ->
      let artifact = Prompt_revision.artifact revision in
      if Option.is_some artifact.program_bundle || Option.is_some artifact.content_bundle
      then
        Error
          (failure "stored script or document custody is missing its parser attachment")
      else Ok None
    | Some parsed, Some custody ->
      (match policy with
       | None ->
         Error (failure "owned script custody requires explicit host preparation policy")
       | Some policy ->
         let open Result.Let_syntax in
         let%bind plan = plan policy ~startup ~parsed ~invocation in
         Startup.within startup ~map_error:compilation_error ~f:(fun () ->
           restore_programs policy ~startup ~plan ~custody))
    | None, Some _ | Some _, None ->
      Error
        (failure "script custody is missing its matching original parser or inventory"))
;;

let with_revision policy ?startup revision ~env ~invocation ~checkpoint ~f =
  let compilation_limits =
    Option.value_map policy ~default:Compiler.default_limits ~f:compilation
  in
  Startup.with_context
    ?startup
    ~limits:compilation_limits
    ~env
    ~checkpoint
    ~map_error:compilation_error
    ~f:(fun startup ->
      let open Result.Let_syntax in
      let%bind programs = restore policy ~startup ~revision ~invocation in
      f startup ~compilation_limits ~programs)
    ()
;;

let select_child_role policy ~startup ~invocation ~revision ~authored =
  let open Result.Let_syntax in
  match Prompt_revision.parser_context revision, Prompt_revision.programs revision with
  | None, None ->
    let%map (_ : Chatml_prompt_programs.Restored.t option) =
      restore policy ~startup ~revision ~invocation
    in
    None
  | Some parsed, Some custody ->
    (match policy with
     | None ->
       Error (failure "owned child custody requires explicit host preparation policy")
     | Some policy ->
       let%bind plan = plan policy ~startup ~parsed ~invocation in
       let%map selected =
         Chat_response.Retained_selection.select
           plan
           ~custody
           ~authored
           ~limits:policy.restoration
           ~max_work_steps:policy.max_custody_work_steps
           ~checkpoint:(fun () -> Startup.checkpoint startup)
         |> map_diagnostics ~to_string:Chatmd_shell_spec.Diagnostic.to_string
       in
       Some (plan, selected))
  | None, Some _ | Some _, None ->
    Error (failure "child custody is missing its matching original parser or inventory")
;;

let selected_custody = function
  | Chat_response.Retained_selection.Legacy custody | Selected custody -> custody
;;

let select_child policy ~startup ~parent source =
  let open Result.Let_syntax in
  let checkpoint () = Startup.checkpoint startup in
  let%bind revision =
    Authored_agent_source.resource_revision ~checkpoint ~parent source
  in
  let%bind selected =
    select_child_role
      policy
      ~startup
      ~invocation:Delegated_ui
      ~revision
      ~authored:(Authored_agent_source.authored_sources source)
  in
  match selected with
  | None -> Ok (source, revision)
  | Some (_, selected) ->
    let%bind source =
      Authored_agent_source.with_programs ~checkpoint source (selected_custody selected)
    in
    let%map revision =
      Authored_agent_source.resource_revision ~checkpoint ~parent source
    in
    source, revision
;;

let with_authored_child
      ?startup
      policy
      ~parent
      ~tool_name
      ~content_limits
      ~env
      ~checkpoint
      ~f
  =
  let compilation_limits =
    Option.value_map policy ~default:Compiler.default_limits ~f:compilation
  in
  Startup.with_context
    ?startup
    ~limits:compilation_limits
    ~env
    ~checkpoint
    ~map_error:compilation_error
    ~f:(fun startup ->
      let open Result.Let_syntax in
      let%bind source, revision =
        Startup.within startup ~map_error:compilation_error ~f:(fun () ->
          let%bind source =
            Authored_agent_source.capture
              ~checkpoint:(fun () -> Startup.checkpoint startup)
              ?content_limits
              ~parent
              ~tool_name
              ()
          in
          select_child policy ~startup ~parent source)
      in
      f startup ~source ~revision)
    ()
;;

(* Preparation IDs are ephemeral: custody remains the source identity and no ID
   chosen here is installed, reserved or used to infer native authority. *)
let child_preparation_id parent source =
  let artifact = Prompt_revision.artifact parent in
  let digest =
    [%sexp
      ("ochat.captured-child-preparation.v1" : string)
    , (Agent_protocol.Id.Prompt_revision.to_string artifact.revision_id : string)
    , (artifact.manifest_sha256 : string)
    , (Captured_child_source.root_relative_path source : string)
    , (Captured_child_source.programs_sha256 source : string option)
    , (Captured_child_source.content_sha256 source : string option)]
    |> Sexp.to_string_mach
    |> Chatmd_shell_spec.Source_ref.digest
  in
  Agent_protocol.Id.Prompt_revision.of_string ("prv_captured_" ^ digest)
;;

let with_captured_child
      policy
      ?startup
      parent
      ~reference
      ~invocation
      ~content_limits
      ~env
      ~checkpoint
      ~f
  =
  let compilation_limits = compilation policy in
  Startup.with_context
    ?startup
    ~limits:compilation_limits
    ~env
    ~checkpoint
    ~map_error:compilation_error
    ~f:(fun startup ->
      let checkpoint () = Startup.checkpoint startup in
      let open Result.Let_syntax in
      let%bind revision, programs =
        Startup.within startup ~map_error:compilation_error ~f:(fun () ->
          let%bind source =
            Captured_child_source.capture ~parent ~reference ~content_limits ~checkpoint
          in
          let%bind revision_id = child_preparation_id parent source in
          let%bind revision =
            Captured_child_source.resource_revision
              source
              ~parent
              ~revision_id
              ~checkpoint
          in
          let%bind selected =
            select_child_role
              (Some policy)
              ~startup
              ~invocation
              ~revision
              ~authored:(Captured_child_source.authored_sources source)
          in
          match selected with
          | None ->
            Error (failure "owned captured child has no retained parser/program custody")
          | Some (plan, selected) ->
            let%bind source =
              Captured_child_source.with_programs
                source
                (selected_custody selected)
                ~checkpoint
            in
            let%bind revision_id = child_preparation_id parent source in
            let%bind revision =
              Captured_child_source.resource_revision
                source
                ~parent
                ~revision_id
                ~checkpoint
            in
            let%bind programs =
              restore_programs policy ~startup ~plan ~custody:(selected_custody selected)
            in
            (match selected, programs with
             | Chat_response.Retained_selection.Legacy _, None | Selected _, Some _ ->
               Ok (revision, programs)
             | Legacy _, Some _ | Selected _, None ->
               Error
                 (failure
                    "captured child role restoration changed its admitted selection")))
      in
      f startup ~revision ~compilation_limits ~programs)
    ()
;;

let inspect_shell t ?startup revision ~env ~invocation ~checkpoint =
  with_revision
    (Some t)
    ?startup
    revision
    ~env
    ~invocation
    ~checkpoint
    ~f:(fun startup ~compilation_limits ~programs ->
      let prompt_elements =
        match Prompt_revision.parser_context revision with
        | Some parsed -> Prompt.Chat_markdown.Parsed.elements parsed
        | None -> Prompt_revision.elements revision
      in
      Runtime.inspect_prepared_shell
        ~startup
        ?programs
        ~env
        ~limits:compilation_limits
        ~checkpoint
        ~platform:(Runtime.platform ())
        ~prompt_elements
        ()
      |> map_diagnostics ~to_string:Runtime.diagnostic_to_string)
;;
