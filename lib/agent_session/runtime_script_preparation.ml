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

let plan t ~startup ~parsed ~delegated_moderator =
  Startup.within startup ~map_error:compilation_error ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind shell =
      Runtime.prepare_shell_manifest
        ~platform:(Runtime.platform ())
        ~prompt_elements:(Prompt.Chat_markdown.Parsed.elements parsed)
      |> map_diagnostics ~to_string:Runtime.diagnostic_to_string
    in
    Plan.create
      parsed
      ~moderator_surface:Chatml.Chatml_builtin_surface.ui_moderator_surface
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

let restore policy ~startup ~revision ~delegated_moderator =
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
         let%bind plan = plan policy ~startup ~parsed ~delegated_moderator in
         Startup.within startup ~map_error:compilation_error ~f:(fun () ->
           restore_programs policy ~startup ~plan ~custody))
    | None, Some _ | Some _, None ->
      Error
        (failure "script custody is missing its matching original parser or inventory"))
;;

let with_revision policy ?startup revision ~env ~delegated_moderator ~checkpoint ~f =
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
      let%bind programs = restore policy ~startup ~revision ~delegated_moderator in
      f startup ~compilation_limits ~programs)
    ()
;;
