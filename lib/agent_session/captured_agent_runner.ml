open Core
module CM = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module Programs = Chatml_prompt_programs
module Startup = Chat_response.Startup_compilation
module Driver = Chat_response.Driver
module Port = Chat_response.Tool.Agent_source_runner
module Preparation = Runtime_script_preparation

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
  { domain_id = (Stdlib.Domain.self () :> int)
  ; script_preparation
  ; content_limits
  ; policy_revision
  ; manifest_authorizer
  ; approval_provider
  ; response_dir
  }
;;

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

let rec bind_with_checkpoint t ~revision ~sw ~checkpoint =
  let open Result.Let_syntax in
  let%map parsed, projection, identity = admit_revision t revision ~checkpoint in
  let activity = Runtime_activity.create ~sw in
  let rec port : Port.t =
    { revision = identity
    ; run =
        (fun ~declaration ~ctx ~source ~observer items ->
          invoke
            t
            ~activity
            ~parent:revision
            ~caller:port
            ~ctx
            ~source
            ~observer
            ~reference:(fun ~checkpoint ->
              tool_reference t parsed declaration ~checkpoint)
            items)
    ; run_content =
        (fun ~declaration ~ctx ->
          invoke
            t
            ~activity
            ~parent:revision
            ~caller:port
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

and invoke t ~activity ~parent ~caller ~ctx ~source ~observer ~reference items =
  check_domain t |> runner_result;
  Runtime_activity.with_switch activity (fun ~sw ->
    let env = Chat_response.Ctx.env ctx in
    Startup.with_context
      ~limits:(Preparation.compilation t.script_preparation)
      ~env
      ~checkpoint:Eio.Fiber.check
      ~map_error:compilation_error
      ~f:(fun startup ->
        let checkpoint () = Startup.checkpoint startup in
        let open Result.Let_syntax in
        let%bind reference =
          Startup.within startup ~map_error:compilation_error ~f:(fun () ->
            reference ~checkpoint)
        in
        Preparation.with_captured_child
          t.script_preparation
          ~startup
          parent
          ~reference
          ~invocation:Headless
          ~content_limits:t.content_limits
          ~env
          ~checkpoint
          ~f:(fun startup ~revision ~compilation_limits ~programs ->
            let%bind child =
              Startup.within startup ~map_error:compilation_error ~f:(fun () ->
                bind_with_checkpoint t ~revision ~sw ~checkpoint)
            in
            let%bind parsed =
              match Prompt_revision.parser_context revision with
              | Some parsed -> Ok parsed
              | None -> Error (failure "captured child has no owned parser")
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
                 ~checkpoint
                 ?programs
                 ?document_projection:(Prompt_revision.document_projection revision)
                 ~prompt_dir:child_dir
                 ~response_dir:t.response_dir
                 ?observer
                 ~source
                 ~shell_manifest_authorizer:t.manifest_authorizer
                 ~shell_approval_provider:t.approval_provider
                 ~ctx
                 items)))
      ()
    |> runner_result)
;;

let bind t ~revision ~sw =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  bind_with_checkpoint t ~revision ~sw ~checkpoint:Eio.Fiber.check
;;
