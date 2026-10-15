open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module B = Chat_response.Background_request
module CM = Prompt.Chat_markdown
module Sources = Agent_session.Prompt_script_preparation
module Runner = Agent_session.Captured_agent_runner
module Owner = Agent_session.Prompt_source_owner
module Revision = Agent_session.Prompt_revision
module Catalog = Agent_session.Prompt_catalog
module Jobs = Agent_session.Script_job_service
module Res = Openai.Responses

let store_ok result =
  result
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let answer text : Res.Response_stream.t =
  Output_item_done
    { item =
        Output_message
          { role = Assistant
          ; id = "selected-answer"
          ; content = [ { annotations = []; text; _type = "output_text" } ]
          ; status = "completed"
          ; phase = None
          ; _type = "message"
          }
    ; output_index = 0
    ; type_ = "response.output_item.done"
    }
;;

let call () : Res.Response_stream.t list =
  let item : Res.Function_call.t =
    { name = "nested"
    ; arguments = {|{"input":"nested input"}|}
    ; call_id = "selected-nested"
    ; _type = "function_call"
    ; id = Some "selected-nested"
    ; status = Some "completed"
    }
  in
  [ Output_item_added
      { item = Function_call item
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = item.arguments
      ; item_id = "selected-nested"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = Function_call item; output_index = 0; type_ = "response.output_item.done" }
  ]
;;

let texts inputs =
  List.concat_map inputs ~f:(function
    | Res.Item.Input_message message ->
      List.filter_map message.content ~f:(function
        | Text { text; _ } -> Some (String.strip text)
        | Image _ -> None)
    | _ -> [])
;;

let with_parent env f =
  let bytes = Cstruct.create 16 in
  Eio.Flow.read_exact (Eio.Stdenv.secure_random env) bytes;
  let basename =
    "och242-queue-"
    ^ String.concat_map (Cstruct.to_string bytes) ~f:(fun c ->
      sprintf "%02x" (Char.to_int c))
  in
  let directory = Eio.Path.(Eio.Stdenv.fs env / "/tmp" / basename) in
  Eio.Path.mkdir ~perm:0o700 directory;
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
    (fun () ->
       Eio.Switch.run (fun sw ->
         let root = Eio.Path.(directory / "source") in
         let responses = Eio.Path.(directory / "responses") in
         Eio.Path.mkdir ~perm:0o700 root;
         Eio.Path.mkdir ~perm:0o700 responses;
         let save name body =
           Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) body
         in
         let child label =
           sprintf
             {|<developer>%s</developer>
<developer><agent src="content.chatmd" local source_mode="captured"/></developer>
<tool name="nested" agent="nested.chatmd" local source_mode="captured"/>|}
             label
         in
         save "root.chatmd" {|<tool name="child" agent="child.chatmd" local/>|};
         save "child.chatmd" (child "A old");
         save "content.chatmd" "<developer>C own</developer>";
         save "nested.chatmd" "<developer>D own</developer>";
         let definition =
           Agent_session.Prompt_definition.create
             ~root_invocation:Ui
             ~id:(P.Id.Prompt_definition.of_string "prd_queued_child" |> protocol_ok)
             ~config_name:"queued-child"
             ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
             ~allowed_workspaces:[]
             ~permission_profile:"fixture"
             ~runtime_policy:None
             ~enabled:true
             ~description:None
           |> store_ok
         in
         let data =
           Agent_store.Data_root.create
             ~env
             ~path:(Eio.Path.native_exn Eio.Path.(directory / "data"))
           |> store_ok
         in
         let source_host =
           Agent_server.Project_source_host.open_owned
             Agent_server.Project_source_configuration.local_only
             ~sw
             ~fs:(Eio.Stdenv.fs env)
             ~data_root:data
             ~clock:(Eio.Stdenv.mono_clock env)
             ~checkpoint:Eio.Fiber.check
             ()
           |> Result.map_error ~f:(fun error ->
             Sexp.to_string_hum (Agent_server.Project_source_host.Error.sexp_of_t error))
           |> Result.ok_or_failwith
         in
         let artifacts =
           Agent_store.Prompt_artifact_store.create
             ~env
             ~root:(Agent_store.Data_root.prompt_artifacts_path data)
           |> store_ok
         in
         let policy =
           Agent_server.Project_source_host.with_host source_host ~f:(fun policy _ ->
             policy)
           |> Result.map_error ~f:(fun _ -> "source host unavailable")
           |> Result.ok_or_failwith
         in
         let catalog =
           Catalog.create ~env ~artifact_store:artifacts ~content_limits:None
         in
         let candidate =
           Catalog.prepare_owned
             catalog
             ~prepare_programs:(fun scope capture ->
               Agent_server.Project_source_host.with_host source_host ~f:(fun _ host ->
                 Sources.capture
                   policy
                   ~root_role:Headless
                   ~host
                   ~scope
                   ~capture
                   ~operation:(fun _ -> Chatml_project_build.Operation.Initial_prepare)
                   ~provided:[]
                   ~env
                   ~checkpoint:Eio.Fiber.check
                   ()
                 |> Result.map ~f:Sources.Prepared.programs)
               |> Result.map_error ~f:(fun _ -> failwith "source host unavailable")
               |> Result.ok_or_failwith)
             ~sw
             ~source_limits:
               (Owner.Limits.create ~max_path_components:64 ~max_runtime_leases:8
                |> Or_error.ok_exn)
             ~max_roots:8
             ~checkpoint:Eio.Fiber.check
             ~transaction_id:(fun _ -> P.Id.Transaction.create ())
             ~created_at:timestamp
             [ definition ]
           |> Or_error.ok_exn
         in
         Catalog.install_owned catalog candidate |> Or_error.ok_exn;
         let parent =
           match
             (Catalog.find catalog definition.id |> Option.value_exn).availability
           with
           | Ready revision -> revision
           | Disabled | Unavailable _ -> failwith "owned parent unavailable"
         in
         let lease =
           Agent_session.Prompt_source_lease.create
             (Revision.artifact parent).source_receipt
             ~selection:(Catalog.source_selection catalog definition.id)
             ~sw
             ~checkpoint:Eio.Fiber.check
         in
         let source =
           Runner.Source.create
             ~policy_revision:
               (Chatml_source.Digest.of_contents "fixture-source-v1" ~checkpoint:ignore)
             ~compilation:(Sources.Policy.compilation policy)
             ~parser_limits:(Agent_session.Prompt_revision_builder.capture_limits ())
             ~max_work_steps:(Sources.Policy.max_work_steps policy)
             ~preprocess:Fn.id
             ~prepare_programs:(fun startup ~tool_contract:_ ~invocation scope capture ->
               Agent_server.Project_source_host.with_host source_host ~f:(fun _ host ->
                 Sources.capture_child
                   policy
                   ~startup
                   ~invocation
                   ~host
                   ~scope
                   ~capture
                   ~operation:(fun _ ->
                     Chatml_project_build.Operation.Build_without_previous)
                   ~provided:[]
                   ~env
                   ~checkpoint:(fun () ->
                     Chat_response.Startup_compilation.checkpoint startup)
                   ()
                 |> Result.map ~f:Sources.Prepared.programs)
               |> Result.map_error ~f:(fun _ -> failwith "source host unavailable")
               |> Result.ok_or_failwith)
           |> Or_error.ok_exn
         in
         let runtime_policy =
           Agent_server.Daemon.default_options.script_preparation |> Option.value_exn
         in
         f sw root responses save child parent artifacts lease source runtime_policy))
;;

let%expect_test
    "saved queued tool selects B while its content and tool select their own children"
  =
  with_actor (fun env sw actor _writer backend ->
    with_parent
      env
      (fun
          resource_sw
           root
           responses
           save
           child
           parent
           artifacts
           lease
           source
           runtime_policy
         ->
         let observed = Queue.create () in
         let outer_calls = ref 0 in
         let inference =
           Inference_fixture.create
             ~namespace:"selected-queue"
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs ->
               let current = texts inputs in
               Queue.enqueue observed current;
               let events =
                 if List.mem current "C own" ~equal:String.equal
                 then [ answer "content answer" ]
                 else if List.mem current "D own" ~equal:String.equal
                 then [ answer "nested answer" ]
                 else (
                   [%test_eq: string list]
                     [ "B selected"; "content answer"; "go" ]
                     current;
                   Int.incr outer_calls;
                   if Int.equal !outer_calls 1
                   then call ()
                   else (
                     [%test_eq: int] 2 !outer_calls;
                     assert (
                       List.exists inputs ~f:(function
                         | Res.Item.Function_call_output
                             { output = Text "nested answer"; _ } -> true
                         | _ -> false));
                     [ answer "selected answer" ]))
               in
               Stdlib.List.to_seq events)
         in
         let ctx =
           Inference_fixture.ctx
             inference
             ~env
             ~dir:root
             ~tool_dir:responses
             ~cache:(Chat_response.Cache.create ~max_size:1 ())
             ()
         in
         let runner =
           Runner.create
             ~script_preparation:runtime_policy
             ~tool_contract:Chatml_compilation.Tool_contract.Standard
             ~content_limits:None
             ~policy_revision:
               (Chatml_source.Digest.of_contents "fixture-runtime-v1" ~checkpoint:ignore)
             ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
             ~approval_provider:Shell_runtime.Approval_broker.None_available
             ~response_dir:responses
         in
         let port =
           Runner.bind_installed
             runner
             ~source
             ~artifact_store:artifacts
             ~source_lease:lease
             ~env
             ~revision:parent
             ~sw:resource_sw
           |> protocol_ok
         in
         let elements = Revision.elements parent in
         let host =
           Chat_response.Agent_runtime.host
             ~env
             ~workspace:root
             ~tool_dir:responses
             ~prompt_dir:root
             ~session_dir:responses
             ~cache_dir:responses
             ~home:responses
             ~session_id:"selected-queue"
             ~prompt_elements:elements
           |> Result.map_error ~f:(fun _ -> "native host unavailable")
           |> Result.ok_or_failwith
         in
         let native =
           Chat_response.Agent_runtime.create
             ~agent_source_runner:port
             ~approval_store:(Shell_access.Approval.create_store ())
             ~sw:resource_sw
             ~ctx
             ~host
             ~platform:(Chat_response.Agent_runtime.platform ())
             ~prompt_elements:elements
             ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
             ~approval_provider:Shell_runtime.Approval_broker.None_available
             ~run_agent:
               (fun
                 ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
               failwith "legacy runner must not execute")
             ()
           |> Result.map_error ~f:(fun _ -> "native preparation unavailable")
           |> Result.ok_or_failwith
         in
         let registry =
           Lazy.force native.capabilities |> Background_execution_tests.cap
         in
         save "child.chatmd" (child "B selected");
         let staged = ref None in
         let source_host =
           Queued_source_tests.host actor ~stage:(fun owner request ->
             let open Result.Let_syntax in
             let%bind job = A.prepare_background_job_launch actor ~owner request in
             let%map () =
               A.stage_background_job
                 actor
                 ~job
                 ~capacity:{ publish = ignore; abort = ignore }
             in
             staged := Some job;
             job)
         in
         let service =
           Jobs.create
             ~env
             ~policy:Chat_response.One_off_request.default_policy
             ~current_capabilities:(fun () -> registry)
             ~prepare_source:(fun ~binding ~owner ~request ~checkpoint ->
               Chat_response.Agent_runtime.prepare_agent_source
                 native
                 ~binding
                 ~launch:(Chat_response.Source_launch.conversation owner)
                 ~request
                 ~max_registrations:16
                 ~checkpoint)
             ~host:source_host
         in
         let issuing = add_claimed_job actor in
         with_job actor issuing (fun ~job:_ ~execute ->
           execute ~invocation:(Job_fixtures.root issuing) (fun ~dispatched ->
             Jobs.with_scope
               service
               ~owner:(Invocation dispatched.context.id)
               ~selected:registry
               ~error:P.Error.invalid_request
               (fun scope ->
                  let open Result.Let_syntax in
                  let%bind id =
                    (Jobs.moderator_transaction scope).handlers.start_tool
                      ~name:"child"
                      ~input:(`Object [ "input", `String "go" ])
                    |> Result.map_error ~f:P.Error.invalid_request
                  in
                  let%map () =
                    source_host.select (Invocation dispatched.context.id) [ id ]
                  in
                  I.Complete `Null)))
         |> protocol_ok
         |> ignore;
         let staged = Option.value_exn !staged in
         let request =
           B.of_json ~policy:Chat_response.One_off_request.default_policy staged.payload
           |> protocol_ok
         in
         let selected = B.agent_artifact request |> Option.value_exn in
         assert (
           Agent_store.Prompt_artifact_store.exists
             artifacts
             (B.Agent_artifact.revision_id selected));
         List.iter (Eio.Path.read_dir root) ~f:(fun name ->
           Eio.Path.rmtree Eio.Path.(root / name));
         Agent_session.Prompt_source_lease.close lease;
         let tools =
           Background_execution_tests.tools (fun () -> registry)
           |> fun tools ->
           Agent_session.Script_tool_calls.with_admission_check tools ~check:(fun _ _ ->
             Ok ())
         in
         (match
            Queued_source_tests.dispatch env sw actor backend staged request tools
          with
          | Succeeded (`String "selected answer") -> ()
          | _ -> failwith "selected queued child failed");
         [%test_eq: string list list]
           [ [ "C own" ]
           ; [ "B selected"; "content answer"; "go" ]
           ; [ "D own"; "nested input" ]
           ; [ "B selected"; "content answer"; "go" ]
           ]
           (Queue.to_list observed);
         print_endline
           "installed B survives source deletion; nested content C and tool D each \
            execute once"));
  [%expect
    {| installed B survives source deletion; nested content C and tool D each execute once |}]
;;
