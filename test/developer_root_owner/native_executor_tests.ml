open! Core
module P = Agent_protocol
module H = P.Host_invocation
module I = P.Invocation
module R = Agent_store.Developer_run_store
module O = Agent_server.Developer_root_owner
module X = Agent_server.Developer_native_executor
module D = Agent_session.Developer_invocation_owner.Selected
module C = Chat_response.Tool_capability
module T = Root_owner_tests
module Scope = Agent_session.Developer_job_scope
module Jobs = Agent_server.Developer_script_job_service
module Disclosure = Agent_server.Developer_result_disclosure
module Blobs = Agent_store.Blob_store
module F = Developer_run_store_test_suite.Developer_run_store_tests
module Selected = Developer_run_store_test_suite.Developer_run_selected_tests

let ok = T.ok

let registry ?unrelated ~custom calls =
  let module Definition = struct
    type input = string

    let name = "read_file"
    let description = Some "selected native fixture"
    let type_ = if custom then "custom" else "function"
    let parameters = `Object [ "type", `String (if custom then "string" else "object") ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun input ->
         Int.incr calls;
         assert (String.equal input "{}");
         Openai.Responses.Tool_output.Output.Text "private result")
  in
  C.create
    ~owner:"selected-native-test"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "selected-resource")
    ((Chatmd_shell_spec.Source_ref.digest "selected-native-v1", implementation)
     :: Option.to_list unrelated)
  |> ok
;;

let pair_registry ~on_parent parent_calls child_calls =
  let module Parent = struct
    type input = string

    let name = "parent_tool"
    let description = Some "nested parent fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let module Child = struct
    type input = string

    let name = "child_tool"
    let description = Some "nested child fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let parent =
    Ochat_function.create_function
      (module Parent)
      (fun _ ->
         Int.incr parent_calls;
         on_parent ();
         Openai.Responses.Tool_output.Output.Text "parent")
  in
  let child =
    Ochat_function.create_function
      (module Child)
      (fun _ ->
         Int.incr child_calls;
         Openai.Responses.Tool_output.Output.Text "child")
  in
  C.create
    ~owner:"nested-selected-native-test"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "nested-resource")
    [ Chatmd_shell_spec.Source_ref.digest "parent-native-v1", parent
    ; Chatmd_shell_spec.Source_ref.digest "child-native-v1", child
    ]
  |> ok
;;

let invocation owner selected reference ~custom =
  H.create_developer
    { H.id = P.Id.Invocation.create ()
    ; owner = P.Execution_owner.Developer owner
    ; origin = Script
    ; parent_invocation = None
    ; parent_job = None
    ; tool_name = reference.C.name
    ; implementation_revision = reference.implementation_revision
    ; capability_fingerprint = C.fingerprint selected
    ; input = (if custom then `String "{}" else `Object [])
    ; created_at = T.now ()
    ; deadline = None
    }
  |> ok
;;

let stage_root_job
      ?(tools = [])
      ?(source = "let main input = Task.pure(input)")
      ?(input = `Null)
      ?(policy = Chat_response.One_off_request.default_policy)
      env
      root
      selected
  =
  let prepared =
    Chat_response.One_off_script.prepare_in_domain
      ~env
      ~capabilities:selected
      ~tools
      ~source
      ()
    |> Result.map_error ~f:(fun diagnostics ->
      Sexp.to_string_hum [%sexp (diagnostics : Chatmd_shell_spec.Diagnostic.t list)])
    |> Result.ok_or_failwith
  in
  let request =
    Chat_response.Background_request.script ~prepared ~input ~policy
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> Result.ok_or_failwith
  in
  O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
    let id =
      O.Action.stage_background action ~request ~checkpoint:ignore
      |> Result.map_error ~f:(fun error -> error.P.Error.message)
      |> Result.ok_or_failwith
    in
    let selected =
      O.Action.seal action ~selected:[ id ]
      |> Result.map_error ~f:(fun error -> error.P.Error.message)
      |> Result.ok_or_failwith
    in
    Ok (id, selected))
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let succeeded_claim claim =
  let state = P.Developer_job.state (R.Claim.job claim) in
  { state with
    status = P.Job.Succeeded
  ; completed_at = Some (T.now ())
  ; result = Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
  }
;;

let with_executor
      ?(checkpoint = ignore)
      ?(on_current = ignore)
      ?(actor_is_current = fun () -> true)
      ?on_store
      ?on_capacity
      ?on_owner
      ?max_job_payload_bytes
      ?(max_active_workers = 1)
      ?(per_session = 2)
      ?(wrap_env = Fn.id)
      ?(now = T.now)
      ?(job_policy = Chat_response.One_off_request.default_policy)
      ~run_name
      ~make_selected
      ?make_selected_with_switch
      f
  =
  T.with_store ~checkpoint ~wrap_env ?on_store ?max_job_payload_bytes (fun env sw store ->
    let captured = T.capture () in
    let owner = T.owner run_name 0 in
    let handle, _ =
      R.admit_epoch
        store
        ~owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
        ~previous:None
      |> ok
    in
    let selected =
      match make_selected_with_switch with
      | None -> make_selected env
      | Some make_selected -> make_selected env sw
    in
    let limits =
      O.Limits.create
        ~max_active_runs:2
        ~max_staged_jobs:2
        ~max_native_occupancy:2
        ~max_active_workers
        ~max_quarantined_workers:2
        ~max_active_inference_attempts:8
        ~max_admitted_call_bindings:32
      |> Or_error.ok_exn
    in
    let capacity = T.capacity ~per_session () in
    Option.iter on_capacity ~f:(fun notify -> notify capacity);
    let owners = O.Registry.create ~sw ~store ~capacity ~now ~limits in
    let root =
      O.Registry.admit
        owners
        ~actor:
          (Operator_authorization.guarded ~principal:T.principal ~is_current:(fun () ->
             on_current ();
             actor_is_current ()))
        ~capture:captured
        ~registry:selected
        ~selected
        ~job_policy
        ~selected_model:None
        ~handle
        ~workspace_conflict_domain:"selected-native-test"
      |> ok
    in
    Option.iter on_owner ~f:(fun notify -> notify owners capacity);
    let executor = X.create ~root |> ok in
    let result = f ~env ~root ~executor ~handle ~owner ~selected in
    O.close_and_wait root |> ok;
    O.Registry.close_and_wait owners |> ok;
    result)
;;

let with_job_executor
      ?(checkpoint = ignore)
      ?(current_actor = fun root -> O.original_actor root)
      ?(on_current = ignore)
      ?(actor_is_current = fun () -> true)
      ?(wrap_env = Fn.id)
      ?(on_store = fun _ _ _ -> ())
      ?on_capacity
      ?on_owner
      ?max_job_payload_bytes
      ?(max_active_workers = 1)
      ?(per_session = 2)
      ?(now = T.now)
      ?(job_policy = Chat_response.One_off_request.default_policy)
      ~run_name
      ~make_selected
      ?make_selected_with_switch
      ?(prepare_source =
        fun ~parent:_ ~binding:_ ~request:_ ~checkpoint ->
          checkpoint ();
          Ok None)
      f
  =
  let resources = ref None in
  with_executor
    ~checkpoint
    ~on_current
    ~actor_is_current
    ~wrap_env
    ?on_capacity
    ?on_owner
    ?max_job_payload_bytes
    ~max_active_workers
    ~per_session
    ~now
    ~job_policy
    ~run_name
    ~make_selected
    ?make_selected_with_switch
    ~on_store:(fun env session store ->
      on_store env session store;
      let root = Agent_store.Session_store.data_root session in
      let blobs =
        Blobs.create
          ~env
          ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path root)
          ~durable_directory:(Agent_store.Data_root.durable_blobs_path root)
          ~max_upload_bytes:65536L
        |> ok
      in
      Blobs.bind_developer_runs blobs ~session_store:session store
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
      |> Result.ok_or_failwith;
      let limits =
        Disclosure.Limits.create
          ~max_result_bytes:65536
          ~json:Document_schema.Limits.default
        |> ok
      in
      let policy = Disclosure.Policy.create ~authorize:(fun _ _ -> true) in
      let disclosure = Disclosure.create ~runs:store ~blobs ~policy ~limits |> ok in
      resources := Some (store, disclosure, blobs))
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let store, disclosure, blobs = Option.value_exn !resources in
       let jobs =
         Jobs.create
           ~root
           ~store
           ~disclosure
           ~env
           ~current_actor:(fun () -> current_actor root)
           ~queued_issuer:None
           ~prepare_source
         |> Result.map_error ~f:(fun error -> error.P.Error.message)
         |> Result.ok_or_failwith
       in
       let executor = X.with_job_service executor jobs |> ok in
       f ~env ~root ~executor ~handle ~owner ~selected ~jobs ~store ~blobs)
;;

let background_runner_with_runtime
      ~agent_runtime
      ~sw:_
      ~root
      ~store:_
      ~blobs
      ~env
      ~managed_definition
      ~jobs
      ~storage
  =
  let limits =
    Agent_store.Developer_result_publisher.Limits.create
      ~inline_bytes:1024
      ~max_result_bytes:65536
      ~max_pending:2
    |> ok
  in
  let results =
    Agent_server.Developer_root_owner.Job_result.bind root ~blobs ~limits ~storage |> ok
  in
  Agent_server.Developer_background_job_runner.create
    ~root
    ~results
    ~env
    ~managed_definition
    ~agent_runtime
    ~jobs
    ~authorize:(fun _ _ -> Ok ())
    ~prepare_output:(function
      | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
      | _ -> Error (P.Error.invalid_request "unexpected output"))
  |> ok
;;

let background_runner ~sw ~root ~store ~blobs ~env ~managed_definition ~jobs ~storage =
  background_runner_with_runtime
    ~agent_runtime:None
    ~sw
    ~root
    ~store
    ~blobs
    ~env
    ~managed_definition
    ~jobs
    ~storage
;;

let await_developer_status ~env ~handle id wanted =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    let rec wait () =
      let job = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
      let state = P.Developer_job.state job in
      if wanted state.status
      then state
      else (
        match state.status with
        | P.Job.Failed _ | Cancelled | Interrupted _ ->
          failwith (Sexp.to_string_hum [%sexp (state.status : P.Job.status)])
        | Queued | Running | Waiting_permission _ | Waiting_completion _ | Succeeded ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          wait ())
    in
    wait ())
;;

type installed_agent_fixture =
  { runtime : Chat_response.Agent_runtime.t
  ; selected : C.t
  ; source_root : Eio.Fs.dir_ty Eio.Path.t
  ; source : Agent_session.Captured_agent_runner.Source.t
  ; parent : Agent_session.Prompt_revision.t
  ; artifacts : Agent_store.Prompt_artifact_store.t
  ; policy : Agent_session.Runtime_script_preparation.t
  ; response_root : Eio.Fs.dir_ty Eio.Path.t
  }

type installed_agent_source =
  { source_root : Eio.Fs.dir_ty Eio.Path.t
  ; response_root : Eio.Fs.dir_ty Eio.Path.t
  ; source : Agent_session.Captured_agent_runner.Source.t
  ; parent : Agent_session.Prompt_revision.t
  ; artifacts : Agent_store.Prompt_artifact_store.t
  ; policy : Agent_session.Runtime_script_preparation.t
  }

let default_child_contents =
  {|<developer>Original child instructions</developer>
<developer><agent src="content.chatmd" local source_mode="captured"/></developer>|}
;;

let installed_agent_source_with_contents
      ?(agent_names = [ "child" ])
      ~child_contents
      ~env
      ~data_root
      ()
  =
  let module Builder = Agent_session.Prompt_revision_builder in
  let module Runner = Agent_session.Captured_agent_runner in
  let source_root = Eio.Path.(Eio.Stdenv.fs env / data_root / "agent-source") in
  let response_root = Eio.Path.(Eio.Stdenv.fs env / data_root / "agent-responses") in
  Eio.Path.mkdir ~perm:0o700 source_root;
  Eio.Path.mkdir ~perm:0o700 response_root;
  let save name contents =
    Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(source_root / name) contents
  in
  save
    "root.chatmd"
    (String.concat
       ~sep:"\n"
       (List.map agent_names ~f:(fun name ->
          sprintf
            {|<tool name="%s" agent="%s.chatmd" local source_mode="captured"/>|}
            name
            name)));
  List.iter agent_names ~f:(fun name -> save (name ^ ".chatmd") child_contents);
  save "content.chatmd" "<developer>Content own instructions</developer>";
  save "input.json" {|{"type":"object"}|};
  save "output.json" {|{"type":"string"}|};
  let definition =
    Agent_session.Prompt_definition.create
      ~root_invocation:Ui
      ~id:(P.Id.Prompt_definition.create ())
      ~config_name:"claimed-agent"
      ~root_file:(Eio.Path.native_exn Eio.Path.(source_root / "root.chatmd"))
      ~allowed_workspaces:[]
      ~permission_profile:"fixture"
      ~runtime_policy:None
      ~enabled:true
      ~description:None
    |> ok
  in
  let artifacts =
    Agent_store.Prompt_artifact_store.create
      ~env
      ~root:(Filename.concat data_root "prompt-artifacts")
    |> ok
  in
  let parent =
    Builder.build
      ~env
      ~artifact_store:artifacts
      ~transaction_id:(P.Id.Transaction.create ())
      ~created_at:(T.now ())
      definition
    |> Result.map_error ~f:(fun errors ->
      Sexp.to_string_hum ([%sexp_of: Builder.Diagnostic.t list] errors))
    |> Result.ok_or_failwith
  in
  let policy =
    Agent_server.Daemon.default_options.script_preparation |> Option.value_exn
  in
  let source =
    Runner.Source.create
      ~policy_revision:
        (Chatml_source.Digest.of_contents "claimed-agent-source" ~checkpoint:ignore)
      ~compilation:(Agent_session.Runtime_script_preparation.compilation policy)
      ~parser_limits:(Builder.capture_limits ())
      ~max_work_steps:
        (Agent_session.Runtime_script_preparation.max_custody_work_steps policy)
      ~preprocess:Fn.id
      ~prepare_programs:(fun _ ~tool_contract:_ ~invocation:_ _ _ ->
        failwith "saved Agent artifact must not prepare a new file source")
    |> Or_error.ok_exn
  in
  { source_root; response_root; source; parent; artifacts; policy }
;;

let installed_agent_fixture_with_contents
      ?run_selected_target
      ?with_deferred_context
      ~deferred
      ~child_contents
      ~on_run
      ~env
      ~sw
      ~data_root
      ~observed
      ()
  =
  let module Revision = Agent_session.Prompt_revision in
  let module Runner = Agent_session.Captured_agent_runner in
  let { source_root; response_root; source; parent; artifacts; policy } =
    installed_agent_source_with_contents ~child_contents ~env ~data_root ()
  in
  let runner =
    Runner.create
      ~script_preparation:policy
      ~tool_contract:Chatml_compilation.Tool_contract.Standard
      ~content_limits:None
      ~policy_revision:
        (Chatml_source.Digest.of_contents "claimed-agent-runtime" ~checkpoint:ignore)
      ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
      ~approval_provider:Shell_runtime.Approval_broker.None_available
      ~response_dir:response_root
  in
  let port =
    Runner.bind_installed
      runner
      ~source
      ~artifact_store:artifacts
      ~source_lease:Agent_session.Prompt_source_lease.legacy
      ~env
      ~revision:parent
      ~sw
    |> ok
  in
  let port =
    { port with
      run =
        (fun ~implementation ~declaration ~ctx ~source ~observer items ->
          on_run ();
          port.run ~implementation ~declaration ~ctx ~source ~observer items)
    }
  in
  let inference =
    Inference_fixture.create
      ~namespace:"claimed-agent"
      ~default_model:"fixture-model"
      ~post_stream:(fun ~sw:_ ~inputs ->
        let input_text =
          List.concat_map inputs ~f:(function
            | Openai.Responses.Item.Input_message message ->
              List.filter_map message.content ~f:(function
                | Text { text; _ } -> Some text
                | Image _ -> None)
            | _ -> [])
        in
        observed := input_text :: !observed;
        let answer =
          if List.mem input_text "Content own instructions" ~equal:String.equal
          then "content answer"
          else "captured child result"
        in
        Stdlib.List.to_seq
          [ Openai.Responses.Response_stream.Output_item_done
              { item =
                  Output_message
                    { role = Assistant
                    ; id = "claimed-agent-answer"
                    ; content =
                        [ { annotations = []; text = answer; _type = "output_text" } ]
                    ; status = "completed"
                    ; phase = None
                    ; _type = "message"
                    }
              ; output_index = 0
              ; type_ = "response.output_item.done"
              }
          ])
  in
  let ctx =
    Inference_fixture.ctx
      inference
      ~env
      ~dir:source_root
      ~tool_dir:response_root
      ~cache:(Chat_response.Cache.create ~max_size:1 ())
      ()
  in
  let elements = Revision.elements parent in
  let host =
    Chat_response.Agent_runtime.host
      ~env
      ~workspace:source_root
      ~tool_dir:response_root
      ~prompt_dir:source_root
      ~session_dir:response_root
      ~cache_dir:response_root
      ~home:response_root
      ~session_id:"claimed-agent"
      ~prompt_elements:elements
    |> ok
  in
  let deferred_registrations =
    if deferred
    then (
      let binding =
        Runner.Source_binding.create
          ~source
          ~tool_contract:Chatml_compilation.Tool_contract.Standard
          ~runtime_policy_revision:
            (Chatml_source.Digest.of_contents
               "claimed-deferred-runtime.v1"
               ~checkpoint:ignore)
          ~content_limits:None
          ~artifact_store:artifacts
          ~source_lease:Agent_session.Prompt_source_lease.legacy
      in
      let prepared =
        Runner.Deferred_developer.prepare
          ?run_selected_target
          binding
          ~script_preparation:policy
          ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~response_dir:response_root
          ~env
          ~revision:parent
          ~sw
          ~max_agents:1
          ~max_work_steps:100_000
          ~checkpoint:ignore
          ~with_context:(fun ~implementation ~invocation:_ f ->
            let borrowed =
              Agent_session.Native_tool_invocation.borrow_selected () |> ok
            in
            Runner.Claimed_developer.with_selected
              ~borrowed
              ~implementation
              (fun selected ->
                 Runner.Claimed_developer.Selected.check selected |> ok;
                 on_run ();
                 Ok
                   (match with_deferred_context with
                    | None -> f ctx
                    | Some with_deferred_context ->
                      with_deferred_context selected (fun () -> f ctx)))
            |> ok)
        |> ok
      in
      Runner.Deferred_developer.registrations prepared)
    else []
  in
  let runtime =
    if deferred
    then
      (Chat_response.Agent_runtime.prepare_extensions_without_inference
         ~native_registrations:deferred_registrations
         ~approval_store:(Shell_access.Approval.create_store ())
         ~sw
         ~host
         ~platform:(Chat_response.Agent_runtime.platform ())
         ~prompt_elements:elements
         ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
         ~approval_provider:Shell_runtime.Approval_broker.None_available
         ()
       |> Result.map_error ~f:(fun diagnostics ->
         Sexp.to_string_hum
           ([%sexp_of: Chat_response.Agent_runtime.diagnostic list] diagnostics))
       |> Result.ok_or_failwith)
        .native
    else
      Chat_response.Agent_runtime.create
        ~agent_source_runner:port
        ~approval_store:(Shell_access.Approval.create_store ())
        ~sw
        ~ctx
        ~host
        ~platform:(Chat_response.Agent_runtime.platform ())
        ~prompt_elements:elements
        ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
        ~approval_provider:Shell_runtime.Approval_broker.None_available
        ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
          failwith "legacy Agent runner must not execute")
        ()
      |> ok
  in
  let selected = Lazy.force runtime.capabilities |> ok in
  assert (
    List.exists runtime.native_registrations ~f:(fun registration ->
      match registration.source_requirement with
      | Chat_response.Native_tool_pool.Source_requirement.Source (Some _) -> true
      | Source None | Ordinary -> false));
  { runtime; selected; source_root; source; parent; artifacts; policy; response_root }
;;

let installed_agent_fixture ~on_run ~env ~sw ~data_root ~observed =
  installed_agent_fixture_with_contents
    ~deferred:false
    ~child_contents:default_child_contents
    ~on_run
    ~env
    ~sw
    ~data_root
    ~observed
    ()
;;

let stage_registered_agent_tool ?(name = "child") jobs =
  let module Runtime = Chatml_host_runtime in
  let module Value = Chatml.Chatml_value_codec in
  let compiled =
    Runtime.compile_script
      ~surface:Chatml.Chatml_extension_surface.one_off_v1
      ~source:
        (sprintf
           {|let main input = Job.start_tool("%s", `Object([{key="input"; value=`String("go")}]))|}
           name)
      ()
    |> Result.ok_or_failwith
  in
  let config : Runtime.runtime_config =
    { surface = Chatml.Chatml_extension_surface.one_off_v1
    ; operations = Runtime.default_operations ~handlers:Runtime.default_handlers ()
    }
  in
  Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
    let config = Jobs.Scope.install scope config in
    let effects = ref [] in
    let value =
      Runtime.run_entrypoint
        ~prepare_result:(fun ~value:_ ~local_effects ->
          effects := local_effects;
          Ok ignore)
        config
        compiled
        ~entrypoint:"main"
        ~arguments:[ Value.import_json (`Object []) ]
        ()
      |> Result.map_error ~f:P.Error.invalid_request
    in
    Result.map value ~f:(fun value -> value, !effects))
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
  |> fst
  |> function
  | Chatml.Chatml_lang.VString id -> P.Id.Job.of_string id |> ok
  | _ -> failwith "Job.start_tool returned no job ID"
;;

let with_registered_agent
      ?(checkpoint = ignore)
      ?(on_current = ignore)
      ?(actor_is_current = fun () -> true)
      ?(deferred = false)
      ?(child_contents = default_child_contents)
      ~run_name
      ~observed
      ~on_run
      f
  =
  let data_root = ref None in
  let fixture = ref None in
  let preparations = ref 0 in
  with_job_executor
    ~run_name
    ~max_job_payload_bytes:4096
    ~checkpoint
    ~on_current
    ~actor_is_current
    ~on_store:(fun _env session _store ->
      data_root
      := Some (Agent_store.Data_root.path (Agent_store.Session_store.data_root session)))
    ~make_selected:(fun _ -> failwith "selected runtime requires the owning switch")
    ~make_selected_with_switch:(fun env sw ->
      let value =
        installed_agent_fixture_with_contents
          ~deferred
          ~child_contents
          ~on_run
          ~env
          ~sw
          ~data_root:(Option.value_exn !data_root)
          ~observed
          ()
      in
      fixture := Some value;
      value.selected)
    ~prepare_source:(fun ~parent ~binding ~request ~checkpoint ->
      Int.incr preparations;
      let fixture = Option.value_exn !fixture in
      let open Result.Let_syntax in
      let%bind launch = Jobs.Source_parent.launch parent in
      assert (
        match Chat_response.Source_launch.developer_parent launch with
        | Some Chat_response.Source_launch.Developer_parent.Root_action -> true
        | Some _ | None -> false);
      let%bind () = Jobs.Source_parent.check parent ~binding in
      Chat_response.Agent_runtime.prepare_agent_source
        fixture.runtime
        ~binding
        ~launch
        ~request
        ~max_registrations:16
        ~checkpoint)
    (fun ~env ~root ~executor ~handle ~owner ~selected ~jobs ~store ~blobs ->
       f
         ~env
         ~root
         ~executor
         ~handle
         ~owner
         ~selected
         ~jobs
         ~store
         ~blobs
         ~fixture:(Option.value_exn !fixture)
         ~preparations)
;;

let%test_unit
    "claimed Tool runs its exact saved Agent artifact through the installed runtime"
  =
  let observed = ref [] in
  let selected_registry = ref None in
  let current_root = ref None in
  let borrowed_agent = ref None in
  with_registered_agent
    ~run_name:"dvr_claimed_agent_artifact"
    ~observed
    ~on_run:(fun () ->
      if Option.is_none !borrowed_agent
      then (
        let borrowed = Agent_session.Native_tool_invocation.borrow_selected () |> ok in
        let binding = C.find (Option.value_exn !selected_registry) ~name:"child" |> ok in
        let implementation = C.native_implementation binding |> Option.value_exn in
        let result =
          Agent_session.Captured_agent_runner.Claimed_developer.with_selected
            ~borrowed
            ~implementation
            (fun selected ->
               let module Selected =
                 Agent_session.Captured_agent_runner.Claimed_developer.Selected
               in
               Selected.check selected |> ok;
               borrowed_agent := Some selected;
               match
                 O.Model_inference.with_claimed_agent
                   (Option.value_exn !current_root)
                   ~selected
                   ~f:(fun _ ~sw:_ -> ())
               with
               | Error { message = "developer inference provider is unavailable"; _ } ->
                 Ok ()
               | Ok () | Error _ ->
                 Error
                   (P.Error.invalid_request
                      "saved Agent Root proof did not reach provider selection"))
        in
        ignore (result |> ok : unit)))
    (fun ~env
      ~root
      ~executor:_
      ~handle
      ~owner:_
      ~selected
      ~jobs
      ~store
      ~blobs
      ~fixture
      ~preparations ->
       let source_registration =
         List.find_exn fixture.runtime.native_registrations ~f:(fun registration ->
           match registration.source_requirement with
           | Chat_response.Native_tool_pool.Source_requirement.Source (Some _) -> true
           | Source None | Ordinary -> false)
       in
       Chat_response.Agent_runtime.admit_exact_saved_agent_registration
         fixture.runtime
         ~registration:source_registration
         ~max_registrations:16
         ~checkpoint:ignore
       |> ok;
       let counterfeit_source =
         Chat_response.Native_source_registration.create
           ~implementation:source_registration.implementation
           ~prepare:(fun ~launch:_ ~request:_ ~checkpoint:_ -> Ok None)
       in
       assert (
         Result.is_error
           (Chat_response.Agent_runtime.admit_exact_saved_agent_registration
              fixture.runtime
              ~registration:
                { source_registration with
                  source_requirement =
                    Chat_response.Native_tool_pool.Source_requirement.Source
                      (Some counterfeit_source)
                }
              ~max_registrations:16
              ~checkpoint:ignore));
       let id = stage_registered_agent_tool jobs in
       selected_registry := Some selected;
       current_root := Some root;
       assert (Int.equal !preparations 1);
       let stored = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
       let request =
         Chat_response.Background_request.of_json
           ~policy:Chat_response.One_off_request.default_policy
           (P.Developer_job.payload stored)
         |> ok
       in
       assert (Option.is_some (Chat_response.Background_request.agent_artifact request));
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         Eio.Path.(fixture.source_root / "child.chatmd")
         "<developer>Revised child instructions</developer>";
       Eio.Switch.run (fun source_switch ->
         let source_binding =
           Agent_session.Captured_agent_runner.Source_binding.create
             ~source:fixture.source
             ~tool_contract:Chatml_compilation.Tool_contract.Standard
             ~runtime_policy_revision:
               (Chatml_source.Digest.of_contents
                  "claimed-deferred-source.v1"
                  ~checkpoint:ignore)
             ~content_limits:None
             ~artifact_store:fixture.artifacts
             ~source_lease:Agent_session.Prompt_source_lease.legacy
         in
         let deferred =
           Agent_session.Captured_agent_runner.Deferred_developer.prepare
             source_binding
             ~script_preparation:fixture.policy
             ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
             ~approval_provider:Shell_runtime.Approval_broker.None_available
             ~response_dir:fixture.response_root
             ~env
             ~revision:fixture.parent
             ~sw:source_switch
             ~max_agents:1
             ~max_work_steps:100_000
             ~checkpoint:ignore
             ~with_context:(fun ~implementation:_ ~invocation:_ _ ->
               failwith "unbound deferred Agent must not borrow a Context")
           |> ok
         in
         [%test_eq: int]
           1
           (List.length
              (Agent_session.Captured_agent_runner.Deferred_developer.registrations
                 deferred)));
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner_with_runtime
             ~agent_runtime:(Some fixture.runtime)
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         let completed =
           Agent_server.Developer_background_job_runner.run_candidate runner candidate
           |> ok
           |> Option.value_exn
         in
         assert (P.Id.Job.equal id (P.Developer_job.id completed));
         assert (
           match (P.Developer_job.state completed).status with
           | P.Job.Succeeded -> true
           | Failed _
           | Queued
           | Running
           | Waiting_permission _
           | Waiting_completion _
           | Cancelled
           | Interrupted _ -> false);
         assert (
           List.exists !observed ~f:(fun row ->
             List.mem row "Original child instructions" ~equal:String.equal));
         assert (
           List.exists !observed ~f:(fun row ->
             List.mem row "Original child instructions" ~equal:String.equal
             && List.mem row "content answer" ~equal:String.equal));
         assert (
           List.exists !observed ~f:(fun row ->
             List.mem row "Content own instructions" ~equal:String.equal));
         assert (
           not
             (List.exists !observed ~f:(fun row ->
                List.mem row "Revised child instructions" ~equal:String.equal)));
         assert (
           P.Id.Job.equal
             id
             (P.Developer_job.id
                (R.find_job (R.observe handle |> ok) id |> Option.value_exn)));
         assert (
           Result.is_error
             (Agent_session.Captured_agent_runner.Claimed_developer.Selected.check_now
                (Option.value_exn !borrowed_agent)));
         borrowed_agent := None;
         let second_id = stage_registered_agent_tool jobs in
         let second_stored =
           R.find_job (R.observe handle |> ok) second_id |> Option.value_exn
         in
         let second_request =
           Chat_response.Background_request.of_json
             ~policy:Chat_response.One_off_request.default_policy
             (P.Developer_job.payload second_stored)
           |> ok
         in
         let first_artifact =
           Chat_response.Background_request.agent_artifact request |> Option.value_exn
         in
         let second_artifact =
           Chat_response.Background_request.agent_artifact second_request
           |> Option.value_exn
         in
         assert (
           P.Id.Prompt_revision.equal
             (Chat_response.Background_request.Agent_artifact.revision_id first_artifact)
             (Chat_response.Background_request.Agent_artifact.revision_id second_artifact));
         assert (
           Chatml_source.Digest.equal
             (Chat_response.Background_request.Agent_artifact.manifest_sha256
                first_artifact)
             (Chat_response.Background_request.Agent_artifact.manifest_sha256
                second_artifact));
         let observed_before_second = List.length !observed in
         let second_candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore
           |> ok
           |> List.find_exn ~f:(fun candidate ->
             P.Id.Job.equal second_id (P.Developer_job.id (O.Job_candidate.job candidate)))
         in
         let second_completed =
           Agent_server.Developer_background_job_runner.run_candidate
             runner
             second_candidate
           |> ok
           |> Option.value_exn
         in
         assert (
           match (P.Developer_job.state second_completed).status with
           | P.Job.Succeeded -> true
           | Failed _
           | Queued
           | Running
           | Waiting_permission _
           | Waiting_completion _
           | Cancelled
           | Interrupted _ -> false);
         let second_observed =
           List.take !observed (List.length !observed - observed_before_second)
         in
         assert (
           List.exists second_observed ~f:(fun row ->
             List.mem row "Original child instructions" ~equal:String.equal));
         assert (
           not
             (List.exists second_observed ~f:(fun row ->
                List.mem row "Revised child instructions" ~equal:String.equal)))))
;;

let%test_unit "deferred saved Agent runs literal child and denies unselected effects" =
  let cases =
    [ "literal", "<developer>Original literal child instructions</developer>", true
    ; ( "builtin"
      , {|<developer>Original child instructions</developer>
<tool name="read_file"/>|}
      , false )
    ; ( "managed"
      , {|<developer>Original child instructions</developer>
<script id="handler" kind="tool" language="chatml">
let run ctx input = Task.pure(`Complete(`String("managed")))
</script>
<tool name="managed" type="chatml" script="handler" entrypoint="run" input_schema="input.json" output_schema="output.json"/>|}
      , false )
    ]
  in
  List.iter cases ~f:(fun (name, child_contents, should_succeed) ->
    let observed = ref [] in
    let entered = ref 0 in
    with_registered_agent
      ~deferred:true
      ~child_contents
      ~run_name:("dvr_deferred_" ^ name)
      ~observed
      ~on_run:(fun () -> Int.incr entered)
      (fun ~env
        ~root
        ~executor:_
        ~handle
        ~owner:_
        ~selected:_
        ~jobs
        ~store
        ~blobs
        ~fixture
        ~preparations ->
         let id = stage_registered_agent_tool jobs in
         assert (Int.equal !preparations 1);
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(fixture.source_root / "child.chatmd")
           "<developer>Changed after admission</developer>";
         Eio.Switch.run (fun sw ->
           let runner =
             background_runner_with_runtime
               ~agent_runtime:(Some fixture.runtime)
               ~sw
               ~root
               ~store
               ~blobs
               ~env
               ~managed_definition:None
               ~jobs
               ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
           in
           let candidate =
             O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
           in
           ignore
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate
              : (P.Developer_job.t option, P.Error.t) result);
           let persisted = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
           let failure =
             match (P.Developer_job.state persisted).status with
             | P.Job.Succeeded -> None
             | Failed error -> Some error
             | Queued
             | Running
             | Waiting_permission _
             | Waiting_completion _
             | Cancelled
             | Interrupted _ -> failwith "deferred child has no terminal status"
           in
           assert (Bool.equal should_succeed (Option.is_none failure));
           assert (Int.equal !entered 1);
           if should_succeed
           then (
             assert (
               List.exists !observed ~f:(fun row ->
                 List.mem row "Original literal child instructions" ~equal:String.equal));
             assert (
               not
                 (List.exists !observed ~f:(fun row ->
                    List.mem row "Changed after admission" ~equal:String.equal))))
           else (
             assert (List.is_empty !observed);
             assert (Option.is_some failure)))))
;;

let%test_unit "claimed Agent source rejects actor revocation during its Claim read" =
  let observed = ref [] in
  let source_started = ref false in
  let root_checked = ref false in
  let current = ref true in
  let hold_next_read = ref true in
  let reached, reached_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  with_registered_agent
    ~run_name:"dvr_claimed_agent_revoke"
    ~observed
    ~on_run:(fun () -> source_started := true)
    ~on_current:(fun () -> if !source_started then root_checked := true)
    ~actor_is_current:(fun () -> !current)
    ~checkpoint:(fun () ->
      if !root_checked && !hold_next_read
      then (
        hold_next_read := false;
        Eio.Promise.resolve reached_u ();
        Eio.Promise.await release))
    (fun ~env
      ~root
      ~executor:_
      ~handle:_
      ~owner:_
      ~selected:_
      ~jobs
      ~store
      ~blobs
      ~fixture
      ~preparations ->
       let id = stage_registered_agent_tool jobs in
       assert (Int.equal !preparations 1);
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner_with_runtime
             ~agent_runtime:(Some fixture.runtime)
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         let finished, finished_u = Eio.Promise.create () in
         Eio.Fiber.fork ~sw (fun () ->
           Eio.Promise.resolve
             finished_u
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate));
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
           Eio.Promise.await reached);
         assert !source_started;
         current := false;
         Eio.Promise.resolve release_u ();
         (match Eio.Promise.await finished with
          | Error _ | Ok None -> ()
          | Ok (Some job) ->
            assert (P.Id.Job.equal id (P.Developer_job.id job));
            assert (
              match (P.Developer_job.state job).status with
              | P.Job.Succeeded -> false
              | Failed _
              | Queued
              | Running
              | Waiting_permission _
              | Waiting_completion _
              | Cancelled
              | Interrupted _ -> true));
         assert (List.is_empty !observed)))
;;

let%test_unit "nested Agent callback cannot consume outer saved artifact pin" =
  let observed = ref [] in
  let source_calls = ref 0 in
  let rejected = ref false in
  let descendant_checked = ref false in
  let descendant_error = ref None in
  let saved_selected = ref None in
  let nested_invocation = ref None in
  let current_root = ref None in
  let selected_registry = ref None in
  let run_nested = ref (fun () -> ()) in
  with_registered_agent
    ~run_name:"dvr_nested_agent_artifact"
    ~observed
    ~on_run:(fun () ->
      Int.incr source_calls;
      if Int.equal !source_calls 1
      then (
        let borrowed = Agent_session.Native_tool_invocation.borrow_selected () |> ok in
        let binding = C.find (Option.value_exn !selected_registry) ~name:"child" |> ok in
        let implementation = C.native_implementation binding |> Option.value_exn in
        Agent_session.Captured_agent_runner.Claimed_developer.with_selected
          ~borrowed
          ~implementation
          (fun selected ->
             saved_selected := Some selected;
             O.Model_inference.check_saved_agent_source
               (Option.value_exn !current_root)
               ~selected
             |> ok;
             !run_nested ();
             Ok ())
        |> ok)
      else if Int.equal !source_calls 2
      then (
        let child = Agent_session.Native_tool_invocation.borrow_selected () |> ok in
        let actual =
          Agent_session.Native_tool_invocation.selected_borrowed_invocation child
        in
        let expected = Option.value_exn !nested_invocation in
        if not (P.Id.Invocation.equal (H.context actual).id (H.context expected).id)
        then descendant_error := Some "second source call retained parent selected borrow"
        else (
          match
            O.Model_inference.check_saved_agent_source_descendant_now
              (Option.value_exn !current_root)
              ~selected:(Option.value_exn !saved_selected)
              ~child
          with
          | Ok () -> descendant_checked := true
          | Error error -> descendant_error := Some error.message))
      else failwith "Agent source entered an unexpected invocation")
    (fun ~env
      ~root
      ~executor
      ~handle
      ~owner
      ~selected
      ~jobs
      ~store:_
      ~blobs:_
      ~fixture
      ~preparations ->
       let id = stage_registered_agent_tool jobs in
       assert (Int.equal !preparations 1);
       current_root := Some root;
       selected_registry := Some selected;
       let binding = C.find selected ~name:"child" |> ok in
       let reference = C.reference binding in
       (run_nested
        := fun () ->
             let observed_before = List.length !observed in
             let borrowed =
               Agent_session.Native_tool_invocation.borrow_selected () |> ok
             in
             let original =
               Agent_session.Native_tool_invocation.selected_borrowed_invocation borrowed
             in
             let child =
               H.create_developer
                 { (H.context (invocation owner selected reference ~custom:false)) with
                   parent_invocation = Some (H.context original).id
                 ; parent_job = (H.context original).parent_job
                 ; input = (H.context original).input
                 }
               |> ok
             in
             nested_invocation := Some child;
             let terminal =
               X.run_child
                 executor
                 ~reference
                 ~invocation:child
                 ~authorize:(fun _ _ -> Ok ())
                 ~prepare_output:(fun _ -> Ok (`String "nested"))
               |> ok
             in
             (match H.status terminal with
              | Resolved (Fail _) -> rejected := true
              | Admitted | Dispatching | Published _
              | Resolved (Complete _ | Pending _ | Cancelled _) ->
                failwith "nested same-function Agent consumed outer saved pin");
             assert (Int.equal observed_before (List.length !observed)));
       let expected = R.observe handle |> ok in
       let completed =
         O.with_claimed_job
           root
           ~expected
           ~lookup:(R.find_job_exact expected id)
           (fun ~active ~sw:_ ->
              let claim = Scope.claim active in
              let job = R.Claim.job claim in
              let policy = O.job_policy root in
              let request =
                Chat_response.Background_request.of_json
                  ~policy
                  (P.Developer_job.payload job)
                |> ok
              in
              let prepared =
                Chat_response.Background_request.prepare
                  ~env
                  ~current_capabilities:(fun () -> selected)
                  ~policy
                  request
                |> ok
              in
              let invocation =
                let admitted = invocation owner selected reference ~custom:false in
                let input =
                  match prepared with
                  | Chat_response.Background_request.Tool { input; _ } -> input
                  | Script _ -> failwith "staged Agent request is not a Tool"
                in
                H.create_developer
                  { (H.context admitted) with parent_job = Some id; input }
                |> ok
              in
              let parent = O.job_parent root active |> ok in
              let check_current () =
                let capture_fingerprint =
                  Chatml_developer_capture.fingerprint (O.capture root)
                in
                let open Result.Let_syntax in
                let%bind () = Scope.Parent.check parent ~owner ~capture_fingerprint in
                Scope.Parent.check_now parent ~owner ~capture_fingerprint
              in
              let run_native () =
                X.run_job_child
                  executor
                  ~active
                  ~reference
                  ~invocation
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok (`String "outer"))
              in
              let open Result.Let_syntax in
              let%bind terminal =
                Agent_session.Captured_agent_runner.Claimed_developer.with_saved_artifact
                  ~active
                  ~parent
                  ~request
                  ~policy
                  ~prepared
                  ~binding
                  ~agent_runtime:fixture.runtime
                  ~max_registrations:16
                  ~checkpoint:ignore
                  ~invocation
                  ~check_current
                  run_native
              in
              match H.status terminal with
              | Resolved (Complete _) -> Ok (succeeded_claim claim)
              | Admitted | Dispatching | Published _
              | Resolved (Fail _ | Pending _ | Cancelled _) ->
                Error (P.Error.invalid_request "outer native callback did not complete"))
       in
       let completed = completed |> ok |> Option.value_exn in
       assert (P.Id.Job.equal id (P.Developer_job.id completed));
       assert (
         match (P.Developer_job.state completed).status with
         | P.Job.Succeeded -> true
         | Failed _
         | Queued
         | Running
         | Waiting_permission _
         | Waiting_completion _
         | Cancelled
         | Interrupted _ -> false);
       assert !rejected;
       if not !descendant_checked
       then
         failwithf
           "nested descendant proof failed (%d calls): %s"
           !source_calls
           (Option.value !descendant_error ~default:"no result")
           ();
       assert (Int.equal !source_calls 2))
;;

let%test_unit "shared scheduler overlaps two physically admitted jobs of one root" =
  let host = ref None in
  let entered = ref 0 in
  let both_entered, resolve_both = Eio.Promise.create () in
  let release_first, resolve_first = Eio.Promise.create () in
  let release_second, resolve_second = Eio.Promise.create () in
  let released_first = ref false in
  let released_second = ref false in
  let unblock () =
    if not !released_first
    then (
      released_first := true;
      Eio.Promise.resolve resolve_first ());
    if not !released_second
    then (
      released_second := true;
      Eio.Promise.resolve resolve_second ())
  in
  with_job_executor
    ~run_name:"dvr_shared_scheduler_overlap"
    ~max_active_workers:2
    ~on_owner:(fun owners capacity -> host := Some (owners, capacity))
    ~make_selected:(fun _ ->
      pair_registry
        ~on_parent:(fun () ->
          Int.incr entered;
          if Int.equal !entered 2 then Eio.Promise.resolve resolve_both ();
          Eio.Promise.await
            (if Int.equal !entered 1 then release_first else release_second))
        (ref 0)
        (ref 0))
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let owners, capacity = Option.value_exn !host in
       let parent = C.find selected ~name:"parent_tool" |> ok in
       let pinned = C.select selected ~names:[ "parent_tool" ] |> ok in
       let request =
         Chat_response.Background_request.tool
           ~capabilities:pinned
           ~reference:(C.reference parent)
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let ids =
         O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let ids =
             List.init 2 ~f:(fun _ ->
               O.Action.stage_background action ~request ~checkpoint:ignore |> ok)
           in
           let selected = O.Action.seal action ~selected:ids |> ok in
           Ok (ids, selected))
         |> ok
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let scheduler =
           Agent_server.Job_scheduler.start
             ~sw
             ~clock:(Eio.Stdenv.clock env)
             ~registry:(Agent_server.Session_registry.create ())
             ~developer_registry:(Some owners)
             ~capacity
             ~model_job_inference:(fun _ _ ->
               Error (P.Error.invalid_request "unused conversation inference"))
         in
         Exn.protect
           ~finally:(fun () ->
             unblock ();
             Agent_server.Job_scheduler.close_and_wait scheduler |> ok)
           ~f:(fun () ->
             let foreign =
               Stdlib.Domain.spawn (fun () ->
                 let joined = Agent_server.Job_scheduler.close_and_wait scheduler in
                 let closed =
                   try
                     Agent_server.Job_scheduler.close scheduler;
                     false
                   with
                   | Invalid_argument _ -> true
                 in
                 joined, closed)
             in
             (match Stdlib.Domain.join foreign with
              | Error { P.Error.code = Invalid_state; _ }, true -> ()
              | _ -> failwith "foreign scheduler shutdown mutated the owner");
             assert (Agent_server.Job_scheduler.is_running scheduler);
             Agent_server.Job_scheduler.attach_developer scheduler ~root ~runner |> ok;
             Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
               Eio.Promise.await both_entered);
             assert (Int.equal !entered 2);
             unblock ();
             List.iter ids ~f:(fun id ->
               let state =
                 await_developer_status ~env ~handle id (function
                   | P.Job.Succeeded -> true
                   | _ -> false)
               in
               assert (Int.equal state.attempt 1));
             assert (Int.equal !entered 2))))
;;

let%test_unit "developer scheduler retirement joins an entered protected handler" =
  let host = ref None in
  let entered, resolve_entered = Eio.Promise.create () in
  let release, resolve_release = Eio.Promise.create () in
  let calls = ref 0 in
  with_job_executor
    ~run_name:"dvr_scheduler_retire_join"
    ~on_owner:(fun owners capacity -> host := Some (owners, capacity))
    ~make_selected:(fun _ ->
      pair_registry
        ~on_parent:(fun () ->
          Int.incr calls;
          Eio.Promise.resolve resolve_entered ();
          Eio.Cancel.protect (fun () -> Eio.Promise.await release))
        (ref 0)
        (ref 0))
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let owners, capacity = Option.value_exn !host in
       let parent = C.find selected ~name:"parent_tool" |> ok in
       let pinned = C.select selected ~names:[ "parent_tool" ] |> ok in
       let request =
         Chat_response.Background_request.tool
           ~capabilities:pinned
           ~reference:(C.reference parent)
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let id =
         O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           let selected = O.Action.seal action ~selected:[ id ] |> ok in
           Ok (id, selected))
         |> ok
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let scheduler =
           Agent_server.Job_scheduler.start
             ~sw
             ~clock:(Eio.Stdenv.clock env)
             ~registry:(Agent_server.Session_registry.create ())
             ~developer_registry:(Some owners)
             ~capacity
             ~model_job_inference:(fun _ _ ->
               Error (P.Error.invalid_request "unused conversation inference"))
         in
         Exn.protect
           ~finally:(fun () ->
             if not (Eio.Promise.is_resolved release)
             then Eio.Promise.resolve resolve_release ();
             Agent_server.Job_scheduler.close_and_wait scheduler |> ok)
           ~f:(fun () ->
             Agent_server.Job_scheduler.attach_developer scheduler ~root ~runner |> ok;
             Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
               Eio.Promise.await entered);
             Eio.Fiber.fork ~sw (fun () ->
               Eio.Fiber.yield ();
               Eio.Promise.resolve resolve_release ());
             Agent_server.Job_scheduler.retire_developer scheduler ~root |> ok;
             assert (Eio.Promise.is_resolved release);
             assert (Int.equal !calls 1);
             let state =
               R.find_job (R.observe handle |> ok) id
               |> Option.value_exn
               |> P.Developer_job.state
             in
             assert (Int.equal state.attempt 1))))
;;

let%test_unit "claimed Tool target uses one physical native and result publisher" =
  List.iter
    [ Agent_store.Developer_result_publisher.Storage.Automatic; Artifact ]
    ~f:(fun storage ->
      let parent_calls = ref 0 in
      let child_calls = ref 0 in
      with_job_executor
        ~run_name:
          (match storage with
           | Automatic -> "dvr_claimed_tool_inline"
           | Artifact -> "dvr_claimed_tool_artifact")
        ~make_selected:(fun _ -> pair_registry ~on_parent:ignore parent_calls child_calls)
        (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
           let parent = C.find selected ~name:"parent_tool" |> ok in
           let pinned = C.select selected ~names:[ "parent_tool" ] |> ok in
           let request =
             Chat_response.Background_request.tool
               ~capabilities:pinned
               ~reference:(C.reference parent)
               ~input:(`Object [])
               ~policy:Chat_response.One_off_request.default_policy
             |> ok
           in
           let selected_id =
             O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
               let id =
                 O.Action.stage_background action ~request ~checkpoint:ignore |> ok
               in
               let selected = O.Action.seal action ~selected:[ id ] |> ok in
               Ok (id, selected))
             |> ok
           in
           Eio.Switch.run (fun sw ->
             let runner =
               background_runner
                 ~sw
                 ~root
                 ~store
                 ~blobs
                 ~env
                 ~managed_definition:None
                 ~jobs
                 ~storage
             in
             let candidates =
               O.Job_candidate.read_current root ~checkpoint:ignore |> ok
             in
             assert (Int.equal (List.length candidates) 1);
             let candidate = List.hd_exn candidates in
             assert (
               P.Id.Job.equal
                 selected_id
                 (P.Developer_job.id (O.Job_candidate.job candidate)));
             let available =
               Agent_server.Developer_background_job_runner.availability runner candidate
               |> ok
             in
             assert (
               Agent_server.Developer_background_job_runner.Availability.equal
                 available
                 Runnable);
             let completed =
               Agent_server.Developer_background_job_runner.run_candidate runner candidate
               |> ok
               |> Option.value_exn
             in
             assert (Int.equal !parent_calls 1);
             assert (Int.equal !child_calls 0);
             assert (P.Id.Job.equal selected_id (P.Developer_job.id completed));
             assert (
               match (P.Developer_job.state completed).status with
               | P.Job.Succeeded -> true
               | _ -> false);
             assert (
               match storage, (P.Developer_job.state completed).result with
               | Automatic, Some (Inline (P.Completion.Succeeded (`String "parent"))) ->
                 true
               | Artifact, Some (Artifact { outcome = P.Stored_completion.Succeeded; _ })
                 -> true
               | _ -> false);
             let rejected_id =
               O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
                 let id =
                   O.Action.stage_background action ~request ~checkpoint:ignore |> ok
                 in
                 let selected = O.Action.seal action ~selected:[ id ] |> ok in
                 Ok (id, selected))
               |> ok
             in
             let candidate =
               O.Job_candidate.read_current root ~checkpoint:ignore
               |> ok
               |> List.find_exn ~f:(fun candidate ->
                 P.Id.Job.equal
                   rejected_id
                   (P.Developer_job.id (O.Job_candidate.job candidate)))
             in
             let decisions = ref 0 in
             assert (
               Result.is_error
                 (Agent_server.Developer_background_job_runner.run_candidate
                    runner
                    ~on_capacity_decision:(function
                      | Agent_server.Developer_job_worker.Capacity_decision.Acquired ->
                        Int.incr decisions;
                        O.narrow_selection root ~names:[] |> ok
                      | Unavailable | Rejected ->
                        failwith "postcapacity test did not acquire the shared slot")
                    candidate));
             assert (Int.equal !decisions 1);
             assert (Int.equal !parent_calls 1);
             let unchanged =
               R.find_job (R.observe handle |> ok) rejected_id |> Option.value_exn
             in
             assert (Int.equal (P.Developer_job.state unchanged).attempt 0);
             assert (
               match (P.Developer_job.state unchanged).status with
               | P.Job.Queued -> true
               | Running
               | Waiting_permission _
               | Waiting_completion _
               | Succeeded
               | Failed _
               | Cancelled
               | Interrupted _ -> false))))
;;

let%test_unit "claimed Script executes selected native and retains its lexical child" =
  let calls = ref 0 in
  with_job_executor
    ~run_name:"dvr_claimed_script_native_child"
    ~max_job_payload_bytes:4096
    ~make_selected:(fun _ -> registry ~custom:false calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let source =
         {|let main input =
  Task.bind(
    Task.catch(
      Task.bind(Job.start_tool("read_file", input), fun discarded -> Task.fail("discard")),
      fun ignored -> Task.pure(`Null)),
    fun ignored -> Task.bind(Tool.call("read_file", input), fun result ->
      match result with
      | `Ok(value) -> Task.bind(Job.start_tool("read_file", input), fun id ->
          match value with
          | `String(text) -> Task.pure(`String(id ++ ":" ++ text))
          | _ -> Task.fail("expected native text"))
      | `Error(message) -> Task.fail(message)))|}
       in
       let parent_id =
         stage_root_job
           ~tools:[ "read_file" ]
           ~source
           ~input:(`Object [])
           env
           root
           selected
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         assert (Agent_server.Developer_background_job_runner.issued_by_root runner root);
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         assert (
           Agent_server.Developer_background_job_runner.Availability.equal
             (Agent_server.Developer_background_job_runner.availability runner candidate
              |> ok)
             Runnable);
         let completed =
           Agent_server.Developer_background_job_runner.run_candidate runner candidate
           |> Result.map_error ~f:(fun error -> error.P.Error.message)
           |> Result.ok_or_failwith
           |> Option.value_exn
         in
         assert (Int.equal !calls 1);
         assert (P.Id.Job.equal (P.Developer_job.id completed) parent_id);
         let child_id =
           match (P.Developer_job.state completed).result with
           | Some (Inline (P.Completion.Succeeded (`String text))) ->
             let id, native = String.lsplit2_exn text ~on:':' in
             assert (String.equal native "private result");
             P.Id.Job.of_string id |> ok
           | _ -> failwith "claimed Script did not return selected native result"
         in
         let child = R.find_job (R.observe handle |> ok) child_id |> Option.value_exn in
         let queued_ids =
           O.Job_candidate.read_current root ~checkpoint:ignore
           |> ok
           |> List.map ~f:(fun candidate ->
             P.Developer_job.id (O.Job_candidate.job candidate))
         in
         assert (List.equal P.Id.Job.equal queued_ids [ child_id ]);
         let launch = P.Developer_job.launch child in
         (match P.Developer_job.Launch.parent_job launch with
          | Some (id, attempt) when P.Id.Job.equal id parent_id && Int.equal attempt 1 ->
            ()
          | None | Some _ -> failwith "Script child lost claimed parent attempt");
         assert (Int.equal (P.Developer_job.Launch.nested_depth launch) 1)))
;;

let%test_unit "claimed Script cannot catch a physical admission error into another call" =
  let calls = ref 0 in
  let clock = ref (T.now ()) in
  with_job_executor
    ~run_name:"dvr_claimed_script_fatal_admission"
    ~max_job_payload_bytes:4096
    ~now:(fun () -> !clock)
    ~make_selected:(fun _ -> registry ~custom:false calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let parent_id =
         stage_root_job
           ~tools:[ "read_file" ]
           ~source:
             {|let main input = Task.bind(
  Task.catch(Tool.call("not_selected", input), fun ignored -> Tool.call("read_file", input)),
  fun ignored -> Task.pure(input))|}
           ~input:(`Object [])
           env
           root
           selected
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         assert (
           Result.is_error
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate));
         assert (Int.equal !calls 0);
         let persisted =
           R.find_job (R.observe handle |> ok) parent_id |> Option.value_exn
         in
         assert (
           match (P.Developer_job.state persisted).status with
           | P.Job.Failed _ -> true
           | _ -> false);
         let tiny_policy =
           { Chat_response.One_off_request.default_policy with max_output_bytes = 1 }
         in
         let expired_id = stage_root_job ~policy:tiny_policy env root selected in
         clock := P.Timestamp.of_string "2026-10-14T00:00:00Z" |> ok;
         let expired_candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         (match
            Agent_server.Developer_background_job_runner.run_candidate
              runner
              expired_candidate
          with
          | Error error ->
            assert (P.Error.equal_code error.code Resource_limit);
            assert (
              String.equal
                error.message
                "Script completion exceeds its saved output limit")
          | Ok _ -> failwith "undersized expired Script completion was accepted");
         assert (Int.equal !calls 0);
         let expired =
           R.find_job (R.observe handle |> ok) expired_id |> Option.value_exn
         in
         assert (
           match (P.Developer_job.state expired).status with
           | P.Job.Failed _ -> true
           | _ -> false)))
;;

let%test_unit "malformed saved Async_tool enters one physical Failed attempt" =
  let calls = ref 0 in
  let host = ref None in
  with_job_executor
    ~run_name:"dvr_claimed_malformed_async"
    ~on_owner:(fun owners capacity -> host := Some (owners, capacity))
    ~make_selected:(fun _ -> registry ~custom:false calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected:_ ~jobs ~store ~blobs ->
       let owners, capacity = Option.value_exn !host in
       let capture_fingerprint = Chatml_developer_capture.fingerprint (O.capture root) in
       let id = Selected.job_id "job_claimed_malformed_async" in
       let original =
         Selected.job
           (O.owner root)
           capture_fingerprint
           ~id
           ~parent:P.Developer_job.Launch.Run_root
       in
       let malformed =
         P.Developer_job.create
           ~owner:(O.owner root)
           ~capture_fingerprint
           ~id
           ~launch:(P.Developer_job.launch original)
           ~kind:P.Job.Async_tool
           ~payload:(`Object [ "not_a_background_request", `True ])
           ~retry_policy:P.Job.Never
           ~created_at:(P.Developer_job.created_at original)
           ~state:(P.Developer_job.state original)
         |> ok
       in
       let unsupported_id = Selected.job_id "job_claimed_unsupported_kind" in
       let unsupported =
         P.Developer_job.create
           ~owner:(O.owner root)
           ~capture_fingerprint
           ~id:unsupported_id
           ~launch:(P.Developer_job.launch original)
           ~kind:P.Job.Shell_process
           ~payload:(`Object [])
           ~retry_policy:P.Job.Never
           ~created_at:(P.Developer_job.created_at original)
           ~state:(P.Developer_job.state original)
         |> ok
       in
       let prepared =
         R.prepare_root_selected
           handle
           ~expected:(R.observe handle |> ok)
           ~phase:R.Checkpoint.Active
           ~selected:[ malformed; unsupported ]
         |> ok
       in
       (match
          R.commit_and_settle
            handle
            ~admit:(fun () -> true)
            prepared
            (R.Handoff.create ~settle:ignore)
          |> ok
        with
        | R.Settlement.Installed _ -> ()
        | Absent | Prewrite_conflict | Diverged ->
          failwith "malformed queued fixture was not installed");
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         assert (
           Agent_server.Developer_background_job_runner.Availability.equal
             (Agent_server.Developer_background_job_runner.availability runner candidate
              |> ok)
             Malformed_supported);
         let scheduler =
           Agent_server.Job_scheduler.start
             ~sw
             ~clock:(Eio.Stdenv.clock env)
             ~registry:(Agent_server.Session_registry.create ())
             ~developer_registry:(Some owners)
             ~capacity
             ~model_job_inference:(fun _ _ ->
               Error (P.Error.invalid_request "unused conversation inference"))
         in
         Exn.protect
           ~finally:(fun () -> Agent_server.Job_scheduler.close_and_wait scheduler |> ok)
           ~f:(fun () ->
             Agent_server.Job_scheduler.attach_developer scheduler ~root ~runner |> ok;
             let state =
               await_developer_status ~env ~handle id (function
                 | P.Job.Failed _ -> true
                 | _ -> false)
             in
             assert (Int.equal state.attempt 1));
         assert (Int.equal !calls 0);
         let persisted = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
         assert (Int.equal (P.Developer_job.state persisted).attempt 1);
         assert (
           match (P.Developer_job.state persisted).status with
           | P.Job.Failed _ -> true
           | _ -> false);
         let unsupported =
           R.find_job (R.observe handle |> ok) unsupported_id |> Option.value_exn
         in
         assert (Int.equal (P.Developer_job.state unsupported).attempt 0);
         assert (
           match (P.Developer_job.state unsupported).status with
           | P.Job.Queued -> true
           | _ -> false)))
;;

let%test_unit "claimed Tool keeps selected Artifact after uncertain terminal save" =
  let calls = ref 0 in
  let tracking = ref false in
  let writes = ref 0 in
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  with_job_executor
    ~run_name:"dvr_claimed_tool_uncertain_artifact"
    ~make_selected:(fun _ -> registry ~custom:false calls)
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~before_rename:(fun () ->
          if !tracking
          then (
            Int.incr writes;
            if Int.equal !writes 4 then rename_armed := true))
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let binding = C.find selected ~name:"read_file" |> ok in
       let request =
         Chat_response.Background_request.tool
           ~capabilities:selected
           ~reference:(C.reference binding)
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let id =
         O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           let selection = O.Action.seal action ~selected:[ id ] |> ok in
           Ok (id, selection))
         |> ok
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Artifact
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         tracking := true;
         assert (
           Result.is_error
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate));
         tracking := false;
         assert (Int.equal !writes 4);
         assert (not !rename_armed);
         assert (Int.equal !calls 1);
         sync_blocked := false;
         let persisted = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
         assert (
           match (P.Developer_job.state persisted).result with
           | Some (Artifact { outcome = P.Stored_completion.Succeeded; _ }) -> true
           | None | Some (Inline _) | Some (Artifact _) -> false);
         (match
            Agent_server.Developer_background_job_runner.run_candidate runner candidate
          with
          | Ok None -> ()
          | Ok (Some _) | Error _ ->
            failwith "stale candidate restarted an uncertain claimed Tool");
         assert (Int.equal !calls 1)))
;;

let%test_unit "claimed Script does not replay native effect after uncertain result save" =
  let calls = ref 0 in
  let tracking = ref false in
  let writes = ref 0 in
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  with_job_executor
    ~run_name:"dvr_claimed_script_uncertain_artifact"
    ~max_job_payload_bytes:4096
    ~make_selected:(fun _ -> registry ~custom:false calls)
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~before_rename:(fun () ->
          if !tracking
          then (
            Int.incr writes;
            if Int.equal !writes 4 then rename_armed := true))
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let id =
         stage_root_job
           ~tools:[ "read_file" ]
           ~source:
             {|let main input = Task.bind(Tool.call("read_file", input), fun result ->
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(message) -> Task.fail(message))|}
           ~input:(`Object [])
           env
           root
           selected
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:None
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Artifact
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         tracking := true;
         assert (
           Result.is_error
             (Agent_server.Developer_background_job_runner.run_candidate runner candidate));
         tracking := false;
         assert (Int.equal !calls 1);
         assert (not !rename_armed);
         sync_blocked := false;
         let persisted = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
         assert (
           match (P.Developer_job.state persisted).result with
           | Some (Artifact { outcome = P.Stored_completion.Succeeded; _ }) -> true
           | None | Some (Inline _) | Some (Artifact _) -> false);
         (match
            Agent_server.Developer_background_job_runner.run_candidate runner candidate
          with
          | Ok None -> ()
          | Ok (Some _) | Error _ ->
            failwith "stale candidate replayed an uncertain claimed Script");
         assert (Int.equal !calls 1)))
;;

let%test_unit "developer root Job scope selects only the returned physical start" =
  let calls = ref 0 in
  let data_root_path = ref None in
  let after_actor_read = ref ignore in
  let swap_during_read = ref false in
  let swapped = ref false in
  let other_principal =
    let id = P.Id.Principal.of_string "pri_other_developer_job_reader" |> ok in
    P.Principal.create
      ~id
      ~authentication_kind:"local.test"
      ~scopes:P.Scope.Set.empty
      ~attributes:[]
    |> ok
  in
  let foreign_actor =
    Operator_authorization.guarded ~principal:other_principal ~is_current:(fun () -> true)
  in
  with_job_executor
    ~on_store:(fun _ session _ ->
      data_root_path
      := Some (Agent_store.Data_root.path (Agent_store.Session_store.data_root session)))
    ~checkpoint:(fun () -> !after_actor_read ())
    ~current_actor:(fun root ->
      if !swap_during_read
      then
        if !swapped
        then foreign_actor
        else (
          (after_actor_read := fun () -> swapped := true);
          O.original_actor root)
      else O.original_actor root)
    ~run_name:"dvr_script_root_job"
    ~make_selected:(fun _ -> registry ~custom:false calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected:_ ~jobs ~store:_ ~blobs:_ ->
       let module Runtime = Chatml_host_runtime in
       let module Value = Chatml.Chatml_value_codec in
       let persisted_job_ids () =
         let reader =
           Agent_store.Retention_reader.create
             ~env
             ~root:(Option.value_exn !data_root_path)
             ~max_entries:64
             ~max_bytes:65536
           |> ok
         in
         R.Result_root.rows_for_inventory
           (R.Result_root.of_handle handle |> ok)
           ~reader
           ~checkpoint:ignore
         |> ok
         |> R.Result_root.Inventory_rows.jobs
         |> List.map ~f:P.Developer_job.id
         |> List.sort ~compare:P.Id.Job.compare
       in
       let effects = ref [] in
       let compiled =
         Runtime.compile_script
           ~surface:Chatml.Chatml_extension_surface.one_off_v1
           ~source:
             {|let main input =
  Task.bind(Task.catch(
    Task.bind(Job.start_tool("read_file", input), fun discarded -> Task.fail("discard")),
    fun ignored -> Task.pure("caught")), fun ignored ->
  Task.bind(Job.start_tool("read_file", input), fun id -> Task.pure(id)))|}
           ()
         |> Result.ok_or_failwith
       in
       let config : Runtime.runtime_config =
         { surface = Chatml.Chatml_extension_surface.one_off_v1
         ; operations = Runtime.default_operations ~handlers:Runtime.default_handlers ()
         }
       in
       let value, ordinary =
         Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
           let config = Jobs.Scope.install scope config in
           let value =
             Runtime.run_entrypoint
               ~prepare_result:(fun ~value:_ ~local_effects ->
                 effects := local_effects;
                 Ok ignore)
               config
               compiled
               ~entrypoint:"main"
               ~arguments:[ Value.import_json (`Object []) ]
               ()
             |> Result.map_error ~f:P.Error.invalid_request
           in
           Result.map value ~f:(fun value -> value, !effects))
         |> Result.map_error ~f:(fun error -> error.P.Error.message)
         |> Result.ok_or_failwith
       in
       assert (List.is_empty ordinary);
       let ids, _ =
         Chat_response.Background_job_operations.split_starts !effects
         |> Result.ok_or_failwith
       in
       let id = List.hd_exn ids in
       assert (
         match value with
         | Chatml.Chatml_lang.VString returned ->
           String.equal returned (P.Id.Job.to_string id)
         | _ -> false);
       let job = R.find_job (R.observe handle |> ok) id |> Option.value_exn in
       assert (
         P.Execution_owner.equal_developer (P.Developer_job.owner job) (O.owner root));
       assert (Int.equal !calls 0);
       let script_compiled =
         Runtime.compile_script
           ~surface:Chatml.Chatml_extension_surface.one_off_v1
           ~source:
             {|let main input = Job.start_script(`Object([
  {key="source"; value=`String("let main input = Task.pure(input)")},
  {key="tools"; value=`Array([])},
  {key="input"; value=`Null}
]))|}
           ()
         |> Result.ok_or_failwith
       in
       let script_effects = ref [] in
       ignore
         (Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
            let config = Jobs.Scope.install scope config in
            Runtime.run_entrypoint
              ~prepare_result:(fun ~value:_ ~local_effects ->
                script_effects := local_effects;
                Ok ignore)
              config
              script_compiled
              ~entrypoint:"main"
              ~arguments:[ Value.import_json `Null ]
              ()
            |> Result.map_error ~f:P.Error.invalid_request
            |> Result.map ~f:(fun value -> value, !script_effects))
          |> Result.map_error ~f:(fun error -> error.P.Error.message)
          |> Result.ok_or_failwith
          : Chatml.Chatml_lang.value * Chatml.Chatml_lang.eff list);
       let script_ids, _ =
         Chat_response.Background_job_operations.split_starts !script_effects
         |> Result.ok_or_failwith
       in
       assert (Int.equal (List.length script_ids) 1);
       assert (
         Option.is_some (R.find_job (R.observe handle |> ok) (List.hd_exn script_ids)));
       let run_job_query source =
         let source =
           String.substr_replace_all
             source
             ~pattern:"JOB_ID"
             ~with_:(P.Id.Job.to_string id)
         in
         let compiled =
           Runtime.compile_script
             ~surface:Chatml.Chatml_extension_surface.one_off_v1
             ~source
             ()
           |> Result.ok_or_failwith
         in
         let value, ordinary =
           Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
             let config = Jobs.Scope.install scope config in
             let effects = ref [] in
             let value =
               Runtime.run_entrypoint
                 ~prepare_result:(fun ~value:_ ~local_effects ->
                   effects := local_effects;
                   Ok ignore)
                 config
                 compiled
                 ~entrypoint:"main"
                 ~arguments:[ Value.import_json (`Object []) ]
                 ()
               |> Result.map_error ~f:P.Error.invalid_request
             in
             Result.map value ~f:(fun value -> value, !effects))
           |> Result.map_error ~f:(fun error -> error.P.Error.message)
           |> Result.ok_or_failwith
         in
         assert (List.is_empty ordinary);
         Value.export_json value |> Result.ok_or_failwith
       in
       let pending = run_job_query {|let main input = Job.read_result("JOB_ID")|} in
       assert (
         match pending with
         | `Null -> true
         | _ -> false);
       let before = run_job_query {|let main input = Job.get("JOB_ID")|} in
       assert (
         match before with
         | `Object fields ->
           (match
              ( List.Assoc.find fields "status" ~equal:String.equal
              , List.Assoc.find fields "completion" ~equal:String.equal
              , List.Assoc.find fields "completion_kind" ~equal:String.equal )
            with
            | Some (`String "queued"), Some `Null, Some `Null -> true
            | _ -> false)
         | _ -> false);
       ignore
         (run_job_query
            {|let main input = Task.bind(Job.cancel("JOB_ID"), fun ignored -> Task.pure(`Null))|}
          : Jsonaf.t);
       let after = run_job_query {|let main input = Job.get("JOB_ID")|} in
       assert (
         match after with
         | `Object fields ->
           List.Assoc.find fields "status" ~equal:String.equal
           |> Option.value_map ~default:false ~f:(function
             | `String value -> String.equal value "cancelled"
             | _ -> false)
         | _ -> false);
       let model_compiled =
         Runtime.compile_script
           ~surface:Chatml.Chatml_extension_surface.developer_tool_v1
           ~source:
             {|let run ctx input = Task.bind(Model.spawn("agent_prompt_v1", input), fun id ->
  Task.pure(`Complete(input)))|}
           ()
         |> Result.ok_or_failwith
       in
       let before_model = persisted_job_ids () in
       let unavailable =
         Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
           let model_config : Runtime.runtime_config =
             { surface = Chatml.Chatml_extension_surface.developer_tool_v1
             ; operations = Runtime.default_operations ()
             }
           in
           let model_config = Jobs.Scope.install scope model_config in
           Runtime.run_entrypoint
             ~scoped_async_operation:Runtime.Model_spawn
             model_config
             model_compiled
             ~entrypoint:"run"
             ~arguments:
               [ Value.import_json `Null
               ; Value.import_json
                   (`Object [ "prompt", `String "p"; "input", `String "i" ])
               ]
             ()
           |> Result.map_error ~f:P.Error.invalid_request
           |> Result.map ~f:(fun value -> value, []))
       in
       assert (Result.is_error unavailable);
       assert (List.equal P.Id.Job.equal before_model (persisted_job_ids ()));
       swap_during_read := true;
       let denied =
         Or_error.try_with (fun () ->
           run_job_query {|let main input = Job.read_result("JOB_ID")|})
       in
       assert !swapped;
       assert (
         match denied with
         | Error error ->
           String.is_substring
             (Error.to_string_hum error)
             ~substring:"developer result is unauthorized"
         | Ok _ -> false))
;;

let managed_job_registry
      ?(tool_contract = Chatml_compilation.Tool_contract.Standard)
      ?(completion_schema = false)
      ?base_registry
      ?(source =
        "let run ctx input = Task.bind(Job.start_tool(\"read_file\", input), fun id ->\n\
        \  Task.pure(`Complete(input)))")
      env
      calls
  =
  let module M = Chat_response.Managed_tool_registry in
  let base =
    Option.value_or_thunk base_registry ~default:(fun () -> registry ~custom:false calls)
  in
  let dir = Eio.Stdenv.cwd env in
  let source_loader =
    Source_loader.captured_filesystem
      ~root:dir
      ~sources:
        [ "input.json", {|{"type":"object"}|}
        ; "output.json", {|{"type":"object"}|}
        ; "completion.json", {|{"type":"string"}|}
        ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir
      ~source_loader
      (Printf.sprintf
         {|<tool name="read_file"/>
<script id="owned" language="chatml" kind="tool">
%s
</script>
<tool name="summary" type="chatml" script="owned" entrypoint="run" input_schema="input.json" output_schema="output.json"%s><uses tool="read_file"/></tool>|}
         source
         (if completion_schema then {| completion_schema="completion.json"|} else ""))
  in
  let managed =
    M.prepare
      ~env
      ~owner:"developer-managed-jobs"
      ~capabilities:base
      ~tool_contract
      elements
    |> Result.map_error ~f:(fun errors ->
      List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> String.concat ~sep:"; ")
    |> Result.ok_or_failwith
  in
  managed, M.capabilities managed
;;

let%test_unit "claimed managed Pending waits on selected owned Job and captured schema" =
  let calls = ref 0 in
  let prepared = ref None in
  with_job_executor
    ~run_name:"dvr_claimed_managed_pending"
    ~make_selected:(fun env ->
      let managed, selected =
        managed_job_registry
          ~completion_schema:true
          ~source:
            {|let run ctx input = Task.bind(Job.start_tool("read_file", input), fun id ->
  Task.pure(`Pending(`Job(id), input)))|}
          env
          calls
      in
      prepared := Some managed;
      selected)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store ~blobs ->
       let module M = Chat_response.Managed_tool_registry in
       let managed = Option.value_exn !prepared in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let request =
         Chat_response.Background_request.tool
           ~capabilities:selected
           ~reference
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let id =
         O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           let selected = O.Action.seal action ~selected:[ id ] |> ok in
           Ok (id, selected))
         |> ok
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:(Some managed)
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         let completed =
           Agent_server.Developer_background_job_runner.run_candidate runner candidate
           |> ok
           |> Option.value_exn
         in
         assert (P.Id.Job.equal id (P.Developer_job.id completed));
         assert (Int.equal !calls 0);
         let child_id =
           O.Job_candidate.read_current root ~checkpoint:ignore
           |> ok
           |> List.find_exn ~f:(fun candidate ->
             not (P.Id.Job.equal id (P.Developer_job.id (O.Job_candidate.job candidate))))
           |> O.Job_candidate.job
           |> P.Developer_job.id
         in
         assert (Option.is_some (R.find_job (R.observe handle |> ok) child_id));
         assert (
           match (P.Developer_job.state completed).status with
           | P.Job.Waiting_completion dependency ->
             (match dependency.work with
              | I.Job waited ->
                P.Id.Job.equal waited child_id
                && Jsonaf.exactly_equal
                     (Option.value_exn dependency.completion_schema)
                     (`Object [ "type", `String "string" ])
              | Subscription _ -> false)
           | _ -> false)))
;;

let%test_unit "claimed managed Job.start keeps the request's pinned tool subset" =
  let read_calls = ref 0 in
  let extra_calls = ref 0 in
  let prepared = ref None in
  let module Extra = struct
    type input = string

    let name = "child_tool"
    let description = Some "unrelated current root tool"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let extra =
    Ochat_function.create_function
      (module Extra)
      (fun _ ->
         Int.incr extra_calls;
         Openai.Responses.Tool_output.Output.Text "extra")
  in
  with_job_executor
    ~run_name:"dvr_claimed_managed_subset"
    ~make_selected:(fun env ->
      let base =
        registry
          ~unrelated:(Chatmd_shell_spec.Source_ref.digest "unrelated-v1", extra)
          ~custom:false
          read_calls
      in
      let managed, selected =
        managed_job_registry
          ~base_registry:base
          ~source:
            {|let run ctx input =
  Task.bind(Task.catch(Job.start_tool("child_tool", input), fun ignored -> Task.pure("denied")),
    fun ignored -> Task.bind(Job.start_tool("read_file", input), fun id ->
      Task.pure(`Pending(`Job(id), input))))|}
          env
          read_calls
      in
      prepared := Some managed;
      selected)
    (fun ~env ~root ~executor:_ ~handle:_ ~owner:_ ~selected ~jobs ~store ~blobs ->
       let managed = Option.value_exn !prepared in
       let summary = C.find selected ~name:"summary" |> ok in
       let pinned = C.select selected ~names:[ "summary"; "read_file" ] |> ok in
       assert (Result.is_ok (C.find selected ~name:"child_tool"));
       let request =
         Chat_response.Background_request.tool
           ~capabilities:pinned
           ~reference:(C.reference summary)
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let id =
         O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
           let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
           let selection = O.Action.seal action ~selected:[ id ] |> ok in
           Ok (id, selection))
         |> ok
       in
       Eio.Switch.run (fun sw ->
         let runner =
           background_runner
             ~sw
             ~root
             ~store
             ~blobs
             ~env
             ~managed_definition:(Some managed)
             ~jobs
             ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
         in
         let candidate =
           O.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
         in
         let completed =
           Agent_server.Developer_background_job_runner.run_candidate runner candidate
           |> ok
           |> Option.value_exn
         in
         let children = O.Job_candidate.read_current root ~checkpoint:ignore |> ok in
         assert (P.Id.Job.equal id (P.Developer_job.id completed));
         assert (Int.equal !extra_calls 0);
         assert (Int.equal !read_calls 0);
         assert (Int.equal (List.length children) 1);
         let child_id = P.Developer_job.id (O.Job_candidate.job (List.hd_exn children)) in
         assert (
           match (P.Developer_job.state completed).status with
           | P.Job.Waiting_completion { work = I.Job actual; _ } ->
             P.Id.Job.equal actual child_id
           | _ -> false)))
;;

let%test_unit
    "selected managed ChatML installs Job operations only in its live native scope"
  =
  let calls = ref 0 in
  let prepared = ref None in
  with_job_executor
    ~run_name:"dvr_script_managed_job"
    ~make_selected:(fun env ->
      let managed, selected =
        managed_job_registry
          ~source:
            {|let run ctx input = Task.bind(Tool.call("read_file", input), fun response ->
  match response with
  | `Ok(_) -> Task.bind(Job.start_tool("read_file", input), fun id ->
      Task.pure(`Complete(input)))
  | `Error(_) -> Task.fail("nested native failed"))|}
          env
          calls
      in
      prepared := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle ~owner ~selected ~jobs:_ ~store:_ ~blobs:_ ->
       let module M = Chat_response.Managed_tool_registry in
       let managed = Option.value_exn !prepared in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted =
         H.create_developer
           { (H.context (invocation owner selected reference ~custom:false)) with
             implementation_revision = M.execution_revision managed binding |> ok
           }
         |> ok
       in
       let executor = X.with_managed_definition executor ~definition:managed ~env |> ok in
       let terminal =
         X.run_root
           executor
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(function
             | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
             | _ -> Error (P.Error.invalid_request "unexpected output"))
         |> ok
       in
       assert (I.equal_status (H.status terminal) (Resolved (Complete (`Object []))));
       let candidates = O.Job_candidate.read_current root ~checkpoint:ignore |> ok in
       assert (Int.equal (List.length candidates) 1);
       let child = O.Job_candidate.job (List.hd_exn candidates) in
       assert (
         Option.is_some (R.find_job (R.observe handle |> ok) (P.Developer_job.id child)));
       assert (Int.equal !calls 1);
       assert (Result.is_error (Agent_session.Native_tool_invocation.borrow_selected ()));
       let next =
         H.create_developer { (H.context admitted) with id = P.Id.Invocation.create () }
         |> ok
       in
       let second =
         X.run_root
           executor
           ~reference
           ~invocation:next
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(function
             | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
             | _ -> Error (P.Error.invalid_request "unexpected output"))
         |> ok
       in
       assert (I.equal_status (H.status second) (Resolved (Complete (`Object []))));
       assert (Int.equal !calls 2);
       let after = O.Job_candidate.read_current root ~checkpoint:ignore |> ok in
       assert (Int.equal (List.length after) 2);
       assert (
         List.exists after ~f:(fun candidate ->
           P.Id.Job.equal
             (P.Developer_job.id child)
             (P.Developer_job.id (O.Job_candidate.job candidate))));
       assert (
         Int.equal
           (List.dedup_and_sort
              ~compare:P.Id.Job.compare
              (List.map after ~f:(fun candidate ->
                 P.Developer_job.id (O.Job_candidate.job candidate)))
            |> List.length)
           2))
;;

let%test_unit "managed ABI reports effective host limits and enforces output cap" =
  let module M = Chat_response.Managed_tool_registry in
  let managed_definition = ref None in
  let default = Chat_response.One_off_request.default_policy in
  let job_policy =
    { default with
      execution =
        { default.execution with
          fuel = 1234
        ; max_calls = 3
        ; max_invocation_depth = 2
        ; max_value_bytes = 4096
        }
    ; max_output_bytes = 2048
    }
  in
  with_job_executor
    ~job_policy
    ~run_name:"dvr_managed_effective_abi"
    ~make_selected:(fun env ->
      let managed, selected =
        managed_job_registry
          ~source:
            {|let run ctx input = Task.pure(`Complete(`Object([
  {key="limits"; value=`Object([
    {key="fuel"; value=`String(to_string(ctx.limits.fuel))},
    {key="max_nested_calls"; value=`String(to_string(ctx.limits.max_nested_calls))},
    {key="max_invocation_depth"; value=`String(to_string(ctx.limits.max_invocation_depth))},
    {key="max_output_bytes"; value=`String(to_string(ctx.limits.max_output_bytes))}])},
  {key="input"; value=input}])))|}
          env
          (ref 0)
      in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root:_ ~executor ~handle:_ ~owner ~selected ~jobs:_ ~store:_ ~blobs:_ ->
       let managed = Option.value_exn !managed_definition in
       let executor = X.with_managed_definition executor ~definition:managed ~env |> ok in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let run input =
         let invocation = invocation owner selected reference ~custom:false in
         let invocation =
           H.create_developer
             { (H.context invocation) with
               implementation_revision = M.execution_revision managed binding |> ok
             ; input
             }
           |> ok
         in
         X.run_root
           executor
           ~reference
           ~invocation
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(function
             | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
             | _ -> Error (P.Error.invalid_request "unexpected managed output"))
         |> ok
       in
       let limit_field name = function
         | `Object fields -> List.Assoc.find fields name ~equal:String.equal
         | _ -> None
       in
       let small = run (`Object []) in
       let limits =
         match H.status small with
         | Resolved (Complete value) -> limit_field "limits" value |> Option.value_exn
         | status ->
           failwith
             ("effective ABI did not complete: "
              ^ Sexp.to_string_hum (I.sexp_of_status status))
       in
       assert (
         Jsonaf.exactly_equal
           (Option.value_exn (limit_field "fuel" limits))
           (`String "1234"));
       assert (
         Jsonaf.exactly_equal
           (Option.value_exn (limit_field "max_nested_calls" limits))
           (`String "3"));
       assert (
         Jsonaf.exactly_equal
           (Option.value_exn (limit_field "max_invocation_depth" limits))
           (`String "2"));
       assert (
         Jsonaf.exactly_equal
           (Option.value_exn (limit_field "max_output_bytes" limits))
           (`String "2048"));
       let large = run (`Object [ "payload", `String (String.make 3000 'x') ]) in
       assert (
         match H.status large with
         | Resolved (Fail _) -> true
         | _ -> false))
;;

let%test_unit "fatal managed child admission stops later native and Job effects" =
  let module M = Chat_response.Managed_tool_registry in
  let read_calls = ref 0 in
  let foreign_calls = ref 0 in
  let prepared = ref None in
  let module Foreign = struct
    type input = string

    let name = "foreign_tool"
    let description = Some "not in the managed declaration's dependency ceiling"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let foreign =
    Ochat_function.create_function
      (module Foreign)
      (fun _ ->
         Int.incr foreign_calls;
         Openai.Responses.Tool_output.Output.Text "foreign")
  in
  with_job_executor
    ~run_name:"dvr_managed_fatal_child"
    ~make_selected:(fun env ->
      let base =
        registry
          ~unrelated:(Chatmd_shell_spec.Source_ref.digest "foreign-v1", foreign)
          ~custom:false
          read_calls
      in
      let managed, selected =
        managed_job_registry
          ~base_registry:base
          ~source:
            {|let run ctx input = Task.bind(
  Task.catch(Tool.call("foreign_tool", input), fun ignored -> Task.pure(`Error("caught"))),
  fun ignored -> Task.bind(Job.start_tool("read_file", input), fun id ->
    Task.pure(`Complete(input))))|}
          env
          read_calls
      in
      prepared := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle:_ ~owner ~selected ~jobs:_ ~store:_ ~blobs:_ ->
       let managed = Option.value_exn !prepared in
       let executor = X.with_managed_definition executor ~definition:managed ~env |> ok in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer
           { (H.context admitted) with
             implementation_revision = M.execution_revision managed binding |> ok
           }
         |> ok
       in
       let terminal =
         X.run_root
           executor
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(function
             | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
             | _ -> Error (P.Error.invalid_request "unexpected output"))
         |> ok
       in
       assert (
         match H.status terminal with
         | Resolved (Fail _) -> true
         | _ -> false);
       assert (Int.equal !read_calls 0);
       assert (Int.equal !foreign_calls 0);
       assert (List.is_empty (O.Job_candidate.read_current root ~checkpoint:ignore |> ok)))
;;

let%test_unit "developer managed Model.spawn requires admitted provider selection" =
  let module M = Chat_response.Managed_tool_registry in
  let prepared = ref None in
  with_job_executor
    ~run_name:"dvr_managed_no_provider"
    ~make_selected:(fun env ->
      let managed, selected =
        managed_job_registry
          ~tool_contract:Chatml_compilation.Tool_contract.Developer_model_spawn
          ~source:
            {|let run ctx input = Task.bind(Model.spawn("agent_prompt_v1", input), fun id ->
  Task.pure(`Complete(input)))|}
          env
          (ref 0)
      in
      prepared := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle:_ ~owner ~selected ~jobs:_ ~store:_ ~blobs:_ ->
       let managed = Option.value_exn !prepared in
       let executor = X.with_managed_definition executor ~definition:managed ~env |> ok in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer
           { (H.context admitted) with
             implementation_revision = M.execution_revision managed binding |> ok
           }
         |> ok
       in
       let terminal =
         X.run_root
           executor
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(function
             | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
             | _ -> Error (P.Error.invalid_request "unexpected output"))
         |> ok
       in
       assert (
         match H.status terminal with
         | Resolved (Fail _) -> true
         | _ -> false);
       assert (List.is_empty (O.Job_candidate.read_current root ~checkpoint:ignore |> ok)))
;;

let%test_unit
    "lexical root cleanup retains uncertain selected capacity for exact reconciliation"
  =
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  let capacity = ref None in
  let calls = ref 0 in
  with_job_executor
    ~run_name:"dvr_script_uncertain_root"
    ~make_selected:(fun _ -> registry ~custom:false calls)
    ~per_session:1
    ~on_capacity:(fun value -> capacity := Some value)
    ~wrap_env:(fun env ->
      F.fault_after_authority_rename_env
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env:_ ~root ~executor:_ ~handle ~owner:_ ~selected:_ ~jobs ~store:_ ~blobs:_ ->
       let module Runtime = Chatml_host_runtime in
       let module Value = Chatml.Chatml_value_codec in
       let effects = ref [] in
       let invoked = ref 0 in
       let compiled =
         Runtime.compile_script
           ~surface:Chatml.Chatml_extension_surface.one_off_v1
           ~source:{|let main input = Job.start_tool("read_file", input)|}
           ()
         |> Result.ok_or_failwith
       in
       let config : Runtime.runtime_config =
         { surface = Chatml.Chatml_extension_surface.one_off_v1
         ; operations = Runtime.default_operations ~handlers:Runtime.default_handlers ()
         }
       in
       let uncertain =
         Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope ~sw:_ ->
           Int.incr invoked;
           let config = Jobs.Scope.install scope config in
           let value =
             Runtime.run_entrypoint
               ~prepare_result:(fun ~value:_ ~local_effects ->
                 effects := local_effects;
                 Ok ignore)
               config
               compiled
               ~entrypoint:"main"
               ~arguments:[ Value.import_json (`Object []) ]
               ()
             |> Result.map_error ~f:P.Error.invalid_request
           in
           Result.map value ~f:(fun value ->
             rename_armed := true;
             value, !effects))
       in
       assert (Result.is_error uncertain);
       assert (Int.equal !invoked 1);
       let ids, _ =
         Chat_response.Background_job_operations.split_starts !effects
         |> Result.ok_or_failwith
       in
       let id = List.hd_exn ids in
       let project_source =
         let prepared = Chatml_developer_capture.prepared (O.capture root) in
         match Chatml_prepared_program.provenance prepared with
         | Project { declaration; lock = _ } -> declaration
         | Implicit -> Chatml_prepared_program.entry prepared
       in
       let key =
         Agent_server.Job_capacity.Key.developer
           ~owner:(O.owner root)
           ~project_source
           ~workspace_conflict_domain:"selected-native-test"
           ~kind:P.Job.Async_tool
           ~nested_depth:0
         |> ok
       in
       (match Agent_server.Job_capacity.try_acquire (Option.value_exn !capacity) key with
        | Ok None -> ()
        | Ok (Some _) | Error _ ->
          failwith "lexical cleanup released the uncertain selected reservation");
       assert (
         Result.is_error
           (Jobs.with_root_action jobs ~phase:R.Checkpoint.Active (fun ~scope:_ ~sw:_ ->
              failwith "unknown physical handoff admitted another root callback")));
       sync_blocked := false;
       O.reconcile_pending root |> ok;
       assert (Option.is_some (R.find_job (R.observe handle |> ok) id));
       assert (Int.equal !invoked 1))
;;

let managed_registry env =
  let module Managed = Chat_response.Managed_tool_registry in
  let base =
    C.create
      ~owner:"selected-managed-test"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "managed-resource")
      []
    |> ok
  in
  let dir = Eio.Stdenv.cwd env in
  let source_loader =
    Source_loader.captured_filesystem
      ~root:dir
      ~sources:
        [ "input.json", {|{"type":"object"}|}; "output.json", {|{"type":"object"}|} ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir
      ~source_loader
      {|<script id="owned" language="chatml" kind="tool">
let run ctx input = Task.pure(`Complete(input))
</script>
<tool name="summary" type="chatml" script="owned" entrypoint="run" input_schema="input.json" output_schema="output.json"/>|}
  in
  let managed =
    Managed.prepare ~env ~owner:"selected-managed-test" ~capabilities:base elements
    |> Result.map_error ~f:(fun diagnostics ->
      String.concat
        ~sep:"; "
        (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string))
    |> Result.ok_or_failwith
  in
  managed, Managed.capabilities managed
;;

let%test_unit "selected native Function and Custom share durable dispatch policy" =
  List.iter [ false; true ] ~f:(fun custom ->
    let calls = ref 0 in
    with_executor
      ~run_name:(if custom then "dvr_selected_custom" else "dvr_selected_function")
      ~make_selected:(fun _env -> registry ~custom calls)
      (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
         let reference = List.hd_exn (C.references selected) in
         let admitted = invocation owner selected reference ~custom in
         let terminal =
           X.run_root
             executor
             ~reference
             ~invocation:admitted
             ~authorize:(fun _ _ -> Ok ())
             ~prepare_output:(fun _ -> Ok (`String "disclosed"))
           |> ok
         in
         assert (Int.equal !calls 1);
         assert (
           I.equal_status (H.status terminal) (Resolved (Complete (`String "disclosed"))));
         let observed = R.observe handle |> ok in
         let prior =
           R.find_invocation observed (H.context admitted).id
           |> R.Invocation_lookup.prior
           |> Option.value_exn
         in
         assert (
           H.equal_context
             (H.context terminal)
             (H.context (R.Invocation_observation.value prior)));
         assert (
           I.equal_status
             (H.status terminal)
             (H.status (R.Invocation_observation.value prior)));
         let after_wait = invocation owner selected reference ~custom in
         let revoked =
           X.run_root
             executor
             ~reference
             ~invocation:after_wait
             ~authorize:(fun _ _ -> O.narrow_selection root ~names:[])
             ~prepare_output:(fun _ -> Ok (`String "disclosed"))
           |> ok
         in
         (match H.status revoked with
          | Resolved (Fail _) -> ()
          | _ -> failwith "replaced selected capability reached a native effect");
         assert (Int.equal !calls 1)))
;;

let%test_unit "claimed worker alone admits job child and inherited nested ancestry" =
  let parent_calls = ref 0 in
  let child_calls = ref 0 in
  let invoke_nested = ref (fun () -> ()) in
  let retained_active = ref None in
  let nested_id = ref None in
  with_executor
    ~run_name:"dvr_claimed_native_parent"
    ~make_selected:(fun _env ->
      pair_registry ~on_parent:(fun () -> !invoke_nested ()) parent_calls child_calls)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let job_id = stage_root_job env root selected in
       let parent_binding = C.find selected ~name:"parent_tool" |> ok in
       let parent_reference = C.reference parent_binding in
       let child_binding = C.find selected ~name:"child_tool" |> ok in
       let child_reference = C.reference child_binding in
       let parent_invocation =
         let admitted = invocation owner selected parent_reference ~custom:false in
         H.create_developer { (H.context admitted) with parent_job = Some job_id } |> ok
       in
       (invoke_nested
        := fun () ->
             let admitted = invocation owner selected child_reference ~custom:false in
             let child =
               H.create_developer
                 { (H.context admitted) with
                   parent_invocation = Some (H.context parent_invocation).id
                 ; parent_job = Some job_id
                 }
               |> ok
             in
             nested_id := Some (H.context child).id;
             ignore
               (X.run_child
                  executor
                  ~reference:child_reference
                  ~invocation:child
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok (`String "child"))
                : (H.t, P.Error.t) result));
       let expected = R.observe handle |> ok in
       let completed =
         O.with_claimed_job
           root
           ~expected
           ~lookup:(R.find_job_exact expected job_id)
           (fun ~active ~sw:_ ->
              retained_active := Some active;
              let ancestor = O.job_parent root active |> ok in
              assert (P.Id.Job.equal (Scope.Parent.job_id ancestor) job_id);
              assert (Int.equal (Scope.Parent.attempt ancestor) 1);
              assert (Int.equal (Scope.Parent.nested_depth ancestor) 0);
              assert (
                Result.is_error
                  (Scope.Parent.check_now
                     ancestor
                     ~owner:(T.owner "dvr_foreign_claim" 0)
                     ~capture_fingerprint:
                       (Chatml_developer_capture.fingerprint (O.capture root))));
              assert (
                Result.is_error
                  (Scope.Parent.check_now
                     ancestor
                     ~owner
                     ~capture_fingerprint:
                       (Chatml_source.Digest.of_contents
                          "foreign capture"
                          ~checkpoint:ignore)));
              let bad_context =
                H.create_developer
                  { (H.context parent_invocation) with
                    id = P.Id.Invocation.create ()
                  ; parent_job = Some (P.Id.Job.create ())
                  }
                |> ok
              in
              assert (
                Result.is_error
                  (X.run_job_child
                     executor
                     ~active
                     ~reference:parent_reference
                     ~invocation:bad_context
                     ~authorize:(fun _ _ -> Ok ())
                     ~prepare_output:(fun _ -> Ok (`String "parent"))));
              assert (Int.equal !parent_calls 0);
              let terminal =
                X.run_job_child
                  executor
                  ~active
                  ~reference:parent_reference
                  ~invocation:parent_invocation
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok (`String "parent"))
                |> ok
              in
              assert (
                I.equal_status
                  (H.status terminal)
                  (Resolved (Complete (`String "parent"))));
              Ok (succeeded_claim (Scope.claim active)))
         |> ok
         |> Option.value_exn
       in
       assert (Int.equal !parent_calls 1);
       assert (Int.equal !child_calls 1);
       (match (P.Developer_job.state completed).status with
        | P.Job.Succeeded -> ()
        | _ -> failwith "claimed worker did not durably complete");
       let observed = R.observe handle |> ok in
       let child_id = Option.value_exn !nested_id in
       assert (
         Option.is_some (R.Invocation_lookup.prior (R.find_invocation observed child_id)));
       let pinned = C.select selected ~names:[ "parent_tool" ] |> ok in
       let pinned_job = stage_root_job env root selected in
       let pinned_parent =
         let admitted = invocation owner pinned parent_reference ~custom:false in
         H.create_developer { (H.context admitted) with parent_job = Some pinned_job }
         |> ok
       in
       (invoke_nested
        := fun () ->
             let child =
               H.create_developer
                 { (H.context (invocation owner pinned child_reference ~custom:false)) with
                   parent_invocation = Some (H.context pinned_parent).id
                 ; parent_job = Some pinned_job
                 }
               |> ok
             in
             let result =
               X.run_child
                 executor
                 ~reference:child_reference
                 ~invocation:child
                 ~authorize:(fun _ _ -> Ok ())
                 ~prepare_output:(fun _ -> Ok (`String "child"))
             in
             match result with
             | Ok terminal ->
               (match H.status terminal with
                | Resolved (Fail { code = "invocation.stale_binding"; _ }) -> ()
                | _ -> failwith "pinned child was not rejected by selected binding")
             | Error _ -> failwith "pinned child rejected before binding admission");
       let expected = R.observe handle |> ok in
       ignore
         (O.with_claimed_job
            root
            ~expected
            ~lookup:(R.find_job_exact expected pinned_job)
            (fun ~active ~sw:_ ->
               let terminal =
                 X.run_job_child
                   ~selected:pinned
                   executor
                   ~active
                   ~reference:parent_reference
                   ~invocation:pinned_parent
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(fun _ -> Ok (`String "parent"))
                 |> ok
               in
               assert (
                 I.equal_status
                   (H.status terminal)
                   (Resolved (Complete (`String "parent"))));
               Ok (succeeded_claim (Scope.claim active)))
          |> ok
          : P.Developer_job.t option);
       assert (Int.equal !parent_calls 2);
       assert (Int.equal !child_calls 1);
       let expired = Option.value_exn !retained_active in
       let retry =
         H.create_developer
           { (H.context parent_invocation) with id = P.Id.Invocation.create () }
         |> ok
       in
       assert (
         Result.is_error
           (X.run_job_child
              executor
              ~active:expired
              ~reference:parent_reference
              ~invocation:retry
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "parent"))));
       assert (Int.equal !parent_calls 2))
;;

let%test_unit "claimed worker action atomically installs its selected child" =
  List.iter [ `Succeeded; `Queued; `Selection_changed ] ~f:(fun mode ->
    let calls = ref 0 in
    with_executor
      ~run_name:"dvr_claimed_selected_action"
      ~make_selected:(fun env -> registry ~custom:false calls)
      (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ->
         let parent_id = Selected.job_id "job_claimed_retry_selected_parent" in
         let capture_fingerprint =
           Chatml_developer_capture.fingerprint (O.capture root)
         in
         let original =
           Selected.job
             (O.owner root)
             capture_fingerprint
             ~id:parent_id
             ~parent:P.Developer_job.Launch.Run_root
         in
         let parent =
           P.Developer_job.create
             ~owner:(O.owner root)
             ~capture_fingerprint
             ~id:parent_id
             ~launch:(P.Developer_job.launch original)
             ~kind:(P.Developer_job.kind original)
             ~payload:(P.Developer_job.payload original)
             ~retry_policy:(P.Job.Safe_retry { max_attempts = 2; backoff_ms = 0 })
             ~created_at:(P.Developer_job.created_at original)
             ~state:(P.Developer_job.state original)
           |> ok
         in
         let prepared_root =
           R.prepare_root_selected
             handle
             ~expected:(R.observe handle |> ok)
             ~phase:R.Checkpoint.Active
             ~selected:[ parent ]
           |> ok
         in
         ignore
           (R.commit_and_settle
              handle
              ~admit:(fun () -> true)
              prepared_root
              (R.Handoff.create ~settle:ignore)
            |> ok
            : R.Settlement.t);
         let prepared =
           Chat_response.One_off_script.prepare_in_domain
             ~env
             ~capabilities:selected
             ~tools:[]
             ~source:"let main input = Task.pure(input)"
             ()
           |> ok
         in
         let request =
           Chat_response.Background_request.script
             ~prepared
             ~input:`Null
             ~policy:Chat_response.One_off_request.default_policy
           |> ok
         in
         let candidates = O.Job_candidate.read_current root ~checkpoint:ignore |> ok in
         let candidate =
           List.find_exn candidates ~f:(fun candidate ->
             P.Id.Job.equal (P.Developer_job.id (O.Job_candidate.job candidate)) parent_id)
         in
         let selected_child = ref None in
         let dropped_child = ref None in
         let outcome =
           O.Job_candidate.with_claimed_selected
             root
             candidate
             (fun ~active ~action ~sw:_ ->
                incr calls;
                let dropped =
                  O.Worker_action.stage_background action ~request ~checkpoint:ignore
                  |> ok
                in
                dropped_child := Some dropped;
                O.Worker_action.rollback_staged action dropped |> ok;
                match mode with
                | `Selection_changed ->
                  O.narrow_selection root ~names:[] |> ok;
                  (match
                     O.Worker_action.stage_background action ~request ~checkpoint:ignore
                   with
                   | Error error ->
                     assert (
                       String.equal
                         error.P.Error.message
                         "developer worker selected registry changed")
                   | Ok _ -> failwith "changed Worker selection staged a child");
                  Error (P.Error.invalid_request "selected registry changed")
                | `Succeeded | `Queued ->
                  let child =
                    O.Worker_action.stage_background action ~request ~checkpoint:ignore
                    |> ok
                  in
                  selected_child := Some child;
                  let selection = O.Worker_action.seal action ~selected:[ child ] |> ok in
                  let after =
                    match mode with
                    | `Succeeded -> succeeded_claim (Scope.claim active)
                    | `Queued ->
                      { (P.Developer_job.state (R.Claim.job (Scope.claim active))) with
                        status = P.Job.Queued
                      ; started_at = None
                      ; next_run_at = Some (T.now ())
                      ; completed_at = None
                      ; result = None
                      ; retry_failure = Some (P.Error.invalid_request "retry")
                      }
                    | `Selection_changed -> assert false
                  in
                  Ok (after, selection))
         in
         let observed = R.observe handle |> ok in
         (match mode, outcome with
          | `Succeeded, Ok (Some (parent, disposition)) ->
            let child_id = Option.value_exn !selected_child in
            assert (P.Id.Job.equal (P.Developer_job.id parent) parent_id);
            (match disposition with
             | Agent_server.Developer_job_worker.Children.Settlement.Selected_installed ->
               ()
             | Selected_omitted_after_epoch_change ->
               failwith "active claimed selection was omitted");
            let child = R.find_job observed child_id |> Option.value_exn in
            let launch = P.Developer_job.launch child in
            (match P.Developer_job.Launch.parent launch with
             | Run_root -> ()
             | Invocation _ -> failwith "claimed worker child invented invocation parent");
            (match P.Developer_job.Launch.parent_job launch with
             | Some (id, attempt) when P.Id.Job.equal id parent_id && Int.equal attempt 1
               -> ()
             | None | Some _ -> failwith "claimed worker child lost exact parent attempt");
            assert (Int.equal (P.Developer_job.Launch.nested_depth launch) 1)
          | `Queued, Error error ->
            let child_id = Option.value_exn !selected_child in
            assert (
              String.equal
                error.P.Error.message
                "developer queued completion cannot select child jobs");
            assert (Option.is_none (R.find_job observed child_id));
            let parent = R.find_job observed parent_id |> Option.value_exn in
            (match (P.Developer_job.state parent).status with
             | P.Job.Failed _ -> ()
             | _ -> failwith "invalid selected parent did not fail durably");
            ignore (O.current root |> ok : O.Current.t)
          | `Selection_changed, Error _ ->
            assert (Option.is_none !selected_child);
            let parent = R.find_job observed parent_id |> Option.value_exn in
            (match (P.Developer_job.state parent).status with
             | P.Job.Failed _ -> ()
             | _ -> failwith "changed Worker selection did not fail durably")
          | (`Succeeded | `Queued | `Selection_changed), (Ok None | Ok (Some _) | Error _)
            -> failwith "claimed selected worker returned the wrong outcome");
         assert (Option.is_none (R.find_job observed (Option.value_exn !dropped_child)));
         assert (Int.equal !calls 1)))
;;

let%test_unit "claimed worker lexical jobs retain the saved tool ceiling" =
  let parent_calls = ref 0 in
  let child_calls = ref 0 in
  with_job_executor
    ~run_name:"dvr_claimed_pinned_ceiling"
    ~make_selected:(fun _ -> pair_registry ~on_parent:ignore parent_calls child_calls)
    (fun ~env ~root ~executor:_ ~handle ~owner:_ ~selected ~jobs ~store:_ ~blobs:_ ->
       let pinned = C.select selected ~names:[ "parent_tool" ] |> ok in
       let parent_id = stage_root_job ~tools:[ "parent_tool" ] env root pinned in
       let request capabilities name =
         let binding = C.find capabilities ~name |> ok in
         Chat_response.Background_request.tool
           ~capabilities
           ~reference:(C.reference binding)
           ~input:(`Object [])
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let pinned_request = request pinned "parent_tool" in
       let wider_request = request selected "child_tool" in
       let unselected_child = ref None in
       let candidate =
         O.Job_candidate.read_current root ~checkpoint:ignore
         |> ok
         |> List.find_exn ~f:(fun candidate ->
           P.Id.Job.equal (P.Developer_job.id (O.Job_candidate.job candidate)) parent_id)
       in
       let completed =
         Jobs.with_claimed_worker jobs ~candidate (fun ~active ~scope ~sw:_ ->
           assert (
             Result.is_error
               (Jobs.Scope.stage_background
                  scope
                  ~request:wider_request
                  ~checkpoint:ignore));
           unselected_child
           := Some
                (Jobs.Scope.stage_background
                   scope
                   ~request:pinned_request
                   ~checkpoint:ignore
                 |> ok);
           Ok (succeeded_claim (Scope.claim active), []))
         |> ok
         |> Option.value_exn
       in
       assert (P.Id.Job.equal (P.Developer_job.id completed) parent_id);
       let observed = R.observe handle |> ok in
       assert (Option.is_none (R.find_job observed (Option.value_exn !unselected_child)));
       assert (Int.equal !parent_calls 0);
       assert (Int.equal !child_calls 0))
;;

let%test_unit "job child permission wait rechecks selection and real deadline" =
  let expired_at = P.Timestamp.of_string "2026-10-13T00:00:02Z" |> ok in
  let deadline = P.Timestamp.of_string "2026-10-13T00:00:01Z" |> ok in
  List.iter [ `Revoke; `Expire ] ~f:(fun mode ->
    let calls = ref 0 in
    let clock = ref (T.now ()) in
    with_executor
      ~now:(fun () -> !clock)
      ~run_name:
        (match mode with
         | `Revoke -> "dvr_job_revoke"
         | `Expire -> "dvr_job_expire")
      ~make_selected:(fun _env -> registry ~custom:false calls)
      (fun ~env ~root ~executor ~handle ~owner ~selected ->
         let job_id = stage_root_job env root selected in
         let reference = List.hd_exn (C.references selected) in
         let admitted = invocation owner selected reference ~custom:false in
         let admitted =
           H.create_developer
             { (H.context admitted) with
               parent_job = Some job_id
             ; deadline = Some deadline
             }
           |> ok
         in
         let expected = R.observe handle |> ok in
         ignore
           (O.with_claimed_job
              root
              ~expected
              ~lookup:(R.find_job_exact expected job_id)
              (fun ~active ~sw:_ ->
                 let terminal =
                   X.run_job_child
                     executor
                     ~active
                     ~reference
                     ~invocation:admitted
                     ~authorize:(fun _ _ ->
                       Eio.Fiber.yield ();
                       match mode with
                       | `Revoke -> O.narrow_selection root ~names:[]
                       | `Expire ->
                         clock := expired_at;
                         Ok ())
                     ~prepare_output:(fun _ -> Ok (`String "private"))
                   |> ok
                 in
                 (match H.status terminal with
                  | Resolved (Fail _) -> ()
                  | _ -> failwith "expired or revoked job parent reached native effect");
                 Ok (succeeded_claim (Scope.claim active)))
            |> ok
            : P.Developer_job.t option);
         assert (Int.equal !calls 0)))
;;

let%test_unit "cancelled claimed parent cannot start an effect while child joins" =
  let calls = ref 0 in
  let original = Failure "claimed parent cancelled before child joined" in
  let ready, ready_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let child_denied = ref false in
  with_executor
    ~run_name:"dvr_cancelled_job_parent"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let job_id = stage_root_job env root selected in
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer { (H.context admitted) with parent_job = Some job_id } |> ok
       in
       let expected = R.observe handle |> ok in
       (match
          Eio.Cancel.sub (fun cancellation ->
            O.with_claimed_job
              root
              ~expected
              ~lookup:(R.find_job_exact expected job_id)
              (fun ~active ~sw ->
                 Eio.Fiber.fork ~sw (fun () ->
                   Eio.Cancel.protect (fun () ->
                     Eio.Promise.resolve ready_u ();
                     Eio.Promise.await release;
                     child_denied
                     := Result.is_error
                          (X.run_job_child
                             executor
                             ~active
                             ~reference
                             ~invocation:admitted
                             ~authorize:(fun _ _ -> Ok ())
                             ~prepare_output:(fun _ -> Ok (`String "private")))));
                 Eio.Promise.await ready;
                 Eio.Cancel.protect (fun () ->
                   Eio.Cancel.cancel cancellation original;
                   Eio.Promise.resolve release_u ());
                 Ok (succeeded_claim (Scope.claim active))))
        with
        | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual original)
        | Ok _ | Error _ -> failwith "claimed worker cancellation identity was lost");
       assert !child_denied;
       assert (Int.equal !calls 0))
;;

let%test_unit "saved-source child retains its exact entered parent active" =
  let effects = ref 0 in
  with_executor
    ~run_name:"dvr_source_child_parent"
    ~make_selected:(fun _env -> registry ~custom:false effects)
    (fun ~env:_ ~root ~executor:_ ~handle:_ ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let issuer =
         D.create
           ~owner
           ~capture_fingerprint:(Chatml_developer_capture.fingerprint (O.capture root))
           ~ceiling:(O.native_ceiling root)
           ~selected:(fun () -> O.native_selected root)
           ~current:(fun ~owner:_ ~capture_fingerprint:_ ->
             Result.map (O.current root) ~f:ignore)
           ~now:(fun () -> O.native_now root)
         |> ok
       in
       let parent_live = ref true in
       let retained = ref None in
       let revision, completion =
         O.with_native_registration root ~invocation:admitted (fun ~scope ->
           let dispatch : D.dispatch_storage =
             { read = O.Native_terminal.read_dispatch scope
             ; save_dispatch = O.Native_terminal.save_dispatch scope
             }
           in
           let sealed = ref None in
           let completion =
             D.with_invocation
               issuer
               ~parent:D.Root
               ~invocation:admitted
               ~dispatch
               ~parent_live_now:(fun () ->
                 !parent_live && O.Native_terminal.effect_parent_live_now scope)
               ~frozen:(fun () -> None)
               (fun ~active ~dispatched ->
                  O.with_native_callback scope ~dispatched (fun ~sw:_ ->
                    let source_child =
                      D.create_source_child
                        ~parent_issuer:issuer
                        ~parent_active:active
                        ~ceiling:selected
                        ~current_child:(fun () -> Ok ())
                      |> ok
                    in
                    let child_of parent =
                      H.create_developer
                        { (H.context admitted) with
                          id = P.Id.Invocation.create ()
                        ; parent_invocation = Some (H.context parent).id
                        }
                      |> ok
                    in
                    let child = child_of dispatched in
                    retained := Some (source_child, active, child);
                    assert (
                      Result.is_ok
                        (D.check_invocation
                           source_child
                           ~parent:(D.Source_child active)
                           child));
                    assert (
                      Result.is_error
                        (D.check_invocation source_child ~parent:D.Root admitted));
                    let other = child_of dispatched in
                    let other_dispatch : D.dispatch_storage =
                      { read = (fun _ -> Ok None)
                      ; save_dispatch =
                          (fun ~expected_revision ~dispatched:_ ->
                            assert (Option.is_none expected_revision);
                            Ok 1)
                      }
                    in
                    let child_completion =
                      D.with_invocation
                        source_child
                        ~parent:(D.Source_child active)
                        ~invocation:child
                        ~dispatch:other_dispatch
                        ~parent_live_now:(fun () -> true)
                        ~frozen:(fun () -> None)
                        (fun ~active:child_active ~dispatched:child_dispatched ->
                           let nested = child_of child_dispatched in
                           assert (
                             Result.is_ok
                               (D.check_invocation
                                  source_child
                                  ~parent:(D.Child child_active)
                                  nested));
                           parent_live := false;
                           assert (
                             Result.is_error
                               (D.check_invocation
                                  source_child
                                  ~parent:(D.Child child_active)
                                  nested));
                           assert (
                             Result.is_error
                               (D.check_parent_now
                                  source_child
                                  ~parent:(D.Child child_active)
                                  nested));
                           assert (
                             Result.is_error
                               (D.inherited_job_parent source_child child_active));
                           parent_live := true;
                           Ok (I.Complete `Null, None))
                      |> ok
                    in
                    ignore
                      (D.Completion.finish child_completion ~settlement:(Terminal_saved 2)
                       |> ok);
                    let other_completion =
                      D.with_invocation
                        issuer
                        ~parent:(D.Child active)
                        ~invocation:other
                        ~dispatch:other_dispatch
                        ~parent_live_now:(fun () -> true)
                        ~frozen:(fun () -> None)
                        (fun ~active:other_active ~dispatched:other_dispatched ->
                           let wrong_child = child_of other_dispatched in
                           assert (
                             Result.is_error
                               (D.check_invocation
                                  source_child
                                  ~parent:(D.Source_child other_active)
                                  wrong_child));
                           Ok (I.Complete `Null, None))
                      |> ok
                    in
                    ignore
                      (D.Completion.finish other_completion ~settlement:(Terminal_saved 2)
                       |> ok);
                    sealed
                    := Some
                         (O.Native_terminal.seal
                            scope
                            ~outcome:(I.Complete `Null)
                            ~selected:[]
                          |> ok);
                    Ok (I.Complete `Null, None)))
             |> ok
           in
           let selected = Option.value_exn !sealed in
           let revision =
             O.Native_terminal.submit
               scope
               ~terminal:(D.Completion.terminal completion)
               ~selected:(Some selected)
             |> ok
           in
           Ok (revision, completion))
         |> ok
       in
       ignore (D.Completion.finish completion ~settlement:(Terminal_saved revision) |> ok);
       let source_child, parent_active, child = Option.value_exn !retained in
       assert (
         Result.is_error
           (D.check_parent_now source_child ~parent:(D.Source_child parent_active) child));
       assert (Int.equal !effects 0))
;;

let%test_unit "cancelled native parent switch rejects protected nested child" =
  let original = Failure "native parent switch failed before child join" in
  let ready, ready_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let child_denied = ref false in
  let effect_calls = ref 0 in
  with_executor
    ~run_name:"dvr_native_parent_switch"
    ~make_selected:(fun _env -> registry ~custom:false effect_calls)
    (fun ~env:_ ~root ~executor:_ ~handle:_ ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let child =
         let initial = invocation owner selected reference ~custom:false in
         H.create_developer
           { (H.context initial) with parent_invocation = Some (H.context admitted).id }
         |> ok
       in
       let issuer =
         D.create
           ~owner
           ~capture_fingerprint:(Chatml_developer_capture.fingerprint (O.capture root))
           ~ceiling:(O.native_ceiling root)
           ~selected:(fun () -> O.native_selected root)
           ~current:(fun ~owner:_ ~capture_fingerprint:_ ->
             Result.map (O.current root) ~f:ignore)
           ~now:(fun () -> O.native_now root)
         |> ok
       in
       let scope, completion =
         O.with_native_registration root ~invocation:admitted (fun ~scope ->
           let dispatch : D.dispatch_storage =
             { read = O.Native_terminal.read_dispatch scope
             ; save_dispatch = O.Native_terminal.save_dispatch scope
             }
           in
           let completion =
             D.with_invocation
               issuer
               ~parent:D.Root
               ~invocation:admitted
               ~dispatch
               ~parent_live_now:(fun () -> O.Native_terminal.effect_parent_live_now scope)
               ~frozen:(fun () -> None)
               (fun ~active ~dispatched ->
                  O.with_native_callback scope ~dispatched (fun ~sw ->
                    Eio.Fiber.fork ~sw (fun () ->
                      Eio.Cancel.protect (fun () ->
                        Eio.Promise.resolve ready_u ();
                        Eio.Promise.await release;
                        child_denied
                        := Result.is_error
                             (D.check_parent_now issuer ~parent:(D.Child active) child);
                        if not !child_denied then Int.incr effect_calls));
                    Eio.Promise.await ready;
                    Eio.Cancel.protect (fun () ->
                      Eio.Switch.fail sw original;
                      Eio.Promise.resolve release_u ());
                    Ok (I.Complete `Null, None)))
             |> ok
           in
           O.Native_terminal.remember_failure
             scope
             ~terminal:(D.Completion.terminal completion)
           |> ok;
           Ok (scope, completion))
         |> ok
       in
       assert !child_denied;
       assert (Int.equal !effect_calls 0);
       (match D.Completion.dispatch completion with
        | Installed _ -> ()
        | Awaiting_reconciliation -> failwith "failed native parent lost durable dispatch");
       let terminal_revision = O.Native_terminal.resolve_remembered scope |> ok in
       match
         D.Completion.finish completion ~settlement:(Terminal_saved terminal_revision)
       with
       | exception actual -> assert (phys_equal actual original)
       | Ok _ | Error _ -> failwith "failed native parent lost exception identity")
;;

let%test_unit "job expiry during managed preparation cannot reserve child capacity" =
  let module M = Chat_response.Managed_tool_registry in
  let managed_definition = ref None in
  let clock = ref (T.now ()) in
  let expired_at = P.Timestamp.of_string "2026-10-13T00:00:02Z" |> ok in
  let deadline = P.Timestamp.of_string "2026-10-13T00:00:01Z" |> ok in
  let prepared = ref 0 in
  with_job_executor
    ~now:(fun () -> !clock)
    ~prepare_source:(fun ~parent:_ ~binding:_ ~request:_ ~checkpoint ->
      Int.incr prepared;
      checkpoint ();
      clock := expired_at;
      Ok None)
    ~run_name:"dvr_job_prepare_expiry"
    ~make_selected:(fun env ->
      let managed, selected = managed_job_registry env (ref 0) in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle ~owner ~selected ~jobs:_ ~store:_ ~blobs:_ ->
       let job_id = stage_root_job env root selected in
       let managed = Option.value_exn !managed_definition in
       let executor = X.with_managed_definition executor ~definition:managed ~env |> ok in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer
           { (H.context admitted) with
             implementation_revision = M.execution_revision managed binding |> ok
           ; parent_job = Some job_id
           ; deadline = Some deadline
           }
         |> ok
       in
       let expected = R.observe handle |> ok in
       ignore
         (O.with_claimed_job
            root
            ~expected
            ~lookup:(R.find_job_exact expected job_id)
            (fun ~active ~sw:_ ->
               let terminal =
                 X.run_job_child
                   executor
                   ~active
                   ~reference
                   ~invocation:admitted
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(function
                     | Openai.Responses.Tool_output.Output.Text value ->
                       Ok (`String value)
                     | _ -> Error (P.Error.invalid_request "unexpected managed result"))
                 |> ok
               in
               (match H.status terminal with
                | Resolved (Fail _) -> ()
                | _ -> failwith "managed preparation ignored claimed-parent expiry");
               Ok (succeeded_claim (Scope.claim active)))
          |> ok
          : P.Developer_job.t option);
       assert (Int.equal !prepared 1);
       assert (List.is_empty (O.Job_candidate.read_current root ~checkpoint:ignore |> ok));
       ignore (stage_root_job env root selected : P.Id.Job.t))
;;

let%test_unit "managed job child stages exact claimed attempt and nesting depth" =
  let module M = Chat_response.Managed_tool_registry in
  let managed_definition = ref None in
  with_job_executor
    ~run_name:"dvr_job_managed_ancestry"
    ~make_selected:(fun env ->
      let managed, selected = managed_job_registry env (ref 0) in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle ~owner ~selected ~jobs:_ ~store:_ ~blobs:_ ->
       let parent_id = stage_root_job env root selected in
       let managed = Option.value_exn !managed_definition in
       let executor = X.with_managed_definition executor ~definition:managed ~env |> ok in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer
           { (H.context admitted) with
             implementation_revision = M.execution_revision managed binding |> ok
           ; parent_job = Some parent_id
           }
         |> ok
       in
       let expected = R.observe handle |> ok in
       ignore
         (O.with_claimed_job
            root
            ~expected
            ~lookup:(R.find_job_exact expected parent_id)
            (fun ~active ~sw:_ ->
               let terminal =
                 X.run_job_child
                   executor
                   ~active
                   ~reference
                   ~invocation:admitted
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(function
                     | Openai.Responses.Tool_output.Output.Text value ->
                       Ok (`String value)
                     | _ -> Error (P.Error.invalid_request "unexpected managed result"))
                 |> ok
               in
               if
                 not
                   (I.equal_status (H.status terminal) (Resolved (Complete (`Object []))))
               then failwith (Sexp.to_string_hum (I.sexp_of_status (H.status terminal)));
               Ok (succeeded_claim (Scope.claim active)))
          |> ok
          : P.Developer_job.t option);
       let candidates = O.Job_candidate.read_current root ~checkpoint:ignore |> ok in
       assert (Int.equal (List.length candidates) 1);
       let child_id = P.Developer_job.id (O.Job_candidate.job (List.hd_exn candidates)) in
       let child = R.find_job (R.observe handle |> ok) child_id |> Option.value_exn in
       let launch = P.Developer_job.launch child in
       (match P.Developer_job.Launch.parent launch with
        | Invocation id when P.Id.Invocation.equal id (H.context admitted).id -> ()
        | Run_root | Invocation _ -> failwith "child launch lost invocation parent");
       (match P.Developer_job.Launch.parent_job launch with
        | Some (id, attempt) when P.Id.Job.equal id parent_id && Int.equal attempt 1 -> ()
        | None | Some _ -> failwith "child launch lost claimed attempt");
       assert (Int.equal (P.Developer_job.Launch.nested_depth launch) 1))
;;

let%test_unit "early selected read failure never enters handler and releases scope" =
  let armed = ref false in
  let fail_next_checkpoint = ref false in
  let failure = Failure "selected read failed before dispatch" in
  let checkpoint () =
    if !fail_next_checkpoint
    then (
      fail_next_checkpoint := false;
      raise failure)
  in
  let on_current () =
    if !armed
    then (
      armed := false;
      fail_next_checkpoint := true)
  in
  let calls = ref 0 in
  with_executor
    ~checkpoint
    ~on_current
    ~run_name:"dvr_selected_early_read"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let run () =
         X.run_root
           executor
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(fun _ -> Ok (`String "disclosed"))
       in
       armed := true;
       (match run () with
        | exception actual -> assert (phys_equal actual failure)
        | Ok _ | Error _ -> failwith "early Store exception identity was lost");
       assert (Int.equal !calls 0);
       assert (
         Option.is_none
           (R.find_invocation (R.observe handle |> ok) (H.context admitted).id
            |> R.Invocation_lookup.prior));
       ignore (O.current root |> ok : O.Current.t);
       ignore (run () |> ok : H.t);
       assert (Int.equal !calls 1))
;;

let%test_unit "uncertain selected dispatch retains exact failure until owner recovery" =
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  let fail_before_rename = ref false in
  let fail_recovery_checkpoint = ref false in
  let original = Failure "original selected dispatch failure" in
  let later = Failure "later selected recovery failure" in
  let calls = ref 0 in
  with_executor
    ~checkpoint:(fun () ->
      if !fail_recovery_checkpoint
      then (
        fail_recovery_checkpoint := false;
        raise later))
    ~run_name:"dvr_selected_uncertain"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    ~wrap_env:(fun env ->
      Developer_run_store_test_suite.Developer_run_store_tests
      .fault_after_authority_rename_env
        ~before_rename:(fun () ->
          if !fail_before_rename
          then (
            fail_before_rename := false;
            fail_recovery_checkpoint := true;
            raise original))
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       rename_armed := true;
       assert (
         Result.is_error
           (X.run_root
              executor
              ~reference
              ~invocation:admitted
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "disclosed"))));
       assert (not !rename_armed);
       assert (Int.equal !calls 0);
       sync_blocked := false;
       let id = (H.context admitted).id in
       let prior () =
         R.find_invocation (R.observe handle |> ok) id
         |> R.Invocation_lookup.prior
         |> Option.value_exn
         |> R.Invocation_observation.value
       in
       assert (I.equal_status (H.status (prior ())) Dispatching);
       assert (Result.is_error (O.current root));
       O.reconcile_pending root |> ok;
       (match H.status (prior ()) with
        | Resolved (Fail _) -> ()
        | _ -> failwith "retained native failure was not durably resolved");
       assert (Int.equal !calls 0);
       ignore (O.current root |> ok : O.Current.t);
       let second =
         H.create_developer { (H.context admitted) with id = P.Id.Invocation.create () }
         |> ok
       in
       fail_before_rename := true;
       (match
          X.run_root
            executor
            ~reference
            ~invocation:second
            ~authorize:(fun _ _ -> Ok ())
            ~prepare_output:(fun _ -> Ok (`String "disclosed"))
        with
        | exception actual -> assert (phys_equal actual original)
        | Ok _ | Error _ -> failwith "recovery replaced the original dispatch exception");
       assert (Int.equal !calls 0);
       O.reconcile_pending root |> ok;
       assert (
         Option.is_none
           (R.find_invocation (R.observe handle |> ok) (H.context second).id
            |> R.Invocation_lookup.prior));
       ignore (O.current root |> ok : O.Current.t))
;;

let%test_unit "absent native recovery serializes another scope's dispatch writer" =
  let pause_first_recovery = ref false in
  let reached, reached_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let checkpoint () =
    if !pause_first_recovery
    then (
      pause_first_recovery := false;
      Eio.Promise.resolve reached_u ();
      Eio.Promise.await release)
  in
  let calls = ref 0 in
  with_executor
    ~checkpoint
    ~run_name:"dvr_selected_distinct_dispatch_owner"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env:_ ~root ~executor:_ ~handle:_ ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let first = invocation owner selected reference ~custom:false in
       let second = invocation owner selected reference ~custom:false in
       let first_scope =
         O.with_native_registration root ~invocation:first (fun ~scope -> Ok scope) |> ok
       in
       let dispatched = H.dispatch second |> ok in
       Eio.Switch.run (fun sw ->
         let ready, ready_u = Eio.Promise.create () in
         let go, go_u = Eio.Promise.create () in
         let save_started, save_started_u = Eio.Promise.create () in
         let second_saved = ref false in
         let finished, finished_u = Eio.Promise.create () in
         Eio.Fiber.fork ~sw (fun () ->
           let result =
             O.with_native_registration root ~invocation:second (fun ~scope ->
               Eio.Promise.resolve ready_u ();
               Eio.Promise.await go;
               Eio.Promise.resolve save_started_u ();
               let revision =
                 O.Native_terminal.save_dispatch scope ~expected_revision:None ~dispatched
                 |> ok
               in
               second_saved := true;
               let selected, terminal =
                 O.with_native_callback scope ~dispatched (fun ~sw:_ ->
                   let selected =
                     O.Native_terminal.seal scope ~outcome:(I.Complete `Null) ~selected:[]
                     |> ok
                   in
                   let terminal = H.resolve dispatched (I.Complete `Null) |> ok in
                   Ok (selected, terminal))
                 |> ok
               in
               ignore
                 (O.Native_terminal.submit scope ~terminal ~selected:(Some selected) |> ok
                  : int);
               Ok revision)
           in
           Eio.Promise.resolve finished_u result);
         Eio.Promise.await ready;
         let witness = O.current root |> ok in
         let first_finished, first_finished_u = Eio.Promise.create () in
         pause_first_recovery := true;
         Eio.Fiber.fork ~sw (fun () ->
           Eio.Promise.resolve
             first_finished_u
             (O.Native_terminal.reconcile_dispatch first_scope));
         Eio.Promise.await reached;
         Eio.Promise.resolve go_u ();
         Eio.Promise.await save_started;
         assert (not !second_saved);
         assert (Result.is_ok (O.check_effect_now root witness));
         Eio.Promise.resolve release_u ();
         (match Eio.Promise.await first_finished |> ok with
          | O.Native_terminal.Absent -> ()
          | Installed _ | Uncertain -> failwith "untouched scope acquired dispatch");
         ignore (Eio.Promise.await finished |> ok : int);
         assert !second_saved;
         assert (Result.is_error (O.check_effect_now root witness)));
       assert (Int.equal !calls 0);
       ignore (O.current root |> ok : O.Current.t))
;;

let%test_unit "nested selected child uses live direct-parent proof and joins" =
  let parent_calls = ref 0 in
  let child_calls = ref 0 in
  let on_parent = ref (fun () -> failwith "parent executor was not installed") in
  with_executor
    ~run_name:"dvr_selected_nested"
    ~make_selected:(fun _env ->
      pair_registry ~on_parent:(fun () -> !on_parent ()) parent_calls child_calls)
    (fun ~env:_ ~root:_ ~executor ~handle ~owner ~selected ->
       let reference name = C.find selected ~name |> ok |> C.reference in
       let root = invocation owner selected (reference "parent_tool") ~custom:false in
       let child =
         H.create_developer
           { (H.context
                (invocation owner selected (reference "child_tool") ~custom:false))
             with
             parent_invocation = Some (H.context root).id
           }
         |> ok
       in
       (on_parent
        := fun () ->
             let terminal =
               X.run_child
                 executor
                 ~reference:(reference "child_tool")
                 ~invocation:child
                 ~authorize:(fun _ _ -> Ok ())
                 ~prepare_output:(fun _ -> Ok (`String "child disclosed"))
               |> ok
             in
             assert (
               I.equal_status
                 (H.status terminal)
                 (Resolved (Complete (`String "child disclosed")))));
       let parent =
         X.run_root
           executor
           ~reference:(reference "parent_tool")
           ~invocation:root
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(fun _ -> Ok (`String "parent disclosed"))
         |> ok
       in
       assert (
         I.equal_status
           (H.status parent)
           (Resolved (Complete (`String "parent disclosed"))));
       assert (Int.equal !parent_calls 1 && Int.equal !child_calls 1);
       assert (
         Result.is_error
           (X.run_child
              executor
              ~reference:(reference "child_tool")
              ~invocation:
                (H.create_developer
                   { (H.context child) with id = P.Id.Invocation.create () }
                 |> ok)
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "child disclosed"))));
       let observed = R.observe handle |> ok in
       List.iter [ root; child ] ~f:(fun admitted ->
         let durable =
           R.find_invocation observed (H.context admitted).id
           |> R.Invocation_lookup.prior
           |> Option.value_exn
           |> R.Invocation_observation.value
         in
         match H.status durable with
         | Resolved (Complete _) -> ()
         | _ -> failwith "nested selected invocation did not finish durably"))
;;
