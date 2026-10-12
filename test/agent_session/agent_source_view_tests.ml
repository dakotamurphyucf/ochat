open Core
open Fixtures
open Job_fixtures
module Calls = Agent_session.Script_tool_calls
module Revision = Agent_session.Prompt_revision
module Builder = Agent_session.Prompt_revision_builder
module Runtime = Chat_response.Agent_runtime
module View = Agent_session.Agent_source_code_view

let%test_unit "selected source cannot borrow a different native invocation" =
  with_actor (fun env sw actor _writer _backend ->
    let workspace = (A.state actor |> protocol_ok).spec.workspace_instance in
    let root = Eio.Path.(Eio.Stdenv.fs env / workspace.canonical_root.native_path) in
    let save name contents =
      Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) contents
    in
    save
      "root.chatmd"
      {|<tool name="probe"/><tool name="selected" agent="child.chatmd" local/>|};
    save "child.chatmd" "<developer>Selected source</developer>";
    let definition =
      Agent_session.Prompt_definition.create
        ~root_invocation:Ui
        ~id:prompt_id
        ~config_name:"source-view-invocation"
        ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let artifacts =
      Agent_store.Prompt_artifact_store.create
        ~env
        ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
      |> store_ok
    in
    let original =
      Builder.build
        ~env
        ~artifact_store:artifacts
        ~transaction_id:(Agent_protocol.Id.Transaction.create ())
        ~created_at:timestamp
        definition
      |> Authored_agent_source_tests.built
    in
    let selected =
      Builder.reparse
        ~definition
        ~artifact:(Revision.artifact original)
        ~materialized_tree:(Revision.materialized_tree original)
        ()
      |> Authored_agent_source_tests.built
    in
    let other = ref None
    and self = ref None
    and tested = ref false
    and authorizations = ref 0 in
    let delayed_reader = Eio.Fiber.create_key () in
    let completed_checks = ref 0 in
    let halted_after_checks = ref 0 in
    let delayed_waiting, delayed_waiting_u = Eio.Promise.create () in
    let delayed_release, delayed_release_u = Eio.Promise.create () in
    let delayed_result, delayed_result_u = Eio.Promise.create () in
    let module Probe = struct
      type input = string

      let name = "probe"
      let description = None
      let type_ = "function"
      let parameters = `Object [ "type", `String "object" ]
      let input_of_string input = input
    end
    in
    let implementation =
      Ochat_function.create_function
        (module Probe)
        (fun _ ->
           let services = Calls.current_native_services () |> Result.ok_or_failwith in
           let before = !authorizations in
           let context =
             Calls.agent_source_context
               services
               ~defining_revision:original
               ~implementation:(Option.value_exn !other)
             |> protocol_ok
           in
           assert (Option.is_none context);
           [%test_eq: int] before !authorizations;
           let absent_context =
             Calls.agent_source_context
               services
               ~defining_revision:original
               ~implementation:(Option.value_exn !self)
             |> protocol_ok
           in
           assert (Option.is_none absent_context);
           [%test_eq: int] before !authorizations;
           Eio.Fiber.fork ~sw (fun () ->
             Eio.Fiber.with_binding delayed_reader true (fun () ->
               let result =
                 Calls.agent_source_context
                   services
                   ~defining_revision:original
                   ~implementation:(Option.value_exn !self)
               in
               Eio.Promise.resolve delayed_result_u (Result.is_error result)));
           Eio.Promise.await delayed_waiting;
           tested := true;
           Openai.Responses.Tool_output.Output.Text "checked")
    in
    self := Some implementation;
    let registration : Runtime.native_registration =
      { implementation
      ; implementation_revision = Chatmd_shell_spec.Source_ref.digest "probe-v1"
      ; result_contract = Native_output
      ; authoring_metadata = None
      ; source_requirement = Ordinary
      }
    in
    let inference =
      Inference_fixture.create
        ~namespace:"source-view"
        ~default_model:"fixture-model"
        ~post_stream:(fun ~sw:_ ~inputs:_ ->
          failwith "source ownership probe must not invoke a model")
    in
    let ctx =
      Inference_fixture.ctx
        inference
        ~env
        ~dir:root
        ~tool_dir:root
        ~cache:(Chat_response.Cache.create ~max_size:1 ())
        ()
    in
    let elements = Revision.elements original in
    let host =
      Runtime.host
        ~env
        ~workspace:root
        ~tool_dir:root
        ~prompt_dir:(Revision.materialized_tree original)
        ~session_dir:root
        ~cache_dir:root
        ~home:root
        ~session_id:"source-view"
        ~prompt_elements:elements
      |> Result.map_error ~f:(fun _ -> "source-view host failed")
      |> Result.ok_or_failwith
    in
    let native =
      Runtime.prepare_extensions
        ~native_registrations:[ registration ]
        ~sw
        ~ctx
        ~host
        ~platform:(Runtime.platform ())
        ~prompt_elements:elements
        ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
        ~approval_provider:Shell_runtime.Approval_broker.None_available
        ~approval_store:(Shell_access.Approval.create_store ())
        ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
          failwith "source ownership probe must not invoke an agent")
        ()
      |> Result.map_error ~f:(fun _ -> "source-view runtime failed")
      |> Result.ok_or_failwith
      |> fun prepared -> prepared.Runtime.native
    in
    let capabilities = Lazy.force native.capabilities |> Background_execution_tests.cap in
    let selected_function =
      C.find capabilities ~name:"selected"
      |> Background_execution_tests.cap
      |> C.native_implementation
      |> Option.value_exn
    in
    other := Some selected_function;
    let view =
      View.create
        ~original
        ~selected
        ~native
        ~sw
        ~max_work_steps:10_000
        ~checkpoint:Eio.Fiber.check
      |> protocol_ok
    in
    (* The pure association exists; only the genuine invocation gate must stop it. *)
    assert (
      Option.is_some
        (View.find view ~defining_revision:original ~implementation:selected_function
         |> protocol_ok));
    let tools =
      Calls.create
        ~registry:(fun () -> capabilities)
        ~moderator_names:String.Set.empty
        ~now:(fun () -> timestamp)
        ~is_halted:(fun () ->
          if
            Option.value (Eio.Fiber.get delayed_reader) ~default:false
            && Int.equal !completed_checks 2
          then (
            Int.incr halted_after_checks;
            (* The helper's final halted check has completed. Hold the getter's
               subsequent affinity validation, which also reads actor liveness. *)
            if Int.equal !halted_after_checks 2
            then (
              Eio.Promise.resolve delayed_waiting_u ();
              Eio.Promise.await delayed_release));
          false)
        ~requires_active_moderator:(fun _ -> false)
        ~authorize:(fun _ _ ->
          Int.incr authorizations;
          Ok ())
        ~prepare_output:(function
          | Openai.Responses.Tool_output.Output.Text text ->
            Eio.Promise.resolve delayed_release_u ();
            assert (Eio.Promise.await delayed_result);
            Ok (`String text)
          | Content _ -> failwith "unexpected source-view probe output")
        ~defer_observation:(fun _ -> Ok ())
      |> fun tools ->
      Calls.with_admission_check tools ~check:(fun _ _ ->
        if Option.value (Eio.Fiber.get delayed_reader) ~default:false
        then Int.incr completed_checks;
        Ok ())
      |> fun tools -> Calls.with_agent_source_code_view tools view
    in
    let reference =
      C.find capabilities ~name:"probe" |> Background_execution_tests.cap |> C.reference
    in
    let request =
      Chat_response.Background_request.tool
        ~capabilities
        ~reference
        ~input:(`Object [])
        ~policy:Chat_response.One_off_request.default_policy
      |> protocol_ok
    in
    let job =
      add_claimed_job ~payload:(Chat_response.Background_request.to_json request) actor
    in
    let result =
      Background_execution_tests.run env actor job request tools |> protocol_ok
    in
    (match result.resolved.status with
     | Resolved (Complete (`String "checked")) -> ()
     | _ -> failwith "source-view probe did not complete successfully");
    assert !tested;
    [%test_eq: int] 1 !authorizations)
;;
