open Core
open Fixtures
module P = Agent_protocol
module C = Chat_response.Tool_capability
module D = Agent_store.Delegation_store
module Store = Agent_store.Session_store
module Artifact = Agent_store.Prompt_artifact_store
module State = Agent_session.Session_state
module Source = Agent_session.Authored_agent_source
module Binding = Agent_session.Authored_agent_binding
module Authority = Agent_session.Delegation_authority
module CM = Prompt.Chat_markdown

let digest = Chatmd_shell_spec.Source_ref.digest

let caps_ok result =
  result |> Result.map_error ~f:(fun error -> error.C.message) |> Result.ok_or_failwith
;;

let source ~root_path ~policy =
  let definition =
    Agent_session.Prompt_definition.create
      ~id:prompt_id
      ~config_name:"authored-authority"
      ~root_file:(Eio.Path.native_exn Eio.Path.(root_path / "root.chatmd"))
      ~allowed_workspaces:[ workspace_id ]
      ~permission_profile:"interactive"
      ~runtime_policy:None
      ~enabled:true
      ~description:None
    |> store_ok
  in
  let artifact =
    Artifact.Artifact.create
      ~revision_id:prompt_revision_id
      ~root_chatmd:
        (sprintf
           {|<tool name="researcher" agent="child.chatmd" local persistence="%s"/>|}
           (match policy with
            | CM.Persistent -> "persistent"
            | Optional -> "optional"))
      ~sources:
        [ Artifact.Source.create
            ~relative_path:"child.chatmd"
            ~contents:{|<tool name="read_file"/>|}
          |> store_ok
        ]
      ~parser_schema_version:5
      ~runtime_schema_version:1
      ~created_at:timestamp
      ()
    |> store_ok
  in
  let parent_revision =
    Agent_session.Prompt_revision.create
      ~definition
      ~artifact
      ~materialized_tree:root_path
      ~elements:
        [ CM.Tool
            (Persistent_agent
               ( { name = "researcher"
                 ; description = None
                 ; agent = Eio.Path.native_exn Eio.Path.(root_path / "child.chatmd")
                 ; is_local = true
                 }
               , policy ))
        ]
  in
  Source.capture ~parent:parent_revision ~tool_name:"researcher" |> protocol_ok
;;

let%expect_test "authored private tools remain scoped across mixed delegation ancestry" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let root_path =
        Eio.Path.(Eio.Stdenv.fs env / workspace_instance.canonical_root.native_path)
      in
      let source = source ~root_path ~policy:Persistent in
      let calls = ref 0 in
      let private_registry ?resources () =
        Generated_definition_tests.registry ?resources calls
        |> fun registry -> C.select registry ~names:[ "read_file" ] |> caps_ok
      in
      let private_caps = private_registry () in
      let make_public capabilities =
        let registration =
          Agent_session.Authored_agent_call.registration
            ~source
            ~capabilities
            ~services:(fun _ ->
              incr calls;
              failwith "authority inspection cannot resolve invocation services")
            ()
          |> protocol_ok
        in
        C.create
          ~result_contracts:[ "researcher", registration.result_contract ]
          ~owner:"authored-parent"
          ~resource_fingerprint:(digest "root resources")
          [ registration.implementation_revision, registration.implementation ]
        |> caps_ok
      in
      let public = ref (make_public private_caps) in
      let private_current = ref private_caps in
      let bind () =
        Binding.bind
          ~source
          ~public:!public
          ~reference:(List.hd_exn (C.references !public))
          ~capabilities:!private_current
        |> protocol_ok
      in
      let binding = ref (bind ()) in
      assert (Result.is_error (C.select !public ~names:[ "read_file" ]));
      let store =
        Store.create
          ~env
          ~sw
          ~root:(Filename.concat workspace_instance.canonical_root.native_path "ledger")
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"authored-authority"
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () -> Store.close store |> store_ok)
        ~f:(fun () ->
          let ledger = Store.delegations store in
          let base =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          in
          let root =
            ref { base with lifecycle = { desired = Running; observed = Idle } }
          in
          let reserve (parent : State.t) label authored_tool =
            let child_session_id = P.Id.Session.create () in
            let revision_id = P.Id.Prompt_revision.create () in
            let admission : D.Admission.t =
              { child_session_id
              ; revision_id
              ; transaction_id = P.Id.Transaction.create ()
              ; manifest_sha256 = digest label
              ; parent_revision_id = parent.spec.prompt_revision_id
              ; parent_stop_epoch = Some parent.stop_epoch
              ; authored_tool
              ; authority_sha256 = Authority.fingerprint parent |> protocol_ok
              ; capability_pins =
                  Chat_response.Background_request.capability_pins private_caps
                  |> protocol_ok
              ; lifetime = Owned
              ; created_at = timestamp
              }
            in
            let record =
              D.reserve
                ledger
                ~key:
                  { parent_session_id = parent.identity.session_id
                  ; parent_generation = parent.identity.generation
                  ; principal_id
                  ; idempotency_key = P.Idempotency_key.of_string label |> protocol_ok
                  }
                ~request_sha256:(digest label)
                ~admission
                ~max_records:8
                ~max_bytes:1048576
              |> store_ok
              |> function
              | D.New record -> record
              | _ -> failwith "expected new admission"
            in
            List.iter [ D.Artifact_installed; Child_installed; Linked ] ~f:(fun stage ->
              D.advance ledger record stage |> store_ok |> ignore);
            let child =
              { parent with
                identity = { parent.identity with session_id = child_session_id }
              ; spec =
                  { parent.spec with
                    prompt_revision_id = revision_id
                  ; delegation = Some (D.reference record)
                  ; protocol =
                      { parent.spec.protocol with prompt = Generated revision_id }
                  }
              }
            in
            record, child
          in
          let origin : D.Admission.authored_tool =
            { name = "researcher"; source_sha256 = Source.fingerprint source }
          in
          let authored, middle = reserve !root "authored" (Some origin) in
          let generated, _ = reserve middle "generated-grandchild" None in
          let wrong, _ =
            reserve !root "wrong-specialist" (Some { origin with name = "reviewer" })
          in
          let grant = ref true in
          let revoke_during_lookup = ref false in
          let adapter_calls = ref 0 in
          let host : Authority.host =
            { state =
                (fun id ->
                  Eio.Fiber.yield ();
                  if P.Id.Session.equal id !root.identity.session_id
                  then Ok !root
                  else (
                    assert (P.Id.Session.equal id middle.identity.session_id);
                    Ok middle))
            ; resolve =
                (fun reference ->
                  D.resolve ledger reference
                  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
            ; capabilities =
                (fun id ->
                  Eio.Fiber.yield ();
                  Ok
                    (if P.Id.Session.equal id !root.identity.session_id
                     then !public
                     else !private_current))
            }
          in
          let authored_capabilities record ~public =
            incr adapter_calls;
            match !grant with
            | false ->
              Error
                (P.Error.create
                   Permission_denied
                   ~message:"authored grant revoked"
                   ~retryable:false
                   ())
            | true ->
              let resolved =
                Binding.resolve !binding ~record ~public ~current:!private_current
              in
              Eio.Fiber.yield ();
              if !revoke_during_lookup then grant := false;
              resolved
          in
          let guard ?(with_adapter = true) record =
            Authority.create
              ~host
              ~reference:(D.reference record)
              ~capabilities:!private_current
              ?authored_capabilities:
                (if with_adapter then Some authored_capabilities else None)
              ()
          in
          let denied (result : (unit, P.Error.t) result) =
            match result with
            | Error { code = Permission_denied; _ } -> ()
            | _ -> failwith "authority unexpectedly allowed private resources"
          in
          Authority.check_execution (guard generated) |> protocol_ok;
          Authority.check_execution (guard ~with_adapter:false generated) |> denied;
          Authority.check_execution (guard wrong) |> denied;
          let original_public = !public in
          public := C.select !public ~names:[] |> caps_ok;
          Authority.check_execution (guard generated) |> denied;
          public := original_public;
          private_current := private_registry ~resources:"changed private file roots" ();
          Authority.check_execution (guard authored) |> denied;
          (match Authority.check_execution (guard generated) with
           | Error { code = Invalid_request; message; _ } ->
             [%test_eq: string]
               "background capability configuration changed; re-admission required"
               message
           | Error error -> raise_s [%sexp (error : P.Error.t)]
           | Ok () -> failwith "generated child accepted changed resource pins");
          private_current := private_caps;
          revoke_during_lookup := true;
          Authority.check_execution (guard generated) |> denied;
          revoke_during_lookup := false;
          grant := true;
          let old_guard = guard generated in
          private_current := private_registry ();
          public := make_public !private_current;
          Authority.check_execution old_guard |> denied;
          binding := bind ();
          Authority.check_execution old_guard |> denied;
          Authority.check_execution (guard generated) |> protocol_ok;
          assert (Result.is_error (C.select !public ~names:[ "read_file" ]));
          root := { !root with lifecycle = { desired = Stopped; observed = Stopped } };
          Authority.check_execution (guard generated) |> denied;
          root := { !root with lifecycle = { desired = Running; observed = Idle } };
          D.revoke ledger authored Authority_changed |> store_ok |> ignore;
          Authority.check_execution (guard generated) |> denied;
          assert (!adapter_calls > 0);
          [%test_eq: int] 0 !calls)));
  print_endline
    "authored private tools support generated descendants without entering the root \
     public selection";
  print_endline
    "missing adapter, wrong specialist, narrowed wrapper, private-root changes and quiet \
     revocation deny";
  print_endline
    "fresh bindings require fresh guards; stop and ancestor revocation still deny; no \
     tool executed";
  [%expect
    {|
    authored private tools support generated descendants without entering the root public selection
    missing adapter, wrong specialist, narrowed wrapper, private-root changes and quiet revocation deny
    fresh bindings require fresh guards; stop and ancestor revocation still deny; no tool executed
    |}]
;;
