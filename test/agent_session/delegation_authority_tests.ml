open Core
open Fixtures
module P = Agent_protocol
module D = Agent_store.Delegation_store
module S = Agent_store.Session_store
module State = Agent_session.Session_state
module Authority = Agent_session.Delegation_authority
module C = Chat_response.Tool_capability
module Request = Chat_response.Background_request

let digest = Chatmd_shell_spec.Source_ref.digest

let%expect_test "descendants revalidate private ancestry and its exact live narrowing" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let store =
        S.create
          ~env
          ~sw
          ~root:(Filename.concat workspace_instance.canonical_root.native_path "ledger")
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"ancestry-test"
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () -> S.close store |> store_ok)
        ~f:(fun () ->
          let ledger = S.delegations store in
          let calls = ref 0 in
          let original = Generated_definition_tests.registry calls in
          let narrow registry =
            C.select registry ~names:[ "read_file" ]
            |> Result.map_error ~f:(fun error -> error.C.message)
            |> Result.ok_or_failwith
          in
          let selected = narrow original in
          let root =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          in
          let root = { root with lifecycle = { desired = Running; observed = Idle } } in
          let reserve (parent : State.t) child_id revision_id =
            let admission : D.Admission.t =
              { child_session_id = child_id
              ; revision_id
              ; transaction_id = P.Id.Transaction.create ()
              ; manifest_sha256 = digest "captured-child"
              ; parent_revision_id = parent.spec.prompt_revision_id
              ; authority_sha256 = Authority.fingerprint parent |> protocol_ok
              ; capability_pins = Request.capability_pins selected |> protocol_ok
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
                  ; idempotency_key = P.Idempotency_key.of_string "child" |> protocol_ok
                  }
                ~request_sha256:(digest "request")
                ~admission
                ~max_records:8
                ~max_bytes:1048576
              |> store_ok
              |> function
              | D.New record -> record
              | _ -> failwith "unexpected prior admission"
            in
            (* This fixture supplies trusted host states/stages to the authority
             checker. Actual cross-store creation is qualified separately. *)
            List.iter [ D.Artifact_installed; Child_installed; Linked ] ~f:(fun stage ->
              ignore (D.advance ledger record stage |> store_ok : D.record));
            record
          in
          let middle_id = P.Id.Session.create () in
          let middle_revision = P.Id.Prompt_revision.create () in
          let ancestor = reserve root middle_id middle_revision in
          let middle =
            { root with
              identity = { root.identity with session_id = middle_id }
            ; spec =
                { root.spec with
                  prompt_revision_id = middle_revision
                ; delegation = Some (D.reference ancestor)
                ; protocol =
                    { root.spec.protocol with prompt = Generated middle_revision }
                }
            }
          in
          let leaf =
            reserve middle (P.Id.Session.create ()) (P.Id.Prompt_revision.create ())
          in
          let root_state = ref root in
          let root_capabilities = ref original in
          let middle_capabilities = ref selected in
          let during_lookup = ref (fun () -> ()) in
          let host : Authority.host =
            { state =
                (fun id ->
                  Eio.Fiber.yield ();
                  if P.Id.Session.equal id root.identity.session_id
                  then Ok !root_state
                  else (
                    assert (P.Id.Session.equal id middle_id);
                    Ok middle))
            ; resolve =
                (fun reference ->
                  D.resolve ledger reference
                  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
            ; capabilities =
                (fun id ->
                  Eio.Fiber.yield ();
                  if P.Id.Session.equal id root.identity.session_id
                  then (
                    !during_lookup ();
                    Ok !root_capabilities)
                  else (
                    assert (P.Id.Session.equal id middle_id);
                    Ok !middle_capabilities))
            }
          in
          let authority ?max_depth capabilities =
            Authority.create
              ?max_depth
              ~host
              ~reference:(D.reference leaf)
              ~capabilities
              ()
          in
          let guard = authority selected in
          let check label guard =
            match Authority.check_execution guard with
            | Ok () -> print_s [%sexp (label : string), "allowed"]
            | Error error ->
              assert (String.is_substring error.message ~substring:"delegation.");
              print_s
                [%sexp
                  (label : string)
                , (String.prefix error.message (String.index_exn error.message ':')
                   : string)]
          in
          check "unchanged narrowed ancestry" guard;
          check "bounded ancestry" (authority ~max_depth:1 selected);
          middle_capabilities := original;
          check "middle cannot regain root's unused tool" guard;
          middle_capabilities := selected;
          root_state := { root with identity = { root.identity with generation = 1 } };
          check "root generation changed" guard;
          root_state := root;
          (during_lookup
           := fun () ->
                root_state
                := { root with lifecycle = { desired = Stopped; observed = Stopped } });
          check "root stops during yielding lookup" guard;
          (during_lookup := fun () -> ());
          root_state := root;
          root_capabilities := Generated_definition_tests.registry calls;
          check "equivalent replacement still needs fresh live bindings" guard;
          middle_capabilities := narrow !root_capabilities;
          check "freshly rebound complete chain" (authority !middle_capabilities);
          root_capabilities := original;
          middle_capabilities := selected;
          ignore (D.revoke ledger ancestor Authority_changed |> store_ok : D.record);
          check "ancestor revoked while leaf stays linked" guard;
          [%test_eq: int] 0 !calls)));
  [%expect
    {|
    ("unchanged narrowed ancestry" allowed)
    ("bounded ancestry" delegation.ancestry_limit)
    ("middle cannot regain root's unused tool" delegation.bindings_changed)
    ("root generation changed" delegation.authority_changed)
    ("root stops during yielding lookup" delegation.parent_inactive)
    ("equivalent replacement still needs fresh live bindings"
     delegation.bindings_changed)
    ("freshly rebound complete chain" allowed)
    ("ancestor revoked while leaf stays linked" delegation.revoked)
    |}]
;;
