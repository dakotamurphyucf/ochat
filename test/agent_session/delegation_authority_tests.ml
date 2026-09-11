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

let%expect_test
    "independent lifetime removes execution dependence but preserves transitive authority"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let store =
        S.create
          ~env
          ~sw
          ~root:(Filename.concat workspace_instance.canonical_root.native_path "ledger")
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"independent-authority"
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () -> S.close store |> store_ok)
        ~f:(fun () ->
          let ledger = S.delegations store in
          let calls = ref 0 in
          let original = Generated_definition_tests.registry calls in
          let selected =
            C.select original ~names:[ "read_file" ]
            |> Result.map_error ~f:(fun error -> error.C.message)
            |> Result.ok_or_failwith
          in
          let base =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          in
          let root = { base with lifecycle = { desired = Running; observed = Idle } } in
          let grant_digest = digest "explicit operator lifetime policy revision" in
          let reserve (parent : State.t) label lifetime =
            let child_session_id = P.Id.Session.create () in
            let revision_id = P.Id.Prompt_revision.create () in
            let admission : D.Admission.t =
              { child_session_id
              ; revision_id
              ; transaction_id = P.Id.Transaction.create ()
              ; manifest_sha256 = digest label
              ; parent_revision_id = parent.spec.prompt_revision_id
              ; parent_stop_epoch = Some parent.stop_epoch
              ; authority_sha256 = Authority.fingerprint parent |> protocol_ok
              ; capability_pins = Request.capability_pins selected |> protocol_ok
              ; lifetime
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
              | _ -> failwith "unexpected prior independent admission"
            in
            (* Trusted fixture states isolate ancestry validation. Actual factory
               independent creation/resource recovery is not qualified here. *)
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
          let ancestor, middle = reserve root "middle" Owned in
          let leaf, independent =
            reserve
              middle
              "independent"
              (Independent { authorization_sha256 = grant_digest })
          in
          let owned_peer, _ = reserve middle "owned-peer" Owned in
          let owned_tip, _ = reserve independent "owned-tip" Owned in
          let root_state = ref (Some root) in
          let middle_state = ref middle in
          let independent_state = ref independent in
          let grant = ref (Some grant_digest) in
          let during_lookup = ref (fun () -> ()) in
          let independent_authorization (record : D.record) =
            Eio.Fiber.yield ();
            match record.admission.lifetime, !grant with
            | Independent { authorization_sha256 }, Some current
              when String.equal authorization_sha256 current -> Ok ()
            | _ ->
              Error
                (P.Error.create
                   Permission_denied
                   ~message:"delegation.lifetime_denied: independent authority changed"
                   ~retryable:false
                   ())
          in
          let host : Authority.host =
            { state =
                (fun id ->
                  Eio.Fiber.yield ();
                  if P.Id.Session.equal id root.identity.session_id
                  then
                    Result.of_option
                      !root_state
                      ~error:
                        (P.Error.create
                           Permission_denied
                           ~message:"delegation.parent_missing: parent deleted"
                           ~retryable:false
                           ())
                  else if P.Id.Session.equal id middle.identity.session_id
                  then Ok !middle_state
                  else (
                    assert (P.Id.Session.equal id independent.identity.session_id);
                    Ok !independent_state))
            ; resolve =
                (fun reference ->
                  D.resolve ledger reference
                  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
            ; capabilities =
                (fun id ->
                  Eio.Fiber.yield ();
                  !during_lookup ();
                  Ok
                    (if P.Id.Session.equal id root.identity.session_id
                     then original
                     else selected))
            }
          in
          let guard ?authorize_independent record =
            Authority.create
              ?authorize_independent
              ~host
              ~reference:(D.reference record)
              ~capabilities:selected
              ()
          in
          let authorized record =
            guard ~authorize_independent:independent_authorization record
          in
          let check label guard =
            match Authority.check_execution guard with
            | Ok () -> print_s [%sexp (label : string), "allowed"]
            | Error error ->
              print_s
                [%sexp
                  (label : string)
                , (String.prefix error.message (String.index_exn error.message ':')
                   : string)]
          in
          let stopped (state : State.t) =
            { state with
              lifecycle = { desired = Stopped; observed = Stopped }
            ; stop_epoch = Int64.succ state.stop_epoch
            }
          in
          check "default host rejects independent lifetime" (guard leaf);
          check "explicit current lifetime authority" (authorized leaf);
          root_state := Some (stopped root);
          middle_state := stopped middle;
          check "stopped Owned ancestry above independent edge" (authorized leaf);
          check "Owned peer still needs parent execution" (authorized owned_peer);
          check "Owned descendant below independent edge" (authorized owned_tip);
          independent_state := stopped independent;
          check "Owned descendant stops with independent parent" (authorized owned_tip);
          independent_state := { independent with stop_epoch = 1L };
          check "Owned descendant cannot miss stop and restart" (authorized owned_tip);
          independent_state := independent;
          (during_lookup := fun () -> middle_state := stopped !middle_state);
          check "independent parent stop during lookup" (authorized leaf);
          (during_lookup := fun () -> grant := None);
          check "host revocation during lookup" (authorized leaf);
          (during_lookup := fun () -> ());
          grant := Some (digest "replacement policy");
          check "changed host grant cannot reuse admission" (authorized leaf);
          grant := Some grant_digest;
          let previous = Option.value_exn !root_state in
          root_state
          := Some
               { previous with
                 spec =
                   { previous.spec with
                     permission_profile_digest = digest "changed policy"
                   }
               };
          check "stopped ancestor policy still checked" (authorized leaf);
          root_state := None;
          check "deleted ancestor still denies" (authorized leaf);
          root_state := Some previous;
          check "unchanged authority after independent stops" (authorized leaf);
          D.revoke ledger ancestor Authority_changed |> store_ok |> ignore;
          check "ancestor revocation crosses independent boundary" (authorized leaf);
          [%test_eq: int] 0 !calls)));
  [%expect
    {|
    ("default host rejects independent lifetime" delegation.lifetime_unavailable)
    ("explicit current lifetime authority" allowed)
    ("stopped Owned ancestry above independent edge" allowed)
    ("Owned peer still needs parent execution" delegation.parent_inactive)
    ("Owned descendant below independent edge" allowed)
    ("Owned descendant stops with independent parent" delegation.parent_inactive)
    ("Owned descendant cannot miss stop and restart" delegation.parent_stopped)
    ("independent parent stop during lookup" allowed)
    ("host revocation during lookup" delegation.lifetime_denied)
    ("changed host grant cannot reuse admission" delegation.lifetime_denied)
    ("stopped ancestor policy still checked" delegation.authority_changed)
    ("deleted ancestor still denies" delegation.parent_missing)
    ("unchanged authority after independent stops" allowed)
    ("ancestor revocation crosses independent boundary" delegation.revoked)
    |}]
;;

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
              ; parent_stop_epoch = Some parent.stop_epoch
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
          root_state := root;
          (during_lookup := fun () -> root_state := { root with stop_epoch = 1L });
          check "root stops and restarts during yielding lookup" guard;
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
    ("root stops and restarts during yielding lookup"
     delegation.authority_changed)
    ("equivalent replacement still needs fresh live bindings"
     delegation.bindings_changed)
    ("freshly rebound complete chain" allowed)
    ("ancestor revoked while leaf stays linked" delegation.revoked)
    |}]
;;
