open Core
open Fixtures
module G = Agent_session.Generated_definition
module C = Chat_response.Tool_capability
module Store = Agent_store.Prompt_artifact_store
module CM = Prompt.Chat_markdown

let digest = Chatmd_shell_spec.Source_ref.digest

let get = function
  | Ok value -> value
  | Error errors ->
    failwith
      (String.concat
         ~sep:"; "
         (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
;;

let expect code = function
  | Error errors ->
    assert (
      List.exists errors ~f:(fun error ->
        String.equal error.Chatmd_shell_spec.Diagnostic.code code))
  | Ok _ -> failwith ("expected " ^ code)
;;

let registry ?(resources = "parent-roots") calls =
  let native name =
    let module Definition = struct
      type input = string

      let name = name
      let description = None
      let type_ = "function"
      let parameters = `True
      let input_of_string value = value
    end
    in
    Ochat_function.create_function
      (module Definition)
      (fun value ->
         Int.incr calls;
         Openai.Responses.Tool_output.Output.Text value)
  in
  C.create
    ~owner:"parent"
    ~resource_fingerprint:(digest resources)
    (List.map [ "read_file"; "unused_parent_tool" ] ~f:(fun name ->
       digest name, native name))
  |> function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : C.error)]
;;

let bundle ?(instructions = "Child instructions") () =
  Chatmd_source_bundle.create
    ~root_file:"child/main.chatmd"
    ~sources:
      [ ( "child/main.chatmd"
        , "<config model=\"child-model\" reasoning_effort=\"high\"/><developer>"
          ^ instructions
          ^ "</developer><import src=\"refs.chatmd\"/><script id=\"coordinator\" \
             language=\"chatml\" kind=\"moderator\" api=\"extensibility-v1\" \
             src=\"moderator.chatml\"/>" )
      ; "child/refs.chatmd", {|<tool type="inherited" name="read_file"/>|}
      ; ( "child/moderator.chatml"
        , "let initial_state = fail(\"initializer must not run\")\n\
           let on_event ctx state event = Task.pure(state)" )
      ]
    ()
  |> Result.ok_or_failwith
;;

let%expect_test "generated installation requires its exact durable unrevoked reservation" =
  let module D = Agent_store.Delegation_store in
  let module S = Agent_store.Session_store in
  let module P = Agent_protocol in
  with_temp_directory (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let store =
        S.create
          ~env
          ~sw
          ~root:(Filename.concat temporary "data")
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"generated-reservation"
        |> store_ok
      in
      let delegations = S.delegations store in
      let artifact_store =
        Store.create
          ~env
          ~root:(Agent_store.Data_root.prompt_artifacts_path (S.data_root store))
        |> store_ok
      in
      let calls = ref 0 in
      let parent = registry calls in
      let revision_id = P.Id.Prompt_revision.create () in
      let created_at = P.Timestamp.now () in
      let references = C.references parent in
      let prepare bundle =
        G.prepare
          ~env
          ~dir:Eio.Path.(Eio.Stdenv.fs env / temporary)
          ~revision_id
          ~created_at
          ~current_capabilities:(fun () -> parent)
          ~references
          bundle
        |> get
      in
      let prepared = prepare (bundle ()) in
      let key =
        D.Key.
          { parent_session_id = session_id
          ; parent_generation = 0
          ; principal_id = P.Id.Principal.create ()
          ; idempotency_key = P.Idempotency_key.of_string "captured-child" |> protocol_ok
          }
      in
      let admission =
        D.Admission.
          { child_session_id = P.Id.Session.create ()
          ; revision_id
          ; transaction_id = P.Id.Transaction.create ()
          ; manifest_sha256 = (G.artifact prepared).manifest_sha256
          ; parent_revision_id = P.Id.Prompt_revision.create ()
          ; parent_stop_epoch = None
          ; authored_tool = None
          ; goal_origin = None
          ; captured_recurrence = None
          ; authority_sha256 = digest "host admission"
          ; capability_pins = G.capability_pins prepared
          ; lifetime = Owned
          ; created_at
          ; inference_target = Some (delegation_inference_target ())
          }
      in
      let reservation =
        match
          D.reserve
            delegations
            ~key
            ~request_sha256:(digest "creation")
            ~admission
            ~max_records:8
            ~max_bytes:1048576
          |> store_ok
        with
        | D.New record -> record
        | _ -> failwith "expected new reservation"
      in
      let altered = prepare (bundle ~instructions:"different child" ()) in
      G.install_reserved ~delegations ~reservation ~artifact_store altered
      |> expect "delegation.reservation";
      assert (not (Store.exists artifact_store revision_id));
      let installed =
        G.install_reserved ~delegations ~reservation ~artifact_store prepared |> get
      in
      assert (D.equal_stage installed.stage Artifact_installed);
      assert (
        D.equal_record
          installed
          (G.install_reserved ~delegations ~reservation ~artifact_store prepared |> get));
      (* Source selection is DATA-only: the actual executable still rebinds to
         a distinct genuine registry and must match its selected artifact. *)
      G.Source.load ~artifact_store ~record:installed ()
      |> expect "delegation.source_record";
      G.Metadata_source.load ~artifact_store ~record:installed ()
      |> expect "delegation.metadata_source_record";
      let linked_record ~lifetime definition suffix =
        let source_admission =
          { admission with
            child_session_id = P.Id.Session.create ()
          ; revision_id = (G.artifact definition).revision_id
          ; transaction_id = P.Id.Transaction.create ()
          ; manifest_sha256 = (G.artifact definition).manifest_sha256
          ; capability_pins = G.capability_pins definition
          ; lifetime
          }
        in
        let source_key =
          { key with idempotency_key = P.Idempotency_key.of_string suffix |> protocol_ok }
        in
        let reserved =
          match
            D.reserve
              delegations
              ~key:source_key
              ~request_sha256:(digest suffix)
              ~admission:source_admission
              ~max_records:8
              ~max_bytes:1048576
            |> store_ok
          with
          | D.New record -> record
          | _ -> failwith "expected distinct source DATA reservation"
        in
        let staged =
          G.install_reserved ~delegations ~reservation:reserved ~artifact_store definition
          |> get
        in
        let child = D.advance delegations staged Child_installed |> store_ok in
        D.advance delegations child Linked |> store_ok
      in
      let independent_record definition suffix =
        linked_record
          ~lifetime:(Independent { authorization_sha256 = digest "source fixture" })
          definition
          suffix
      in
      let independent = independent_record prepared "source-moderator" in
      let source = G.Source.load ~artifact_store ~record:independent () |> get in
      assert (D.Reference.equal (G.Source.reference source) (D.reference independent));
      [%test_eq: string]
        (G.artifact prepared).manifest_sha256
        (G.Source.artifact source).manifest_sha256;
      assert (
        List.equal
          (fun (a, x) (b, y) -> String.equal a b && String.equal x y)
          (G.capability_pins prepared)
          (G.Source.capability_pins source));
      let fresh_parent = registry calls in
      let executable =
        G.restore
          ~env
          ~artifact_store
          ~revision_id
          ~manifest_sha256:(G.artifact prepared).manifest_sha256
          ~current_capabilities:(fun () -> fresh_parent)
          ~pins:(G.capability_pins prepared)
          ()
        |> get
      in
      G.Source.check_definition source ~definition:executable |> protocol_ok;
      let _, observer =
        Chat_response.Moderator_manager.Registry.of_definition
          Chat_response.Moderator_manager.Registry.empty
          (Chat_response.Generated_admission.definition (G.admission executable))
        |> Result.ok_or_failwith
      in
      let actual =
        Option.map observer ~f:(fun observer ->
          P.Invocation.
            { script_id = Chat_response.Moderator_manager.Registry.script_id observer
            ; source_sha256 =
                Chat_response.Moderator_manager.Registry.source_hash observer
            })
      in
      assert (Option.is_some actual);
      assert (Option.equal P.Invocation.equal_observer actual (G.Source.moderator source));
      let check_metadata record =
        let metadata = G.Metadata_source.load ~artifact_store ~record () |> get in
        assert (
          D.Reference.equal (G.Metadata_source.reference metadata) (D.reference record));
        assert (
          Store.Artifact.same_content
            (G.artifact prepared)
            (G.Metadata_source.artifact metadata));
        [%test_eq: string]
          (G.artifact prepared).manifest_sha256
          (G.Metadata_source.artifact metadata).manifest_sha256;
        assert (
          List.equal
            (fun (name, pin) (other_name, other_pin) ->
               String.equal name other_name && String.equal pin other_pin)
            (G.capability_pins prepared)
            (G.Metadata_source.capability_pins metadata));
        assert (
          Option.equal
            P.Invocation.equal_observer
            actual
            (G.Metadata_source.moderator metadata));
        metadata
      in
      let _ = check_metadata independent in
      let owned = linked_record ~lifetime:Owned prepared "source-owned" in
      let _ = check_metadata owned in
      G.Source.load ~artifact_store ~record:owned () |> expect "delegation.source_record";
      G.Metadata_source.load
        ~artifact_store
        ~record:owned
        ~source_limits:{ Chatmd_source_bundle.default_limits with max_source_bytes = 1 }
        ()
      |> expect "delegation.source_bounds";
      (* Invocation ownership is an authored-tool family, not a third lawful
         plain-generated lifetime. Exercise the actual store invariant instead
         of manufacturing a private record that Metadata_source could accept. *)
      let invocation_key =
        { key with
          idempotency_key =
            P.Idempotency_key.of_string "source-invocation-owned" |> protocol_ok
        }
      in
      let invocation_admission =
        { admission with
          child_session_id = P.Id.Session.create ()
        ; transaction_id = P.Id.Transaction.create ()
        ; lifetime = Invocation_owned { invocation_id = P.Id.Invocation.create () }
        }
      in
      (match
         D.reserve
           delegations
           ~key:invocation_key
           ~request_sha256:(digest "source-invocation-owned")
           ~admission:invocation_admission
           ~max_records:8
           ~max_bytes:1048576
       with
       | Error (Agent_store.Store_error.Corrupt message) ->
         [%test_eq: string]
           "invalid delegated creation identity or capability pins"
           message
       | Error error ->
         raise_s [%sexp "unexpected store refusal", (error : Agent_store.Store_error.t)]
       | Ok _ -> failwith "plain-generated Invocation_owned reservation was accepted");
      assert (Option.is_none (D.find delegations invocation_key |> store_ok));
      (match G.Source.check_definition source ~definition:altered with
       | Error error -> assert (P.Error.equal_code error.code Invalid_state)
       | Ok () -> failwith "mismatched executable source accepted");
      let no_moderator =
        G.prepare
          ~env
          ~dir:Eio.Path.(Eio.Stdenv.fs env / temporary)
          ~revision_id:(P.Id.Prompt_revision.create ())
          ~created_at
          ~current_capabilities:(fun () -> parent)
          ~references
          (Chatmd_source_bundle.create
             ~root_file:"plain.chatmd"
             ~sources:
               [ ( "plain.chatmd"
                 , {|<developer>Plain child.</developer><tool type="inherited" name="read_file"/>|}
                 )
               ]
             ()
           |> Result.ok_or_failwith)
        |> get
      in
      let plain_record = independent_record no_moderator "source-plain" in
      let plain_source = G.Source.load ~artifact_store ~record:plain_record () |> get in
      assert (Option.is_none (G.Source.moderator plain_source));
      let plain_metadata =
        G.Metadata_source.load ~artifact_store ~record:plain_record () |> get
      in
      assert (Option.is_none (G.Metadata_source.moderator plain_metadata));
      G.Source.check_definition plain_source ~definition:no_moderator |> protocol_ok;
      let changed_store =
        Store.create ~env ~root:(Filename.concat temporary "source-substitution")
        |> store_ok
      in
      G.install ~artifact_store:changed_store ~transaction_id altered |> get;
      G.Source.load ~artifact_store:changed_store ~record:independent ()
      |> expect "delegation.artifact_identity";
      G.Metadata_source.load ~artifact_store:changed_store ~record:owned ()
      |> expect "delegation.artifact_identity";
      (* The old Independent-only guard still precedes artifact validation. *)
      G.Source.load ~artifact_store:changed_store ~record:owned ()
      |> expect "delegation.source_record";
      let revoked = D.revoke delegations independent Authority_changed |> store_ok in
      G.Source.load ~artifact_store ~record:revoked ()
      |> expect "delegation.source_record";
      G.Metadata_source.load ~artifact_store ~record:revoked ()
      |> expect "delegation.metadata_source_record";
      (* Both metadata routes use the same full materialized-tree validator. *)
      let materialized =
        Store.materialized_tree artifact_store (G.artifact no_moderator).revision_id
      in
      let path = Eio.Path.(materialized / (G.artifact no_moderator).root_relative_path) in
      Eio.Process.run
        (Eio.Stdenv.process_mgr env)
        [ "/bin/chmod"; "600"; Eio.Path.native_exn path ];
      Eio.Path.save ~create:(`Or_truncate 0o600) path "changed after installation";
      let original_errors =
        match G.Source.load ~artifact_store ~record:plain_record () with
        | Error errors -> errors
        | Ok _ -> failwith "original Source accepted changed materialized tree"
      in
      let metadata_errors =
        match G.Metadata_source.load ~artifact_store ~record:plain_record () with
        | Error errors -> errors
        | Ok _ -> failwith "metadata Source accepted changed materialized tree"
      in
      [%test_eq: string list]
        (List.map original_errors ~f:Chatmd_shell_spec.Diagnostic.to_string)
        (List.map metadata_errors ~f:Chatmd_shell_spec.Diagnostic.to_string);
      [%test_eq: int] 0 !calls;
      let _ = D.revoke delegations reservation Parent_stopped |> store_ok in
      G.install_reserved ~delegations ~reservation ~artifact_store prepared
      |> expect "delegation.revoked";
      let _ = Store.load artifact_store revision_id |> store_ok in
      let authored_revision = P.Id.Prompt_revision.create () in
      let authored =
        G.with_identity prepared ~revision_id:authored_revision ~created_at |> get
      in
      let authored_admission =
        { admission with
          child_session_id = P.Id.Session.create ()
        ; revision_id = authored_revision
        ; transaction_id = P.Id.Transaction.create ()
        ; manifest_sha256 = (G.artifact authored).manifest_sha256
        ; authored_tool =
            Some { name = "researcher"; source_sha256 = digest "authored source" }
        }
      in
      let authored_reservation =
        D.reserve
          delegations
          ~key:
            { key with
              idempotency_key = P.Idempotency_key.of_string "authored" |> protocol_ok
            }
          ~request_sha256:(digest "authored request")
          ~admission:authored_admission
          ~max_records:8
          ~max_bytes:1048576
        |> store_ok
        |> function
        | D.New record -> record
        | _ -> failwith "expected authored reservation"
      in
      G.install_reserved
        ~delegations
        ~reservation:authored_reservation
        ~artifact_store
        authored
      |> expect "delegation.reservation";
      assert (not (Store.exists artifact_store authored_revision));
      print_s
        [%sexp
          { stage = (installed.stage : D.stage)
          ; initializer_or_tool_calls = (!calls : int)
          ; original_artifact_retained = (Store.exists artifact_store revision_id : bool)
          }];
      S.close store |> store_ok));
  [%expect
    {|
    ((stage Artifact_installed) (initializer_or_tool_calls 0)
     (original_artifact_retained true))
    |}]
;;

let%expect_test
    "generated artifact roundtrip pins bytes and current inherited configuration without \
     initialization"
  =
  with_temp_directory (fun env temporary ->
    let dir = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    let calls = ref 0 in
    let parent = registry calls in
    let revision_id = Agent_protocol.Id.Prompt_revision.create () in
    let artifact_store =
      Store.create ~env ~root:(Filename.concat temporary "artifacts") |> store_ok
    in
    let prepare ?(current_capabilities = fun () -> parent) bundle =
      G.prepare
        ~env
        ~dir
        ~revision_id
        ~created_at:timestamp
        ~current_capabilities
        ~references:(C.references parent)
        bundle
    in
    let prepared = prepare (bundle ()) |> get in
    assert (not (Store.exists artifact_store revision_id));
    let artifact = G.artifact prepared in
    assert (
      Option.is_none artifact.prompt_definition_id
      && Option.is_none artifact.canonical_source);
    [%test_eq: int] 2 artifact.runtime_schema_version;
    [%test_eq: int] 2 (List.length artifact.sources);
    [%test_eq: string list] [ "read_file" ] (List.map (G.capability_pins prepared) ~f:fst);
    G.install ~artifact_store ~transaction_id prepared |> get;
    G.install
      ~artifact_store
      ~transaction_id:(Agent_protocol.Id.Transaction.create ())
      prepared
    |> get;
    let reloaded_store =
      Store.create ~env ~root:(Filename.concat temporary "artifacts") |> store_ok
    in
    let fresh_parent = registry calls in
    let restore
          ?(current_capabilities = fun () -> fresh_parent)
          ?(pins = G.capability_pins prepared)
          ()
      =
      G.restore
        ~env
        ~artifact_store:reloaded_store
        ~revision_id
        ~manifest_sha256:artifact.manifest_sha256
        ~current_capabilities
        ~pins
        ()
    in
    let restored = restore () |> get in
    [%test_eq: string] artifact.manifest_sha256 (G.artifact restored).manifest_sha256;
    let admitted = G.admission restored in
    [%test_eq: int]
      1
      (List.length (Chat_response.Generated_admission.moderators admitted));
    assert (
      List.exists (Chat_response.Generated_admission.elements admitted) ~f:(function
        | CM.Config { model = Some "child-model"; reasoning_effort = Some "high"; _ } ->
          true
        | _ -> false));
    let selected = Chat_response.Generated_admission.capabilities admitted in
    [%test_eq: string list]
      [ "read_file" ]
      (List.map (C.references selected) ~f:(fun reference -> reference.name));
    let current =
      C.find fresh_parent ~name:"read_file"
      |> function
      | Ok value -> value
      | Error _ -> assert false
    in
    let restored_binding =
      C.find selected ~name:"read_file"
      |> function
      | Ok value -> value
      | Error _ -> assert false
    in
    assert (phys_equal current restored_binding);
    let changed = registry ~resources:"broader-reconfigured-root" calls in
    expect
      "delegation.capability_changed"
      (restore ~current_capabilities:(fun () -> changed) ());
    let wider =
      Chat_response.Background_request.capability_pins fresh_parent |> protocol_ok
    in
    expect "delegation.selection_changed" (restore ~pins:wider ());
    let conflicted =
      prepare (bundle ~instructions:"different immutable source" ()) |> get
    in
    expect
      "delegation.artifact_conflict"
      (G.install ~artifact_store ~transaction_id conflicted);
    let other_store =
      Store.create ~env ~root:(Filename.concat temporary "substituted") |> store_ok
    in
    G.install ~artifact_store:other_store ~transaction_id conflicted |> get;
    expect
      "delegation.artifact_identity"
      (G.restore
         ~env
         ~artifact_store:other_store
         ~revision_id
         ~manifest_sha256:artifact.manifest_sha256
         ~current_capabilities:(fun () -> fresh_parent)
         ~pins:(G.capability_pins prepared)
         ());
    [%test_eq: string]
      artifact.manifest_sha256
      (Store.load artifact_store revision_id |> store_ok).manifest_sha256;
    let definition =
      Agent_session.Prompt_definition.create
        ~root_invocation:Ui
        ~id:prompt_id
        ~config_name:"ordinary"
        ~root_file:(Filename.concat temporary "ordinary.chatmd")
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    assert (
      Result.is_error
        (Agent_session.Prompt_revision_builder.restore
           ~artifact_store
           definition
           revision_id));
    let tree = Store.materialized_tree artifact_store revision_id in
    let script = Eio.Path.(tree / "child/moderator.chatml") in
    Eio.Path.unlink script;
    Eio.Path.save ~create:(`Exclusive 0o600) script "let changed = true";
    expect "delegation.artifact" (restore ());
    [%test_eq: int] 0 !calls;
    print_s
      [%sexp
        "captured closure; one inherited binding; model retained; immutable retry; \
         changed authority/tree rejected; no initialization"]);
  [%expect
    {| "captured closure; one inherited binding; model retained; immutable retry; changed authority/tree rejected; no initialization" |}]
;;

let%expect_test "generated capture rejects a stale parent selection after compilation" =
  with_temp_directory (fun env temporary ->
    let calls = ref 0 in
    let first = registry calls in
    let second = registry calls in
    let reads = ref 0 in
    let current_capabilities () =
      Int.incr reads;
      if !reads = 1 then first else second
    in
    let result =
      G.prepare
        ~env
        ~dir:Eio.Path.(Eio.Stdenv.fs env / temporary)
        ~revision_id:(Agent_protocol.Id.Prompt_revision.create ())
        ~created_at:timestamp
        ~current_capabilities
        ~references:(C.references first)
        (bundle ())
    in
    assert (Result.is_error result);
    [%test_eq: int] 2 !reads;
    [%test_eq: int] 0 !calls;
    print_s
      [%sexp "equivalent re-registration still invalidates the captured live reference"]);
  [%expect
    {| "equivalent re-registration still invalidates the captured live reference" |}]
;;
