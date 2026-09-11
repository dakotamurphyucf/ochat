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
          ; authority_sha256 = digest "host admission"
          ; capability_pins = G.capability_pins prepared
          ; lifetime = Owned
          ; created_at
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
        | New record -> record
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
      let _ = D.revoke delegations reservation Parent_stopped |> store_ok in
      G.install_reserved ~delegations ~reservation ~artifact_store prepared
      |> expect "delegation.revoked";
      let _ = Store.load artifact_store revision_id |> store_ok in
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
