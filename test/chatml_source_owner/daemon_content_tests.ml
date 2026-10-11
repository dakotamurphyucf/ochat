open Core
open Support
module P = Agent_protocol
module Fixture = Agent_server_test_support
module Daemon = Agent_server.Daemon
module Revision = Agent_session.Prompt_revision

let definition = Agent_server.Catalog_identity.prompt_definition "restart.prompt"

let with_daemon env parent root f =
  Eio.Switch.run (fun sw ->
    let home = Eio.Path.native_exn parent in
    let workspace = Eio.Path.native_exn root in
    let daemon =
      Daemon.start
        ~sw
        ~env
        ~config:(Fixture.config home workspace (Filename.concat workspace "root.chatmd"))
        ~tool_dir:workspace
        ~home
        ~process_start_identity:None
        ~options:
          { Daemon.default_options with
            startup_mode = On_demand
          ; inference_policy =
              Fixture.inference_policy
                ~default_model:"fixture-model"
                ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "unexpected model dispatch")
          }
        ()
      |> Fixture.protocol_ok
    in
    Fun.protect
      ~finally:(fun () -> Daemon.shutdown daemon |> Fixture.protocol_ok)
      (fun () ->
         let connection = Fixture.connection daemon (Fixture.principal ()) in
         Fixture.initialize connection;
         f daemon connection))
;;

let snapshot connection session_id =
  match
    Agent_client.Connection.request
      connection
      (Session_get { session_id; history = None })
    |> Fixture.protocol_ok
  with
  | Session_get snapshot -> P.Public.Snapshot.fields snapshot
  | _ -> failwith "expected actual session snapshot"
;;

let literals revision =
  List.concat_map (Revision.elements revision) ~f:(function
    | Prompt.Chat_markdown.Developer { content = Some (Items items); _ } ->
      List.filter_map items ~f:(function
        | Basic { text = Some text; document_url = None; _ } -> Some text
        | _ -> None)
    | _ -> [])
;;

let developer_text entries =
  List.concat_map entries ~f:(fun entry ->
    let payload = P.Public_history.full_payload entry |> Option.value_exn in
    match History_entry.Payload.(Semantic.view (semantic payload)) with
    | Message { role = Developer; content; _ } ->
      List.map content ~f:(function
        | History_entry.Payload.Content.Text { text; _ } -> text
        | Refusal _ | Image _ | Unknown _ -> failwith "expected literal developer text")
    | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ -> [])
;;

let%expect_test "durable daemon history retains captured local and builtin documents" =
  with_workspace (fun env parent root ->
    save
      root
      "root.chatmd"
      {|<developer><doc src="guide.md" local capture/></developer>
<developer><doc src="builtin:chatml"/></developer>|};
    save root "guide.md" "original guide";
    let builtin =
      Builtin_instructions.Id.of_string "chatml"
      |> Or_error.ok_exn
      |> Builtin_instructions.find
      |> ok
      |> Builtin_instructions.Asset.contents
    in
    let session_id, revision_id, original_history =
      with_daemon env parent root (fun daemon connection ->
        let revision = Daemon_history_tests.ready daemon in
        [%test_eq: string list] [ "original guide"; builtin ] (literals revision);
        let session, _ = Fixture.create_session connection in
        let history = (snapshot connection session.id).canonical_history in
        [%test_eq: string list]
          [ "original guide"; builtin ]
          (developer_text history.entries);
        assert (history.reached_start && history.reached_end);
        session.id, Revision.id revision, history.entries)
    in
    let check_retained daemon connection =
      let current = snapshot connection session_id in
      assert (P.Session.equal_desired_state current.session.desired_state Stopped);
      assert (Option.is_none current.session.active_operation);
      assert (
        List.equal
          P.Public_history.equal
          original_history
          current.canonical_history.entries);
      let retained =
        Catalog.restore_revision
          (Daemon.prompts daemon)
          ~definition_id:definition
          ~revision_id
        |> ok
      in
      [%test_eq: string list] [ "original guide"; builtin ] (literals retained)
    in
    save root "guide.md" "edited guide";
    with_daemon env parent root (fun daemon connection ->
      let fresh = Daemon_history_tests.ready daemon in
      assert (not (P.Id.Prompt_revision.equal revision_id (Revision.id fresh)));
      [%test_eq: string list] [ "edited guide"; builtin ] (literals fresh);
      check_retained daemon connection);
    Eio.Path.unlink Eio.Path.(root / "guide.md");
    with_daemon env parent root (fun daemon connection ->
      (match Catalog.find (Daemon.prompts daemon) definition with
       | Some { availability = Unavailable _; _ } -> ()
       | _ -> failwith "missing current document must prevent fresh capture");
      check_retained daemon connection);
    print_endline
      "fresh catalog sees edits; saved history and content survive restart and deletion");
  [%expect
    {| fresh catalog sees edits; saved history and content survive restart and deletion |}]
;;
