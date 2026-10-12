open Core
open Support
module P = Agent_protocol
module Fixture = Agent_server_test_support
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Actor = Agent_session.Session_actor
module Client = Agent_client
module Response = Openai.Responses.Response_stream

let call name id arguments =
  let item arguments status =
    Response.Item.Function_call
      { name; arguments; call_id = id; _type = "function_call"; id = Some id; status }
  in
  let arguments = Jsonaf.to_string arguments in
  [ Response.Output_item_added
      { item = item "" None; output_index = 0; type_ = "response.output_item.added" }
  ; Function_call_arguments_done
      { arguments
      ; item_id = id
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = item arguments (Some "completed")
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let answer text =
  [ Response.Output_item_done
      { item =
          Output_message
            { role = Assistant
            ; id = "persistent-answer"
            ; status = "completed"
            ; content = [ { annotations = []; text; _type = "output_text" } ]
            ; phase = None
            ; _type = "message"
            }
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let texts inputs =
  List.concat_map inputs ~f:(function
    | Openai.Responses.Item.Input_message message ->
      List.filter_map message.content ~f:(function
        | Text { text; _ } -> Some (String.strip text)
        | Image _ -> None)
    | _ -> [])
;;

let state daemon id =
  let entry = Registry.find (Daemon.registry daemon) id |> Option.value_exn in
  Actor.state entry.actor |> Fixture.protocol_ok
;;

let child label =
  sprintf
    {|<developer>child %s</developer>
<script id="reply" kind="shell_before_interceptor" language="chatml">
let initial_state = 0
let before event state = Task.bind(Intercept.respond("%s:" ++ to_string(state), ""), fun ignored -> Task.pure(state+1))
</script>
<shell_access id="private"><policy default="allow" merge="replace"/>
<interceptors><interceptor id="reply" phase="before" script="reply" lifecycle="session" failure="error"/></interceptors>
</shell_access>
<tool name="probe" type="shell" mode="fixed" runtime="private" command="/bin/echo" result="structured"/>|}
    label
    label
;;

let%expect_test "persistent File code and captured code retain original native authority" =
  with_workspace (fun env home root ->
    save
      root
      "root.chatmd"
      {|<developer>persistent parent</developer>
<tool name="fresh" agent="child.chatmd" local persistence="optional"/>
<tool name="captured" agent="child.chatmd" local persistence="optional" source_mode="captured"/>|};
    save root "child.chatmd" (child "A");
    let phase = ref 0 in
    let calls = ref 0 in
    let selected = ref ("fresh", "B", 0, None) in
    let create_call = ref "" in
    let provider ~sw:_ ~inputs =
      incr calls;
      let tool, label, counter, existing = !selected in
      let serialized =
        List.map inputs ~f:(fun item ->
          Openai.Responses.Item.jsonaf_of_t item |> Jsonaf.to_string)
        |> String.concat ~sep:"\n"
      in
      let contains text = String.is_substring serialized ~substring:text in
      match !phase with
      | 0 ->
        assert (List.mem (texts inputs) "persistent parent" ~equal:String.equal);
        phase := 1;
        create_call := sprintf "persistent-create-%d" !calls;
        call
          tool
          !create_call
          (`Object
              ([ "input", `String "Execute your probe."; "mode", `String "persistent" ]
               @ Option.to_list
                   (Option.map existing ~f:(fun id ->
                      "session_id", P.Id.Session.to_json id))))
      | 1 ->
        assert (List.mem (texts inputs) ("child " ^ label) ~equal:String.equal);
        phase := 2;
        call "probe" (sprintf "persistent-probe-%d" !calls) (`Object [])
      | 2 ->
        assert (contains (sprintf "%s:%d" label counter));
        phase := 3;
        answer ("completed-" ^ label)
      | 3 ->
        assert (contains ("completed-" ^ label));
        phase := 4;
        Stdlib.Seq.empty
      | _ -> failwith "unexpected persistent inference"
    in
    let with_daemon parent f =
      Eio.Switch.run (fun sw ->
        let daemon =
          Daemon.start
            ~sw
            ~env
            ~config:
              (Fixture.config
                 (Eio.Path.native_exn home)
                 (Eio.Path.native_exn root)
                 (Eio.Path.native_exn Eio.Path.(root / "root.chatmd")))
            ~tool_dir:(Eio.Path.native_exn root)
            ~home:(Eio.Path.native_exn home)
            ~process_start_identity:None
            ~options:
              { Daemon.default_options with
                startup_mode =
                  (match parent with
                   | None -> On_demand
                   | Some _ -> Execute)
              ; qualify_chatml_extensions = true
              ; inference_policy =
                  Fixture.inference_policy
                    ~default_model:"fixture-model"
                    ~post_stream:provider
              }
            ()
          |> Fixture.protocol_ok
        in
        Exn.protect
          ~finally:(fun () ->
            Eio.Cancel.protect (fun () -> Daemon.shutdown daemon |> Fixture.protocol_ok))
          ~f:(fun () ->
            let original =
              match parent with
              | None ->
                Agent_session.Prompt_revision.id (Daemon_history_tests.ready daemon)
              | Some (_, original) ->
                (match Catalog.entries (Daemon.prompts daemon) with
                 | [ { availability = Unavailable _; _ } ] -> ()
                 | _ -> failwith "missing child did not make current catalog unavailable");
                original
            in
            let connection = Fixture.connection daemon (Fixture.principal ()) in
            Fixture.initialize connection;
            let changed = Eio.Condition.create () in
            let on_update _ = Eio.Condition.broadcast changed in
            let handle =
              (match parent with
               | None ->
                 Client.Session_handle.create
                   ~sw
                   ~clock:(Eio.Stdenv.clock env)
                   ~connection
                   ~spec:(Fixture.session_spec ~start_immediately:true ())
                   ~mode:Read_write
                   ~on_update
                   ()
               | Some (session_id, _) ->
                 Client.Session_handle.attach
                   ~sw
                   ~clock:(Eio.Stdenv.clock env)
                   ~connection
                   ~session_id
                   ~mode:Read_write
                   ~on_update
                   ())
              |> Fixture.protocol_ok
            in
            Exn.protect
              ~finally:(fun () ->
                Client.Session_handle.close handle;
                Client.Connection.close connection)
              ~f:(fun () ->
                let run tool label counter existing =
                  phase := 0;
                  selected := tool, label, counter, existing;
                  Client.Session_handle.send_message
                    handle
                    { kind = Plain_text
                    ; text = "Call the selected specialist."
                    ; attachments = []
                    }
                  |> Fixture.protocol_ok
                  |> ignore;
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                    Eio.Condition.loop_no_mutex changed (fun () ->
                      let fields =
                        Client.Session_handle.projection handle
                        |> Client.Projection.snapshot
                        |> P.Public.Snapshot.fields
                      in
                      Option.iter fields.failure ~f:(fun error ->
                        raise_s [%sexp (error : P.Error.t)]);
                      if !phase = 4 && Option.is_none fields.session.active_operation
                      then Some ()
                      else None));
                  let parent = state daemon (Client.Session_handle.session_id handle) in
                  let invocation =
                    List.find_exn parent.invocations ~f:(fun invocation ->
                      Option.equal
                        String.equal
                        invocation.P.Invocation.context.provider_call_id
                        (Some !create_call))
                  in
                  let output =
                    match invocation.status with
                    | Published (Complete output) -> output
                    | _ -> raise_s [%sexp (invocation : P.Invocation.t)]
                  in
                  let id =
                    Jsonaf.member_exn "session_id" output
                    |> P.Id.Session.of_json
                    |> Fixture.protocol_ok
                  in
                  let snapshot = state daemon id in
                  assert (
                    List.exists snapshot.shell.extension_snapshots ~f:(fun snapshot ->
                      match snapshot.Session.Shell_state.Extension_snapshot.state with
                      | Record fields ->
                        (match List.Assoc.find fields "state" ~equal:String.equal with
                         | Some (Int value) -> Int.equal value (counter + 1)
                         | None | Some _ -> false)
                      | _ -> false));
                  assert (
                    P.Id.Prompt_revision.equal
                      original
                      (state daemon (Client.Session_handle.session_id handle)).spec
                        .prompt_revision_id);
                  id
                in
                f daemon handle original run)))
    in
    let parent, original, b, selected_revision =
      with_daemon None (fun daemon handle original run ->
        save root "child.chatmd" (child "B");
        let b = run "fresh" "B" 0 None in
        save root "child.chatmd" (child "C");
        let c = run "fresh" "C" 0 None in
        assert (not (P.Id.Session.equal b c));
        Eio.Path.unlink Eio.Path.(root / "child.chatmd");
        ignore (run "captured" "A" 0 None : P.Id.Session.t);
        [%test_eq: P.Id.Session.t] b (run "fresh" "B" 1 (Some b));
        [%test_eq: int] 16 !calls;
        ( Client.Session_handle.session_id handle
        , original
        , b
        , (state daemon b).spec.prompt_revision_id ))
    in
    with_daemon
      (Some (parent, original))
      (fun daemon _handle _original run ->
         [%test_eq: P.Id.Session.t] b (run "fresh" "B" 2 (Some b));
         assert (
           P.Id.Prompt_revision.equal
             selected_revision
             (state daemon b).spec.prompt_revision_id);
         [%test_eq: int] 20 !calls;
         assert (not (Eio.Path.is_file Eio.Path.(root / "child.chatmd"))));
    print_endline
      "File selects B/C; Captured retains A; warm/cold resume retains B without sources; \
       selected hook snapshots commit");
  [%expect
    {|File selects B/C; Captured retains A; warm/cold resume retains B without sources; selected hook snapshots commit|}]
;;
