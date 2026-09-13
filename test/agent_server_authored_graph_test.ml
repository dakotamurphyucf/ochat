open Core
open Agent_server_test_support
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module Owner = Agent_server.Runtime_owner
module H = Agent_client.Session_handle
module Res = Openai.Responses

let state entry = A.state entry.R.actor |> protocol_ok
let id entry = (state entry).identity.session_id
let member json name = Jsonaf.member_exn name json
let session_id json = member json "session_id" |> P.Id.Session.of_json |> protocol_ok

let tools entry =
  Owner.with_background_runtime entry.R.runtime (fun runtime ->
    let native = Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime in
    Lazy.force native.capabilities
    |> Result.map ~f:(fun capabilities ->
      Chat_response.Tool_capability.references capabilities
      |> List.map ~f:(fun reference -> reference.Chat_response.Tool_capability.name)
      |> List.sort ~compare:String.compare)
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request error.Chat_response.Tool_capability.message))
  |> protocol_ok
;;

let function_call serial name arguments =
  let call_id = sprintf "graph-%d" serial in
  let open Res.Response_stream in
  [ Output_item_added
      { item =
          Function_call
            { name
            ; arguments = ""
            ; call_id
            ; _type = "function_call"
            ; id = Some call_id
            ; status = None
            }
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = Jsonaf.to_string arguments
      ; item_id = call_id
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let%expect_test "nested authored bindings survive independent ancestry stop and restart" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let save name text =
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / name)
            text
        in
        save
          "parent.chatmd"
          {|<developer>GRAPH_ROOT</developer><tool name="branch" agent="middle.chatmd" local persistence="persistent"/>|};
        save
          "middle.chatmd"
          {|<developer>GRAPH_MIDDLE</developer><tool name="leaf" agent="leaf.chatmd" local persistence="persistent"/>|};
        save
          "leaf.chatmd"
          {|<developer>GRAPH_LEAF</developer><tool name="read_file"><read id="private" path="${workspace}"/></tool>|};
        save "value.txt" "captured-private-read";
        let configuration = config root root (Filename.concat root "parent.chatmd") in
        let requests = ref 0 in
        let continuation = ref None in
        let provider ~sw:_ ~inputs =
          Int.incr requests;
          match List.last inputs with
          | Some (Res.Item.Function_call_output _) -> Stdlib.Seq.empty
          | _ ->
            let marker =
              List.find_map_exn inputs ~f:(function
                | Res.Item.Input_message message ->
                  let json = Res.Item.jsonaf_of_t (Input_message message) in
                  (match
                     String.equal (member json "role" |> Jsonaf.string_exn) "developer"
                   with
                   | true -> Some (Jsonaf.to_string json)
                   | false -> None)
                | _ -> None)
            in
            let input = [ "input", `String "Use the captured specialist." ] in
            let name, args =
              if String.is_substring marker ~substring:"GRAPH_ROOT"
              then "branch", `Object input
              else if String.is_substring marker ~substring:"GRAPH_LEAF"
              then
                ( "read_file"
                , `Object [ "root", `String "private"; "file", `String "value.txt" ] )
              else (
                let fields =
                  match !continuation with
                  | None -> input
                  | Some child -> input @ [ "session_id", P.Id.Session.to_json child ]
                in
                continuation := None;
                "leaf", `Object fields)
            in
            function_call !requests name args
        in
        let find daemon child = R.load (D.registry daemon) child |> protocol_ok in
        let with_daemon f =
          Eio.Switch.run (fun sw ->
            let before = !requests in
            let daemon =
              D.start
                ~sw
                ~env
                ~config:configuration
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { D.default_options with
                    qualify_chatml_extensions = true
                  ; independent_lifetime_policy = Some "authored-graph-v1"
                  ; model_post_stream = Some provider
                  }
                ()
              |> protocol_ok
            in
            [%test_eq: int] before !requests;
            Exn.protect
              ~finally:(fun () -> D.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      f sw daemon client))))
        in
        let with_handle sw client entry f =
          let handle =
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client
              ~session_id:(id entry)
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok
          in
          Exn.protect ~finally:(fun () -> H.close handle) ~f:(fun () -> f handle)
        in
        let rec idle entry =
          match (state entry).active_operation with
          | None -> state entry
          | Some _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            idle entry
        in
        let model_result entry ~before =
          let invocation =
            List.find_exn (state entry).invocations ~f:(fun invocation ->
              P.Invocation.equal_origin invocation.context.origin Model
              && not
                   (List.exists before ~f:(fun old ->
                      P.Id.Invocation.equal
                        old.P.Invocation.context.id
                        invocation.context.id)))
          in
          match invocation.status with
          | Published (Complete value) -> value
          | status -> raise_s [%sexp "graph call failed", (status : P.Invocation.status)]
        in
        let invoke sw client entry =
          let before = (state entry).invocations in
          with_handle sw client entry (fun handle ->
            H.send_message
              handle
              { kind = Plain_text; text = "Run the tool."; attachments = [] }
            |> protocol_ok
            |> ignore;
            ignore (idle entry : Agent_session.Session_state.t));
          model_result entry ~before
        in
        let check_read entry ~before =
          assert (
            String.is_substring
              (Jsonaf.to_string (model_result entry ~before))
              ~substring:"captured-private-read")
        in
        let stopped entry =
          match (state entry).lifecycle.desired, (state entry).lifecycle.observed with
          | Stopped, Stopped -> ()
          | _ -> failwith "owned ancestor was not stopped"
        in
        let running entry =
          match (state entry).lifecycle.desired, (state entry).lifecycle.observed with
          | Running, Idle -> ()
          | _ -> failwith "independent branch was not running"
        in
        let parent_id, middle_id, independent_id, leaf_id =
          with_daemon (fun sw daemon client ->
            let parent, _ = create_session ~start_immediately:true client in
            let parent = find daemon parent.id in
            let middle = invoke sw client parent |> session_id |> find daemon in
            [%test_eq: string list] [ "branch" ] (tools parent);
            [%test_eq: string list] [ "leaf" ] (tools middle);
            let first_leaf =
              model_result middle ~before:[] |> session_id |> find daemon
            in
            check_read first_leaf ~before:[];
            [%test_eq: string list] [ "read_file" ] (tools first_leaf);
            let definition =
              Owner.with_background_runtime middle.runtime (fun runtime ->
                let native =
                  Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
                in
                let capabilities =
                  Lazy.force native.capabilities
                  |> Result.map_error ~f:(fun error ->
                    P.Error.invalid_request error.Chat_response.Tool_capability.message)
                in
                let open Result.Let_syntax in
                let%bind capabilities = capabilities in
                let bundle =
                  Chatmd_source_bundle.create
                    ~root_file:"independent.chatmd"
                    ~sources:
                      [ ( "independent.chatmd"
                        , {|<developer>GRAPH_INDEPENDENT</developer><tool type="inherited" name="leaf"/>|}
                        )
                      ]
                    ()
                  |> Result.ok_or_failwith
                in
                Agent_session.Generated_definition.prepare
                  ~env
                  ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
                  ~revision_id:(P.Id.Prompt_revision.create ())
                  ~created_at:(P.Timestamp.now ())
                  ~current_capabilities:(fun () -> capabilities)
                  ~references:(Chat_response.Tool_capability.references capabilities)
                  bundle
                |> Result.map_error ~f:(fun errors ->
                  P.Error.invalid_request
                    (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
                     |> String.concat ~sep:"\n")))
              |> protocol_ok
            in
            let independent =
              Agent_server.Session_factory.create_generated_session
                ~start_immediately:true
                ~lifetime:Independent
                (D.factory daemon)
                ~parent_session_id:(id middle)
                ~idempotency_key:
                  (P.Idempotency_key.of_string "nested-independent" |> protocol_ok)
                ~display_name:None
                definition
              |> protocol_ok
            in
            with_handle sw client parent (fun handle ->
              H.stop handle ~mode:Cancel |> protocol_ok |> ignore);
            stopped parent;
            stopped middle;
            stopped first_leaf;
            assert (not (Owner.is_loaded parent.runtime));
            assert (not (Owner.is_loaded middle.runtime));
            running independent;
            let leaf = invoke sw client independent |> session_id |> find daemon in
            check_read leaf ~before:[];
            assert (not (P.Id.Session.equal (id leaf) (id first_leaf)));
            id parent, id middle, id independent, id leaf)
        in
        save
          "leaf.chatmd"
          "<developer>Changed live source without private tools.</developer>";
        with_daemon (fun sw daemon client ->
          let parent = find daemon parent_id in
          let middle = find daemon middle_id in
          let independent = find daemon independent_id in
          stopped parent;
          stopped middle;
          running independent;
          let leaf = find daemon leaf_id in
          let before = (state leaf).invocations in
          continuation := Some leaf_id;
          let continued = invoke sw client independent |> session_id in
          assert (P.Id.Session.equal leaf_id continued);
          check_read leaf ~before;
          stopped parent;
          stopped middle;
          assert (not (Owner.is_loaded parent.runtime));
          assert (not (Owner.is_loaded middle.runtime)));
        print_endline
          "nested private bindings: call, stopped ancestry, independent continuation, \
           pinned restart"));
  [%expect
    {| nested private bindings: call, stopped ancestry, independent continuation, pinned restart |}]
;;
