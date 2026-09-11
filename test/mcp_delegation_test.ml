open Core
module A = Chat_response.Agent_runtime
module C = Chat_response.Tool_capability
module B = Chat_response.Background_request
module G = Chat_response.Generated_admission
module CM = Prompt.Chat_markdown

let runtime = function
  | Ok value -> value
  | Error errors ->
    failwith (List.map errors ~f:A.diagnostic_to_string |> String.concat ~sep:"\n")
;;

let caps = function
  | Ok value -> value
  | Error error -> failwith error.C.message
;;

let%expect_test "inherited MCP identity and live catalog cannot silently change" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = Core_unix.mkdtemp "/tmp/ochat-mcp-delegation.XXXXXX" in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        List.iter [ "a"; "b" ] ~f:(fun name ->
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / name);
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(path / name / "catalog")
            "original");
        let outcome =
          Eio.Switch.run (fun sw ->
            Result.try_with (fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                let create endpoint =
                  let peer =
                    Filename.concat (Core_unix.getcwd ()) "mcp_delegation_peer.exe"
                  in
                  let source =
                    sprintf {|<tool mcp_server="stdio:%s %s/%s"/>|} peer root endpoint
                  in
                  let prompt_elements =
                    CM.parse_chat_inputs ~source:"parent.chatmd" ~dir:path source
                  in
                  let ctx =
                    Chat_response.Ctx.create
                      ~env
                      ~dir:path
                      ~tool_dir:path
                      ~cache:(Chat_response.Cache.create ~max_size:1 ())
                  in
                  let host =
                    A.host
                      ~env
                      ~workspace:path
                      ~tool_dir:path
                      ~prompt_dir:path
                      ~session_dir:path
                      ~cache_dir:path
                      ~home:path
                      ~session_id:"parent"
                      ~resource_runner:None
                      ~prompt_elements
                    |> runtime
                  in
                  A.create
                    ~sw
                    ~ctx
                    ~host
                    ~platform:(A.platform ())
                    ~prompt_elements
                    ~manifest_authorizer:
                      Shell_runtime.Manifest_authorizer.assume_authorized
                    ~approval_provider:Shell_runtime.Approval_broker.None_available
                    ~approval_store:(Shell_access.Approval.create_store ())
                    ~run_agent:
                      (fun
                        ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
                      failwith "unexpected model")
                    ()
                  |> runtime
                in
                let parent = create "a" in
                let public = Lazy.force parent.capabilities |> caps in
                let bundle =
                  Chatmd_source_bundle.create
                    ~root_file:"child.chatmd"
                    ~sources:
                      [ ( "child.chatmd"
                        , {|<developer>Child.</developer><tool type="inherited" name="echo"/>|}
                        )
                      ]
                    ()
                  |> Result.ok_or_failwith
                in
                let admitted =
                  G.prepare
                    ~env
                    ~dir:path
                    ~ceiling:public
                    ~requested_names:[ "echo" ]
                    bundle
                  |> Result.map_error ~f:(fun errors ->
                    List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
                    |> String.concat ~sep:"\n")
                  |> Result.ok_or_failwith
                in
                let selected = G.capabilities admitted in
                let pins =
                  B.capability_pins selected
                  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
                  |> Result.ok_or_failwith
                in
                let child =
                  A.inherit_native ~parent ~capabilities:selected () |> runtime
                in
                [%test_eq: string list]
                  [ "echo" ]
                  (List.map child.functions ~f:(fun fn ->
                     fn.Ochat_function.info.function_.name));
                assert (
                  phys_equal
                    (C.find selected ~name:"echo" |> caps)
                    (C.find public ~name:"echo" |> caps));
                let echo = List.hd_exn child.functions in
                let run () =
                  match echo.run "{}" with
                  | Text "peer-result" -> ()
                  | _ -> failwith "wrong endpoint result"
                in
                run ();
                let other = create "b" in
                assert (
                  Result.is_error
                    (B.rebind_capabilities
                       ~pins
                       ~capabilities:(Lazy.force other.capabilities |> caps)));
                let change =
                  List.find_exn parent.functions ~f:(fun fn ->
                    String.equal fn.info.function_.name "change_catalog")
                in
                let mutate mode =
                  change.run (Jsonaf.to_string (`Object [ "mode", `String mode ]))
                  |> ignore;
                  (* Let the declaration's notification consumer process the delivered
               invalidation before probing the already-advertised inherited tool. *)
                  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01
                in
                let denied invoke =
                  match invoke "{}" with
                  | _ -> failwith "changed MCP catalog reached tools/call"
                  | exception Failure message ->
                    [%test_eq: string]
                      "mcp.catalog_changed: reload the tool definition before calling it"
                      message
                in
                mutate "schema";
                denied
                  (echo.run_with_progress ~invocation:Ochat_function.Invocation.silent);
                let changed = create "a" in
                assert (
                  Result.is_error
                    (B.rebind_capabilities
                       ~pins
                       ~capabilities:(Lazy.force changed.capabilities |> caps)));
                mutate "removed";
                denied echo.run;
                mutate "original";
                run ();
                let restored = create "a" in
                let rebound =
                  B.rebind_capabilities
                    ~pins
                    ~capabilities:(Lazy.force restored.capabilities |> caps)
                  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
                  |> Result.ok_or_failwith
                in
                assert (
                  not
                    (phys_equal
                       (C.find rebound ~name:"echo" |> caps)
                       (C.find selected ~name:"echo" |> caps)));
                [%test_eq: string list]
                  [ "echo"; "change_catalog"; "change_catalog"; "change_catalog"; "echo" ]
                  (In_channel.read_lines (Filename.concat root "a/calls"));
                print_endline
                  "original MCP runner inherited; endpoint/schema substitutions reject; \
                   invalidated and removed tools never execute; matching reconnect \
                   rebinds")))
        in
        match outcome with
        | Ok () -> ()
        | Error exn -> raise exn));
  [%expect
    {| original MCP runner inherited; endpoint/schema substitutions reject; invalidated and removed tools never execute; matching reconnect rebinds |}]
;;
