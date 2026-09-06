open! Core
module CM = Prompt.Chat_markdown
module Event = Chat_response.Tool_execution_event
module Tool = Chat_response.Tool

let%expect_test "MCP discovery caches isolate identities, invalidation and expiry" =
  Eio_main.run (fun _ ->
    let now = ref 0. in
    let count_a = ref 0
    and count_b = ref 0 in
    let cache identity count =
      Chat_response.Mcp_discovery_cache.create
        ~now:(fun () -> !now)
        ~load:(fun () ->
          Int.incr count;
          [ identity ])
    in
    let a = cache "identity-a-private-tool" count_a in
    let b = cache "identity-b-private-tool" count_b in
    assert (
      List.equal
        String.equal
        (Chat_response.Mcp_discovery_cache.get a)
        [ "identity-a-private-tool" ]);
    assert (
      List.equal
        String.equal
        (Chat_response.Mcp_discovery_cache.get b)
        [ "identity-b-private-tool" ]);
    Chat_response.Mcp_discovery_cache.invalidate a;
    ignore (Chat_response.Mcp_discovery_cache.get a : string list);
    ignore (Chat_response.Mcp_discovery_cache.get b : string list);
    assert (!count_a = 2 && !count_b = 1);
    now := 301.;
    ignore (Chat_response.Mcp_discovery_cache.get b : string list);
    assert (!count_b = 2);
    print_endline "isolated catalogs; invalidation local; Eio-clock expiry reloads");
  [%expect {| isolated catalogs; invalidation local; Eio-clock expiry reloads |}]
;;

let%expect_test "failed and cancelled MCP discovery loads do not populate the cache" =
  Eio_main.run (fun env ->
    let attempts = ref 0 in
    let cache =
      Chat_response.Mcp_discovery_cache.create
        ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock env))
        ~load:(fun () ->
          Int.incr attempts;
          match !attempts with
          | 1 -> failwith "discovery fixture failure"
          | 2 -> Eio.Fiber.await_cancel ()
          | _ -> [ "recovered-tool" ])
    in
    assert (
      Result.is_error
        (Or_error.try_with (fun () -> Chat_response.Mcp_discovery_cache.get cache)));
    (match
       Eio.Time.with_timeout (Eio.Stdenv.clock env) 0.01 (fun () ->
         Ok (Chat_response.Mcp_discovery_cache.get cache))
     with
     | Error `Timeout -> ()
     | Ok _ -> failwith "blocked discovery did not time out");
    assert (
      List.equal
        String.equal
        (Chat_response.Mcp_discovery_cache.get cache)
        [ "recovered-tool" ]);
    ignore (Chat_response.Mcp_discovery_cache.get cache : string list);
    assert (!attempts = 3);
    print_endline "failure/cancellation release mutex; only successful result cached");
  [%expect {| failure/cancellation release mutex; only successful result cached |}]
;;

let source =
  let position : Chatmd_shell_spec.Source_ref.position =
    { offset = 0; line = 1; column = 1 }
  in
  Chatmd_shell_spec.Source_ref.create
    ~file:"test.chatmd"
    ~source_dir:"."
    ~prompt_dir:"."
    ~namespace:None
    ~start_pos:position
    ~end_pos:position
    ~source:""
;;

let classification = function
  | None -> "hidden"
  | Some (name, Event.Subagent) -> name ^ ":agent"
  | Some (name, Event.Shell_script) -> name ^ ":shell"
;;

let%expect_test "Agent page includes subagents and manifest shell tools" =
  [ CM.Agent
      { name = "research"
      ; description = None
      ; agent = "researcher.chatmd"
      ; is_local = true
      }
  ; CM.Custom { name = "legacy"; description = None; command = "printf"; source }
  ; CM.Builtin "read_file"
  ; CM.Builtin "fork"
  ]
  |> List.map ~f:(fun declaration ->
    Tool.agent_page_classification declaration |> classification)
  |> List.iter ~f:print_endline;
  [%expect
    {|
    research:agent
    legacy:shell
    hidden
    fork:agent
    |}]
;;
