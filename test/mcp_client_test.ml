open Core

let with_test_client f =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let stub_dir = Filename.dirname Sys.(get_argv ()).(0) in
      let stub_path = Filename.concat stub_dir "mcp_stub_server.exe" in
      let uri = "stdio:" ^ stub_path in
      let client = Mcp_client.connect ~sw ~env ~uri in
      Fun.protect ~finally:(fun () -> Mcp_client.close client) (fun () -> f client)))
;;

let%expect_test "list tools + call echo" =
  with_test_client (fun client ->
    let tools =
      match Mcp_client.list_tools client with
      | Ok xs -> xs
      | Error e ->
        print_endline @@ Printf.sprintf "list_tools error: %s" e;
        []
    in
    print_s [%sexp (List.map tools ~f:(fun t -> t.Mcp_types.Tool.name) : string list)];
    let arguments = `Object [ "value", `String "hello" ] in
    match Mcp_client.call_tool client ~name:"echo" ~arguments with
    | Ok r -> print_s [%sexp (r : Mcp_types.Tool_result.t)]
    | Error e -> print_endline (Printf.sprintf "call_tool error: %s" e));
  [%expect
    {|
    (echo)
    ((content ((Text "hello"))) (is_error false))
    |}]
;;
