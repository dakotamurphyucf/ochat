open Core

let with_client f =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let uri = "stdio:mcp_stub_server" in
      let client = Mcp_client.connect ~sw ~env ~uri in
      Fun.protect ~finally:(fun () -> Mcp_client.close client) (fun () -> f client)))
;;

let%expect_test "list_tools (sync + async)" =
  with_client (fun client ->
    (* synchronous helper *)
    let tools = Result.ok_or_failwith (Mcp_client.list_tools client) in
    print_s [%sexp (List.map tools ~f:(fun t -> t.name) : string list)];
    (* async helper should resolve to the same result *)
    let promise = Mcp_client.list_tools_async client in
    let async_tools = Eio.Promise.await promise |> Result.ok_or_failwith in
    print_s [%sexp (List.map async_tools ~f:(fun t -> t.name) : string list)]);
  [%expect "\n    (echo reverse add)\n    (echo reverse add)\n  "]
;;

let%expect_test "call_tool echo (sync + async)" =
  with_client (fun client ->
    let args = `Object [ "value", `String "hi" ] in
    let res =
      Result.ok_or_failwith (Mcp_client.call_tool client ~name:"echo" ~arguments:args)
    in
    print_s [%sexp (res : Mcp_types.Tool_result.t)];
    let p = Mcp_client.call_tool_async client ~name:"echo" ~arguments:args in
    let res2 = Eio.Promise.await p |> Result.ok_or_failwith in
    print_s [%sexp (res2 : Mcp_types.Tool_result.t)]);
  [%expect
    {|
((content ((Text hi))) (is_error false))
((content ((Text hi))) (is_error false))
|}]
;;

let%expect_test "unknown tool error" =
  with_client (fun client ->
    let args = `Object [] in
    match Mcp_client.call_tool client ~name:"does_not_exist" ~arguments:args with
    | Ok _ -> print_endline "unexpected ok"
    | Error e -> print_endline ("error:" ^ e));
  [%expect {| error:RPC error -32000 – unknown tool |}]
;;

let%expect_test "parallel async calls" =
  with_client (fun client ->
    (* run concurrent reverse calls *)
    let inputs = [ "one"; "two"; "three"; "four"; "five" ] in
    let promises =
      List.map inputs ~f:(fun v ->
        Mcp_client.call_tool_async
          client
          ~name:"reverse"
          ~arguments:(`Object [ "value", `String v ]))
    in
    let outputs =
      Eio.Fiber.List.map (fun p -> Eio.Promise.await p |> Result.ok_or_failwith) promises
    in
    print_s [%sexp (outputs : Mcp_types.Tool_result.t list)]);
  [%expect
    {|
(((content ((Text eno))) (is_error false))
 ((content ((Text owt))) (is_error false))
 ((content ((Text eerht))) (is_error false))
 ((content ((Text ruof))) (is_error false))
 ((content ((Text evif))) (is_error false)))
|}]
;;
