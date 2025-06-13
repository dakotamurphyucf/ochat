open Core

(* Integration test that starts the real Streamable-HTTP MCP server in-process
   and exercises it through the high-level [Mcp_client] API.  The server is
   initialised with a single minimal "echo" tool that echoes back the string
   passed in the [value] argument.  The test validates the following flow:

   1.  The client completes the initialise / initialized handshake.
   2.  The "echo" tool appears in the result of [tools/list].
   3.  Invoking the tool returns the expected payload.

   We use a random port in the range 9000-9999 to avoid clashes with other
   services that may be running on the CI machine.  All fibres are spawned
   under a single switch so that cancelling it at the end terminates the
   embedded server cleanly. *)

module JT = Mcp_types

exception Completed of string

let random_port () = 9000 + Random.int 1000

let make_echo_tool () : JT.Tool.t * Mcp_server_core.tool_handler =
  let spec : JT.Tool.t =
    { name = "echo"
    ; description = Some "Echo back the given value"
    ; input_schema =
        Jsonaf.of_string
          "{\"type\":\"object\",\"properties\":{\"value\":{\"type\":\"string\"}},\"required\":[\"value\"]}"
    }
  in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match Jsonaf.member "value" args with
    | Some (`String s) -> Ok (`String s)
    | _ -> Error "Missing or invalid 'value' field"
  in
  spec, handler
;;

let%expect_test "MCP HTTP server end-to-end" =
  (* Use a deterministic seed so that the port number is reproducible in the
     expectation output. *)
  let port = random_port () in
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Build server core and register the echo tool. *)
    let core = Mcp_server_core.create () in
    let spec, handler = make_echo_tool () in
    Mcp_server_core.register_tool core spec handler;
    (* Launch the HTTP server in a background fibre. *)
    Eio.Fiber.fork ~sw (fun () ->
      Mcp_server_http.run ~require_auth:false ~env ~core ~port);
    (* Connect the client. *)
    (* Provide dev credentials via environment variables so the HTTP
       transport can obtain an access token through the /token endpoint. *)
    let uri = sprintf "http://127.0.0.1:%d/mcp" port in
    let client = Mcp_client.connect ~auth:false ~sw ~env uri in
    (* 1) list tools *)
    let tools =
      match Mcp_client.list_tools client with
      | Ok l -> l
      | Error msg -> failwith msg
    in
    let tool_names =
      List.map tools ~f:(fun t -> t.JT.Tool.name) |> String.concat ~sep:","
    in
    printf "tools: %s\n" tool_names;
    (* 2) call echo tool *)
    let args = `Object [ "value", `String "hi" ] in
    let result =
      match Mcp_client.call_tool client ~name:"echo" ~arguments:args with
      | Ok r -> r
      | Error msg -> failwith msg
    in
    (match result.content with
     | JT.Tool_result.Json (`String s) :: _ -> printf "echo returned: %s\n" s
     | JT.Tool_result.Text s :: _ -> printf "echo returned: %s\n" s
     | _ -> printf "unexpected tool result\n");
    (* Shut down. *)
    Mcp_client.close client;
    (* Cancel the switch so that the server fibre terminates and the test can
     exit. *)
    Eio.Switch.fail sw (Completed "done")
  with
  | Completed _ ->
    ();
    [%expect
      {|
    tools: echo
    echo returned: hi
    |}]
;;
