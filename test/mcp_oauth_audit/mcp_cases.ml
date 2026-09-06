open Core
module JT = Mcp_types.Jsonrpc

type fixture =
  { client : Mcp_client.t
  ; incoming : (Jsonaf.t, exn) result Eio.Stream.t
  ; send : (Jsonaf.t -> unit) ref
  }

let request id = JT.make_request ~id:(JT.Id.of_int id) ~method_:"test" ()
let reply id value = JT.ok ~id:(JT.Id.of_int id) value |> JT.jsonaf_of_response
let pending fixture = Mcp_client.For_testing.pending_count fixture.client
let push fixture value = Eio.Stream.add fixture.incoming (Ok value)

let with_fixture _env f =
  Eio.Switch.run (fun sw ->
    let incoming = Eio.Stream.create 64 in
    let send = ref (fun (_ : Jsonaf.t) -> ()) in
    let recv () =
      match Eio.Stream.take incoming with
      | Ok json -> json
      | Error exn -> raise exn
    in
    let client =
      Mcp_client.For_testing.create
        ~sw
        ~recv
        ~send:(fun json -> !send json)
        ~close:(fun () -> ())
    in
    Fun.protect
      (fun () -> f { client; incoming; send })
      ~finally:(fun () -> Mcp_client.close client))
;;

let assert_drained fixture promises =
  let errors = List.map promises ~f:(fun p -> Eio.Promise.await p |> Fixture.error) in
  Fixture.check (pending fixture = 0) "pending RPC leaked";
  Fixture.check (Mcp_client.is_closed fixture.client) "client not terminal";
  Fixture.check
    (List.for_all errors ~f:(String.equal (List.hd_exn errors)))
    "pending requests received inconsistent terminal failures"
;;

let receive_failure exn env =
  with_fixture env (fun fixture ->
    let promises =
      List.init 3 ~f:(fun i -> Mcp_client.rpc_async fixture.client (request i))
    in
    Eio.Stream.add fixture.incoming (Error exn);
    assert_drained fixture promises;
    ignore (Mcp_client.rpc fixture.client (request 20) |> Fixture.error : string))
;;

let explicit_close env =
  with_fixture env (fun fixture ->
    let listed = Mcp_client.list_tools_async fixture.client in
    let called =
      Mcp_client.call_tool_async fixture.client ~name:"test" ~arguments:(`Object [])
    in
    Mcp_client.close fixture.client;
    Mcp_client.close fixture.client;
    Fixture.check
      (String.equal (Fixture.error (Eio.Promise.await listed)) "Connection_closed")
      "list_tools was not closed";
    Fixture.check
      (String.equal (Fixture.error (Eio.Promise.await called)) "Connection_closed")
      "call_tool was not closed";
    Fixture.check (pending fixture = 0) "explicit close leaked pending RPCs")
;;

let send_failure env =
  with_fixture env (fun fixture ->
    let first = Mcp_client.rpc_async fixture.client (request 1) in
    (fixture.send := fun _ -> failwith "send failed");
    let second = Mcp_client.rpc_async fixture.client (request 2) in
    assert_drained fixture [ first; second ])
;;

let send_cancellation env =
  with_fixture env (fun fixture ->
    let first = Mcp_client.rpc_async fixture.client (request 1) in
    let entered, enter = Eio.Promise.create () in
    let cancelled = ref false in
    (fixture.send
     := fun _ ->
          Eio.Promise.resolve enter ();
          Eio.Fiber.await_cancel ());
    Eio.Fiber.first
      (fun () ->
         try
           ignore (Mcp_client.rpc_async fixture.client (request 2) : _ Eio.Promise.t)
         with
         | Eio.Cancel.Cancelled _ as exn ->
           cancelled := true;
           raise exn)
      (fun () -> Eio.Promise.await entered);
    Fixture.check !cancelled "send cancellation swallowed";
    assert_drained fixture [ first ])
;;

let cancel_wait_and_late_reply env =
  with_fixture env (fun fixture ->
    let entered, enter = Eio.Promise.create () in
    (fixture.send := fun _ -> Eio.Promise.resolve enter ());
    let cancelled = ref false in
    Eio.Fiber.first
      (fun () ->
         try
           ignore (Mcp_client.rpc fixture.client (request 1) : (Jsonaf.t, string) result)
         with
         | Eio.Cancel.Cancelled _ as exn ->
           cancelled := true;
           raise exn)
      (fun () -> Eio.Promise.await entered);
    Fixture.check !cancelled "wait cancellation swallowed";
    Fixture.check (pending fixture = 0) "cancelled wait leaked request";
    (fixture.send := fun _ -> ());
    let next = Mcp_client.rpc_async fixture.client (request 2) in
    push fixture (reply 1 (`String "late"));
    push fixture (reply 2 (`String "current"));
    Fixture.check
      (Poly.equal (Eio.Promise.await next) (Ok (`String "current")))
      "late reply corrupted next request")
;;

let response_then_send_failure env =
  with_fixture env (fun fixture ->
    let first = Mcp_client.rpc_async fixture.client (request 1) in
    (fixture.send
     := fun _ ->
          push fixture (reply 1 (`String "completed"));
          ignore (Eio.Promise.await first : (Jsonaf.t, string) result);
          failwith "send failed after earlier response");
    let second = Mcp_client.rpc_async fixture.client (request 2) in
    ignore (Fixture.error (Eio.Promise.await second) : string);
    Fixture.check
      (Poly.equal (Eio.Promise.await first) (Ok (`String "completed")))
      "terminal drain overwrote completed result";
    Fixture.check (pending fixture = 0) "send failure left pending RPC")
;;

let stdio_failure mode env =
  Eio.Switch.run (fun sw ->
    let executable =
      if Filename.is_relative Sys_unix.executable_name
      then
        Filename.concat
          (Eio.Path.native_exn (Eio.Stdenv.cwd env))
          Sys_unix.executable_name
      else Sys_unix.executable_name
    in
    let uri = sprintf "stdio:%s --peer %s" executable mode in
    let client = Mcp_client.connect ~auth:false ~sw ~env uri in
    let listed = Mcp_client.list_tools_async client in
    let called = Mcp_client.call_tool_async client ~name:"test" ~arguments:(`Object []) in
    ignore (Fixture.error (Eio.Promise.await listed) : string);
    ignore (Fixture.error (Eio.Promise.await called) : string);
    Fixture.check (Mcp_client.For_testing.pending_count client = 0) "stdio pending leak";
    Mcp_client.close client)
;;

let http_close env =
  Loopback.with_server
    env
    (fun _ -> Loopback.json "{}")
    (fun sw issuer ->
       let transport =
         Mcp_transport_http.connect ~auth:false ~sw ~env (issuer ^ "/mcp")
       in
       let entered, enter = Eio.Promise.create () in
       let receiver =
         Eio.Fiber.fork_promise ~sw (fun () ->
           Eio.Promise.resolve enter ();
           try
             ignore (Mcp_transport_http.recv transport : Jsonaf.t);
             false
           with
           | Mcp_transport_http.Connection_closed -> true)
       in
       Eio.Promise.await entered;
       Mcp_transport_http.close transport;
       Fixture.check (Eio.Promise.await_exn receiver) "HTTP close did not wake recv")
;;

let cases =
  [ "mcp.receive-eof", receive_failure End_of_file
  ; "mcp.receive-error", receive_failure (Failure "receive failed")
  ; "mcp.explicit-close", explicit_close
  ; "mcp.send-error", send_failure
  ; "mcp.send-cancellation", send_cancellation
  ; "mcp.wait-cancellation-late-reply", cancel_wait_and_late_reply
  ; "mcp.response-before-send-error", response_then_send_failure
  ; "mcp.stdio-eof", stdio_failure "eof"
  ; "mcp.stdio-invalid-json", stdio_failure "invalid"
  ; "mcp.http-close-wakes-receiver", http_close
  ]
;;
