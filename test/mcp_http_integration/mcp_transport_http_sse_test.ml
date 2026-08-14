open Core

(** Additional edge-case tests for the HTTP transport:
    1. Server-Sent Events (SSE) streaming – ensure that [Mcp_transport_http.recv]
       yields every event in order.
    2. Session-ID propagation – server issues an [Mcp-Session-Id] on the first
       response; subsequent requests must include that header.

    We spin up an in-process Piaf server inside the test switch.  The server
    recognises two RPC methods:

    • "initialize"  – replies via SSE stream with two JSON-RPC responses and
                       sets the [Mcp-Session-Id] header to "sid123".

    • "ping"        – normal JSON (application/json) response containing a
                       boolean field [has_sid] that reflects whether the
                       request carried the session header.

    The test asserts that:      
      – both SSE messages are delivered in order;             
      – the second request includes the session header.       
*)

module Http = Mcp_transport_http

exception Completed of string

let port = 8125

(* -------------------------------------------------------------------------- *)
(* Minimal stub server                                                          *)
(* -------------------------------------------------------------------------- *)

let first_request_handled = ref false

let sse_body () : Piaf.Body.t =
  (* Two JSON-RPC responses as separate SSE events. *)
  let event json = sprintf "data: %s\n\n" json in
  let ev1 = event {|{"jsonrpc":"2.0","id":1,"result":{"x":1}}|} in
  let ev2 = event {|{"jsonrpc":"2.0","id":1,"result":{"x":2}}|} in
  let s = Piaf.Stream.of_list [ ev1; ev2 ] in
  let body = Piaf.Body.of_string_stream s in
  Piaf.Stream.close s;
  body
;;

let start_stub_server ~sw env =
  let open Piaf in
  let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let config = Server.Config.create address in
  let handler ({ Server.Handler.request; _ } : _ Server.Handler.ctx) : Response.t =
    let body_str =
      match Body.to_string (Request.body request) with
      | Ok s -> s
      | Error _ -> "{}"
    in
    let parsed = Result.try_with (fun () -> Jsonaf.of_string body_str) in
    match parsed with
    | Error _ -> Response.of_string ~body:"{}" (Status.of_code 400)
    | Ok json ->
      let meth = Jsonaf.member_exn "method" json |> Jsonaf.string_exn in
      (match meth with
       | "initialize" ->
         let headers =
           Piaf.Headers.of_list
             [ "content-type", "text/event-stream"; "Mcp-Session-Id", "sid123" ]
         in
         Response.create ~headers ~body:(sse_body ()) (Status.of_code 200)
       | "ping" ->
         (* Reflect whether the request carried the session header. *)
         let has_sid =
           match Piaf.Headers.get (Request.headers request) "Mcp-Session-Id" with
           | Some "sid123" -> true
           | _ -> false
         in
         let bool_json = if has_sid then `True else `False in
         let json = `Object [ "jsonrpc", `String "2.0"; "has_sid", bool_json ] in
         let headers = Piaf.Headers.of_list [ "content-type", "application/json" ] in
         Response.of_string ~headers ~body:(Jsonaf.to_string json) (Status.of_code 200)
       | _ -> Response.of_string ~body:"{}" (Status.of_code 404))
  in
  let server = Server.create ~config handler in
  ignore (Server.Command.start ~sw env server : Server.Command.t)
;;

(* -------------------------------------------------------------------------- *)
(* Actual test                                                                  *)
(* -------------------------------------------------------------------------- *)

let%expect_test "SSE streaming + session id propagation" =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Launch stub server *)
    Eio.Fiber.fork ~sw (fun () -> start_stub_server ~sw env);
    let uri = Printf.sprintf "http://127.0.0.1:%d/mcp" port in
    let transport = Http.connect ~auth:false ~sw ~env uri in
    (* 1st request: initialize – expect 2 SSE messages. *)
    let init_req = Jsonaf.of_string {|{"jsonrpc":"2.0","id":1,"method":"initialize"}|} in
    Http.send transport init_req;
    let m1 = Http.recv transport in
    let m2 = Http.recv transport in
    print_endline (Jsonaf.to_string m1);
    print_endline (Jsonaf.to_string m2);
    (* 2nd request: ping – server will tell us if it saw the session header. *)
    let ping_req = Jsonaf.of_string {|{"jsonrpc":"2.0","id":2,"method":"ping"}|} in
    Http.send transport ping_req;
    let ping_resp = Http.recv transport in
    (match Jsonaf.member "has_sid" ping_resp with
     | Some `True -> printf "has_sid=true\n"
     | Some `False -> printf "has_sid=false\n"
     | _ -> print_endline "has_sid=?");
    Http.close transport;
    Eio.Switch.fail sw (Completed "done")
  with
  | Completed _ ->
    ();
    [%expect
      {|
{"jsonrpc":"2.0","id":1,"result":{"x":1}}
{"jsonrpc":"2.0","id":1,"result":{"x":2}}
has_sid=true
|}]
;;
