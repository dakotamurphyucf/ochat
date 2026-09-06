open Core
module JT = Mcp_types.Jsonrpc

let sse_headers length =
  sprintf
    "HTTP/1.1 200 OK\r\n\
     Content-Type: text/event-stream\r\n\
     Content-Length: %d\r\n\
     Connection: close\r\n\
     \r\n"
    length
;;

let reply (request : Loopback.request) =
  let rpc = Jsonaf.of_string request.Loopback.body |> JT.request_of_jsonaf in
  let response = JT.ok ~id:rpc.id (`Object [ "tools", `Array [] ]) in
  "data: " ^ Jsonaf.to_string (JT.jsonaf_of_response response) ^ "\n\n"
;;

let premature_sse env =
  Loopback.with_raw_server
    env
    (fun flow _ ->
       let body = "data: [DONE]\n\n" in
       Eio.Flow.copy_string (sse_headers (String.length body) ^ body) flow)
    (fun sw issuer ->
       let t = Mcp_transport_http.connect ~auth:false ~sw ~env (issuer ^ "/mcp") in
       let req = JT.make_request ~id:(JT.Id.of_int 1) ~method_:"tools/list" () in
       Mcp_transport_http.send t (JT.jsonaf_of_request req);
       match Mcp_transport_http.recv t with
       | _ -> failwith "premature SSE EOF was accepted"
       | exception Mcp_transport_http.Connection_closed -> ())
;;

let live_sse env =
  let released, release = Eio.Promise.create () in
  Loopback.with_raw_server
    env
    (fun flow request ->
       let event = reply request in
       let tail = ": keepalive\n\n" in
       Eio.Flow.copy_string
         (sse_headers (String.length event + String.length tail) ^ event)
         flow;
       Eio.Promise.await released;
       Eio.Flow.copy_string tail flow)
    (fun sw issuer ->
       let t = Mcp_transport_http.connect ~auth:false ~sw ~env (issuer ^ "/mcp") in
       Fun.protect
         (fun () ->
            let req = JT.make_request ~id:(JT.Id.of_int 1) ~method_:"tools/list" () in
            Mcp_transport_http.send t (JT.jsonaf_of_request req);
            let response = Mcp_transport_http.recv t |> JT.response_of_jsonaf in
            Fixture.check JT.Id.(response.id = req.id) "wrong SSE response ID";
            Fixture.check
              (not (Mcp_transport_http.is_closed t))
              "normal SSE response closed transport")
         ~finally:(fun () -> Eio.Promise.resolve release ()))
;;

let http_failure response env =
  Loopback.with_server
    env
    (fun _ -> response)
    (fun sw issuer ->
       let t = Mcp_transport_http.connect ~auth:false ~sw ~env (issuer ^ "/mcp") in
       let req = JT.make_request ~id:(JT.Id.of_int 1) ~method_:"tools/list" () in
       Mcp_transport_http.send t (JT.jsonaf_of_request req);
       match Mcp_transport_http.recv t with
       | _ -> failwith "HTTP failure did not terminate recv"
       | exception Mcp_transport_http.Connection_closed -> ())
;;

let cases =
  [ "mcp.sse-premature-eof", premature_sse
  ; "mcp.sse-delivers-before-next-byte", live_sse
  ; "mcp.http-malformed-json", http_failure (Loopback.json "not-json")
  ; "mcp.http-error-status", http_failure { Loopback.status = 400; body = "{}" }
  ; "mcp.http-server-error-status", http_failure { Loopback.status = 503; body = "{}" }
  ]
;;
