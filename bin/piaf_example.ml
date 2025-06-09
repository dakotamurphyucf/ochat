(** piaf_example.ml -- A simple example of a server using Piaf and Eio
   to handle Server-Sent Events (SSE) and JSON requests. 
    This example demonstrates how to set up a basic HTTP server that can
    respond to GET and POST requests, handle SSE streams, and process
   JSON payloads. 
    It uses the Piaf library for HTTP handling and Eio for concurrency.
    The server listens on a specified port and can be extended to handle
    more complex logic or integrate with other systems.
    The server responds to GET requests by streaming events every second,
   and to POST requests by echoing back the received JSON data.
   It is a simple demonstration of how to use Eio and Piaf together to create
   a responsive and concurrent HTTP server.
   It is designed to be easy to understand and modify for various use cases.
   *)

open Core
open Piaf
module P = Piaf

(* Convenience headers *)
let sse_headers =
  P.Headers.of_list
    [ "content-type", "text/event-stream"
    ; "cache-control", "no-cache"
    ; "connection", "keep-alive"
    ]
;;

let json_headers = P.Headers.of_list [ "content-type", "application/json" ]

(* Helper that builds a chunk-encoded SSE response and returns [push] so the
   caller can keep sending events. *)
let make_sse_response () =
  let stream, push = P.Stream.create 64 in
  let body = P.Body.of_string_stream ~length:`Chunked stream in
  let resp = P.Response.create ~headers:sse_headers ~body `OK in
  resp, push, fun () -> Stream.close stream
;;

(* GET  /mcp --------------------------------------------------------------- *)
let handle_get ~sw ~env =
  let resp, push, _ = make_sse_response () in
  (* Example: every second publish the current time                    *)
  Eio.Fiber.fork ~sw (fun () ->
    let rec loop ?(i = 0) () =
      match i with
      | 10 ->
        push (Some "data: [DONE]\n\n");
        (* After 10 iterations, we close the stream *)
        push None
      | _ ->
        (* Create a Server-Sent Event (SSE) with a timestamp *)
        (* Note: In a real application, you would probably use a more
         structured format, but for this example, we just send a string. *)
        let ev = Printf.sprintf "event: time\ndata: {\"ts\": \n" in
        (* SSE is just a text stream, so we push plain strings          *)
        push (Some ev);
        let ev = Printf.sprintf "data: %.03f}\n\n" (Eio.Time.now env#clock) in
        push (Some ev);
        Eio.Time.sleep env#clock 1.0;
        loop ~i:(i + 1) ()
    in
    loop ());
  resp
;;

(* POST /mcp --------------------------------------------------------------- *)
let handle_post ~sw ~env req =
  match P.Body.to_string req.P.Request.body with
  | Error e ->
    P.Response.create
      ~headers:json_headers
      ~body:
        (P.Body.of_string (Printf.sprintf {|{ "error": "%s" }|} (P.Error.to_string e)))
      `Bad_request
  | Ok body ->
    (* Decide whether we got a batch (JSON array) or a single call.            *)
    let is_batch =
      match Jsonaf.of_string body with
      | `Array _ -> true
      | _ -> false
    in
    if is_batch
    then (
      print_endline "is batch";
      (* SSE stream response for a batch of calls -------------------------- *)
      (* Note: this is not a real MCP server, so we don't actually process
         the batch, we just echo the elements back as SSE events.           *)
      (* stream result --------------------------------------------------- *)
      let resp, push, close = make_sse_response () in
      (* For the demo we just echo every element after 100 ms intervals.   *)
      let json = Jsonaf.of_string body in
      let elements =
        match json with
        | `Array l -> l
        | _ -> []
      in
      Eio.Fiber.fork ~sw (fun () ->
        elements
        |> List.iter ~f:(fun elt ->
          let ev = Printf.sprintf "data: %s\n\n" (Jsonaf.to_string elt) in
          push (Some ev);
          print_endline ("Pushed: " ^ ev);
          (* Simulate processing time for each element *)
          Eio.Time.sleep env#clock 0.1);
        close ());
      resp)
    else (
      (* single-shot JSON answer ----------------------------------------- *)
      let result = Printf.sprintf {|{ "echo": %s }|} body in
      P.Response.create ~headers:json_headers ~body:(P.Body.of_string result) `OK)
;;

(* -----------------------------------------------------------------------  *)
let handler ~env ({ P.Server.request; ctx = { sw; _ } } : Request_info.t P.Server.ctx) =
  match P.Request.meth request, P.Request.target request with
  | `GET, "/mcp" -> handle_get ~env ~sw
  | `POST, "/mcp" -> handle_post ~env ~sw request
  | _ -> P.Server.Handler.not_found ()
;;

let run () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let host = Eio.Net.Ipaddr.V4.loopback
  and port = 8080 in
  let config = P.Server.Config.create ~buffer_size:16_384 (`Tcp (host, port)) in
  let srv = P.Server.create ~config (handler ~env) in
  let _cmd = P.Server.Command.start ~sw env srv in
  (* Block forever *)
  Eio.Promise.await (Eio.Promise.create_resolved ())
;;

let () = run ()
