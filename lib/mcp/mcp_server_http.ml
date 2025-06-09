open! Core

module P = Piaf
module JT = Mcp_types

(* ----------------------------------------------------------------- *)

(* Simple in-memory session table for HTTP transport. *)
let sessions : unit String.Table.t = String.Table.create ()

let session_header = "Mcp-Session-Id"

let () = Random.self_init ()

let require_valid_session request =
  match P.Headers.get (P.Request.headers request) session_header with
  | None -> Error `Missing
  | Some sid -> if Hashtbl.mem sessions sid then Ok sid else Error `Unknown

let new_session_id () =
  let hex = "0123456789abcdef" in
  String.init 32 ~f:(fun _ -> hex.[Random.int 16])

let json_headers = P.Headers.of_list [ "content-type", "application/json" ]

let respond_json ?(status = `OK) ?(extra_headers = []) body_string =
  let body = P.Body.of_string body_string in
  let headers =
    if List.is_empty extra_headers then json_headers
    else
      P.Headers.add_list json_headers
        (List.map extra_headers ~f:(fun (k, v) -> (String.lowercase k, v)))
  in
  P.Response.create ~headers ~body status

(* ------------------------------------------------------------------ *)

(* Global registry of active SSE push functions so that future features (such
   as logging or listChanged notifications) can broadcast to every connected
   listener.  For now we keep it extremely simple and just mutate a global
   list protected by the main Eio domain – the HTTP server runs entirely in a
   single domain so data‐races cannot happen. *)

let active_push_streams : (string option -> unit) list ref = ref []

let add_stream push = active_push_streams := push :: !active_push_streams

let remove_stream push =
  active_push_streams := List.filter !active_push_streams ~f:(fun p -> not (phys_equal p push))

(* Utility to push a JSON value to all connected streams *)
let _broadcast_json (json : Jsonaf.t) =
  let payload = "data: " ^ Jsonaf.to_string json ^ "\n\n" in
  List.iter !active_push_streams ~f:(fun push -> try push (Some payload) with _ -> ())

(* Helper functions to broadcast standard list_changed notifications *)
let broadcast_tools_list_changed () =
  _broadcast_json
    (`Object [ "jsonrpc", `String "2.0"; "method", `String "notifications/tools/list_changed" ])

let broadcast_prompts_list_changed () =
  _broadcast_json
    (`Object [ "jsonrpc", `String "2.0"; "method", `String "notifications/prompts/list_changed" ])

(* ------------------------------------------------------------------ *)

let handle_get ~env ~sw ~core:_ (request : P.Request.t) : P.Response.t =
  (* A GET request must include a valid session id. *)
  match require_valid_session request with
  | Error `Missing | Error `Unknown ->
      respond_json ~status:`Not_found {|{"error":"Missing or unknown session id"}|}
  | Ok _sid ->
      (* Build a chunked SSE stream. *)
      let stream, push = P.Stream.create 32 in
      (* Register stream for future broadcasts. *)
      add_stream push;

      (* Keep-alive fibre – every 20 s we send a comment line so that proxies
         know the connection is alive.  We detach the fibre; when the client
         disconnects Piaf will close the stream which will raise [Closed]
         inside the fibre – we catch and terminate silently. *)
      let keepalive () =
        let clock = env#clock in
        let rec loop () =
          Eio.Time.sleep clock 20.0;
          (try push (Some ": keep-alive\n\n") with _ -> ());
          loop ()
        in
        (try loop () with _ -> ())
      in
      Eio.Fiber.fork ~sw keepalive;

      (* When the response body gets closed remove the push function so we no
         longer broadcast to a dead connection. *)
      let body = P.Body.of_string_stream ~length:`Chunked stream in
      P.Body.when_closed ~f:(fun _ -> remove_stream push) body;

      let headers = P.Headers.of_list [ "content-type", "text/event-stream" ] in
      P.Response.create ~headers ~body `OK

let handle_post ~core (request : P.Request.t) : P.Response.t =
  (* Helper to produce a simple error response *)
  let error_json code msg =
    respond_json ~status:code (Printf.sprintf {|{"error":"%s"}|} msg)
  in

  (* Validate or create session *)
  let session_status = require_valid_session request in
  (* Parse body first to maybe detect initialize *)
  match P.Body.to_string (P.Request.body request) with
  | Error e ->
      error_json `Bad_request (P.Error.to_string e)
  | Ok body_string -> (
      (* Try parse JSON *)
      match Or_error.try_with (fun () -> Jsonaf.of_string body_string) with
      | Error _err -> error_json `Bad_request "Invalid JSON"
      | Ok json_in ->
          (* Determine if this is an initialize request *)
          let is_initialize msg_json =
            match Or_error.try_with (fun () -> JT.Jsonrpc.request_of_jsonaf msg_json) with
            | Ok req when String.equal req.method_ "initialize" -> true
            | _ -> false
          in
          let contains_initialize =
            match json_in with
            | `Array arr -> List.exists arr ~f:is_initialize
            | _ -> is_initialize json_in
          in
          (* Session gatekeeping *)
          let gate_result : P.Response.t option =
            match contains_initialize, session_status with
            | true, _ -> None (* allowed even without session, will create *)
            | false, Ok _sid -> None
            | false, Error `Missing ->
                Some (error_json `Not_found "Missing session id")
            | false, Error `Unknown ->
                Some (error_json `Not_found "Unknown session id")
          in
          (match gate_result with
          | Some resp -> resp
          | None ->
              (* Delegate to router *)
              let responses = Mcp_server_router.handle ~core json_in in

              (* Decide on media type – if the client explicitly accepts
                 [text/event-stream] we stream each JSON-RPC response as a
                 separate SSE event, otherwise fall back to a regular
                 application/json payload. *)

              let accepts_sse =
                match P.Headers.get (P.Request.headers request) "accept" with
                | None -> false
                | Some v -> String.is_substring v ~substring:"text/event-stream"
              in

              let (status_headers, body) =
                if accepts_sse then (
                  (* Build one SSE event per response.  We currently generate
                     all at once; a future iteration could stream them using
                     [P.Stream] for large result sets. *)
                  let event_of_json j =
                    let payload = Jsonaf.to_string j in
                    "data: " ^ payload ^ "\n\n" in
                  let sse_body = String.concat ~sep:"" (List.map responses ~f:event_of_json) in
                  let headers =
                    P.Headers.of_list [ "content-type", "text/event-stream" ]
                  in
                  (headers, P.Body.of_string sse_body))
                else (
                  let json_out : Jsonaf.t =
                    match responses with
                    | [] -> `Object []
                    | [ single ] -> single
                    | lst -> `Array lst
                  in
                  ( json_headers, P.Body.of_string (Jsonaf.to_string json_out) ))
              in

              let extra_headers =
                match contains_initialize with
                | true ->
                    let sid = new_session_id () in
                    Hashtbl.set sessions ~key:sid ~data:();
                    [ (session_header, sid) ]
                | false -> []
              in

              let headers = P.Headers.add_list status_headers extra_headers in
              P.Response.create ~headers ~body `OK))

let not_allowed () =
  P.Response.create ~body:(P.Body.of_string "Method Not Allowed") ~headers:json_headers
    `Method_not_allowed

let handler ~env ~core ({ P.Server.request; ctx = req_info } : P.Request_info.t P.Server.ctx)
    : P.Response.t =
  let sw = req_info.sw in
  match (P.Request.meth request, P.Request.target request) with
  | `POST, "/mcp" -> handle_post ~core request
  | `GET, "/mcp" -> handle_get ~env ~sw ~core request
  | _ -> not_allowed ()

let run ~(env : Eio_unix.Stdenv.base) ~(core : Mcp_server_core.t) ~(port : int) : unit =
  (* Register hooks so that any modification to tools or prompts gets sent to
     all listening clients via an SSE notification.  We register before the
     server starts so late additions (e.g. dynamic prompt folder watching)
     are also covered. *)
  Mcp_server_core.add_tools_changed_hook core broadcast_tools_list_changed;
  Mcp_server_core.add_prompts_changed_hook core broadcast_prompts_list_changed;

  Eio.Switch.run (fun sw ->
      let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
      let config = P.Server.Config.create address in
      let srv = P.Server.create ~config (handler ~env ~core) in
      let _cmd = P.Server.Command.start ~sw env srv in
      (* Block forever *)
      Eio.Promise.await (Eio.Promise.create_resolved ()))

