open Core
open Mcp_types
module JT = Jsonrpc

(*------------------------------------------------------------------*)
(* Transport dispatch                                                *)
(*------------------------------------------------------------------*)

module T_stdio = Mcp_transport_stdio
module T_http = Mcp_transport_http

(* Thin runtime union allowing us to choose the concrete transport
   implementation at connection time while still exposing a uniform
   set of helpers. *)

exception Connection_closed

type transport =
  | Stdio of T_stdio.t
  | Http of T_http.t

let transport_send (t : transport) (json : Jsonaf.t) =
  match t with
  | Stdio s ->
    (try T_stdio.send s json with
     | T_stdio.Connection_closed -> raise Connection_closed)
  | Http h ->
    (try T_http.send h json with
     | T_http.Connection_closed -> raise Connection_closed)
;;

let transport_recv (t : transport) : Jsonaf.t =
  match t with
  | Stdio s ->
    (try T_stdio.recv s with
     | T_stdio.Connection_closed -> raise Connection_closed)
  | Http h ->
    (try T_http.recv h with
     | T_http.Connection_closed -> raise Connection_closed)
;;

let transport_close (t : transport) =
  match t with
  | Stdio s -> T_stdio.close s
  | Http h -> T_http.close h
;;

let transport_is_closed (t : transport) =
  match t with
  | Stdio s -> T_stdio.is_closed s
  | Http h -> T_http.is_closed h
;;

(*------------------------------------------------------------------*)
(* Internal state                                                    *)
(*------------------------------------------------------------------*)

module Id_table = Hashtbl.Poly

type pending_resolver = (Jsonaf.t, string) result Eio.Promise.u

type t =
  { transport : transport
  ; sw : Eio.Switch.t
  ; mutable next_id : int
  ; pending : (JT.Id.t, pending_resolver) Id_table.t
  ; notif_stream : Mcp_types.Jsonrpc.notification Eio.Stream.t
  }

(*------------------------------------------------------------------*)
(* Helpers                                                           *)
(*------------------------------------------------------------------*)

let fresh_id c =
  let i = c.next_id in
  c.next_id <- i + 1;
  JT.Id.of_int i
;;

let parse_response json =
  try Ok (JT.response_of_jsonaf json) with
  | exn -> Error (Exn.to_string_mach exn)
;;

let parse_notification json =
  try Ok (JT.notification_of_jsonaf json) with
  | exn -> Error (Exn.to_string_mach exn)
;;

let error_of_rpc_error (e : JT.error_obj) = sprintf "RPC error %d – %s" e.code e.message

let fulfil_resolver resolver (v : (Jsonaf.t, string) result) =
  Eio.Promise.resolve resolver v
;;

(*------------------------------------------------------------------*)
(* RPC helper                                                        *)
(*------------------------------------------------------------------*)

let rpc_async (c : t) (req : JT.request) =
  let promise, resolver = Eio.Promise.create () in
  Id_table.add_exn c.pending ~key:req.id ~data:resolver;
  transport_send c.transport (JT.jsonaf_of_request req);
  promise
;;

let rpc c req = Eio.Promise.await (rpc_async c req)

(*------------------------------------------------------------------*)
(* Receiver loop                                                     *)
(*------------------------------------------------------------------*)

let receiver_loop c =
  let rec loop () =
    match transport_recv c.transport with
    | (exception Connection_closed) | (exception End_of_file) -> ()
    | json ->
      (match parse_response json with
       | Ok resp ->
         (match Id_table.find_and_remove c.pending resp.id with
          | None -> ()
          | Some resolver ->
            let result =
              match resp.result, resp.error with
              | Some r, None -> Ok r
              | None, Some err | Some _, Some err -> Error (error_of_rpc_error err)
              | None, None -> Error "Invalid response: empty"
            in
            fulfil_resolver resolver result)
       | Error _ ->
         (match parse_notification json with
          | Ok notif -> Eio.Stream.add c.notif_stream notif
          | Error _ -> ()));
      loop ()
  in
  try loop () with
  | _ -> ()
;;

(*------------------------------------------------------------------*)
(* Connect / close                                                   *)
(*------------------------------------------------------------------*)

let perform_initialize c =
  let id = fresh_id c in
  let params =
    `Object
      [ "protocolVersion", `String "2025-03-26"
      ; "capabilities", `Object []
      ; "clientInfo", `Object [ "name", `String "ocamlochat"; "version", `String "dev" ]
      ]
  in
  let req = JT.make_request ~id ~method_:"initialize" ~params () in
  (* Use synchronous path here because receiver fibre may not yet be
     running.  We'll read directly. *)
  transport_send c.transport (JT.jsonaf_of_request req);
  let rec wait () =
    match parse_response (transport_recv c.transport) with
    | Ok resp when JT.Id.(resp.id = id) -> resp
    | _ -> wait ()
  in
  let _resp = wait () in
  (* fire-and-forget initialized notification *)
  let notif = JT.notify ~method_:"notifications/initialized" () in
  (try transport_send c.transport (JT.jsonaf_of_notification notif) with
   | _ -> ());
  ()
;;

let connect ?(auth = true) ~sw ~env uri =
  let transport : transport =
    (* Decide transport based on URI scheme.  For historical reasons we
       allow plain strings starting with "stdio:" as well. *)
    let choose_http uri =
      (* We treat any uri with scheme http/https or prefixed with mcp+http* as HTTP *)
      match Uri.scheme (Uri.of_string uri) with
      | Some ("http" | "https" | "mcp+http" | "mcp+https") -> true
      | _ -> false
    in
    if String.is_prefix uri ~prefix:"stdio:"
    then Stdio (T_stdio.connect ~auth ~sw ~env uri)
    else if choose_http uri
    then Http (T_http.connect ~auth ~sw ~env uri)
    else
      (* Fallback to stdio for unknown scheme (keeps backwards compat) *)
      Stdio (T_stdio.connect ~auth ~sw ~env uri)
  in
  let pending = Id_table.create () in
  let notif_stream = Eio.Stream.create 64 in
  (* We need [client] inside the receiver fibre and the fibre handle
     inside [client] – use [let rec] to tie the knot. *)
  let client : t = { transport; sw; next_id = 1; pending; notif_stream } in
  (* Perform blocking initialize before running the daemon and returning because the receiver loop  *)
  perform_initialize client;
  (* Start receiver fibre now that [client] is ready *)
  let _ =
    Eio.Fiber.fork_daemon ~sw (fun () ->
      receiver_loop client;
      `Stop_daemon)
  in
  client
;;

let close c = transport_close c.transport
let is_closed c = transport_is_closed c.transport
let notifications c = c.notif_stream

(*------------------------------------------------------------------*)
(* High-level helpers                                                *)
(*------------------------------------------------------------------*)

let list_tools_async c =
  let id = fresh_id c in
  let req = JT.make_request ~id ~method_:"tools/list" ~params:(`Object []) () in
  let base_promise = rpc_async c req in
  let promise, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:c.sw (fun () ->
    match Eio.Promise.await base_promise with
    | Error _ as e -> Eio.Promise.resolve resolver e
    | Ok json ->
      (match Tools_list_result.t_of_jsonaf json with
       | res -> Eio.Promise.resolve resolver (Ok res.tools)
       | exception _ -> Eio.Promise.resolve resolver (Error "decode failure")));
  promise
;;

let list_tools c = Eio.Promise.await (list_tools_async c)

let call_tool_async c ~name ~arguments =
  let id = fresh_id c in
  let params = `Object [ "name", `String name; "arguments", arguments ] in
  let req = JT.make_request ~id ~method_:"tools/call" ~params () in
  let base_promise = rpc_async c req in
  let promise, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:c.sw (fun () ->
    match Eio.Promise.await base_promise with
    | Error _ as e -> Eio.Promise.resolve resolver e
    | Ok json ->
      (try
         let r = Tool_result.t_of_jsonaf json in
         Eio.Promise.resolve resolver (Ok r)
       with
       | _ -> Eio.Promise.resolve resolver (Error "decode failure")));
  promise
;;

let call_tool c ~name ~arguments = Eio.Promise.await (call_tool_async c ~name ~arguments)
