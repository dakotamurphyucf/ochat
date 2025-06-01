open Core
open Mcp_types
module JT = Jsonrpc
module Transport : Mcp_transport_interface.TRANSPORT = Mcp_transport_stdio

(*------------------------------------------------------------------*)
(* Internal state                                                    *)
(*------------------------------------------------------------------*)

module Id_table = Hashtbl.Poly

type pending_resolver = (Jsonaf.t, string) result Eio.Promise.u

type t =
  { transport : Transport.t
  ; sw : Eio.Switch.t
  ; mutable next_id : int
  ; pending : (JT.Id.t, pending_resolver) Id_table.t
  ; notif_stream : Jsonaf.t Eio.Stream.t
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

let error_of_rpc_error (e : JT.error_obj) = sprintf "RPC error %d – %s" e.code e.message

let fulfil_resolver resolver (v : (Jsonaf.t, string) result) =
  Eio.Promise.resolve resolver v
;;

(*------------------------------------------------------------------*)
(* Receiver loop                                                     *)
(*------------------------------------------------------------------*)

let receiver_loop c =
  let rec loop () =
    match Transport.recv c.transport with
    | (exception Transport.Connection_closed) | (exception End_of_file) -> ()
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
       | Error _ -> Eio.Stream.add c.notif_stream json);
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
      ; "clientInfo", `Object [ "name", `String "ocamlgpt"; "version", `String "dev" ]
      ]
  in
  let req = JT.make_request ~id ~method_:"initialize" ~params () in
  (* Use synchronous path here because receiver fibre may not yet be
     running.  We'll read directly. *)
  Transport.send c.transport (JT.jsonaf_of_request req);
  let rec wait () =
    match parse_response (Transport.recv c.transport) with
    | Ok resp when JT.Id.(resp.id = id) -> resp
    | _ -> wait ()
  in
  let _resp = wait () in
  (* fire-and-forget initialized notification *)
  let notif = JT.notify ~method_:"notifications/initialized" () in
  (try Transport.send c.transport (JT.jsonaf_of_notification notif) with
   | _ -> ());
  ()
;;

let connect ~sw ~env ~uri =
  let transport = Transport.connect ~sw ~env uri in
  let pending = Id_table.create () in
  let notif_stream = Eio.Stream.create 64 in
  (* We need [client] inside the receiver fibre and the fibre handle
     inside [client] – use [let rec] to tie the knot. *)
  let client : t = { transport; sw; next_id = 1; pending; notif_stream } in
  (* Perform blocking initialize before returning. *)
  perform_initialize client;
  (* Start receiver fibre now that [client] is ready *)
  let _ =
    Eio.Fiber.fork_daemon ~sw (fun () ->
      receiver_loop client;
      `Stop_daemon)
  in
  client
;;

let close c = Transport.close c.transport
let is_closed c = Transport.is_closed c.transport
let notifications c = c.notif_stream

(*------------------------------------------------------------------*)
(* RPC helper                                                        *)
(*------------------------------------------------------------------*)

let rpc_async (c : t) (req : JT.request) =
  let promise, resolver = Eio.Promise.create () in
  Id_table.add_exn c.pending ~key:req.id ~data:resolver;
  Transport.send c.transport (JT.jsonaf_of_request req);
  promise
;;

let rpc c req = Eio.Promise.await (rpc_async c req)

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

(*------------------------------------------------------------------*)
(* End *)
