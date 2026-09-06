open Core
open Mcp_types
module JT = Jsonrpc
module Id_table = Hashtbl.Poly

exception Connection_closed

type transport =
  { send : Jsonaf.t -> unit
  ; recv : unit -> Jsonaf.t
  ; close : unit -> unit
  ; is_closed : unit -> bool
  }

type t =
  { transport : transport
  ; sw : Eio.Switch.t
  ; mutable next_id : int
  ; pending : (JT.Id.t, (Jsonaf.t, string) result -> unit) Id_table.t
  ; notif_stream : JT.notification Eio.Stream.t
  ; stopped : string Eio.Promise.t
  ; stop : string Eio.Promise.u
  }

let fresh_id c =
  let i = c.next_id in
  c.next_id <- i + 1;
  JT.Id.of_int i
;;

let error_of_exn = function
  | Connection_closed
  | Mcp_transport_stdio.Connection_closed
  | Mcp_transport_http.Connection_closed
  | End_of_file -> "Connection_closed"
  | Eio.Cancel.Cancelled _ -> "Cancelled"
  | _ -> "MCP transport failed"
;;

let fail_pending c message =
  if Option.is_none (Eio.Promise.peek c.stopped) then Eio.Promise.resolve c.stop message;
  let resolvers = Id_table.data c.pending in
  Id_table.clear c.pending;
  List.iter resolvers ~f:(fun resolve -> resolve (Error message))
;;

let close c =
  fail_pending c "Connection_closed";
  c.transport.close ()
;;

let is_closed c = Option.is_some (Eio.Promise.peek c.stopped) || c.transport.is_closed ()
let notifications c = c.notif_stream

let start_rpc c (req : JT.request) resolve =
  if Option.is_some (Eio.Switch.get_error c.sw) then fail_pending c "Cancelled";
  match Eio.Promise.peek c.stopped with
  | Some message -> resolve (Error message)
  | None ->
    Id_table.add_exn c.pending ~key:req.id ~data:resolve;
    (try c.transport.send (JT.jsonaf_of_request req) with
     | exn ->
       fail_pending c (error_of_exn exn);
       (match exn with
        | Eio.Cancel.Cancelled _ -> raise exn
        | _ -> ()))
;;

let rpc_async c req =
  let promise, resolver = Eio.Promise.create () in
  start_rpc c req (Eio.Promise.resolve resolver);
  promise
;;

let rpc c req =
  let promise = rpc_async c req in
  try Eio.Promise.await promise with
  | Eio.Cancel.Cancelled _ as exn ->
    Option.iter (Id_table.find_and_remove c.pending req.id) ~f:(fun resolve ->
      resolve (Error "Cancelled"));
    raise exn
;;

let dispatch_response c (resp : JT.response) =
  Option.iter (Id_table.find_and_remove c.pending resp.id) ~f:(fun resolve ->
    let result =
      match resp.result, resp.error with
      | Some r, None -> Ok r
      | _, Some err -> Error (sprintf "RPC error %d – %s" err.code err.message)
      | None, None -> Error "Invalid response: empty"
    in
    resolve result)
;;

let dispatch c json =
  match JT.response_of_jsonaf json with
  | resp -> dispatch_response c resp
  | exception _ ->
    (match JT.notification_of_jsonaf json with
     | notif -> Eio.Stream.add c.notif_stream notif
     | exception _ -> ())
;;

let receiver_loop c =
  let rec loop () =
    let json = c.transport.recv () in
    dispatch c json;
    loop ()
  in
  try Eio.Fiber.first loop (fun () -> ignore (Eio.Promise.await c.stopped : string)) with
  | exn ->
    fail_pending c (error_of_exn exn);
    (match exn with
     | Eio.Cancel.Cancelled _ -> raise exn
     | _ -> ())
;;

let create ~sw transport =
  let stopped, stop = Eio.Promise.create () in
  let c =
    { transport
    ; sw
    ; next_id = 1
    ; pending = Id_table.create ()
    ; notif_stream = Eio.Stream.create 64
    ; stopped
    ; stop
    }
  in
  Eio.Switch.on_release sw (fun () -> close c);
  c
;;

let start_receiver c =
  Eio.Fiber.fork_daemon ~sw:c.sw (fun () ->
    receiver_loop c;
    `Stop_daemon)
;;

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
  c.transport.send (JT.jsonaf_of_request req);
  let rec wait () =
    let json = c.transport.recv () in
    match JT.response_of_jsonaf json with
    | resp when JT.Id.(resp.id = id) -> resp
    | _ | (exception _) -> wait ()
  in
  ignore (wait () : JT.response);
  let notif = JT.notify ~method_:"notifications/initialized" () in
  c.transport.send (JT.jsonaf_of_notification notif)
;;

let connect_transport ~auth ~sw ~env uri =
  match Uri.scheme (Uri.of_string uri) with
  | Some ("http" | "https" | "mcp+http" | "mcp+https") ->
    let t = Mcp_transport_http.connect ~auth ~sw ~env uri in
    { send = Mcp_transport_http.send t
    ; recv = (fun () -> Mcp_transport_http.recv t)
    ; close = (fun () -> Mcp_transport_http.close t)
    ; is_closed = (fun () -> Mcp_transport_http.is_closed t)
    }
  | _ ->
    let t = Mcp_transport_stdio.connect ~auth ~sw ~env uri in
    { send = Mcp_transport_stdio.send t
    ; recv = (fun () -> Mcp_transport_stdio.recv t)
    ; close = (fun () -> Mcp_transport_stdio.close t)
    ; is_closed = (fun () -> Mcp_transport_stdio.is_closed t)
    }
;;

let connect ?(auth = true) ~sw ~env uri =
  let c = create ~sw (connect_transport ~auth ~sw ~env uri) in
  perform_initialize c;
  start_receiver c;
  c
;;

let decode decoder json =
  try Ok (decoder json) with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> Error "decode failure"
;;

let map_rpc_async c req decoder =
  let promise, resolver = Eio.Promise.create () in
  start_rpc c req (fun result ->
    Eio.Promise.resolve resolver (Result.bind result ~f:(decode decoder)));
  promise
;;

let list_request c =
  JT.make_request ~id:(fresh_id c) ~method_:"tools/list" ~params:(`Object []) ()
;;

let decode_tools json = (Tools_list_result.t_of_jsonaf json).tools
let list_tools_async c = map_rpc_async c (list_request c) decode_tools
let list_tools c = Result.bind (rpc c (list_request c)) ~f:(decode decode_tools)

let call_request c ~name ~arguments =
  let params = `Object [ "name", `String name; "arguments", arguments ] in
  JT.make_request ~id:(fresh_id c) ~method_:"tools/call" ~params ()
;;

let call_tool_async c ~name ~arguments =
  map_rpc_async c (call_request c ~name ~arguments) Tool_result.t_of_jsonaf
;;

let call_tool c ~name ~arguments =
  Result.bind
    (rpc c (call_request c ~name ~arguments))
    ~f:(decode Tool_result.t_of_jsonaf)
;;

module For_testing = struct
  let create ~sw ~send ~recv ~close =
    let c = create ~sw { send; recv; close; is_closed = (fun () -> false) } in
    start_receiver c;
    c
  ;;

  let pending_count c = Id_table.length c.pending
end
