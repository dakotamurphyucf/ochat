open Core
module Res = Openai.Responses

type outcome =
  | Summary of string
  | Missing_summary
  | Raw_json of string

type request =
  { body : Jsonaf.t
  ; release : outcome Eio.Promise.u
  ; returned : unit Eio.Promise.t
  ; mutable released : bool
  }

type t = { mutable requests : request list }

let output text =
  Res.Item.Output_message
    { role = Assistant
    ; id = "compaction-summary-message"
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
;;

let empty_response : Res.Response.t =
  { id = "compaction-summary-response"
  ; object_ = "response"
  ; created_at = 0
  ; status = Completed
  ; error = None
  ; incomplete_details = None
  ; instructions = None
  ; max_output_tokens = None
  ; model = "gpt-5.6-sol"
  ; output = []
  ; parallel_tool_calls = None
  ; previous_response_id = None
  ; reasoning = None
  ; store = None
  ; temperature = None
  ; text = None
  ; tool_choice = None
  ; tools = None
  ; top_p = None
  ; truncation = None
  ; usage = None
  ; user = None
  ; metadata = None
  }
;;

let response outcome =
  let output =
    match outcome with
    | Summary text -> [ output text ]
    | Missing_summary | Raw_json _ -> []
  in
  let response = { empty_response with output } in
  let headers = Piaf.Headers.of_list [ "content-type", "application/json" ] in
  let body =
    match outcome with
    | Raw_json body -> body
    | Summary _ | Missing_summary -> Res.Response.jsonaf_of_t response |> Jsonaf.to_string
  in
  Piaf.Response.of_string ~headers ~body `OK
;;

let handle_request t incoming =
  let body =
    Piaf.Body.to_string (Piaf.Request.body incoming)
    |> Result.map_error ~f:Piaf.Error.to_string
    |> Result.ok_or_failwith
    |> Jsonaf.of_string
  in
  let gate, release = Eio.Promise.create () in
  let returned, notify_returned = Eio.Promise.create () in
  t.requests <- t.requests @ [ { body; release; returned; released = false } ];
  let response = Eio.Promise.await gate |> response in
  Eio.Promise.resolve notify_returned ();
  response
;;

let handler t ({ Piaf.Server.request; _ } : _ Piaf.Server.ctx) =
  match Piaf.Request.meth request, Piaf.Request.target request with
  | `POST, "/v1/responses" -> handle_request t request
  | _ -> Piaf.Server.Handler.not_found ()
;;

let start ~sw ~env ~port =
  let t = { requests = [] } in
  let ready, notify_ready = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun server_sw ->
      let config = Piaf.Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
      let server = Piaf.Server.create ~config (handler t) in
      ignore (Piaf.Server.Command.start ~sw:server_sw env server : Piaf.Server.Command.t);
      Eio.Promise.resolve notify_ready ());
    `Stop_daemon);
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () -> Eio.Promise.await ready);
  t
;;

let await_request t ~env ~index =
  let rec await () =
    match List.nth t.requests index with
    | Some request -> request
    | None ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      await ()
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. await
;;

let body request = request.body
let request_count t = List.length t.requests

let release request outcome =
  if not request.released
  then (
    request.released <- true;
    Eio.Promise.resolve request.release outcome)
;;

let await_returned request ~env =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    Eio.Promise.await request.returned)
;;
