open Core
module Res = Openai.Responses

type request =
  { body : string
  ; release : unit Eio.Promise.u
  ; mutable released : bool
  }

type t = { mutable requests : request list }

let event json = "data: " ^ Jsonaf.to_string json ^ "\n\n"

let output index =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id = sprintf "background-message-%d" index
    ; content =
        [ { annotations = []; text = "background-result"; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
  in
  let item = Res.Response_stream.Item.Output_message message in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Res.Response_stream.Output_item_done
      { item; output_index = 0; type_ = "response.output_item.done" }
  ]
;;

let response_json index =
  let item =
    match List.hd_exn (output index) with
    | Output_item_added item -> Res.Response_stream.Item.jsonaf_of_t item.item
    | _ -> assert false
  in
  `Object
    [ "id", `String (sprintf "background-response-%d" index)
    ; "object", `String "response"
    ; "created_at", `Number "0"
    ; "model", `String "gpt-4.1"
    ; "output", `Array [ item ]
    ; "parallel_tool_calls", `False
    ; "tool_choice", `String "none"
    ; "tools", `Array []
    ; "temperature", `Number "0"
    ; "top_p", `Number "1"
    ; "status", `String "completed"
    ; "usage", `Null
    ; "user", `Null
    ; "error", `Null
    ; "incomplete_details", `Null
    ; "instructions", `Null
    ; "max_output_tokens", `Null
    ; "metadata", `Object []
    ; "previous_response_id", `Null
    ; "reasoning", `Null
    ; "service_tier", `String "default"
    ; "store", `False
    ; "text", `Object [ "format", `Object [ "type", `String "text" ] ]
    ; "truncation", `String "disabled"
    ]
;;

let response index request =
  let is_stream =
    match Jsonaf.member "stream" (Jsonaf.of_string request) with
    | Some `True -> true
    | _ -> false
  in
  let content_type, body =
    if is_stream
    then (
      let events =
        List.map (output index) ~f:(fun item ->
          Res.Response_stream.jsonaf_of_t item |> event)
      in
      ( "text/event-stream"
      , String.concat
          (events
           @ [ event
                 (`Object
                     [ "type", `String "response.completed"
                     ; "response", response_json index
                     ; "sequence_number", `Number "2"
                     ])
             ; "data: [DONE]\n\n"
             ]) ))
    else "application/json", Jsonaf.to_string (response_json index)
  in
  let headers = Piaf.Headers.of_list [ "content-type", content_type ] in
  Piaf.Response.of_string ~headers ~body `OK
;;

let handler t ({ Piaf.Server.request; _ } : _ Piaf.Server.ctx) =
  match Piaf.Request.meth request, Piaf.Request.target request with
  | `POST, "/v1/responses" ->
    let body =
      Piaf.Body.to_string (Piaf.Request.body request)
      |> Result.map_error ~f:Piaf.Error.to_string
      |> Result.ok_or_failwith
    in
    let gate, release = Eio.Promise.create () in
    let index = List.length t.requests in
    t.requests <- t.requests @ [ { body; release; released = false } ];
    Eio.Promise.await gate;
    response index body
  | _ -> Piaf.Server.Handler.not_found ()
;;

let start ~sw ~env ~port =
  let t = { requests = [] } in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun server_sw ->
      let config = Piaf.Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
      let server = Piaf.Server.create ~config (handler t) in
      ignore (Piaf.Server.Command.start ~sw:server_sw env server : Piaf.Server.Command.t));
    `Stop_daemon);
  t
;;

let request_count t = List.length t.requests
let request_body t ~index = (List.nth_exn t.requests index).body

let release t ~index =
  let request = List.nth_exn t.requests index in
  if not request.released
  then (
    request.released <- true;
    Eio.Promise.resolve request.release ())
;;
