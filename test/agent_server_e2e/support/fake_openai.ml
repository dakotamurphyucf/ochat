open Core
module Res = Openai.Responses

let output_event marker =
  let item =
    Res.Response_stream.Item.Function_call
      { name = "append_to_file"
      ; arguments = ""
      ; call_id = "stock-permission-call"
      ; _type = "function_call"
      ; id = Some "stock-permission-item"
      ; status = Some "in_progress"
      }
  in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Res.Response_stream.Function_call_arguments_done
      { arguments =
          Jsonaf.to_string
            (`Object [ "path", `String marker; "content", `String "executed" ])
      ; item_id = "stock-permission-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
;;

let completed_event =
  `Object
    [ "type", `String "response.completed"
    ; ( "response"
      , `Object
          [ "id", `String "stock-response"
          ; "object", `String "response"
          ; "created_at", `Number "0"
          ; "model", `String "gpt-4.1"
          ; "output", `Array []
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
          ] )
    ; "sequence_number", `Number "2"
    ]
;;

let sse_event json = "data: " ^ Jsonaf.to_string json ^ "\n\n"

let response_body marker ~completed =
  List.map
    (if completed then [] else output_event marker)
    ~f:(fun event -> Res.Response_stream.jsonaf_of_t event |> sse_event)
  |> fun events ->
  String.concat (events @ [ sse_event completed_event; "data: [DONE]\n\n" ])
;;

let handler marker release ({ Piaf.Server.request; _ } : _ Piaf.Server.ctx) =
  match Piaf.Request.meth request, Piaf.Request.target request with
  | `POST, "/v1/responses" ->
    let body =
      Piaf.Body.to_string (Piaf.Request.body request)
      |> Result.map_error ~f:Piaf.Error.to_string
      |> Result.ok_or_failwith
      |> Jsonaf.of_string
    in
    let completed =
      match body with
      | `Object fields ->
        (match List.Assoc.find fields "input" ~equal:String.equal with
         | Some (`Array inputs) ->
           List.exists inputs ~f:(function
             | `Object fields ->
               let field key value =
                 match List.Assoc.find fields key ~equal:String.equal with
                 | Some (`String actual) -> String.equal actual value
                 | _ -> false
               in
               field "type" "function_call_output"
               && field "call_id" "stock-permission-call"
             | _ -> false)
         | _ -> failwith "permission fixture requires a Responses input array")
      | _ -> failwith "permission fixture requires a Responses request"
    in
    Eio.Promise.await release;
    let headers = Piaf.Headers.of_list [ "content-type", "text/event-stream" ] in
    Piaf.Response.of_string ~headers ~body:(response_body marker ~completed) `OK
  | _ -> Piaf.Server.Handler.not_found ()
;;

let start ~sw ~env ~port ~marker ~release =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun server_sw ->
      let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
      let config = Piaf.Server.Config.create address in
      let server = Piaf.Server.create ~config (handler marker release) in
      ignore (Piaf.Server.Command.start ~sw:server_sw env server : Piaf.Server.Command.t));
    `Stop_daemon)
;;
