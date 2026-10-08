open Core
module Res = Openai.Responses

type request =
  { body : Jsonaf.t
  ; push : string option -> unit
  ; mutable closed : bool
  ; streaming : bool
  ; mutable finalized : Res.Item.t Int.Map.t
  ; mutable next_sequence : int64
  ; mutable item_indices : int String.Map.t
  }

type t = { mutable requests : request list }

let handler t ({ Piaf.Server.request; _ } : _ Piaf.Server.ctx) =
  match Piaf.Request.meth request, Piaf.Request.target request with
  | `POST, "/v1/responses" ->
    let body =
      Piaf.Body.to_string (Piaf.Request.body request)
      |> Result.map_error ~f:Piaf.Error.to_string
      |> Result.ok_or_failwith
      |> Jsonaf.of_string
    in
    let streaming = Poly.equal (Jsonaf.member "stream" body) (Some `True) in
    let stream, push = Piaf.Stream.create 128 in
    t.requests
    <- t.requests
       @ [ { body
           ; push
           ; closed = false
           ; streaming
           ; finalized = Int.Map.empty
           ; next_sequence = 0L
           ; item_indices = String.Map.empty
           }
         ];
    Piaf.Response.create
      ~headers:
        (Piaf.Headers.of_list
           [ ( "content-type"
             , if streaming then "text/event-stream" else "application/json" )
           ])
      ~body:(Piaf.Body.of_string_stream ~length:`Chunked stream)
      `OK
  | _ -> Piaf.Server.Handler.not_found ()
;;

let start ~sw ~env ~port =
  let t = { requests = [] } in
  let ready, notify = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun server_sw ->
      let config = Piaf.Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
      let server = Piaf.Server.create ~config (handler t) in
      ignore (Piaf.Server.Command.start ~sw:server_sw env server : Piaf.Server.Command.t);
      Eio.Promise.resolve notify ());
    `Stop_daemon);
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () -> Eio.Promise.await ready);
  t
;;

let await_request t env index =
  Tui_fixture.await env (fun () -> List.nth t.requests index)
;;

let request_count t = List.length t.requests
let request_at t index = List.nth t.requests index
let body t = t.body

let emit t events =
  if t.closed then failwith "provider stream is already closed";
  let push fields =
    let fields =
      ("sequence_number", `Number (Int64.to_string t.next_sequence)) :: fields
    in
    t.next_sequence <- Int64.succ t.next_sequence;
    t.push (Some ("data: " ^ Jsonaf.to_string (`Object fields) ^ "\n\n"))
  in
  List.iter events ~f:(fun event ->
    let fields =
      match Res.Response_stream.jsonaf_of_t event with
      | `Object fields -> fields
      | _ -> assert false
    in
    let item_id =
      match List.Assoc.find fields "item" ~equal:String.equal with
      | Some (`Object item) -> List.Assoc.find item "id" ~equal:String.equal
      | _ -> List.Assoc.find fields "item_id" ~equal:String.equal
    in
    let actual_index =
      match item_id with
      | Some (`String id) ->
        let index =
          match Map.find t.item_indices id with
          | Some index -> index
          | None ->
            let index = Map.length t.item_indices in
            t.item_indices <- Map.set t.item_indices ~key:id ~data:index;
            index
        in
        Some index
      | _ -> None
    in
    (match event with
     | Res.Response_stream.Output_item_done { item; _ } ->
       let item : Res.Item.t =
         match item with
         | Input_message value -> Input_message value
         | Output_message value -> Output_message value
         | Function_call value -> Function_call value
         | Custom_function value -> Custom_tool_call value
         | Reasoning value -> Reasoning value
       in
       t.finalized <- Map.set t.finalized ~key:(Option.value_exn actual_index) ~data:item
     | _ -> ());
    let fields =
      match actual_index with
      | None -> fields
      | Some index ->
        List.map fields ~f:(fun (name, value) ->
          if String.equal name "output_index"
          then name, `Number (Int.to_string index)
          else name, value)
    in
    push fields;
    let part =
      match event with
      | Res.Response_stream.Output_item_added
          { item = Reasoning { id; summary = []; _ }; _ } ->
        Some (id, "response.reasoning_summary_part.added", "summary_index", "summary_text")
      | Output_item_added { item = Output_message { id; content = []; _ }; _ } ->
        Some (id, "response.content_part.added", "content_index", "output_text")
      | _ -> None
    in
    Option.iter part ~f:(fun (id, type_, index_name, part_type) ->
      push
        [ "type", `String type_
        ; "item_id", `String id
        ; "output_index", `Number (Int.to_string (Option.value_exn actual_index))
        ; index_name, `Number "0"
        ; ( "part"
          , `Object
              ([ "type", `String part_type; "text", `String "" ]
               @
               if String.equal part_type "output_text"
               then [ "annotations", `Array [] ]
               else []) )
        ]))
;;

let empty_response : Res.Response.t =
  { id = "tui-response"
  ; object_ = "response"
  ; created_at = 0
  ; status = Completed
  ; error = None
  ; incomplete_details = None
  ; instructions = None
  ; max_output_tokens = None
  ; model = "gpt-4.1"
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

let reply_summary t text =
  if t.closed then failwith "provider request is already closed";
  let output =
    Res.Item.Output_message
      { role = Assistant
      ; id = "tui-summary"
      ; status = "completed"
      ; phase = None
      ; _type = "message"
      ; content = [ { annotations = []; text; _type = "output_text" } ]
      }
  in
  let response =
    Res.Response.jsonaf_of_t { empty_response with output = [ output ] }
    |> Jsonaf.to_string
  in
  t.push
    (Some
       (if t.streaming
        then
          "event: response.completed\n\
           data: {\"type\":\"response.completed\",\"sequence_number\":0,\"response\":"
          ^ response
          ^ "}\n\n"
        else response));
  t.closed <- true;
  t.push None
;;

let finish t =
  if t.closed || not t.streaming then failwith "expected an open stream";
  let response = { empty_response with output = Map.data t.finalized } in
  emit
    t
    [ Res.Response_stream.Response_completed { type_ = "response.completed"; response } ];
  t.closed <- true;
  t.push (Some "data: [DONE]\n\n");
  t.push None
;;

let reasoning id text =
  Res.Response_stream.Item.Reasoning
    { id
    ; summary =
        (if String.is_empty text then [] else [ { text; _type = "summary_text" } ])
    ; _type = "reasoning"
    ; status = Some (if String.is_empty text then "in_progress" else "completed")
    }
;;

let message id text =
  Res.Response_stream.Item.Output_message
    { id
    ; role = Assistant
    ; content =
        (if String.is_empty text
         then []
         else [ { annotations = []; text; _type = "output_text" } ])
    ; status = (if String.is_empty text then "in_progress" else "completed")
    ; phase = None
    ; _type = "message"
    }
;;

let index = function
  | Res.Response_stream.Item.Reasoning _ -> 0
  | _ -> 1
;;

let added item =
  Res.Response_stream.Output_item_added
    { item; output_index = index item; type_ = "response.output_item.added" }
;;

let done_ item =
  Res.Response_stream.Output_item_done
    { item; output_index = index item; type_ = "response.output_item.done" }
;;

let reasoning_delta id delta =
  Res.Response_stream.Reasoning_summary_text_delta
    { item_id = id
    ; delta
    ; summary_index = 0
    ; output_index = 0
    ; type_ = "response.reasoning_summary_text.delta"
    }
;;

let text_delta id delta =
  Res.Response_stream.Output_text_delta
    { item_id = id
    ; delta
    ; content_index = 0
    ; output_index = 1
    ; type_ = "response.output_text.delta"
    }
;;

let fork_call_for ~call_id =
  let item_id = call_id ^ "-item" in
  let arguments = {|{"command":"return fixture result","arguments":[]}|} in
  let item =
    Res.Response_stream.Item.Function_call
      { name = "fork"
      ; arguments = ""
      ; call_id
      ; _type = "function_call"
      ; id = Some item_id
      ; status = Some "in_progress"
      }
  in
  [ added item
  ; Res.Response_stream.Function_call_arguments_done
      { arguments
      ; item_id
      ; output_index = 1
      ; type_ = "response.function_call_arguments.done"
      }
  ; done_
      (Res.Response_stream.Item.Function_call
         { name = "fork"
         ; arguments
         ; call_id
         ; _type = "function_call"
         ; id = Some item_id
         ; status = Some "completed"
         })
  ]
;;

let fork_call () = fork_call_for ~call_id:"tui-fork"
