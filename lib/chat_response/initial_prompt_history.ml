open Core
module CM = Prompt.Chat_markdown

let plain_message (message : CM.msg) =
  let plain (item : CM.content_item) =
    match item with
    | CM.Agent _ -> false
    | Basic item ->
      String.equal item.type_ "text"
      && Option.is_none item.image_url
      && Option.is_none item.document_url
  in
  List.mem [ "user"; "assistant"; "developer"; "system" ] message.role ~equal:String.equal
  && Option.is_none message.function_call
  && Option.is_none message.tool_call
  && Option.is_none message.tool_call_id
  && Option.is_none message.ochat_history_id
  && Option.is_none message.id
  && Option.is_none message.status
  && Option.is_none message.phase
  && (match message.type_ with
      | None | Some "message" -> true
      | _ -> false)
  &&
  match message.content with
  | None | Some (Text _) -> true
  | Some (Items items) -> List.for_all items ~f:plain
;;

let create ~session_id elements =
  let module CM = Prompt.Chat_markdown in
  let module R = Openai.Responses in
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result elements ~init:() ~f:(fun () -> function
      | CM.Msg message
      | System message
      | Developer message
      | User message
      | Assistant message ->
        (match plain_message message with
         | true -> Ok ()
         | false ->
           Error
             (Agent_protocol.Error.invalid_request
                "delegation.initial_history: initial messages require literal text \
                 without resource loading or stored identities"))
      | Tool_call _ | Tool_response _ | Reasoning _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "delegation.initial_history: provider results and reasoning require \
              separate admission")
      | _ -> Ok ())
  in
  let%bind allocator =
    History_entry.Allocator.create
      ~namespace:(Agent_protocol.Id.Session.to_string session_id)
      ~next_sequence:0
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  let items =
    List.filter_map elements ~f:(function
      | CM.Msg message
      | System message
      | Developer message
      | User message
      | Assistant message ->
        let role =
          match message.role with
          | "system" -> R.Input_message.System
          | "developer" -> Developer
          | "user" -> User
          | "assistant" -> Assistant
          | _ -> assert false
        in
        let texts =
          match message.content with
          | None -> []
          | Some (CM.Text text) -> [ text ]
          | Some (Items items) ->
            List.map items ~f:(function
              | Basic item -> Option.value item.text ~default:""
              | Agent _ -> assert false)
        in
        Some
          (R.Item.Input_message
             { role
             ; content =
                 List.map texts ~f:(fun text ->
                   R.Input_message.Text { text; _type = "input_text" })
             ; _type = "message"
             })
      | _ -> None)
  in
  let%map history =
    List.map items ~f:(History_entry.create ~allocator)
    |> Result.all
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  history, History_entry.Allocator.next_sequence allocator
;;
