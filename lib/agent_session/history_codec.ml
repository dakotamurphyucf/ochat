open! Core
module Item = Openai.Responses.Item
module Input_message = Openai.Responses.Input_message

let invalid message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let role_of_input = function
  | Input_message.System | Developer -> Agent_protocol.History.System
  | User -> User
  | Assistant -> Assistant
;;

let classification = function
  | Item.Input_message message ->
    role_of_input message.role, Agent_protocol.History.Message
  | Output_message _ -> Assistant, Message
  | Reasoning _ -> Assistant, Reasoning
  | Function_call _ | Custom_tool_call _ -> Assistant, Tool_call
  | Function_call_output _ | Custom_tool_call_output _ -> Tool, Tool_output
  | Web_search_call _ | File_search_call _ -> Assistant, Other
;;

let to_protocol ?(provenance = Agent_protocol.History.Canonical) entry =
  let item = History_entry.item entry in
  let role, kind = classification item in
  Agent_protocol.History.
    { id = History_entry.id entry
    ; role
    ; kind
    ; payload = Item.jsonaf_of_t item
    ; provenance
    ; redacted = false
    }
;;

let decode_item payload =
  match Result.try_with (fun () -> Item.t_of_jsonaf payload) with
  | Ok item -> Ok item
  | Error exn -> Error (invalid ("invalid durable history payload: " ^ Exn.to_string exn))
;;

let of_protocol entry =
  if entry.Agent_protocol.History.redacted
  then Error (invalid "redacted history cannot be used as canonical model input")
  else
    let open Result.Let_syntax in
    let%bind () = Agent_protocol.History.validate_entry entry in
    let%map item = decode_item entry.payload in
    History_entry.create_with_id ~id:entry.id item
;;

let canonical_encoder ~previous =
  let provenance = Hashtbl.create (module History_entry.Id) in
  List.iter previous ~f:(fun entry ->
    Hashtbl.set provenance ~key:entry.Agent_protocol.History.id ~data:entry.provenance);
  fun entry ->
    to_protocol ?provenance:(Hashtbl.find provenance (History_entry.id entry)) entry
;;

let all_to_protocol ?(previous = []) entries =
  List.map entries ~f:(canonical_encoder ~previous)
;;

let all_of_protocol entries = Result.all (List.map entries ~f:of_protocol)

let user_text ~id text =
  let item =
    Item.Input_message
      { role = User
      ; content = [ Text { text; _type = "input_text" } ]
      ; _type = "message"
      }
  in
  History_entry.create_with_id ~id item
;;
