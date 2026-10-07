open! Core
module Payload = History_entry.Payload

let attribute name value = Printf.sprintf " %s=%S" name value
let history_id id = attribute "ochat-history-id" (History_entry.Id.to_string id)

let present_attribute name = function
  | Payload.Presence.Value value -> attribute name value
  | Absent | Null -> ""
;;

let role = function
  | Payload.Role.System -> "system"
  | Developer -> "developer"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"
;;

let raw text = Printf.sprintf "RAW|\n%s\n|RAW" text

let content = function
  | Payload.Content.Text { text; _ } -> raw text
  | Refusal text -> Printf.sprintf "<refusal>%s</refusal>" (raw text)
  | Image { uri; detail } ->
    Printf.sprintf "<img src=%S%s />" uri (present_attribute "detail" detail)
  | Unknown { kind; raw = value } ->
    Printf.sprintf
      "<ochat-unknown-content kind=%S>%s</ochat-unknown-content>"
      kind
      (raw (Jsonaf.to_string value))
;;

let output = function
  | Payload.Output.Text text -> raw text
  | Content parts -> List.map parts ~f:content |> String.concat ~sep:"\n"
;;

let render_payload_optional ?entry_id payload =
  let semantic = Payload.semantic payload in
  let metadata = Payload.Semantic.metadata semantic in
  let host_id = Option.value_map entry_id ~default:"" ~f:history_id in
  let item_id = present_attribute "id" metadata.item_id in
  let status = present_attribute "status" metadata.status in
  let call_id = present_attribute "tool_call_id" metadata.call_id in
  match Payload.Semantic.view semantic with
  | Message { form; role = message_role; content = parts; phase } ->
    let body = List.map parts ~f:content |> String.concat ~sep:"\n" in
    let phase = present_attribute "phase" phase in
    (match form, message_role with
     | Output, Payload.Role.Assistant ->
       Printf.sprintf
         "<assistant%s%s%s%s>\n%s\n</assistant>\n"
         item_id
         status
         phase
         host_id
         body
     | (Input | Output), (System | Developer | User | Assistant | Tool) ->
       Printf.sprintf
         "<msg role=%S%s%s%s%s>\n%s\n</msg>\n"
         (role message_role)
         item_id
         status
         phase
         host_id
         body)
  | Call { kind; name; namespace; input_bytes; async } ->
    let kind =
      match kind with
      | Payload.Call_kind.Function -> ""
      | Custom -> " type=\"custom_tool_call\""
    in
    let async =
      match async with
      | Payload.Presence.Value value -> attribute "async" (Bool.to_string value)
      | Absent | Null -> ""
    in
    Printf.sprintf
      "<tool_call function_name=%S%s%s%s%s%s%s>\n%s\n</tool_call>\n"
      name
      kind
      call_id
      item_id
      (present_attribute "namespace" namespace)
      async
      host_id
      (raw input_bytes)
  | Result { relation; kind; output = value } ->
    let kind =
      match kind with
      | Payload.Call_kind.Function -> ""
      | Custom -> " type=\"custom_tool_call\""
    in
    let relation =
      match relation with
      | Payload.Call_relation.Bound id ->
        attribute "ochat-call-entry-id" (History_entry.Id.to_string id)
      | Unresolved -> ""
    in
    Printf.sprintf
      "<tool_response%s%s%s%s>\n%s\n</tool_response>\n"
      kind
      call_id
      relation
      host_id
      (output value)
  | Reasoning { readable_summary } ->
    let summaries =
      List.map readable_summary ~f:(fun text ->
        Printf.sprintf "<summary type=\"summary_text\">%s</summary>" (raw text))
      |> String.concat
    in
    Printf.sprintf "<reasoning%s%s%s>%s</reasoning>\n" item_id status host_id summaries
  | Unknown { provider_kind } ->
    Printf.sprintf
      "<ochat-unknown-item kind=%S%s>\n%s\n</ochat-unknown-item>\n"
      provider_kind
      host_id
      (raw (Payload.to_json payload |> Jsonaf.to_string))
;;

let render_payload entry_id payload = render_payload_optional ~entry_id payload
let render_anonymous payload = render_payload_optional payload

let render entries =
  List.map entries ~f:(fun entry ->
    render_payload (History_entry.id entry) (History_entry.payload entry))
  |> String.concat
;;
