open! Core
module Payload = History_entry.Payload
module Public = Agent_protocol.Public.History

let render_payload = History_chatmd.render_payload
let history_id = History_chatmd.history_id
let role = History_chatmd.role
let raw = History_chatmd.raw

let annotation = function
  | Agent_protocol.History.Runtime_notification id ->
    Printf.sprintf
      "<!-- ochat-runtime-notification delivery_id=%S -->\n"
      (Agent_protocol.Id.Delivery.to_string id)
  | Runtime_authoring guidance ->
    Printf.sprintf
      "<!-- ochat-runtime-authoring %s -->\n"
      (Agent_protocol.Authoring_guidance.to_json guidance |> Jsonaf.to_string)
  | Canonical | Moderator_inserted | Moderator_replaced _ -> ""
;;

let render = History_chatmd.render

let render_protocol entries =
  List.map entries ~f:(fun entry ->
    let open Result.Let_syntax in
    let%map history = History_codec.of_protocol entry in
    annotation entry.Agent_protocol.History.provenance
    ^ render_payload (History_entry.id history) (History_entry.payload history))
  |> Result.all
  |> Result.map ~f:String.concat
;;

let header_label = function
  | Transcript.Header.Message value -> role value
  | Call _ -> "tool call"
  | Result _ -> "tool result"
  | Reasoning -> "reasoning"
  | Unknown kind -> "unknown " ^ kind
;;

let render_public_entry (entry : Public.t) =
  match entry.body with
  | Full payload -> annotation entry.provenance ^ render_payload entry.id payload
  | Visible visible ->
    let body =
      match visible with
      | Public.Visible.Message { content; _ } ->
        List.map content ~f:(function
          | Public.Visible.Text text | Refusal text -> text
          | Image { uri; _ } -> Printf.sprintf "[Image: %s]" uri
          | Redacted_part { kind } -> Printf.sprintf "[Redacted content: %s]" kind)
        |> String.concat ~sep:"\n"
      | Reasoning { readable_summary } -> String.concat ~sep:"\n" readable_summary
    in
    (* Disclosure markup is deliberately not canonical ChatMD message syntax. *)
    Printf.sprintf
      "<ochat-public-view disclosure=\"visible\" role=%S%s>\n%s\n</ochat-public-view>\n"
      (Public.Visible.header visible |> header_label)
      (history_id entry.id)
      (raw body)
  | Redacted redaction ->
    let role =
      Option.value_map redaction.disclosed_header ~default:"unavailable" ~f:header_label
    in
    Printf.sprintf
      "<ochat-public-view disclosure=\"redacted\" role=%S%s>\n\
       [Content redacted]\n\
       </ochat-public-view>\n"
      role
      (history_id entry.id)
;;

let render_public entries = List.map entries ~f:render_public_entry |> String.concat
