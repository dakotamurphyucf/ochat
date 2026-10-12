open! Core
module Public = Agent_protocol.Public.History
module Rendering = History_chatmd.Rendering

let emit_annotation output = function
  | Agent_protocol.History.Runtime_notification id ->
    Rendering.text output "<!-- ochat-runtime-notification delivery_id=";
    Rendering.quoted output (Agent_protocol.Id.Delivery.to_string id);
    Rendering.text output " -->\n"
  | Runtime_authoring guidance ->
    (* The validated provenance has fixed schema/count/string bounds. Admit its
       actual projection cells before the existing bounded to_json allocation. *)
    Rendering.charge output ~steps:16;
    List.iter guidance.Agent_protocol.Authoring_guidance.topics ~f:(fun _ ->
      Rendering.charge output ~steps:16);
    List.iter guidance.fragments ~f:(fun fragment ->
      Rendering.charge output ~steps:10;
      List.iter fragment.parts ~f:(fun _ -> Rendering.charge output ~steps:8));
    let json = Agent_protocol.Authoring_guidance.to_json guidance in
    Rendering.charge output ~steps:0;
    Rendering.text output "<!-- ochat-runtime-authoring ";
    Rendering.json output json;
    Rendering.text output " -->\n"
  | Canonical | Moderator_inserted | Moderator_replaced _ -> ()
;;

let render = History_chatmd.render

let render_protocol entries =
  let module Failure = struct
    exception Decode of Agent_protocol.Error.t
  end
  in
  try
    Ok
      (Rendering.render_unbounded ~f:(fun output ->
         List.iter entries ~f:(fun entry ->
           let history =
             match History_codec.of_protocol entry with
             | Ok history -> history
             | Error error -> raise (Failure.Decode error)
           in
           emit_annotation output entry.Agent_protocol.History.provenance;
           Rendering.payload
             output
             ~entry_id:(Some (History_entry.id history))
             (History_entry.payload history))))
  with
  | Failure.Decode error -> Error error
;;

let emit_header output = function
  | Transcript.Header.Message value -> Rendering.quoted output (History_chatmd.role value)
  | Call _ -> Rendering.quoted output "tool call"
  | Result _ -> Rendering.quoted output "tool result"
  | Reasoning -> Rendering.quoted output "reasoning"
  | Unknown kind -> Rendering.quoted output ~prefix:"unknown " kind
;;

let emit_history_id output id =
  Rendering.text output " ochat-history-id=";
  Rendering.quoted_history_id output id
;;

let separated output values ~f =
  let first = ref true in
  List.iter values ~f:(fun value ->
    Rendering.charge output ~steps:1;
    if !first then first := false else Rendering.text output "\n";
    f value)
;;

let emit_visible output = function
  | Public.Visible.Message { content; _ } ->
    separated output content ~f:(function
      | Public.Visible.Text text | Refusal text -> Rendering.text output text
      | Image { uri; _ } ->
        Rendering.text output "[Image: ";
        Rendering.text output uri;
        Rendering.text output "]"
      | Redacted_part { kind } ->
        Rendering.text output "[Redacted content: ";
        Rendering.text output kind;
        Rendering.text output "]")
  | Reasoning { readable_summary } ->
    separated output readable_summary ~f:(Rendering.text output)
;;

let emit_public_entry output (entry : Public.t) =
  Rendering.charge output ~steps:1;
  match entry.body with
  | Full payload ->
    emit_annotation output entry.provenance;
    Rendering.payload output ~entry_id:(Some entry.id) payload
  | Visible visible ->
    (* Disclosure markup remains distinct from canonical ChatMD message syntax. *)
    Rendering.text output "<ochat-public-view disclosure=\"visible\" role=";
    emit_header output (Public.Visible.header visible);
    emit_history_id output entry.id;
    Rendering.text output ">\nRAW|\n";
    emit_visible output visible;
    Rendering.text output "\n|RAW\n</ochat-public-view>\n"
  | Redacted redaction ->
    Rendering.text output "<ochat-public-view disclosure=\"redacted\" role=";
    (match redaction.disclosed_header with
     | None -> Rendering.quoted output "unavailable"
     | Some header -> emit_header output header);
    emit_history_id output entry.id;
    Rendering.text output ">\n[Content redacted]\n</ochat-public-view>\n"
;;

let emit_public output entries = List.iter entries ~f:(emit_public_entry output)

let render_public entries =
  Rendering.render_unbounded ~f:(fun output -> emit_public output entries)
;;

let render_public_bounded entries ~limits ~checkpoint =
  Rendering.render limits ~checkpoint ~f:(fun output -> emit_public output entries)
;;
