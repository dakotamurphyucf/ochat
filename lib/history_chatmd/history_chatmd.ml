open! Core
module Payload = History_entry.Payload

let attribute name value = Printf.sprintf " %s=%S" name value
let history_id id = attribute "ochat-history-id" (History_entry.Id.to_string id)

let role = function
  | Payload.Role.System -> "system"
  | Developer -> "developer"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"
;;

let raw text = Printf.sprintf "RAW|\n%s\n|RAW" text

module Rendering = struct
  include Chatmd_output

  let attribute t name value =
    text t " ";
    text t name;
    text t "=";
    quoted t value
  ;;

  let present_attribute t name = function
    | Payload.Presence.Value value -> attribute t name value
    | Absent | Null -> ()
  ;;

  let quoted_history_id t id =
    let namespace = History_entry.Id.namespace id in
    quoted
      t
      ~prefix:(Int.to_string (String.length namespace) ^ ":")
      ~suffix:(":" ^ Int.to_string (History_entry.Id.sequence id))
      namespace
  ;;

  let history_id t = function
    | None -> ()
    | Some id ->
      text t " ochat-history-id=";
      quoted_history_id t id
  ;;

  let raw t value =
    text t "RAW|\n";
    text t value;
    text t "\n|RAW"
  ;;

  let separated t values ~f =
    let first = ref true in
    List.iter values ~f:(fun value ->
      charge t ~steps:1;
      if !first then first := false else text t "\n";
      f value)
  ;;

  let content t = function
    | Payload.Content.Text { text = value; _ } -> raw t value
    | Refusal value ->
      text t "<refusal>";
      raw t value;
      text t "</refusal>"
    | Image { uri; detail } ->
      text t "<img src=";
      quoted t uri;
      present_attribute t "detail" detail;
      text t " />"
    | Unknown { kind; raw = value } ->
      text t "<ochat-unknown-content kind=";
      quoted t kind;
      text t ">RAW|\n";
      json t value;
      text t "\n|RAW</ochat-unknown-content>"
  ;;

  let output t = function
    | Payload.Output.Text value -> raw t value
    | Content parts -> separated t parts ~f:(content t)
  ;;

  let payload t ~entry_id payload =
    charge t ~steps:1;
    (* Both accessors borrow the already validated immutable projections. *)
    let semantic = Payload.semantic payload in
    let metadata = Payload.Semantic.metadata semantic in
    let item_id () = present_attribute t "id" metadata.item_id in
    let status () = present_attribute t "status" metadata.status in
    let call_id () = present_attribute t "tool_call_id" metadata.call_id in
    let host_id () = history_id t entry_id in
    match Payload.Semantic.view semantic with
    | Message { form; role = message_role; content = parts; phase } ->
      let assistant =
        match form, message_role with
        | Output, Payload.Role.Assistant -> true
        | (Input | Output), (System | Developer | User | Assistant | Tool) -> false
      in
      if assistant
      then text t "<assistant"
      else (
        text t "<msg role=";
        quoted t (role message_role));
      item_id ();
      status ();
      present_attribute t "phase" phase;
      host_id ();
      text t ">\n";
      separated t parts ~f:(content t);
      text t (if assistant then "\n</assistant>\n" else "\n</msg>\n")
    | Call { kind; name; namespace; input_bytes; async } ->
      text t "<tool_call function_name=";
      quoted t name;
      (match kind with
       | Payload.Call_kind.Function -> ()
       | Custom -> text t " type=\"custom_tool_call\"");
      call_id ();
      item_id ();
      present_attribute t "namespace" namespace;
      (match async with
       | Payload.Presence.Value value -> attribute t "async" (Bool.to_string value)
       | Absent | Null -> ());
      host_id ();
      text t ">\n";
      raw t input_bytes;
      text t "\n</tool_call>\n"
    | Result { relation; kind; output = value } ->
      text t "<tool_response";
      (match kind with
       | Payload.Call_kind.Function -> ()
       | Custom -> text t " type=\"custom_tool_call\"");
      call_id ();
      (match relation with
       | Payload.Call_relation.Bound id ->
         text t " ochat-call-entry-id=";
         quoted_history_id t id
       | Unresolved -> ());
      host_id ();
      text t ">\n";
      output t value;
      text t "\n</tool_response>\n"
    | Reasoning { readable_summary } ->
      text t "<reasoning";
      item_id ();
      status ();
      host_id ();
      text t ">";
      List.iter readable_summary ~f:(fun value ->
        charge t ~steps:1;
        text t "<summary type=\"summary_text\">";
        raw t value;
        text t "</summary>");
      text t "</reasoning>\n"
    | Unknown { provider_kind } ->
      text t "<ochat-unknown-item kind=";
      quoted t provider_kind;
      host_id ();
      text t ">\nRAW|\n";
      json t (Payload.to_json payload);
      text t "\n|RAW\n</ochat-unknown-item>\n"
  ;;
end

let render_payload entry_id payload =
  Rendering.render_unbounded ~f:(fun output ->
    Rendering.payload output ~entry_id:(Some entry_id) payload)
;;

let render_anonymous payload =
  Rendering.render_unbounded ~f:(fun output ->
    Rendering.payload output ~entry_id:None payload)
;;

let emit_entries output entries =
  List.iter entries ~f:(fun entry ->
    Rendering.charge output ~steps:1;
    Rendering.payload
      output
      ~entry_id:(Some (History_entry.id entry))
      (History_entry.payload entry))
;;

let render entries =
  Rendering.render_unbounded ~f:(fun output -> emit_entries output entries)
;;

let render_bounded entries ~limits ~checkpoint =
  Rendering.render limits ~checkpoint ~f:(fun output -> emit_entries output entries)
;;
