open! Core
open Types
module Payload = History_entry.Payload
module Public = Agent_protocol.Public.History

let role_string = function
  | Payload.Role.System -> "system"
  | Developer -> "developer"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool_output"
;;

let role_of_header = function
  | Transcript.Header.Message value -> role_string value
  | Call _ -> "tool"
  | Result _ -> "tool_output"
  | Reasoning -> "reasoning"
  | Unknown _ -> "unknown"
;;

let image uri = Printf.sprintf "<image src=%S />" uri

let content_text = function
  | Payload.Content.Text { text; _ } -> text
  | Refusal text -> text
  | Image { uri; _ } -> image uri
  | Unknown { kind; raw } ->
    Printf.sprintf "[Unknown content: %s]\n%s" kind (Jsonaf.to_string raw)
;;

let output_text = function
  | Payload.Output.Text text -> text
  | Content parts -> List.map parts ~f:content_text |> String.concat ~sep:"\n"
;;

module Rendered = struct
  type t =
    { message : message
    ; copy_text : string option
    }

  let create ?copy_text ~role ~strip text =
    { message = role, Util.sanitize ~strip text; copy_text }
  ;;

  let message t = t.message
  let copy_text t = t.copy_text

  let message_content ~form ~role:message_role content =
    let sep =
      match form with
      | Payload.Semantic.Input -> "\n"
      | Output -> " "
    in
    let text = List.map content ~f:content_text |> String.concat ~sep in
    let copy_text =
      if
        List.for_all content ~f:(function
          | Payload.Content.Text _ -> true
          | Refusal _ | Image _ | Unknown _ -> false)
      then Some text
      else None
    in
    let strip =
      match form with
      | Payload.Semantic.Input -> true
      | Output -> false
    in
    create ?copy_text ~role:(role_string message_role) ~strip text
  ;;

  let of_payload payload =
    match Payload.Semantic.view (Payload.semantic payload) with
    | Message { form; role; content; _ } -> message_content ~form ~role content
    | Call { name; input_bytes; _ } ->
      create ~role:"tool" ~strip:true (Printf.sprintf "%s(%s)" name input_bytes)
    | Result { output; _ } ->
      let text = Util.sanitize ~strip:false (output_text output) in
      let text =
        if String.length text > 10_000
        then String.prefix text 10_000 ^ "\n…truncated…"
        else text
      in
      { message = "tool_output", text; copy_text = None }
    | Reasoning { readable_summary } ->
      create ~role:"reasoning" ~strip:false (String.concat ~sep:" " readable_summary)
    | Unknown { provider_kind } ->
      create
        ~role:"unknown"
        ~strip:false
        (Printf.sprintf
           "[Unknown item: %s]\n%s"
           provider_kind
           (Payload.to_json payload |> Jsonaf.to_string))
  ;;

  let of_visible = function
    | Public.Visible.Message { form; role; content; _ } ->
      let sep =
        match form with
        | Payload.Semantic.Input -> "\n"
        | Output -> " "
      in
      let text =
        List.map content ~f:(function
          | Public.Visible.Text text | Refusal text -> text
          | Image { uri; _ } -> image uri
          | Redacted_part { kind } -> Printf.sprintf "[Redacted content: %s]" kind)
        |> String.concat ~sep
      in
      let copy_text =
        if
          List.for_all content ~f:(function
            | Public.Visible.Text _ -> true
            | Refusal _ | Image _ | Redacted_part _ -> false)
        then Some text
        else None
      in
      create
        ?copy_text
        ~role:(role_string role)
        ~strip:
          (match form with
           | Input -> true
           | Output -> false)
        text
    | Reasoning { readable_summary } ->
      create ~role:"reasoning" ~strip:false (String.concat ~sep:" " readable_summary)
  ;;

  let of_redaction (value : Public.Redaction.t) =
    let label =
      Option.value_map value.disclosed_header ~default:"redacted" ~f:role_of_header
    in
    create ~role:label ~strip:false "[Content redacted]"
  ;;

  let of_draft (view : Transcript.Draft.item_view) =
    match view.state with
    | Finalized entry ->
      { (of_payload (History_entry.payload entry)) with copy_text = None }
    | Partial partial ->
      let label =
        Option.value_map view.descriptor.header ~default:"unavailable" ~f:role_of_header
      in
      let prefix =
        match partial.completeness with
        | Prefix_observed -> ""
        | Missing_prefix -> "[Earlier live content unavailable]\n"
      in
      let observed_text (text : Transcript.Draft.text) =
        match partial.completeness, text.completeness with
        | Prefix_observed, Missing_prefix ->
          "[Earlier part content unavailable]\n" ^ text.value
        | Missing_prefix, (Prefix_observed | Missing_prefix)
        | Prefix_observed, Prefix_observed -> text.value
      in
      let text =
        match partial.call_input with
        | Some input ->
          Printf.sprintf
            "%s(%s)"
            (Option.value view.descriptor.call_name ~default:"[Call name unavailable]")
            (observed_text input)
        | None ->
          List.map partial.parts ~f:(fun part ->
            match part.text with
            | Some text -> observed_text text
            | None ->
              (match part.descriptor.kind with
               | Image -> "[Image draft]"
               | Unknown kind -> Printf.sprintf "[Unknown draft content: %s]" kind
               | Text | Refusal | Reasoning_summary | Reasoning_text -> ""))
          |> String.concat ~sep:"\n"
      in
      create ~role:label ~strip:false (prefix ^ text)
  ;;
end

let of_history entries =
  List.map entries ~f:(fun entry ->
    Rendered.of_payload (History_entry.payload entry) |> Rendered.message)
;;

type projection =
  { rows : Projected_message.t list
  ; index_by_id : (Projected_message.Id.t, int) Hashtbl.t
  }

let create_projection rows =
  let index_by_id = Hashtbl.create (module Projected_message.Id) in
  List.iteri rows ~f:(fun index row ->
    Hashtbl.add_exn index_by_id ~key:row.Projected_message.id ~data:index);
  { rows; index_by_id }
;;

let canonical_row entry =
  let rendered = Rendered.of_payload (History_entry.payload entry) in
  Projected_message.canonical_row
    ?editing_text:(Rendered.copy_text rendered)
    ~entry_id:(History_entry.id entry)
    (Rendered.message rendered)
;;

let project_entries entries = List.map entries ~f:canonical_row |> create_projection

let project_effective_entry
      ({ entry; provenance } : Chat_response.Moderation.Effective_entry.t)
  =
  let row = canonical_row entry in
  match provenance with
  | Canonical -> row
  | Moderator_inserted { change_id } ->
    { row with
      provenance = Moderator_inserted { change_id }
    ; source = Moderator_inserted { entry_id = History_entry.id entry; change_id }
    ; editing_text = None
    }
  | Moderator_replacement { target_id; change_id } ->
    { row with
      id = Projected_message.Id.canonical target_id
    ; entry_id = Some target_id
    ; provenance = Moderator_replacement { target_id; change_id }
    ; source = Moderator_replacement { target_id; change_id }
    ; editing_text = None
    }
;;

let project_effective_entries entries =
  List.map entries ~f:project_effective_entry |> create_projection
;;

let public_row (entry : Public.t) =
  let rendered, disclosure, editable =
    match entry.body with
    | Full payload ->
      let rendered = Rendered.of_payload payload in
      rendered, Projected_message.Full, Rendered.copy_text rendered
    | Visible value -> Rendered.of_visible value, Visible, None
    | Redacted value -> Rendered.of_redaction value, Redacted, None
  in
  let host_id =
    match entry.provenance with
    | Agent_protocol.History.Moderator_replaced id -> id
    | Canonical | Moderator_inserted | Runtime_notification _ | Runtime_authoring _ ->
      entry.id
  in
  Projected_message.
    { id = Id.canonical host_id
    ; entry_id = Some host_id
    ; message = Rendered.message rendered
    ; provenance = Public entry.provenance
    ; source =
        Public_history { entry_id = host_id; provenance = entry.provenance; disclosure }
    ; editing_text = editable
    ; revision = 0
    }
;;

let project_public_entries entries = List.map entries ~f:public_row |> create_projection

let draft_row (view : Transcript.Draft.item_view) =
  let key =
    Transcript.Item.key view.descriptor
    |> Transcript.Item.Key.sexp_of_t
    |> Sexp.to_string_mach
  in
  let id =
    match view.descriptor.scope.relation, view.descriptor.entry_id with
    | Root, Some entry_id -> Projected_message.Id.canonical entry_id
    | Root, None | Nested _, _ ->
      Projected_message.Id.local ~namespace:"transcript-draft" ~local_id:key
      |> Result.ok_or_failwith
  in
  Projected_message.
    { id
    ; entry_id = view.descriptor.entry_id
    ; message = Rendered.of_draft view |> Rendered.message
    ; provenance = Streaming
    ; source = Draft { key }
    ; editing_text = None
    ; revision = 0
    }
;;

let rows t = t.rows
let messages t = List.map t.rows ~f:(fun row -> row.Projected_message.message)
let index_of_id t id = Hashtbl.find t.index_by_id id

let append_local t ~id ~message ~provenance ~source =
  let row =
    Projected_message.
      { id
      ; entry_id = None
      ; message
      ; provenance
      ; source
      ; editing_text = None
      ; revision = 0
      }
  in
  create_projection (t.rows @ [ row ])
;;

let append_pending_approval t ~local_id ~text =
  Result.map
    (Projected_message.Id.local ~namespace:"pending-approval" ~local_id)
    ~f:(fun id ->
      append_local
        t
        ~id
        ~message:("system", text)
        ~provenance:Projected_message.Pending_approval
        ~source:(Pending_approval { local_id }))
;;

let append_placeholder t ~local_id ~kind message =
  Result.map (Projected_message.Id.local ~namespace:"placeholder" ~local_id) ~f:(fun id ->
    append_local
      t
      ~id
      ~message
      ~provenance:Projected_message.Placeholder
      ~source:(Placeholder { local_id; kind }))
;;
