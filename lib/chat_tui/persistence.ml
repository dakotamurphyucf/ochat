open Core
module Fetch = Chat_response.Fetch
module Value_codec = Chatml.Chatml_value_codec
module Moderator = Session.Moderator_snapshot

let write_user_message ~dir ~file message =
  let xml = Io.load_doc ~dir file in
  let xml = String.rstrip xml in
  let user_open = "<user>" in
  let user_close = "</user>" in
  let new_msg = Printf.sprintf "%s\n%s\n%s\n" user_open message user_close in
  let updated_xml =
    if String.is_suffix xml ~suffix:(user_open ^ "\n\n" ^ user_close)
    then (
      let base =
        String.drop_suffix xml (String.length user_open + String.length user_close + 2)
      in
      base ^ new_msg)
    else xml ^ "\n" ^ new_msg
  in
  Io.save_doc ~dir file updated_xml
;;

let generic_msg_as_chatmd ?id ~role content =
  let id_attr =
    match id with
    | None -> ""
    | Some id -> Printf.sprintf " id=%S" id
  in
  Printf.sprintf "<msg role=%S%s>\n%s\n</msg>\n" role id_attr content
;;

let history_id_attribute id =
  Printf.sprintf " ochat-history-id=%S" (History_entry.Id.to_string id)
;;

let history_entry_as_chatmd entry = History_chatmd.render [ entry ]
let canonical_entries_as_chatmd = History_chatmd.render

module Checkpoint = struct
  type t = string Hashtbl.M(History_entry.Id).t

  let empty () = Hashtbl.create (module History_entry.Id)

  let fingerprint entry =
    History_entry.payload entry |> History_entry.Payload.to_json |> Jsonaf.to_string
  ;;

  let of_entries entries =
    let checkpoint = empty () in
    List.iter entries ~f:(fun entry ->
      Hashtbl.set checkpoint ~key:(History_entry.id entry) ~data:(fingerprint entry));
    checkpoint
  ;;

  let contains_unchanged t entry =
    Hashtbl.find t (History_entry.id entry)
    |> Option.exists ~f:(String.equal (fingerprint entry))
  ;;
end

let entries_after_checkpoint checkpoint history =
  List.filter history ~f:(fun entry ->
    not (Checkpoint.contains_unchanged checkpoint entry))
;;

let payload_of_moderator_item (item : Moderator.Item.t) =
  let open Result.Let_syntax in
  let result =
    let%bind value = Value_codec.Snapshot.to_value item.value in
    let%bind json = Value_codec.value_to_jsonaf_result value in
    Chat_response.Moderation.Item.to_payload
      (Chat_response.Moderation.Item.create ~id:item.id ~value:json)
  in
  Result.ok result
;;

let moderation_item_as_chatmd (item : Moderator.Item.t) =
  match payload_of_moderator_item item with
  | Some payload -> History_chatmd.render_anonymous payload
  | None -> generic_msg_as_chatmd ~id:item.id ~role:"developer" "Invalid moderation item"
;;

let moderation_replacement_as_chatmd (replacement : Moderator.Overlay.replacement) =
  let rendered =
    match payload_of_moderator_item replacement.item with
    | Some payload -> History_chatmd.render_anonymous payload
    | None -> "Invalid moderation item"
  in
  generic_msg_as_chatmd
    ~id:(Printf.sprintf "moderation-replacement-%s" replacement.target_id)
    ~role:"developer"
    (Printf.sprintf "Moderator replaced item %S with:\n%s" replacement.target_id rendered)
;;

let moderation_deletion_as_chatmd deleted_message_id =
  let content =
    Printf.sprintf
      "Moderator deleted message %S from the effective transcript."
      deleted_message_id
  in
  generic_msg_as_chatmd
    ~id:(Printf.sprintf "moderation-deletion-%s" deleted_message_id)
    ~role:"developer"
    content
;;

let moderation_halt_as_chatmd reason =
  generic_msg_as_chatmd
    ~id:"moderation-halt"
    ~role:"developer"
    (Printf.sprintf "Session ended by moderator: %s" reason)
;;

let overlay_as_chatmd (overlay : Moderator.Overlay.t) =
  let prepended = List.map overlay.prepended_system_items ~f:moderation_item_as_chatmd in
  let appended = List.map overlay.appended_items ~f:moderation_item_as_chatmd in
  let replacements = List.map overlay.replacements ~f:moderation_replacement_as_chatmd in
  let deletions = List.map overlay.deleted_item_ids ~f:moderation_deletion_as_chatmd in
  let halt =
    Option.to_list (Option.map overlay.halted_reason ~f:moderation_halt_as_chatmd)
  in
  String.concat ~sep:"" (prepended @ appended @ replacements @ deletions @ halt)
;;

let history_entries_as_chatmd ~moderator_snapshot ~history =
  let canonical = canonical_entries_as_chatmd history in
  let overlay =
    Option.value_map moderator_snapshot ~default:"" ~f:(fun snapshot ->
      overlay_as_chatmd snapshot.Moderator.overlay)
  in
  canonical ^ overlay
;;

let persist_entries
      ~dir
      ~prompt_file
      ~(checkpoint : Checkpoint.t)
      ~moderator_snapshot
      ~history
  =
  let suffix = entries_after_checkpoint checkpoint history in
  let rendered = history_entries_as_chatmd ~moderator_snapshot ~history:suffix in
  let existing = Io.load_doc ~dir prompt_file |> String.rstrip in
  Io.save_doc ~dir prompt_file (existing ^ "\n" ^ rendered)
;;
