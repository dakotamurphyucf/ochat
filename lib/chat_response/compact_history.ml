open Core
open Openai.Responses

(** Helper to parse the [arguments] JSON of a [read_file] function call and
    extract the path of the file being requested.  We recognise both
    {"path": "..."} (the preferred schema) and {"file": "..."} as fallbacks
    because models occasionally emit the latter. *)
let read_file_path_of_arguments (json_string : string) : string option =
  match Jsonaf.of_string json_string with
  | exception _ -> None
  | `Object fields ->
    List.find_map fields ~f:(fun (key, value) ->
      match key, value with
      | ("path" | "file"), `String p -> Some p
      | _ -> None)
  | _ -> None
;;

(** [collapse_read_file_history items] walks an ordered transcript of
    conversation items and ensures that for each file there is at most one
    [Function_call_output] containing its full contents – the *newest* one.

    Earlier outputs are replaced by a short placeholder string while the
    original [Function_call] items are left intact, thereby satisfying the
    function-call / output pairing contract expected by the OpenAI Responses
    API without keeping redundant large blobs in the prompt. *)
let collapse_read_file_history
      ?(placeholder = "(stale) file content removed — see newer read_file output later")
      (items : Item.t list)
  : Item.t list
  =
  (* --------------------------------------------------------------------- *)
  (* Pass 1: map [call_id] → [file_path] for every [read_file] call          *)
  (* --------------------------------------------------------------------- *)
  let call_id_to_path : (string, string, String.comparator_witness) Map.t =
    List.fold
      items
      ~init:(Map.empty (module String))
      ~f:(fun acc item ->
        match item with
        | Item.Function_call ({ name = "read_file"; _ } as fc) ->
          (match read_file_path_of_arguments fc.arguments with
           | Some path -> Map.set acc ~key:fc.call_id ~data:path
           | None -> acc)
        | Item.Custom_tool_call ({ name = "read_file"; _ } as tc) ->
          (match read_file_path_of_arguments tc.input with
           | Some path -> Map.set acc ~key:tc.call_id ~data:path
           | None -> acc)
        | _ -> acc)
  in
  (* --------------------------------------------------------------------- *)
  (* Pass 2: record the *last* index of a Function_call_output per file      *)
  (* --------------------------------------------------------------------- *)
  let latest_output_idx_tbl : int String.Table.t = String.Table.create () in
  List.iteri items ~f:(fun idx item ->
    match item with
    | Item.Function_call_output fco ->
      (match Map.find call_id_to_path fco.call_id with
       | Some path -> Hashtbl.set latest_output_idx_tbl ~key:path ~data:idx
       | None -> ())
    | Item.Custom_tool_call_output tco ->
      (match Map.find call_id_to_path tco.call_id with
       | Some path -> Hashtbl.set latest_output_idx_tbl ~key:path ~data:idx
       | None -> ())
    | _ -> ());
  (* --------------------------------------------------------------------- *)
  (* Pass 3: build the transformed list, redacting stale outputs             *)
  (* --------------------------------------------------------------------- *)
  List.mapi items ~f:(fun idx item ->
    match item with
    | Item.Function_call_output fco ->
      (match Map.find call_id_to_path fco.call_id with
       | None -> item
       | Some path ->
         let latest_idx = Hashtbl.find_exn latest_output_idx_tbl path in
         if Int.equal idx latest_idx
         then item
         else (
           let redacted = { fco with output = Tool_output.Output.Text placeholder } in
           Item.Function_call_output redacted))
    | Item.Custom_tool_call_output tco ->
      (match Map.find call_id_to_path tco.call_id with
       | None -> item
       | Some path ->
         let latest_idx = Hashtbl.find_exn latest_output_idx_tbl path in
         if Int.equal idx latest_idx
         then item
         else (
           let redacted = { tco with output = Tool_output.Output.Text placeholder } in
           Item.Custom_tool_call_output redacted))
    | _ -> item)
;;

let collapse_read_file_entries
      ?(placeholder = "(stale) file content removed — see newer read_file output later")
      entries
  =
  let module P = History_entry.Payload in
  let key kind alias =
    (match kind with
     | P.Call_kind.Function -> "function:"
     | Custom -> "custom:")
    ^ alias
  in
  let calls = ref String.Map.empty in
  let aliases = ref String.Map.empty in
  let latest = ref String.Map.empty in
  let indexed =
    List.mapi entries ~f:(fun index entry ->
      let semantic = P.semantic (History_entry.payload entry) in
      let metadata = P.Semantic.metadata semantic in
      let path =
        match P.Semantic.view semantic with
        | Call { kind; name; input_bytes; _ } ->
          let path =
            if String.equal name "read_file"
            then read_file_path_of_arguments input_bytes
            else None
          in
          calls
          := Map.set
               !calls
               ~key:(History_entry.Id.to_string (History_entry.id entry))
               ~data:(kind, path);
          (match metadata.call_id with
           | Value alias -> aliases := Map.set !aliases ~key:(key kind alias) ~data:path
           | Absent | Null -> ());
          None
        | Result { kind; relation; _ } ->
          (match relation with
           | Bound id ->
             Map.find !calls (History_entry.Id.to_string id)
             |> Option.bind ~f:(fun (owner, path) ->
               if P.Call_kind.equal kind owner then path else None)
           | Unresolved ->
             (match metadata.call_id with
              | Value alias -> Map.find !aliases (key kind alias) |> Option.join
              | Absent | Null -> None))
        | Message _ | Reasoning _ | Unknown _ -> None
      in
      Option.iter path ~f:(fun path -> latest := Map.set !latest ~key:path ~data:index);
      index, entry, path)
  in
  List.map indexed ~f:(fun (index, entry, path) ->
    match path with
    | None -> entry
    | Some path
      when Option.value_map (Map.find !latest path) ~default:true ~f:(Int.equal index) ->
      entry
    | Some _ ->
      let semantic = P.semantic (History_entry.payload entry) in
      (match P.Semantic.view semantic with
       | Result { kind; relation; _ } ->
         let edited =
           P.Semantic.create
             (Result { kind; relation; output = Text placeholder })
             ~metadata:(P.Semantic.metadata semantic)
           |> Result.ok_or_failwith
           |> P.authored
         in
         History_entry.with_payload entry edited
       | Message _ | Call _ | Reasoning _ | Unknown _ -> entry))
;;
