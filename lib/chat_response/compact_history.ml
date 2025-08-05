open Core
open Jsonaf
open Openai.Responses

(** Helper to parse the [arguments] JSON of a [read_file] function call and
    extract the path of the file being requested.  We recognise both
    {"path": "..."} (the preferred schema) and {"file": "..."} as fallbacks
    because models occasionally emit the latter. *)
let read_file_path_of_arguments (json_string : string) : string option =
  match Jsonaf.of_string json_string with
  | exception _ -> None
  | `Object _ as json -> Option.map ~f:to_string @@ Jsonaf.member "file" json
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
    | _ -> ());
  (* --------------------------------------------------------------------- *)
  (* Pass 3: build the transformed list, redacting stale outputs             *)
  (* --------------------------------------------------------------------- *)
  List.mapi items ~f:(fun idx item ->
    match item with
    | Item.Function_call_output fco ->
      (match Map.find call_id_to_path fco.call_id with
       | None -> item (* not a read_file output *)
       | Some path ->
         let latest_idx = Hashtbl.find_exn latest_output_idx_tbl path in
         if Int.equal idx latest_idx
         then item
         else (
           let redacted = { fco with output = placeholder } in
           Item.Function_call_output redacted))
    | _ -> item)
;;
