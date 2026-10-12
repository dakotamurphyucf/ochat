open Core
module Id = Agent_protocol.Id.Prompt_definition
module F = Document_fields

type t =
  { definition_id : Id.t
  ; root_file : string
  ; fingerprint : string
  }
[@@deriving equal, sexp_of]

let definition_id t = t.definition_id
let root_file t = t.root_file
let fingerprint t = t.fingerprint
let max_path_bytes = 4096

let create ~definition_id ~root_file ~max_path_bytes:limit ~checkpoint =
  checkpoint ();
  let length = String.length root_file in
  if limit < 0 || length = 0 || length > Int.min limit max_path_bytes
  then Or_error.error_string "original source selection path exceeds its byte allowance"
  else if not (Filename.is_absolute root_file)
  then Or_error.error_string "original source selection must be absolute"
  else (
    let valid = ref true in
    for i = 0 to length - 1 do
      if i mod 256 = 0 then checkpoint ();
      if Char.equal root_file.[i] '\000' then valid := false
    done;
    if not !valid
    then Or_error.error_string "original source selection contains NUL"
    else (
      let identifier = Id.to_string definition_id in
      let buffer = Buffer.create (length + String.length identifier + 64) in
      Buffer.add_string buffer "prompt.source-selection.v1";
      List.iter [ identifier; root_file ] ~f:(fun bytes ->
        checkpoint ();
        Buffer.add_string buffer (Int.to_string (String.length bytes));
        Buffer.add_char buffer ':';
        let rec append offset =
          if offset < String.length bytes
          then (
            checkpoint ();
            let len = Int.min 256 (String.length bytes - offset) in
            Buffer.add_substring buffer bytes ~pos:offset ~len;
            append (offset + len))
        in
        append 0);
      let encoded = Buffer.contents buffer in
      let state = ref Digestif.SHA256.empty in
      let rec hash offset =
        if offset < String.length encoded
        then (
          checkpoint ();
          let len = Int.min 256 (String.length encoded - offset) in
          state := Digestif.SHA256.feed_string !state encoded ~off:offset ~len;
          hash (offset + len))
      in
      hash 0;
      let fingerprint = Digestif.SHA256.(get !state |> to_hex) in
      checkpoint ();
      Ok { definition_id; root_file; fingerprint }))
;;

let to_json t =
  `Object
    [ "version", F.decimal_json 1L
    ; "definition_id", `String (Id.to_string t.definition_id)
    ; "root_file", `String t.root_file
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind version = F.required json "version" F.decimal in
  let%bind () =
    if Int64.equal version 1L
    then Ok ()
    else F.invalid "version" "unsupported source selection receipt"
  in
  let%bind definition_id =
    F.required json "definition_id" (fun value ->
      let%bind text = F.string value in
      F.protocol (Id.of_string text))
  in
  let%bind root_file = F.required json "root_file" F.string in
  create ~definition_id ~root_file ~max_path_bytes ~checkpoint:(fun () -> ())
  |> Result.map_error ~f:(fun _ ->
    Document_schema.Error.Invalid_field
      { path = [ "root_file" ]; reason = "invalid original source selection" })
;;

let shape =
  F.shape
    [ "version", Document_schema.Shape.value
    ; "definition_id", Document_schema.Shape.value
    ; "root_file", Document_schema.Shape.value
    ]
;;
