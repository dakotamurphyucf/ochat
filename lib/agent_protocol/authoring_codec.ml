open! Core

let invalid message = Error (Protocol_error.invalid_request message)
let maximum_wire_bytes = 32 * 1024 * 1024
let maximum_wire_integer = 9_007_199_254_740_991
let maximum_depth = 32
let maximum_nodes = 100_000

module Budget = struct
  type t =
    { mutable remaining_bytes : int
    ; mutable remaining_nodes : int
    }

  let create ~max_bytes =
    if max_bytes <= 0 || max_bytes > maximum_wire_bytes
    then invalid "invalid authoring JSON budget"
    else Ok { remaining_bytes = max_bytes; remaining_nodes = maximum_nodes }
  ;;

  let reserve t ~bytes ~nodes =
    if bytes < 0 || nodes < 0 || bytes > t.remaining_bytes || nodes > t.remaining_nodes
    then invalid "authoring JSON exceeds structural limits"
    else (
      t.remaining_bytes <- t.remaining_bytes - bytes;
      t.remaining_nodes <- t.remaining_nodes - nodes;
      Ok ())
  ;;

  let charge_json t ~parent_depth json =
    let charge bytes =
      if bytes > t.remaining_bytes
      then false
      else (
        t.remaining_bytes <- t.remaining_bytes - bytes;
        true)
    in
    let charge_string value =
      charge 2
      && String.for_all value ~f:(fun char ->
        let code = Char.to_int char in
        charge
          (if code < 0x20
           then 6
           else if Char.equal char '"' || Char.equal char '\\'
           then 2
           else 1))
    in
    let rec walk depth json =
      if depth > maximum_depth || t.remaining_nodes = 0
      then false
      else (
        t.remaining_nodes <- t.remaining_nodes - 1;
        match json with
        | `Null | `True | `False -> charge 5
        | `Number value -> charge (String.length value + 2)
        | `String value -> charge_string value
        | `Array values ->
          charge 2
          && List.for_all values ~f:(fun value -> charge 1 && walk (depth + 1) value)
        | `Object fields ->
          charge 2
          && List.for_all fields ~f:(fun (name, value) ->
            charge_string name && charge 2 && walk (depth + 1) value))
    in
    if parent_depth >= 0 && walk (parent_depth + 1) json
    then Ok ()
    else invalid "authoring JSON exceeds structural limits"
  ;;
end

let encoded_size_opt ~max_bytes json =
  match Budget.create ~max_bytes with
  | Error _ -> None
  | Ok budget ->
    (match Budget.charge_json budget ~parent_depth:0 json with
     | Error _ -> None
     | Ok () -> Some (max_bytes - budget.remaining_bytes))
;;

let encoded_size ~max_bytes json =
  match encoded_size_opt ~max_bytes json with
  | Some bytes -> Ok bytes
  | None -> invalid "authoring JSON exceeds structural limits"
;;

let admit_json ~max_bytes json = Result.map (encoded_size ~max_bytes json) ~f:ignore

let fields ~allowed json =
  if Option.is_none (encoded_size_opt ~max_bytes:maximum_wire_bytes json)
  then invalid "authoring JSON exceeds structural limits"
  else (
    match json with
    | `Object entries
      when List.exists entries ~f:(fun (name, _) ->
             not (List.mem allowed name ~equal:String.equal)) ->
      invalid "unknown authoring object field"
    | _ -> Json_codec.fields json)
;;

let required = Json_codec.required
let required_as = Json_codec.required_as

let optional_as fields name decode =
  match Json_codec.optional fields name with
  | None -> Ok None
  | Some `Null -> invalid ("null authoring field: " ^ name)
  | Some json -> Result.map (decode json) ~f:Option.some
;;

let string ~max_bytes json =
  let open Result.Let_syntax in
  let%bind value = Json_codec.string json in
  if String.length value <= max_bytes
  then Ok value
  else invalid "authoring string is too long"
;;

let canonical_int ~minimum = function
  | `Number encoded ->
    (match Int.of_string_opt encoded with
     | Some value
       when value >= minimum
            && value <= maximum_wire_integer
            && String.equal encoded (Int.to_string value) -> Ok value
     | _ -> invalid "expected a canonical bounded authoring integer")
  | _ -> invalid "expected an authoring integer"
;;

let nonnegative = canonical_int ~minimum:0
let positive = canonical_int ~minimum:1
let integer value = `Number (Int.to_string value)
let boolean value = if value then `True else `False

let list ~max_items decode = function
  | `Array values when List.length values <= max_items ->
    Result.all (List.map values ~f:decode)
  | `Array _ -> invalid "too many authoring list items"
  | _ -> invalid "expected an authoring list"
;;

let array encode values = `Array (List.map values ~f:encode)

let optional_field name value encode =
  match value with
  | None -> []
  | Some value -> [ name, encode value ]
;;

let portable_member_string value =
  match
    Chatml_source.Id.create ~package_namespace:"authoring-wire" ~relative_path:value
  with
  | Ok _ -> Ok value
  | Error _ -> invalid "invalid portable authoring member"
;;

let portable_member json =
  let open Result.Let_syntax in
  let%bind value = string ~max_bytes:1024 json in
  portable_member_string value
;;

let bytes_base64 ~max_bytes json =
  let open Result.Let_syntax in
  let%bind encoded = Json_codec.string json in
  if String.length encoded > ((max_bytes + 2) / 3 * 4) + 4
  then invalid "encoded authoring bytes exceed the limit"
  else (
    match Base64.decode encoded with
    | Ok value when String.length value <= max_bytes -> Ok value
    | Ok _ -> invalid "decoded authoring bytes exceed the limit"
    | Error _ -> invalid "invalid authoring base64")
;;

let bytes_base64_json value = `String (Base64.encode_exn value)

let source_id_json id =
  `Object
    [ "namespace_base64", bytes_base64_json (Chatml_source.Id.package_namespace id)
    ; "member", `String (Chatml_source.Id.relative_path id)
    ]
;;

let validate_source_id id =
  if String.length (Chatml_source.Id.package_namespace id) > 1024
  then invalid "authoring source namespace exceeds wire limit"
  else Ok ()
;;

let source_id json =
  let open Result.Let_syntax in
  let%bind fields = fields ~allowed:[ "namespace_base64"; "member" ] json in
  let%bind package_namespace =
    required_as fields "namespace_base64" (bytes_base64 ~max_bytes:1024)
  in
  let%bind relative_path = required_as fields "member" portable_member in
  match Chatml_source.Id.create ~package_namespace ~relative_path with
  | Ok value -> Result.map (validate_source_id value) ~f:(fun () -> value)
  | Error _ -> invalid "invalid authoring source ID"
;;

let digest_json value = `String (Chatml_source.Digest.to_hex value)

let digest json =
  let open Result.Let_syntax in
  let%bind encoded = string ~max_bytes:64 json in
  match Chatml_source.Digest.of_hex encoded with
  | Ok value -> Ok value
  | Error _ -> invalid "invalid authoring digest"
;;

let is_valid_utf8 value =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String value) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar _ -> loop ()
    | `End -> true
    | `Malformed _ -> false
    | `Await -> assert false
  in
  loop ()
;;

let utf8_text ~max_bytes json =
  let open Result.Let_syntax in
  let%bind value = string ~max_bytes json in
  if is_valid_utf8 value then Ok value else invalid "authoring text is not valid UTF-8"
;;
