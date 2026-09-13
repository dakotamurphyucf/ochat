open Core
module Error = Protocol_error

let invalid message = Error (Error.invalid_request message)

let text ~name ~max value =
  if String.is_empty value || String.length value > max
  then invalid (name ^ " must be nonempty and within its byte limit")
  else Ok ()
;;

(* Bound traversal before invoking recursive codecs or allocating serialized
   text. This also validates values constructed directly, not only parsed JSON. *)
let validate_json ?(max_bytes = 8 * 1024 * 1024) ?(max_depth = 128) json =
  let open Result.Let_syntax in
  let remaining = ref max_bytes in
  let charge bytes =
    remaining := !remaining - bytes;
    if !remaining < 0 then invalid "extension JSON exceeds byte limit" else Ok ()
  in
  let rec visit depth json =
    if depth > max_depth
    then invalid "extension JSON exceeds depth limit"
    else (
      let%bind () = charge 1 in
      match json with
      | `String value -> charge (String.length value + 2)
      | `Number value ->
        let%bind () = charge (String.length value) in
        (match Float.of_string_opt value with
         | Some number when Float.is_finite number ->
           (try
              match Jsonaf.of_string value with
              | `Number _ -> Ok ()
              | _ -> invalid "invalid JSON number"
            with
            | _ -> invalid "invalid JSON number")
         | _ -> invalid "extension JSON requires finite numbers")
      | `Array values ->
        List.fold_result values ~init:() ~f:(fun () value -> visit (depth + 1) value)
      | `Object values ->
        let names = Hash_set.create (module String) in
        List.fold_result values ~init:() ~f:(fun () (name, value) ->
          let%bind () = charge (String.length name + 3) in
          if Hash_set.mem names name
          then invalid "duplicate extension JSON field"
          else (
            Hash_set.add names name;
            visit (depth + 1) value))
      | `Null | `True | `False -> Ok ())
  in
  let%bind () = visit 1 json in
  if String.length (Jsonaf.to_string json) > max_bytes
  then invalid "extension JSON exceeds encoded byte limit"
  else Ok ()
;;

let validate_id encode decode id = Result.map (decode (encode id)) ~f:(fun _ -> ())

let closed fields allowed =
  match
    List.find (Json_codec.to_alist fields) ~f:(fun (key, _) ->
      not (List.mem allowed key ~equal:String.equal))
  with
  | None -> Ok ()
  | Some (key, _) -> invalid ("unknown extension field: " ^ key)
;;
