open Core
module Error = Document_schema.Error

let invalid path reason = Error (Error.Invalid_field { path; reason })

let required json name ~decode =
  let path = [ name ] in
  match Document_schema.Json.field json ~name with
  | Absent -> invalid path "required field is absent"
  | Null -> invalid path "null is not allowed"
  | Value value -> decode path value
;;

let version path = function
  | `Number number ->
    let integer =
      String.length number <= 21
      && String.for_all number ~f:(function
        | '-' | '0' .. '9' -> true
        | _ -> false)
    in
    (match if integer then Int.of_string_opt number else None with
     | Some 1 -> Ok ()
     | Some version ->
       Error (Error.Unsupported_version { kind = "chatml.project"; version; target = 1 })
     | None -> invalid path "expected an integer schema version")
  | `Null | `True | `False | `String _ | `Array _ | `Object _ ->
    invalid path "expected an integer schema version"
;;

let namespace path = function
  | `String string when not (String.is_empty string) -> Ok string
  | `String _ -> invalid path "expected a nonempty string"
  | `Null | `True | `False | `Number _ | `Array _ | `Object _ ->
    invalid path "expected a string"
;;

let of_json json ~checkpoint =
  checkpoint ();
  match json with
  | `Object fields ->
    (* Locate only two fields while polling the real bounded field walk. Decode's
       Json validator owns duplicate-key rejection before this helper is called. *)
    let selected =
      List.fold fields ~init:[] ~f:(fun selected (name, value) ->
        checkpoint ();
        if String.equal name "schema_version" || String.equal name "namespace"
        then (name, value) :: selected
        else selected)
    in
    let json = `Object selected in
    let open Result.Let_syntax in
    let%bind () = required json "schema_version" ~decode:version in
    checkpoint ();
    required json "namespace" ~decode:namespace
  | `Null | `True | `False | `String _ | `Number _ | `Array _ ->
    invalid [] "expected an object"
;;

let of_contents contents ~limits ~checkpoint =
  checkpoint ();
  let decoded = Document_schema.Json.decode ~limits contents in
  checkpoint ();
  Result.bind decoded ~f:(fun json -> of_json json ~checkpoint)
;;
