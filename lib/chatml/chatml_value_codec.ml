open Core
open Chatml_lang

let expect_record_field
      (context : string)
      (fields : value String.Map.t)
      (field : string)
  : (value, string) result
  =
  match Map.find fields field with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "%s: missing field '%s'" context field)
;;

let expect_string (context : string) (value : value) : (string, string) result =
  match value with
  | VString s -> Ok s
  | _ -> Error (Printf.sprintf "%s: expected string" context)
;;

let expect_int (context : string) (value : value) : (int, string) result =
  match value with
  | VInt i -> Ok i
  | _ -> Error (Printf.sprintf "%s: expected int" context)
;;

let rec jsonaf_to_value (json : Jsonaf.t) : value =
  match json with
  | `Null -> VVariant ("Null", [])
  | `True -> VVariant ("Bool", [ VBool true ])
  | `False -> VVariant ("Bool", [ VBool false ])
  | `String s -> VVariant ("String", [ VString s ])
  | `Number n -> VVariant ("Number", [ VFloat (Float.of_string n) ])
  | `Array values ->
    VVariant ("Array", [ VArray (values |> List.map ~f:jsonaf_to_value |> Array.of_list) ])
  | `Object fields ->
    let entries =
      fields
      |> List.map ~f:(fun (key, value) ->
        VRecord
          (Map.of_alist_exn
             (module String)
             [ "key", VString key; "value", jsonaf_to_value value ]))
      |> Array.of_list
    in
    VVariant ("Object", [ VArray entries ])
;;

let json_shape_error =
  "expected Json.t (`Null/`Bool/`Number/`String/`Array/`Object)"
;;

let rec list_map_result (items : 'a list) ~(f : 'a -> ('b, string) result)
  : ('b list, string) result
  =
  match items with
  | [] -> Ok []
  | hd :: tl ->
    (match f hd with
     | Error msg -> Error msg
     | Ok value ->
       (match list_map_result tl ~f with
        | Ok rest -> Ok (value :: rest)
        | Error msg -> Error msg))
;;

let rec value_to_jsonaf_result (value : value) : (Jsonaf.t, string) result =
  match value with
  | VVariant ("Null", []) -> Ok `Null
  | VVariant ("Bool", [ VBool true ]) -> Ok `True
  | VVariant ("Bool", [ VBool false ]) -> Ok `False
  | VVariant ("String", [ VString s ]) -> Ok (`String s)
  | VVariant ("Number", [ VFloat f ]) -> Ok (`Number (Float.to_string f))
  | VVariant ("Array", [ VArray values ]) ->
    values
    |> Array.to_list
    |> list_map_result ~f:value_to_jsonaf_result
    |> Result.map ~f:(fun values -> `Array values)
  | VVariant ("Object", [ VArray entries ]) ->
    let field_of_entry (entry : value) : ((string * Jsonaf.t), string) result =
      match entry with
      | VRecord fields ->
        (match expect_record_field "JSON object entry" fields "key" with
         | Error msg -> Error msg
         | Ok key_value ->
           (match expect_string "JSON object entry field 'key'" key_value with
            | Error msg -> Error msg
            | Ok key ->
              (match expect_record_field "JSON object entry" fields "value" with
               | Error msg -> Error msg
               | Ok value ->
                 (match value_to_jsonaf_result value with
                  | Ok json -> Ok (key, json)
                  | Error msg -> Error msg))))
      | _ -> Error "JSON object entry: expected record"
    in
    entries
    |> Array.to_list
    |> list_map_result ~f:field_of_entry
    |> Result.map ~f:(fun fields -> `Object fields)
  | _ -> Error json_shape_error
;;

let value_to_jsonaf_exn (value : value) : Jsonaf.t =
  match value_to_jsonaf_result value with
  | Ok json -> json
  | Error msg -> failwith msg
;;
