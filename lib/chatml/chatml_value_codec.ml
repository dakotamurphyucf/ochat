open Core
open Chatml_lang

let expect_record_field (context : string) (fields : value String.Map.t) (field : string)
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

let json_shape_error = "expected Json.t (`Null/`Bool/`Number/`String/`Array/`Object)"

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
    let field_of_entry (entry : value) : (string * Jsonaf.t, string) result =
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

module Snapshot = struct
  type t =
    | Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Unit
    | Array of t list
    | Record of (string * t) list
    | Variant of string * t list
  [@@deriving sexp, compare, bin_io]

  let field_path path field = Printf.sprintf "%s.%s" path field
  let array_path path index = Printf.sprintf "%s[%d]" path index
  let variant_path path tag index = Printf.sprintf "%s<%s>[%d]" path tag index

  let unsupported path kind =
    Error (Printf.sprintf "%s: %s are not serializable in ChatML snapshots" path kind)
  ;;

  let rec of_value_at ~path (value : value) : (t, string) result =
    match value with
    | VInt i -> Ok (Int i)
    | VFloat f -> Ok (Float f)
    | VBool b -> Ok (Bool b)
    | VString s -> Ok (String s)
    | VUnit -> Ok Unit
    | VArray values ->
      values
      |> Array.to_list
      |> List.mapi ~f:(fun index element ->
        of_value_at ~path:(array_path path index) element)
      |> list_map_result ~f:Fn.id
      |> Result.map ~f:(fun values -> Array values)
    | VRecord fields ->
      fields
      |> Map.to_alist
      |> list_map_result ~f:(fun (field, value) ->
        of_value_at ~path:(field_path path field) value
        |> Result.map ~f:(fun value -> field, value))
      |> Result.map ~f:(fun fields -> Record fields)
    | VVariant (tag, payload) ->
      payload
      |> List.mapi ~f:(fun index element ->
        of_value_at ~path:(variant_path path tag index) element)
      |> list_map_result ~f:Fn.id
      |> Result.map ~f:(fun payload -> Variant (tag, payload))
    | VRef _ -> unsupported path "refs"
    | VClosure _ -> unsupported path "closures"
    | VModule _ -> unsupported path "modules"
    | VBuiltin _ -> unsupported path "builtins"
    | VTask _ -> unsupported path "tasks"
  ;;

  let of_value value = of_value_at ~path:"root" value

  let of_value_exn value =
    match of_value value with
    | Ok snapshot -> snapshot
    | Error msg -> failwith msg
  ;;

  let rec to_value_at ~path (snapshot : t) : (value, string) result =
    match snapshot with
    | Int i -> Ok (VInt i)
    | Float f -> Ok (VFloat f)
    | Bool b -> Ok (VBool b)
    | String s -> Ok (VString s)
    | Unit -> Ok VUnit
    | Array values ->
      values
      |> List.mapi ~f:(fun index element ->
        to_value_at ~path:(array_path path index) element)
      |> list_map_result ~f:Fn.id
      |> Result.map ~f:(fun values -> VArray (Array.of_list values))
    | Record fields ->
      fields
      |> list_map_result ~f:(fun (field, snapshot) ->
        to_value_at ~path:(field_path path field) snapshot
        |> Result.map ~f:(fun value -> field, value))
      |> Result.bind ~f:(fun fields ->
        match String.Map.of_alist fields with
        | `Ok fields -> Ok (VRecord fields)
        | `Duplicate_key field ->
          Error
            (Printf.sprintf "%s: duplicate record field %S in ChatML snapshot" path field))
    | Variant (tag, payload) ->
      payload
      |> List.mapi ~f:(fun index element ->
        to_value_at ~path:(variant_path path tag index) element)
      |> list_map_result ~f:Fn.id
      |> Result.map ~f:(fun payload -> VVariant (tag, payload))
  ;;

  let to_value snapshot = to_value_at ~path:"root" snapshot

  let to_value_exn snapshot =
    match to_value snapshot with
    | Ok value -> value
    | Error msg -> failwith msg
  ;;
end
