open! Core
module D = Chatmd_shell_spec.Diagnostic
module L = Chatml.Chatml_lang

type limits =
  { max_value_bytes : int
  ; max_string_bytes : int
  ; max_array_items : int
  ; max_depth : int
  }

let int_of_bytes bytes =
  Chatmd_shell_spec.Duration.bytes_to_int64 bytes
  |> Int64.min (Int64.of_int Int.max_value)
  |> Int64.to_int_exn
;;

let of_script_limits limits =
  { max_value_bytes = int_of_bytes limits.Chatmd_shell_spec.Chatmd_script_spec.max_value_bytes
  ; max_string_bytes = int_of_bytes limits.max_output_bytes
  ; max_array_items = limits.max_array_items
  ; max_depth = limits.max_depth
  }
;;

let error path message = D.error ~path ~code:"shell.chatml_codec" message

let add_size total amount =
  if total > Int.max_value - amount then Int.max_value else total + amount
;;

let rec measured_size limits path depth value =
  if depth > limits.max_depth
  then Error (error path "value exceeds the maximum nesting depth")
  else measured_shape limits path depth value

and measured_shape limits path depth = function
  | L.VInt _ | VFloat _ -> Ok 8
  | VBool _ -> Ok 1
  | VUnit -> Ok 0
  | VString value ->
    if String.length value > limits.max_string_bytes
    then Error (error path "string exceeds the configured byte limit")
    else Ok (String.length value)
  | VArray values -> measured_array limits path depth values
  | VRecord fields -> measured_record limits path depth fields
  | VVariant (tag, values) ->
    measured_list limits (path @ [ tag ]) depth values
    |> Result.map ~f:(add_size (String.length tag))
  | VRef _ | VClosure _ | VModule _ | VBuiltin _ | VTask _ ->
    Error (error path "runtime-only values cannot cross the host boundary")

and measured_array limits path depth values =
  if Array.length values > limits.max_array_items
  then Error (error path "array exceeds the configured item limit")
  else measured_list limits path depth (Array.to_list values)

and measured_list limits path depth values =
  List.mapi values ~f:(fun index value ->
    measured_size limits (path @ [ Int.to_string index ]) (depth + 1) value)
  |> Result.all
  |> Result.map ~f:(List.fold ~init:0 ~f:add_size)

and measured_record limits path depth fields =
  if Map.length fields > limits.max_array_items
  then Error (error path "record exceeds the configured field limit")
  else
    Map.to_alist fields
    |> List.map ~f:(fun (name, value) ->
      measured_size limits (path @ [ name ]) (depth + 1) value
      |> Result.map ~f:(add_size (String.length name)))
    |> Result.all
    |> Result.map ~f:(List.fold ~init:0 ~f:add_size)
;;

let validate limits value =
  measured_size limits [ "value" ] 0 value
  |> Result.bind ~f:(fun size ->
    if size > limits.max_value_bytes
    then Error (error [ "value" ] "value exceeds the configured total byte limit")
    else Ok ())
;;

let record ~path ~allowed ~required = function
  | L.VRecord fields ->
    let unknown = Set.diff (Map.key_set fields) (String.Set.of_list allowed) in
    let missing = Set.diff (String.Set.of_list required) (Map.key_set fields) in
    if not (Set.is_empty unknown)
    then Error (error (path @ [ Set.min_elt_exn unknown ]) "unknown field")
    else if not (Set.is_empty missing)
    then Error (error (path @ [ Set.min_elt_exn missing ]) "missing field")
    else Ok fields
  | _ -> Error (error path "expected record")
;;

let field ~path fields name =
  Map.find fields name
  |> Result.of_option ~error:(error (path @ [ name ]) "missing field")
;;

let string ~path = function
  | L.VString value -> Ok value
  | _ -> Error (error path "expected string")
;;

let bool ~path = function
  | L.VBool value -> Ok value
  | _ -> Error (error path "expected bool")
;;

let int ~path = function
  | L.VInt value -> Ok value
  | _ -> Error (error path "expected int")
;;

let strings ~path = function
  | L.VArray values ->
    Array.to_list values
    |> List.mapi ~f:(fun index -> string ~path:(path @ [ Int.to_string index ]))
    |> Result.all
  | _ -> Error (error path "expected string array")
;;

let option ~path decode = function
  | L.VVariant ("None", []) -> Ok None
  | VVariant ("Some", [ value ]) -> Result.map (decode ~path value) ~f:Option.some
  | _ -> Error (error path "expected option")
;;

let encode_record fields = L.VRecord (String.Map.of_alist_exn fields)
let encode_strings values = L.VArray (Array.of_list_map values ~f:(fun value -> L.VString value))

let encode_option encode = function
  | None -> L.VVariant ("None", [])
  | Some value -> L.VVariant ("Some", [ encode value ])
;;
