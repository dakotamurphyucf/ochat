open Core
open Chatml_lang
open Jsonaf
module Eval = Chatml_eval
module Value_codec = Chatml_value_codec

type row =
  | TRow_empty
  | TRow_var of string
  | TRow_extend of (string * ty) list * row

and ty =
  | TVar of string
  | TCon of string * ty list
  | TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TArray of ty
  | TRef of ty
  | TTuple of ty list
  | TRecord of row
  | TVariant of row
  | TFun of ty list * ty
  | TMu of string * ty
  | TRec_var of string

let closed_row (fields : (string * ty) list) : row = TRow_extend (fields, TRow_empty)

let open_row (fields : (string * ty) list) (tail : string) : row =
  TRow_extend (fields, TRow_var tail)
;;

let record (fields : (string * ty) list) : ty = TRecord (closed_row fields)

let record_open (fields : (string * ty) list) (tail : string) : ty =
  TRecord (open_row fields tail)
;;

let variant (cases : (string * ty) list) : ty = TVariant (closed_row cases)

let variant_open (cases : (string * ty) list) (tail : string) : ty =
  TVariant (open_row cases tail)
;;

type builtin =
  { name : string
  ; scheme : ty
  ; impl : value list -> value
  }

type builtin_module =
  { name : string
  ; exports : builtin list
  }

let apply_chatml (name : string) (fn : value) (args : value list) : value =
  match Eval.apply_value_result fn args with
  | Ok value -> value
  | Error err ->
    let message =
      match fn with
      | VClosure _ when String.equal err.message "Function arity mismatch" ->
        Printf.sprintf "%s: function arity mismatch" name
      | VBuiltin _ | VClosure _ -> err.message
      | _ -> Printf.sprintf "%s: expected a function" name
    in
    (* Re-raise as [Failure] so the outer VBuiltin call site converts it
       into a span-attached ChatML runtime error. *)
    failwith message
;;

let module_scheme (m : builtin_module) : ty =
  record (List.map m.exports ~f:(fun b -> b.name, b.scheme))
;;

let option_ty (a : string) : ty = variant [ "None", TUnit; "Some", TVar a ]
let task_ty (a : ty) : ty = TCon ("task", [ a ])

let expect_task (name : string) : value -> task = function
  | VTask t -> t
  | _ -> failwith (Printf.sprintf "%s: expected a Task argument" name)
;;

let expect_string (name : string) : value -> string = function
  | VString s -> s
  | _ -> failwith (Printf.sprintf "%s: expected a string argument" name)
;;

let expect_array (name : string) : value -> value array = function
  | VArray arr -> arr
  | _ -> failwith (Printf.sprintf "%s: expected an array argument" name)
;;

let expect_ref (name : string) : value -> value ref = function
  | VRef cell -> cell
  | _ -> failwith (Printf.sprintf "%s: expected a ref argument" name)
;;

let expect_record_like (name : string) : value -> string list = function
  | VRecord fields -> Map.to_alist fields |> List.map ~f:fst
  | VModule menv ->
    Hashtbl.fold menv ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)
    |> List.sort ~compare:String.compare
  | _ -> failwith (Printf.sprintf "%s: expected a record or module argument" name)
;;

let expect_variant (name : string) : value -> string = function
  | VVariant (tag, _payload) -> tag
  | _ -> failwith (Printf.sprintf "%s: expected a variant argument" name)
;;

let rec value_to_string (v : value) : string =
  match v with
  | VInt i -> Int.to_string i
  | VFloat f -> Float.to_string f
  | VBool b -> Bool.to_string b
  | VString s -> s
  | VArray arr ->
    let contents = Array.to_list arr |> List.map ~f:value_to_string in
    "[|" ^ String.concat ~sep:", " contents ^ "|]"
  | VRecord fields ->
    let rendered_fields =
      Map.to_alist fields |> List.map ~f:(fun (k, v') -> k ^ " = " ^ value_to_string v')
    in
    "{ " ^ String.concat ~sep:"; " rendered_fields ^ " }"
  | VRef r -> "ref(" ^ value_to_string !r ^ ")"
  | VModule _ -> "<module>"
  | VClosure _ -> "<closure>"
  | VUnit -> "()"
  | VBuiltin _ -> "<builtin>"
  | VTask task ->
    let eff_to_string (eff : eff) =
      let args = List.map eff.args ~f:value_to_string in
      "{operation = " ^ eff.op ^ ", args = [" ^ String.concat ~sep:", " args ^ "]}"
    in
    let rec print_task task =
      match task with
      | TPure v -> "pure(" ^ value_to_string v ^ ")"
      | TBind (t, v) -> "bind(" ^ print_task t ^ ", " ^ value_to_string v ^ ")"
      | TMap (t, v) -> "map(" ^ print_task t ^ ", " ^ value_to_string v ^ ")"
      | TFail s -> "fail(" ^ s ^ ")"
      | TCatch (t, v) -> "catch(" ^ print_task t ^ ", " ^ value_to_string v ^ ")"
      | TPerform eff -> "perform(" ^ eff_to_string eff ^ ")"
      | TSpawn eff -> "spawn(" ^ eff_to_string eff ^ ")"
    in
    print_task task
  | VVariant (slug, vals) ->
    if List.is_empty vals
    then Printf.sprintf "`%s" slug
    else (
      let inside = vals |> List.map ~f:value_to_string |> String.concat ~sep:", " in
      Printf.sprintf "`%s(%s)" slug inside)
;;

let with_unary_arg (name : string) (f : value -> value) : value list -> value = function
  | [ arg ] -> f arg
  | _ -> failwith (Printf.sprintf "%s: expected exactly one argument" name)
;;

let with_binary_args (name : string) (f : value -> value -> value) : value list -> value
  = function
  | [ lhs; rhs ] -> f lhs rhs
  | _ -> failwith (Printf.sprintf "%s: expected exactly two arguments" name)
;;

let make_unary_builtin (name : string) (scheme : ty) (f : value -> value) : builtin =
  { name; scheme; impl = with_unary_arg name f }
;;

let make_binary_builtin (name : string) (scheme : ty) (f : value -> value -> value)
  : builtin
  =
  { name; scheme; impl = with_binary_args name f }
;;

let expect_int (name : string) : value -> int = function
  | VInt i -> i
  | _ -> failwith (Printf.sprintf "%s: expected an int argument" name)
;;

let expect_record (name : string) : value -> value String.Map.t = function
  | VRecord fields -> fields
  | _ -> failwith (Printf.sprintf "%s: expected a record argument" name)
;;

let with_nullary_args (name : string) (f : unit -> value) : value list -> value = function
  | [] -> f ()
  | _ -> failwith (Printf.sprintf "%s: expected zero arguments" name)
;;

let with_ternary_args (name : string) (f : value -> value -> value -> value)
  : value list -> value
  = function
  | [ a; b; c ] -> f a b c
  | _ -> failwith (Printf.sprintf "%s: expected exactly three arguments" name)
;;

let make_nullary_builtin (name : string) (scheme : ty) (f : unit -> value) : builtin =
  { name; scheme; impl = with_nullary_args name f }
;;

let make_ternary_builtin
      (name : string)
      (scheme : ty)
      (f : value -> value -> value -> value)
  : builtin
  =
  { name; scheme; impl = with_ternary_args name f }
;;

let make_task_nullary_perform_builtin (name : string) (result_ty : ty) ~(op : string)
  : builtin
  =
  make_nullary_builtin
    name
    (TFun ([], task_ty result_ty))
    (fun () -> VTask (TPerform { op; args = [] }))
;;

let make_task_unary_perform_builtin
      (name : string)
      (arg_ty : ty)
      (result_ty : ty)
      ~(op : string)
  : builtin
  =
  make_unary_builtin
    name
    (TFun ([ arg_ty ], task_ty result_ty))
    (fun arg -> VTask (TPerform { op; args = [ arg ] }))
;;

let make_task_binary_perform_builtin
      (name : string)
      (lhs_ty : ty)
      (rhs_ty : ty)
      (result_ty : ty)
      ~(op : string)
  : builtin
  =
  make_binary_builtin
    name
    (TFun ([ lhs_ty; rhs_ty ], task_ty result_ty))
    (fun lhs rhs -> VTask (TPerform { op; args = [ lhs; rhs ] }))
;;

let make_task_nullary_spawn_builtin (name : string) (result_ty : ty) ~(op : string)
  : builtin
  =
  make_nullary_builtin
    name
    (TFun ([], task_ty result_ty))
    (fun () -> VTask (TSpawn { op; args = [] }))
;;

let make_task_unary_spawn_builtin
      (name : string)
      (arg_ty : ty)
      (result_ty : ty)
      ~(op : string)
  : builtin
  =
  make_unary_builtin
    name
    (TFun ([ arg_ty ], task_ty result_ty))
    (fun arg -> VTask (TSpawn { op; args = [ arg ] }))
;;

let make_task_binary_spawn_builtin
      (name : string)
      (lhs_ty : ty)
      (rhs_ty : ty)
      (result_ty : ty)
      ~(op : string)
  : builtin
  =
  make_binary_builtin
    name
    (TFun ([ lhs_ty; rhs_ty ], task_ty result_ty))
    (fun lhs rhs -> VTask (TSpawn { op; args = [ lhs; rhs ] }))
;;

let builtins : builtin list =
  [ make_unary_builtin
      "print"
      (TFun ([ TVar "a" ], TUnit))
      (fun v ->
         Printf.printf "%s \n" (value_to_string v);
         VUnit)
  ; make_unary_builtin
      "to_string"
      (TFun ([ TVar "a" ], TString))
      (fun v -> VString (value_to_string v))
  ; make_unary_builtin
      "length"
      (TFun ([ TArray (TVar "a") ], TInt))
      (fun v -> VInt (Array.length (expect_array "length" v)))
  ; make_unary_builtin
      "string_length"
      (TFun ([ TString ], TInt))
      (fun v -> VInt (String.length (expect_string "string_length" v)))
  ; make_unary_builtin
      "string_is_empty"
      (TFun ([ TString ], TBool))
      (fun v -> VBool (String.is_empty (expect_string "string_is_empty" v)))
  ; make_unary_builtin
      "array_copy"
      (TFun ([ TArray (TVar "a") ], TArray (TVar "a")))
      (fun v -> VArray (Array.copy (expect_array "array_copy" v)))
  ; make_unary_builtin
      "record_keys"
      (TFun ([ TRecord (TRow_var "r") ], TArray TString))
      (fun v ->
         let keys =
           expect_record_like "record_keys" v
           |> List.sort ~compare:String.compare
           |> List.map ~f:(fun key -> VString key)
           |> Array.of_list
         in
         VArray keys)
  ; make_unary_builtin
      "variant_tag"
      (TFun ([ TVariant (TRow_var "r") ], TString))
      (fun v -> VString (expect_variant "variant_tag" v))
  ; make_binary_builtin
      "swap_ref"
      (TFun ([ TRef (TVar "a"); TVar "a" ], TVar "a"))
      (fun lhs rhs ->
         let cell = expect_ref "swap_ref" lhs in
         let old = !cell in
         cell := rhs;
         old)
  ; make_unary_builtin
      "fail"
      (TFun ([ TString ], TVar "a"))
      (fun v -> failwith (expect_string "fail" v))
  ]
;;

let json_entry_ty (self : ty) : ty = record [ "key", TString; "value", self ]

let json_ty : ty =
  TMu
    ( "__builtin_json"
    , variant
        [ "Null", TUnit
        ; "Bool", TBool
        ; "Number", TFloat
        ; "String", TString
        ; "Array", TArray (TRec_var "__builtin_json")
        ; "Object", TArray (json_entry_ty (TRec_var "__builtin_json"))
        ] )
;;

let jsonaf_to_value = Value_codec.jsonaf_to_value

let value_to_jsonaf (name : string) (value : value) : Jsonaf.t =
  match Value_codec.value_to_jsonaf_result value with
  | Ok json -> json
  | Error msg -> failwith (Printf.sprintf "%s: %s" name msg)
;;

let option_of (t : ty) : ty = variant [ "None", TUnit; "Some", t ]
let json_option_ty : ty = option_of json_ty
let item_ty : ty = record [ "id", TString; "value", json_ty ]

let tool_desc_ty : ty =
  record [ "name", TString; "description", TString; "input_schema", json_ty ]
;;

let tool_call_ty : ty = record [ "id", TString; "name", TString; "args", json_ty ]

let tool_result_ty : ty =
  record [ "call_id", TString; "name", TString; "result", json_ty ]
;;

let context_ty : ty =
  record
    [ "session_id", TString
    ; "now_ms", TInt
    ; "phase", TString
    ; "items", TArray item_ty
    ; "available_tools", TArray tool_desc_ty
    ; "session_meta", json_ty
    ]
;;

let tool_call_result_ty : ty = variant [ "Ok", json_ty; "Error", TString ]

let model_call_result_ty : ty =
  variant [ "Ok", json_ty; "Refused", TString; "Error", TString ]
;;

let is_json_tag (tag : string) : bool =
  match tag with
  | "Null" | "Bool" | "Number" | "String" | "Array" | "Object" -> true
  | _ -> false
;;

let expect_json (name : string) (v : value) : value =
  match v with
  | VVariant (tag, _payload) when is_json_tag tag -> v
  | _ ->
    failwith
      (Printf.sprintf
         "%s: expected Json.t (`Null/`Bool/`Number/`String/`Array/`Object)"
         name)
;;

let json_entries_payload (name : string) (v : value) : value array option =
  match expect_json name v with
  | VVariant ("Object", [ VArray entries ]) -> Some entries
  | _ -> None
;;

let json_array_payload (name : string) (v : value) : value array option =
  match expect_json name v with
  | VVariant ("Array", [ VArray arr ]) -> Some arr
  | _ -> None
;;

let json_object_find_field (entries : value array) (key : string) : value option =
  let rec loop i =
    if i >= Array.length entries
    then None
    else (
      match entries.(i) with
      | VRecord m ->
        (match Map.find m "key", Map.find m "value" with
         | Some (VString k), Some v when String.equal k key -> Some v
         | _ -> loop (i + 1))
      | _ -> loop (i + 1))
  in
  loop 0
;;

let mk_json_entry (key : string) (value : value) : value =
  VRecord (Map.of_alist_exn (module String) [ "key", VString key; "value", value ])
;;

let json_string_value (s : string) : value = VVariant ("String", [ VString s ])

let json_array_value (values : value list) : value =
  VVariant ("Array", [ VArray (Array.of_list values) ])
;;

let json_object_value (fields : (string * value) list) : value =
  VVariant
    ( "Object"
    , [ VArray (Array.of_list_map fields ~f:(fun (key, value) -> mk_json_entry key value))
      ] )
;;

let expect_record_field_value
      (name : string)
      (fields : value String.Map.t)
      (field : string)
  : value
  =
  match Map.find fields field with
  | Some value -> value
  | None -> failwith (Printf.sprintf "%s: expected field %S" name field)
;;

let expect_record_string_field
      (name : string)
      (fields : value String.Map.t)
      (field : string)
  : string
  =
  expect_record_field_value name fields field |> expect_string (name ^ "." ^ field)
;;

let expect_record_json_field
      (name : string)
      (fields : value String.Map.t)
      (field : string)
  : value
  =
  expect_record_field_value name fields field |> expect_json (name ^ "." ^ field)
;;

let expect_item_record (name : string) (v : value) : string * value =
  let fields = expect_record name v in
  let id = expect_record_string_field name fields "id" in
  let item_value = expect_record_json_field name fields "value" in
  id, item_value
;;

let string_option_value = function
  | Some value -> VVariant ("Some", [ VString value ])
  | None -> VVariant ("None", [])
;;

let json_string_payload = function
  | VVariant ("String", [ VString s ]) -> Some s
  | _ -> None
;;

let json_field_value (name : string) (json : value) (field : string) : value option =
  match json_entries_payload name json with
  | None -> None
  | Some entries -> json_object_find_field entries field
;;

let text_values_of_parts (parts : value array) : value array =
  Array.filter_map parts ~f:(fun part ->
    json_field_value "Item.text_parts.part" part "text"
    |> Option.bind ~f:json_string_payload
    |> Option.map ~f:(fun text -> VString text))
;;

let output_text_values (output : value) : value array =
  match json_string_payload output with
  | Some text -> [| VString text |]
  | None ->
    (match json_field_value "Item.text_parts.output" output "parts" with
     | Some parts_json ->
       (match json_array_payload "Item.text_parts.output.parts" parts_json with
        | Some parts -> text_values_of_parts parts
        | None -> [||])
     | None -> [||])
;;

let item_input_text_message_value ~(role : string) ~(text : string) : value =
  json_object_value
    [ "type", json_string_value "message"
    ; "role", json_string_value role
    ; ( "content"
      , json_array_value
          [ json_object_value
              [ "type", json_string_value "input_text"; "text", json_string_value text ]
          ] )
    ]
;;

let item_output_text_message_value ~(id : string) ~(text : string) : value =
  json_object_value
    [ "type", json_string_value "message"
    ; "role", json_string_value "assistant"
    ; "id", json_string_value id
    ; ( "content"
      , json_array_value
          [ json_object_value
              [ "type", json_string_value "output_text"
              ; "text", json_string_value text
              ; "annotations", json_array_value []
              ]
          ] )
    ; "status", json_string_value "completed"
    ]
;;

let json_module =
  { name = "Json"
  ; exports =
      [ (* existing: parse/stringify/pretty *)
        make_unary_builtin
          "parse"
          (TFun ([ TString ], json_ty))
          (fun v ->
             let s = expect_string "Json.parse" v in
             try jsonaf_to_value (Jsonaf.of_string s) with
             | exn -> failwith (Printf.sprintf "Json.parse: %s" (Exn.to_string exn)))
      ; make_unary_builtin
          "stringify"
          (TFun ([ json_ty ], TString))
          (fun v ->
             let v = expect_json "Json.stringify" v in
             VString (Jsonaf.to_string (value_to_jsonaf "Json.stringify" v)))
      ; make_unary_builtin
          "pretty"
          (TFun ([ json_ty ], TString))
          (fun v ->
             let v = expect_json "Json.pretty" v in
             VString (Jsonaf.to_string_hum (value_to_jsonaf "Json.pretty" v)))
        (* new: parse_opt / validate *)
      ; make_unary_builtin
          "parse_opt"
          (TFun ([ TString ], json_option_ty))
          (fun v ->
             let s = expect_string "Json.parse_opt" v in
             try VVariant ("Some", [ jsonaf_to_value (Jsonaf.of_string s) ]) with
             | _ -> VVariant ("None", []))
      ; make_unary_builtin
          "validate"
          (TFun ([ TString ], TBool))
          (fun v ->
             let s = expect_string "Json.validate" v in
             try
               ignore (Jsonaf.of_string s);
               VBool true
             with
             | _ -> VBool false)
        (* new: tag *)
      ; make_unary_builtin
          "tag"
          (TFun ([ json_ty ], TString))
          (fun v ->
             match expect_json "Json.tag" v with
             | VVariant (tag, _payload) -> VString tag
             | _ -> assert false)
        (* new: as_* accessors *)
      ; make_unary_builtin
          "as_bool"
          (TFun ([ json_ty ], option_of TBool))
          (fun v ->
             match expect_json "Json.as_bool" v with
             | VVariant ("Bool", [ VBool b ]) -> VVariant ("Some", [ VBool b ])
             | _ -> VVariant ("None", []))
      ; make_unary_builtin
          "as_number"
          (TFun ([ json_ty ], option_of TFloat))
          (fun v ->
             match expect_json "Json.as_number" v with
             | VVariant ("Number", [ VFloat f ]) -> VVariant ("Some", [ VFloat f ])
             | _ -> VVariant ("None", []))
      ; make_unary_builtin
          "as_string"
          (TFun ([ json_ty ], option_of TString))
          (fun v ->
             match expect_json "Json.as_string" v with
             | VVariant ("String", [ VString s ]) -> VVariant ("Some", [ VString s ])
             | _ -> VVariant ("None", []))
      ; make_unary_builtin
          "as_array"
          (TFun ([ json_ty ], option_of (TArray json_ty)))
          (fun v ->
             match json_array_payload "Json.as_array" v with
             | Some arr -> VVariant ("Some", [ VArray arr ])
             | None -> VVariant ("None", []))
      ; make_unary_builtin
          "as_object"
          (TFun ([ json_ty ], option_of (TArray (json_entry_ty json_ty))))
          (fun v ->
             match json_entries_payload "Json.as_object" v with
             | Some entries -> VVariant ("Some", [ VArray entries ])
             | None -> VVariant ("None", []))
        (* new: object helpers *)
      ; make_unary_builtin
          "object_keys"
          (TFun ([ json_ty ], TArray TString))
          (fun v ->
             match json_entries_payload "Json.object_keys" v with
             | None -> VArray [||]
             | Some entries ->
               let keys =
                 entries
                 |> Array.to_list
                 |> List.filter_map ~f:(function
                   | VRecord m ->
                     (match Map.find m "key" with
                      | Some (VString k) -> Some (VString k)
                      | _ -> None)
                   | _ -> None)
                 |> Array.of_list
               in
               VArray keys)
      ; make_binary_builtin
          "get_field"
          (TFun ([ json_ty; TString ], json_option_ty))
          (fun obj key ->
             let key = expect_string "Json.get_field" key in
             match json_entries_payload "Json.get_field" obj with
             | None -> VVariant ("None", [])
             | Some entries ->
               (match json_object_find_field entries key with
                | None -> VVariant ("None", [])
                | Some v -> VVariant ("Some", [ v ])))
        (* new: get_path (string segments; supports numeric segments when current is `Array) *)
      ; make_binary_builtin
          "get_path"
          (TFun ([ json_ty; TArray TString ], json_option_ty))
          (fun root path ->
             let path_arr =
               expect_array "Json.get_path" path
               |> Array.map ~f:(fun v ->
                 match v with
                 | VString s -> s
                 | _ -> failwith "Json.get_path: path must be a string array")
             in
             let rec step (cur : value) (i : int) : value option =
               if i >= Array.length path_arr
               then Some cur
               else (
                 let seg = path_arr.(i) in
                 match expect_json "Json.get_path" cur with
                 | VVariant ("Object", [ VArray entries ]) ->
                   (match json_object_find_field entries seg with
                    | None -> None
                    | Some next -> step next (i + 1))
                 | VVariant ("Array", [ VArray arr ]) ->
                   (match Int.of_string_opt seg with
                    | None -> None
                    | Some idx ->
                      if idx < 0 || idx >= Array.length arr
                      then None
                      else step arr.(idx) (i + 1))
                 | _ -> None)
             in
             match step root 0 with
             | None -> VVariant ("None", [])
             | Some v -> VVariant ("Some", [ v ]))
      ; (* set_field : Json.t -> string -> Json.t -> Json.t *)
        make_ternary_builtin
          "set_field"
          (TFun ([ json_ty; TString; json_ty ], json_ty))
          (fun obj key new_value ->
             let key = expect_string "Json.set_field" key in
             let new_value = expect_json "Json.set_field" new_value in
             match json_entries_payload "Json.set_field" obj with
             | None -> failwith "Json.set_field: expected `Object(...)"
             | Some entries ->
               (* Replace first occurrence in-place, drop duplicates, preserve order.
           If key not found, append at end. *)
               let found = ref false in
               let out_rev =
                 entries
                 |> Array.to_list
                 |> List.fold ~init:[] ~f:(fun acc entry ->
                   match entry with
                   | VRecord m ->
                     (match Map.find m "key" with
                      | Some (VString k) when String.equal k key ->
                        if !found
                        then acc (* drop duplicates *)
                        else (
                          found := true;
                          mk_json_entry key new_value :: acc)
                      | _ -> entry :: acc)
                   | _ -> entry :: acc)
               in
               let out =
                 let out = List.rev out_rev in
                 if !found then out else out @ [ mk_json_entry key new_value ]
               in
               VVariant ("Object", [ VArray (Array.of_list out) ]))
      ; (* remove_field : Json.t -> string -> Json.t *)
        make_binary_builtin
          "remove_field"
          (TFun ([ json_ty; TString ], json_ty))
          (fun obj key ->
             let key = expect_string "Json.remove_field" key in
             match json_entries_payload "Json.remove_field" obj with
             | None -> failwith "Json.remove_field: expected `Object(...)"
             | Some entries ->
               let kept =
                 entries
                 |> Array.to_list
                 |> List.filter ~f:(function
                   | VRecord m ->
                     (match Map.find m "key" with
                      | Some (VString k) -> not (String.equal k key)
                      | _ -> true)
                   | _ -> true)
                 |> Array.of_list
               in
               VVariant ("Object", [ VArray kept ]))
      ]
  }
;;

let entry_ty a = record [ "key", TString; "value", TVar a ]
let tbl_ty a = TRef (TArray (entry_ty a))
let string_option_int_ty : ty = variant [ "None", TUnit; "Some", TInt ]

let string_module : builtin_module =
  { name = "String"
  ; exports =
      [ (* existing *)
        make_unary_builtin
          "length"
          (TFun ([ TString ], TInt))
          (fun v -> VInt (String.length (expect_string "String.length" v)))
      ; make_unary_builtin
          "is_empty"
          (TFun ([ TString ], TBool))
          (fun v -> VBool (String.is_empty (expect_string "String.is_empty" v)))
      ; make_binary_builtin
          "concat"
          (TFun ([ TString; TString ], TString))
          (fun a b ->
             let a = expect_string "String.concat" a in
             let b = expect_string "String.concat" b in
             VString (a ^ b))
      ; (* recommended additions *)
        make_binary_builtin
          "equal"
          (TFun ([ TString; TString ], TBool))
          (fun a b ->
             VBool
               (String.equal
                  (expect_string "String.equal" a)
                  (expect_string "String.equal" b)))
      ; make_binary_builtin
          "contains"
          (TFun ([ TString; TString ], TBool))
          (fun s sub ->
             let s = expect_string "String.contains" s in
             let sub = expect_string "String.contains" sub in
             VBool (String.is_substring s ~substring:sub))
      ; make_binary_builtin
          "starts_with"
          (TFun ([ TString; TString ], TBool))
          (fun s prefix ->
             let s = expect_string "String.starts_with" s in
             let prefix = expect_string "String.starts_with" prefix in
             VBool (String.is_prefix s ~prefix))
      ; make_binary_builtin
          "ends_with"
          (TFun ([ TString; TString ], TBool))
          (fun s suffix ->
             let s = expect_string "String.ends_with" s in
             let suffix = expect_string "String.ends_with" suffix in
             VBool (String.is_suffix s ~suffix))
      ; make_unary_builtin
          "trim"
          (TFun ([ TString ], TString))
          (fun s -> VString (String.strip (expect_string "String.trim" s)))
      ; make_ternary_builtin
          "slice"
          (TFun ([ TString; TInt; TInt ], TString))
          (fun s start len ->
             let s = expect_string "String.slice" s in
             let start = expect_int "String.slice" start in
             let len = expect_int "String.slice" len in
             if start < 0 || len < 0
             then failwith "String.slice: start and len must be non-negative"
             else if start > String.length s
             then failwith "String.slice: start out of bounds"
             else if start + len > String.length s
             then failwith "String.slice: start+len out of bounds"
             else VString (String.sub s ~pos:start ~len))
      ; make_binary_builtin
          "find"
          (TFun ([ TString; TString ], string_option_int_ty))
          (fun s pat ->
             let s = expect_string "String.find" s in
             let pat = expect_string "String.find" pat in
             match String.substr_index s ~pattern:pat with
             | None -> VVariant ("None", [])
             | Some i -> VVariant ("Some", [ VInt i ]))
      ; make_binary_builtin
          "split"
          (TFun ([ TString; TString ], TArray TString))
          (fun s sep ->
             let s = expect_string "String.split" s in
             let sep = expect_string "String.split" sep in
             if String.is_empty sep
             then failwith "String.split: separator must be non-empty"
             else (
               let pat = String.Search_pattern.create sep in
               let parts = String.Search_pattern.split_on pat s in
               VArray (parts |> List.map ~f:(fun p -> VString p) |> Array.of_list)))
      ; make_unary_builtin
          "to_upper"
          (TFun ([ TString ], TString))
          (fun s -> VString (String.uppercase (expect_string "String.to_upper" s)))
      ; make_unary_builtin
          "to_lower"
          (TFun ([ TString ], TString))
          (fun s -> VString (String.lowercase (expect_string "String.to_lower" s)))
      ; make_ternary_builtin
          "replace_all"
          (TFun ([ TString; TString; TString ], TString))
          (fun s pattern with_ ->
             let s = expect_string "String.replace_all" s in
             let pattern = expect_string "String.replace_all" pattern in
             let with_ = expect_string "String.replace_all" with_ in
             if String.is_empty pattern
             then failwith "String.replace_all: pattern must be non-empty"
             else VString (String.substr_replace_all s ~pattern ~with_))
      ]
  }
;;

let array_module : builtin_module =
  { name = "Array"
  ; exports =
      [ (* existing / keep *)
        make_unary_builtin
          "length"
          (TFun ([ TArray (TVar "a") ], TInt))
          (fun v -> VInt (Array.length (expect_array "Array.length" v)))
      ; make_unary_builtin
          "copy"
          (TFun ([ TArray (TVar "a") ], TArray (TVar "a")))
          (fun v -> VArray (Array.copy (expect_array "Array.copy" v)))
      ; make_binary_builtin
          "get"
          (TFun ([ TArray (TVar "a"); TInt ], TVar "a"))
          (fun arr idx ->
             let a = expect_array "Array.get" arr in
             let i = expect_int "Array.get" idx in
             if i < 0 || i >= Array.length a
             then failwith "Array.get: index out of bounds"
             else a.(i))
      ; make_ternary_builtin
          "set"
          (TFun ([ TArray (TVar "a"); TInt; TVar "a" ], TUnit))
          (fun arr idx v ->
             let a = expect_array "Array.set" arr in
             let i = expect_int "Array.set" idx in
             if i < 0 || i >= Array.length a
             then failwith "Array.set: index out of bounds"
             else (
               a.(i) <- v;
               VUnit))
      ; (* new: make *)
        make_binary_builtin
          "make"
          (TFun ([ TInt; TVar "a" ], TArray (TVar "a")))
          (fun n v ->
             let n = expect_int "Array.make" n in
             if n < 0
             then failwith "Array.make: length must be non-negative"
             else VArray (Array.create ~len:n v))
      ; (* new: append *)
        make_binary_builtin
          "append"
          (TFun ([ TArray (TVar "a"); TArray (TVar "a") ], TArray (TVar "a")))
          (fun a b ->
             let a = expect_array "Array.append" a in
             let b = expect_array "Array.append" b in
             VArray (Array.append a b))
      ; (* new: sub(arr, start, len) *)
        make_ternary_builtin
          "sub"
          (TFun ([ TArray (TVar "a"); TInt; TInt ], TArray (TVar "a")))
          (fun arr start len ->
             let a = expect_array "Array.sub" arr in
             let start = expect_int "Array.sub" start in
             let len = expect_int "Array.sub" len in
             if start < 0 || len < 0
             then failwith "Array.sub: start and len must be non-negative"
             else if start > Array.length a
             then failwith "Array.sub: start out of bounds"
             else if start + len > Array.length a
             then failwith "Array.sub: start+len out of bounds"
             else VArray (Array.sub a ~pos:start ~len))
      ; (* new: reverse (pure) *)
        make_unary_builtin
          "reverse"
          (TFun ([ TArray (TVar "a") ], TArray (TVar "a")))
          (fun arr ->
             let a = expect_array "Array.reverse" arr in
             VArray (Array.rev a))
      ; (* new: reverse_in_place (mutating) *)
        make_unary_builtin
          "reverse_in_place"
          (TFun ([ TArray (TVar "a") ], TUnit))
          (fun arr ->
             let a = expect_array "Array.reverse_in_place" arr in
             let i = ref 0 in
             let j = ref (Array.length a - 1) in
             while !i < !j do
               let tmp = a.(!i) in
               a.(!i) <- a.(!j);
               a.(!j) <- tmp;
               i := !i + 1;
               j := !j - 1
             done;
             VUnit)
      ; (* new: swap(arr, i, j) *)
        make_ternary_builtin
          "swap"
          (TFun ([ TArray (TVar "a"); TInt; TInt ], TUnit))
          (fun arr i j ->
             let a = expect_array "Array.swap" arr in
             let i = expect_int "Array.swap" i in
             let j = expect_int "Array.swap" j in
             let n = Array.length a in
             if i < 0 || i >= n || j < 0 || j >= n
             then failwith "Array.swap: index out of bounds"
             else (
               let tmp = a.(i) in
               a.(i) <- a.(j);
               a.(j) <- tmp;
               VUnit))
      ; (* new: fill(arr, v) *)
        make_binary_builtin
          "fill"
          (TFun ([ TArray (TVar "a"); TVar "a" ], TUnit))
          (fun arr v ->
             let a = expect_array "Array.fill" arr in
             for i = 0 to Array.length a - 1 do
               a.(i) <- v
             done;
             VUnit)
      ; make_binary_builtin
          "init"
          (TFun ([ TInt; TFun ([ TInt ], TVar "a") ], TArray (TVar "a")))
          (fun n f ->
             let n = expect_int "Array.init" n in
             if n < 0 then failwith "Array.init: length must be non-negative";
             let arr =
               Array.init n ~f:(fun i -> apply_chatml "Array.init" f [ VInt i ])
             in
             VArray arr)
      ; make_binary_builtin
          "map"
          (TFun ([ TArray (TVar "a"); TFun ([ TVar "a" ], TVar "b") ], TArray (TVar "b")))
          (fun arr f ->
             let a = expect_array "Array.map" arr in
             VArray (Array.map a ~f:(fun x -> apply_chatml "Array.map" f [ x ])))
      ; (* mapi : 'a array -> (int -> 'a -> 'b) -> 'b array *)
        make_binary_builtin
          "mapi"
          (TFun
             ( [ TArray (TVar "a"); TFun ([ TInt; TVar "a" ], TVar "b") ]
             , TArray (TVar "b") ))
          (fun arr f ->
             let a = expect_array "Array.mapi" arr in
             VArray
               (Array.mapi a ~f:(fun i x -> apply_chatml "Array.mapi" f [ VInt i; x ])))
      ; (* iter : 'a array -> ('a -> unit) -> unit *)
        make_binary_builtin
          "iter"
          (TFun ([ TArray (TVar "a"); TFun ([ TVar "a" ], TUnit) ], TUnit))
          (fun arr f ->
             let a = expect_array "Array.iter" arr in
             Array.iter a ~f:(fun x -> ignore (apply_chatml "Array.iter" f [ x ]));
             VUnit)
      ; (* iteri : 'a array -> (int -> 'a -> unit) -> unit *)
        make_binary_builtin
          "iteri"
          (TFun ([ TArray (TVar "a"); TFun ([ TInt; TVar "a" ], TUnit) ], TUnit))
          (fun arr f ->
             let a = expect_array "Array.iteri" arr in
             Array.iteri a ~f:(fun i x ->
               ignore (apply_chatml "Array.iteri" f [ VInt i; x ]));
             VUnit)
      ; (* fold : 'a array -> 'b -> ('b -> 'a -> 'b) -> 'b
                   Call shape: fold(arr, init, f) *)
        make_ternary_builtin
          "fold"
          (TFun
             ( [ TArray (TVar "a"); TVar "b"; TFun ([ TVar "b"; TVar "a" ], TVar "b") ]
             , TVar "b" ))
          (fun arr init f ->
             let a = expect_array "Array.fold" arr in
             let acc = ref init in
             Array.iter a ~f:(fun x -> acc := apply_chatml "Array.fold" f [ !acc; x ]);
             !acc)
      ; (* filter : 'a array -> ('a -> bool) -> 'a array *)
        make_binary_builtin
          "filter"
          (TFun ([ TArray (TVar "a"); TFun ([ TVar "a" ], TBool) ], TArray (TVar "a")))
          (fun arr pred ->
             let a = expect_array "Array.filter" arr in
             let kept =
               a
               |> Array.to_list
               |> List.filter ~f:(fun x ->
                 match apply_chatml "Array.filter" pred [ x ] with
                 | VBool b -> b
                 | _ -> failwith "Array.filter: predicate must return bool")
               |> Array.of_list
             in
             VArray kept)
      ; (* exists : 'a array -> ('a -> bool) -> bool *)
        make_binary_builtin
          "exists"
          (TFun ([ TArray (TVar "a"); TFun ([ TVar "a" ], TBool) ], TBool))
          (fun arr pred ->
             let a = expect_array "Array.exists" arr in
             let rec loop i =
               if i >= Array.length a
               then false
               else (
                 match apply_chatml "Array.exists" pred [ a.(i) ] with
                 | VBool true -> true
                 | VBool false -> loop (i + 1)
                 | _ -> failwith "Array.exists: predicate must return bool")
             in
             VBool (loop 0))
      ; (* for_all : 'a array -> ('a -> bool) -> bool *)
        make_binary_builtin
          "for_all"
          (TFun ([ TArray (TVar "a"); TFun ([ TVar "a" ], TBool) ], TBool))
          (fun arr pred ->
             let a = expect_array "Array.for_all" arr in
             let rec loop i =
               if i >= Array.length a
               then true
               else (
                 match apply_chatml "Array.for_all" pred [ a.(i) ] with
                 | VBool true -> loop (i + 1)
                 | VBool false -> false
                 | _ -> failwith "Array.for_all: predicate must return bool")
             in
             VBool (loop 0))
      ; (* find : 'a array -> ('a -> bool) -> option('a) *)
        make_binary_builtin
          "find"
          (TFun ([ TArray (TVar "a"); TFun ([ TVar "a" ], TBool) ], option_ty "a"))
          (fun arr pred ->
             let a = expect_array "Array.find" arr in
             let rec loop i =
               if i >= Array.length a
               then VVariant ("None", [])
               else (
                 match apply_chatml "Array.find" pred [ a.(i) ] with
                 | VBool true -> VVariant ("Some", [ a.(i) ])
                 | VBool false -> loop (i + 1)
                 | _ -> failwith "Array.find: predicate must return bool")
             in
             loop 0)
      ; (* find_map : 'a array -> ('a -> option('b)) -> option('b) *)
        make_binary_builtin
          "find_map"
          (TFun ([ TArray (TVar "a"); TFun ([ TVar "a" ], option_ty "b") ], option_ty "b"))
          (fun arr f ->
             let a = expect_array "Array.find_map" arr in
             let rec loop i =
               if i >= Array.length a
               then VVariant ("None", [])
               else (
                 match apply_chatml "Array.find_map" f [ a.(i) ] with
                 | VVariant ("None", []) -> loop (i + 1)
                 | VVariant ("Some", [ v ]) -> VVariant ("Some", [ v ])
                 | _ -> failwith "Array.find_map: function must return `None or `Some(x)")
             in
             loop 0)
      ]
  }
;;

let task_module : builtin_module =
  { name = "Task"
  ; exports =
      [ make_unary_builtin
          "pure"
          (TFun ([ TVar "a" ], task_ty (TVar "a")))
          (fun v -> VTask (TPure v))
      ; make_binary_builtin
          "bind"
          (TFun
             ( [ task_ty (TVar "a"); TFun ([ TVar "a" ], task_ty (TVar "b")) ]
             , task_ty (TVar "b") ))
          (fun t k -> VTask (TBind (expect_task "Task.bind" t, k)))
      ; make_binary_builtin
          "map"
          (TFun ([ task_ty (TVar "a"); TFun ([ TVar "a" ], TVar "b") ], task_ty (TVar "b")))
          (fun t f -> VTask (TMap (expect_task "Task.map" t, f)))
      ; make_unary_builtin
          "fail"
          (TFun ([ TString ], task_ty (TVar "a")))
          (fun s -> VTask (TFail (expect_string "Task.fail" s)))
      ; make_binary_builtin
          "catch"
          (TFun
             ( [ task_ty (TVar "a"); TFun ([ TString ], task_ty (TVar "a")) ]
             , task_ty (TVar "a") ))
          (fun t h -> VTask (TCatch (expect_task "Task.catch" t, h)))
      ]
  }
;;

let modules : builtin_module list =
  [ string_module
  ; array_module
  ; json_module
  ; task_module
  ; { name = "Option"
    ; exports =
        [ make_nullary_builtin
            "none"
            (TFun ([], option_ty "a"))
            (fun () -> VVariant ("None", []))
        ; make_unary_builtin
            "some"
            (TFun ([ TVar "a" ], option_ty "a"))
            (fun v -> VVariant ("Some", [ v ]))
        ; make_unary_builtin
            "is_none"
            (TFun ([ option_ty "a" ], TBool))
            (fun v ->
               match v with
               | VVariant ("None", []) -> VBool true
               | VVariant ("Some", [ _ ]) -> VBool false
               | _ -> failwith "Option.is_none: expected `None or `Some(_)")
        ; make_unary_builtin
            "is_some"
            (TFun ([ option_ty "a" ], TBool))
            (fun v ->
               match v with
               | VVariant ("None", []) -> VBool false
               | VVariant ("Some", [ _ ]) -> VBool true
               | _ -> failwith "Option.is_some: expected `None or `Some(_)")
        ; make_binary_builtin
            "get_or"
            (TFun ([ option_ty "a"; TVar "a" ], TVar "a"))
            (fun opt default ->
               match opt with
               | VVariant ("None", []) -> default
               | VVariant ("Some", [ v ]) -> v
               | _ -> failwith "Option.get_or: expected `None or `Some(_)")
        ]
    }
  ; (* Hashtbl: string-keyed, stored as ref(entry array) *)
    { name = "Hashtbl"
    ; exports =
        [ make_nullary_builtin
            "create"
            (TFun ([], tbl_ty "a"))
            (fun () -> VRef (ref (VArray [||])))
        ; make_ternary_builtin
            "set"
            (TFun ([ tbl_ty "a"; TString; TVar "a" ], TUnit))
            (fun tbl k v ->
               let cell = expect_ref "Hashtbl.set" tbl in
               let arr = expect_array "Hashtbl.set" !cell in
               let key = expect_string "Hashtbl.set" k in
               let entry =
                 VRecord
                   (Map.of_alist_exn (module String) [ "key", VString key; "value", v ])
               in
               let n = Array.length arr in
               let rec find i =
                 if i >= n
                 then None
                 else (
                   match arr.(i) with
                   | VRecord m ->
                     (match Map.find m "key" with
                      | Some (VString k') when String.equal k' key -> Some i
                      | _ -> find (i + 1))
                   | _ -> find (i + 1))
               in
               (match find 0 with
                | Some i ->
                  let arr' = Array.copy arr in
                  arr'.(i) <- entry;
                  cell := VArray arr'
                | None ->
                  let arr' =
                    Array.init (n + 1) ~f:(fun i -> if i < n then arr.(i) else entry)
                  in
                  cell := VArray arr');
               VUnit)
        ; make_binary_builtin
            "get"
            (TFun ([ tbl_ty "a"; TString ], option_ty "a"))
            (fun tbl k ->
               let cell = expect_ref "Hashtbl.get" tbl in
               let arr = expect_array "Hashtbl.get" !cell in
               let key = expect_string "Hashtbl.get" k in
               let rec loop i =
                 if i >= Array.length arr
                 then VVariant ("None", [])
                 else (
                   match arr.(i) with
                   | VRecord m ->
                     (match Map.find m "key", Map.find m "value" with
                      | Some (VString k'), Some v when String.equal k' key ->
                        VVariant ("Some", [ v ])
                      | _ -> loop (i + 1))
                   | _ -> loop (i + 1))
               in
               loop 0)
        ; make_binary_builtin
            "mem"
            (TFun ([ tbl_ty "a"; TString ], TBool))
            (fun tbl k ->
               let cell = expect_ref "Hashtbl.mem" tbl in
               let arr = expect_array "Hashtbl.mem" !cell in
               let key = expect_string "Hashtbl.mem" k in
               let rec loop i =
                 if i >= Array.length arr
                 then false
                 else (
                   match arr.(i) with
                   | VRecord m ->
                     (match Map.find m "key" with
                      | Some (VString k') when String.equal k' key -> true
                      | _ -> loop (i + 1))
                   | _ -> loop (i + 1))
               in
               VBool (loop 0))
        ; make_binary_builtin
            "remove"
            (TFun ([ tbl_ty "a"; TString ], TUnit))
            (fun tbl k ->
               let cell = expect_ref "Hashtbl.remove" tbl in
               let arr = expect_array "Hashtbl.remove" !cell in
               let key = expect_string "Hashtbl.remove" k in
               let kept =
                 arr
                 |> Array.to_list
                 |> List.filter ~f:(function
                   | VRecord m ->
                     (match Map.find m "key" with
                      | Some (VString k') -> not (String.equal k' key)
                      | _ -> true)
                   | _ -> true)
                 |> Array.of_list
               in
               cell := VArray kept;
               VUnit)
        ]
    }
  ]
;;

let log_module : builtin_module =
  { name = "Log"
  ; exports =
      [ make_task_unary_perform_builtin "debug" TString TUnit ~op:"Log.debug"
      ; make_task_unary_perform_builtin "info" TString TUnit ~op:"Log.info"
      ; make_task_unary_perform_builtin "warn" TString TUnit ~op:"Log.warn"
      ; make_task_unary_perform_builtin "error" TString TUnit ~op:"Log.error"
      ]
  }
;;

let item_module : builtin_module =
  { name = "Item"
  ; exports =
      [ make_binary_builtin
          "create"
          (TFun ([ TString; json_ty ], item_ty))
          (fun id value ->
             let id = expect_string "Item.create" id in
             let value = expect_json "Item.create" value in
             VRecord
               (Map.of_alist_exn (module String) [ "id", VString id; "value", value ]))
      ; make_unary_builtin
          "id"
          (TFun ([ item_ty ], TString))
          (fun item ->
             let id, _ = expect_item_record "Item.id" item in
             VString id)
      ; make_unary_builtin
          "value"
          (TFun ([ item_ty ], json_ty))
          (fun item ->
             let _, value = expect_item_record "Item.value" item in
             value)
      ; make_unary_builtin
          "kind"
          (TFun ([ item_ty ], option_of TString))
          (fun item ->
             let _, value = expect_item_record "Item.kind" item in
             json_field_value "Item.kind" value "type"
             |> Option.bind ~f:json_string_payload
             |> string_option_value)
      ; make_unary_builtin
          "role"
          (TFun ([ item_ty ], option_of TString))
          (fun item ->
             let _, value = expect_item_record "Item.role" item in
             json_field_value "Item.role" value "role"
             |> Option.bind ~f:json_string_payload
             |> string_option_value)
      ; make_unary_builtin
          "text_parts"
          (TFun ([ item_ty ], TArray TString))
          (fun item ->
             let _, value = expect_item_record "Item.text_parts" item in
             match json_field_value "Item.text_parts" value "content" with
             | Some content_json ->
               (match json_array_payload "Item.text_parts.content" content_json with
                | Some parts -> VArray (text_values_of_parts parts)
                | None -> VArray [||])
             | None ->
               (match json_field_value "Item.text_parts" value "output" with
                | Some output -> VArray (output_text_values output)
                | None -> VArray [||]))
      ; make_ternary_builtin
          "input_text_message"
          (TFun ([ TString; TString; TString ], item_ty))
          (fun id role text ->
             let id = expect_string "Item.input_text_message" id in
             let role = expect_string "Item.input_text_message" role in
             let text = expect_string "Item.input_text_message" text in
             VRecord
               (Map.of_alist_exn
                  (module String)
                  [ "id", VString id; "value", item_input_text_message_value ~role ~text ]))
      ; make_binary_builtin
          "output_text_message"
          (TFun ([ TString; TString ], item_ty))
          (fun id text ->
             let id = expect_string "Item.output_text_message" id in
             let text = expect_string "Item.output_text_message" text in
             VRecord
               (Map.of_alist_exn
                  (module String)
                  [ "id", VString id; "value", item_output_text_message_value ~id ~text ]))
      ]
  }
;;

let turn_module : builtin_module =
  { name = "Turn"
  ; exports =
      [ make_task_unary_perform_builtin
          "prepend_system"
          TString
          TUnit
          ~op:"Turn.prepend_system"
      ; make_task_unary_perform_builtin
          "append_item"
          item_ty
          TUnit
          ~op:"Turn.append_message"
      ; make_task_binary_perform_builtin
          "replace_item"
          TString
          item_ty
          TUnit
          ~op:"Turn.replace_message"
      ; make_task_unary_perform_builtin
          "delete_item"
          TString
          TUnit
          ~op:"Turn.delete_message"
      ; make_task_unary_perform_builtin
          "append_message"
          item_ty
          TUnit
          ~op:"Turn.append_message"
      ; make_task_binary_perform_builtin
          "replace_message"
          TString
          item_ty
          TUnit
          ~op:"Turn.replace_message"
      ; make_task_unary_perform_builtin
          "delete_message"
          TString
          TUnit
          ~op:"Turn.delete_message"
      ; make_task_unary_perform_builtin "halt" TString TUnit ~op:"Turn.halt"
      ]
  }
;;

let tool_module : builtin_module =
  { name = "Tool"
  ; exports =
      [ make_task_nullary_perform_builtin "approve" TUnit ~op:"Tool.approve"
      ; make_task_unary_perform_builtin "reject" TString TUnit ~op:"Tool.reject"
      ; make_task_unary_perform_builtin
          "rewrite_args"
          json_ty
          TUnit
          ~op:"Tool.rewrite_args"
      ; make_task_binary_perform_builtin
          "redirect"
          TString
          json_ty
          TUnit
          ~op:"Tool.redirect"
      ; make_task_binary_perform_builtin
          "call"
          TString
          json_ty
          tool_call_result_ty
          ~op:"Tool.call"
      ; make_task_binary_spawn_builtin "spawn" TString json_ty TString ~op:"Tool.spawn"
      ]
  }
;;

let model_module : builtin_module =
  { name = "Model"
  ; exports =
      [ make_task_binary_perform_builtin
          "call"
          TString
          json_ty
          model_call_result_ty
          ~op:"Model.call"
      ; make_task_binary_spawn_builtin "spawn" TString json_ty TString ~op:"Model.spawn"
      ]
  }
;;

let schedule_module : builtin_module =
  { name = "Schedule"
  ; exports =
      [ make_task_binary_spawn_builtin
          "after_ms"
          TInt
          (TVar "e")
          TString
          ~op:"Schedule.after_ms"
      ; make_task_unary_perform_builtin "cancel" TString TUnit ~op:"Schedule.cancel"
      ]
  }
;;

let runtime_module : builtin_module =
  { name = "Runtime"
  ; exports =
      [ make_task_unary_perform_builtin "emit" (TVar "e") TUnit ~op:"Runtime.emit"
      ; make_task_nullary_perform_builtin
          "request_compaction"
          TUnit
          ~op:"Runtime.request_compaction"
      ; make_task_nullary_perform_builtin "request_turn" TUnit ~op:"Runtime.request_turn"
      ; make_task_unary_perform_builtin
          "end_session"
          TString
          TUnit
          ~op:"Runtime.end_session"
      ]
  }
;;

let core_modules : builtin_module list = modules

let moderator_modules : builtin_module list =
  [ log_module
  ; item_module
  ; turn_module
  ; tool_module
  ; model_module
  ; schedule_module
  ; runtime_module
  ]
;;
