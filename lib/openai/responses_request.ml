open! Core

module Field = struct
  type 'a t =
    | Absent
    | Null
    | Value of 'a
  [@@deriving equal, sexp_of]
end

module Reasoning = struct
  module Effort = struct
    type t =
      | None
      | Minimal
      | Low
      | Medium
      | High
      | Xhigh
      | Max
    [@@deriving equal, sexp_of]
  end

  module Summary = struct
    type t =
      | Auto
      | Concise
      | Detailed
    [@@deriving equal, sexp_of]
  end

  type t =
    { effort : Effort.t Field.t
    ; summary : Summary.t Field.t
    }
end

module Text = struct
  module Verbosity = struct
    type t =
      | Low
      | Medium
      | High
    [@@deriving equal, sexp_of]
  end

  module Format = struct
    type t =
      | Text
      | Json_object
      | Json_schema of
          { name : string
          ; schema : Jsonaf.t
          ; description : string Field.t
          ; strict : bool Field.t
          }
  end

  type t =
    { format : Format.t Field.t
    ; verbosity : Verbosity.t Field.t
    }
end

module Tool_choice = struct
  module Reference = struct
    type t =
      | Function of string
      | Custom of string
  end

  type mode =
    | Auto
    | Required
  [@@deriving equal, sexp_of]

  type t =
    | None
    | Auto
    | Required
    | Named of Reference.t
    | Allowed of
        { mode : mode
        ; tools : Reference.t list
        }
end

module Cache = struct
  module Retention = struct
    type t =
      | In_memory
      | Hours_24
    [@@deriving equal, sexp_of]
  end

  module Options = struct
    type mode =
      | Implicit
      | Explicit
    [@@deriving equal, sexp_of]

    type t =
      { mode : mode Field.t
      ; ttl : [ `Minutes_30 ] Field.t
      }
  end
end

let fail path message = Or_error.error_string (path ^ ": " ^ message)
let bool_json b = if b then `True else `False

let float_json n =
  let number = Float.to_string n in
  `Number (if String.is_suffix number ~suffix:"." then number ^ "0" else number)
;;

let field json key =
  match Jsonaf.member key json with
  | None -> Field.Absent
  | Some `Null -> Null
  | Some value -> Value value
;;

let object_ path = function
  | `Object fields -> Ok fields
  | _ -> fail path "expected object"
;;

let string path = function
  | `String value -> Ok value
  | _ -> fail path "expected string"
;;

let nonempty path json =
  let%bind.Or_error value = string path json in
  if String.is_empty (String.strip value) then fail path "must not be empty" else Ok ()
;;

let boolean path = function
  | `True | `False -> Ok ()
  | _ -> fail path "expected boolean"
;;

let enum path values json =
  let%bind.Or_error value = string path json in
  if List.mem values value ~equal:String.equal
  then Ok ()
  else fail path "unsupported value"
;;

let optional ?(nullable = false) json key validate =
  match field json key with
  | Absent -> Ok ()
  | Null -> if nullable then Ok () else fail key "null is not allowed"
  | Value value -> validate key value
;;

let required ?(nullable = false) json key validate =
  match field json key with
  | Absent -> fail key "required field is missing"
  | Null -> if nullable then Ok () else fail key "null is not allowed"
  | Value value -> validate key value
;;

let closed path json allowed =
  let%bind.Or_error fields = object_ path json in
  List.fold_result fields ~init:() ~f:(fun () (key, _) ->
    if List.mem allowed key ~equal:String.equal
    then Ok ()
    else fail (path ^ "." ^ key) "unsupported field")
;;

(** Bound recursion and cumulative source bytes before any object member lookup.
    The counters belong to this validation invocation only. *)
let validate_json json =
  let nodes = ref 0 in
  let bytes = ref 0 in
  let count size =
    incr nodes;
    bytes := !bytes + size;
    if !nodes > 100_000 || !bytes > 16_777_216
    then fail "request" "JSON validation budget exceeded"
    else Ok ()
  in
  let rec walk depth path json =
    if depth > 64
    then fail path "JSON nesting limit exceeded"
    else (
      let%bind.Or_error () = count 1 in
      match json with
      | `Null | `True | `False -> Ok ()
      | `String s -> count (String.length s)
      | `Number s ->
        let%bind.Or_error () = count (String.length s) in
        (match Or_error.try_with (fun () -> Jsonaf.of_string s) with
         | Ok (`Number parsed) when String.equal parsed s -> Ok ()
         | Ok (`Number _)
         | Ok (`String _ | `Null | `True | `False | `Object _ | `Array _)
         | Error _ -> fail path "invalid JSON number")
      | `Array values ->
        List.fold_result values ~init:() ~f:(fun () value -> walk (depth + 1) path value)
      | `Object fields ->
        let%bind.Or_error _ =
          List.fold_result fields ~init:String.Set.empty ~f:(fun keys (key, value) ->
            if Set.mem keys key
            then fail (path ^ "." ^ key) "duplicate key"
            else (
              let%bind.Or_error () = count (String.length key) in
              let%map.Or_error () = walk (depth + 1) (path ^ "." ^ key) value in
              Set.add keys key))
        in
        Ok ())
  in
  walk 0 "request" json
;;

let schema path json = Or_error.map (object_ path json) ~f:(fun _ -> ())

let name path json =
  let%bind.Or_error () = nonempty path json in
  let%bind.Or_error value = string path json in
  if
    String.length value > 64
    || not
         (String.for_all value ~f:(fun c ->
            Char.is_alphanum c || Char.equal c '_' || Char.equal c '-'))
  then fail path "expected at most64 ASCII letters, digits, underscores or hyphens"
  else Ok ()
;;

let encoded_field key encode = function
  | Field.Absent -> []
  | Null -> [ key, `Null ]
  | Value value -> [ key, encode value ]
;;

let validate_custom_format path json =
  let%bind.Or_error () = required json "type" (fun p -> enum p [ "text"; "grammar" ]) in
  match field json "type" with
  | Value (`String "text") -> closed path json [ "type" ]
  | Value (`String "grammar") ->
    let%bind.Or_error () = closed path json [ "type"; "syntax"; "definition" ] in
    let%bind.Or_error () = required json "syntax" (fun p -> enum p [ "lark"; "regex" ]) in
    required json "definition" nonempty
  | Absent | Null | Value _ -> fail path "unsupported format"
;;

let validate_tool json =
  let%bind.Or_error () =
    required json "type" (fun p -> enum p [ "function"; "custom" ])
  in
  let%bind.Or_error () = required json "name" name in
  match field json "type" with
  | Value (`String "function") ->
    let%bind.Or_error () =
      closed
        "tool"
        json
        [ "type"
        ; "name"
        ; "parameters"
        ; "strict"
        ; "description"
        ; "output_schema"
        ; "async"
        ]
    in
    let%bind.Or_error () = required ~nullable:true json "parameters" schema in
    let%bind.Or_error () = required ~nullable:true json "strict" boolean in
    let%bind.Or_error () =
      optional ~nullable:true json "description" (fun p v ->
        Or_error.map (string p v) ~f:(fun _ -> ()))
    in
    let%bind.Or_error () = optional ~nullable:true json "output_schema" schema in
    optional json "async" boolean
  | Value (`String "custom") ->
    let%bind.Or_error () =
      closed "tool" json [ "type"; "name"; "description"; "format"; "async" ]
    in
    let%bind.Or_error () =
      optional json "description" (fun p v -> Or_error.map (string p v) ~f:(fun _ -> ()))
    in
    let%bind.Or_error () = optional json "format" validate_custom_format in
    optional json "async" boolean
  | Absent | Null | Value _ -> fail "tool" "unsupported local tool"
;;

module Tool = struct
  type t = Jsonaf.t

  module Custom_format = struct
    type t =
      | Text
      | Grammar of
          { syntax : [ `Lark | `Regex ]
          ; definition : string
          }
  end

  let of_jsonaf json =
    let%bind.Or_error () = validate_json json in
    let%map.Or_error () = validate_tool json in
    json
  ;;

  let jsonaf_of_t t = t

  let function_
        ~name
        ~parameters
        ~strict
        ?(description = Field.Absent)
        ?(output_schema = Field.Absent)
        ?(async = Field.Absent)
        ()
    =
    of_jsonaf
      (`Object
          ([ "type", `String "function"; "name", `String name ]
           @ encoded_field "parameters" Fn.id parameters
           @ encoded_field "strict" bool_json strict
           @ encoded_field "description" (fun s -> `String s) description
           @ encoded_field "output_schema" Fn.id output_schema
           @ encoded_field "async" bool_json async))
  ;;

  let format_json = function
    | Custom_format.Text -> `Object [ "type", `String "text" ]
    | Grammar { syntax; definition } ->
      `Object
        [ "type", `String "grammar"
        ; ( "syntax"
          , `String
              (match syntax with
               | `Lark -> "lark"
               | `Regex -> "regex") )
        ; "definition", `String definition
        ]
  ;;

  let custom
        ~name
        ?(description = Field.Absent)
        ?(format = Field.Absent)
        ?(async = Field.Absent)
        ()
    =
    of_jsonaf
      (`Object
          ([ "type", `String "custom"; "name", `String name ]
           @ encoded_field "description" (fun s -> `String s) description
           @ encoded_field "format" format_json format
           @ encoded_field "async" bool_json async))
  ;;
end

let list path json validate =
  match json with
  | `Array values ->
    List.fold_result values ~init:() ~f:(fun () value -> validate path value)
  | _ -> fail path "expected array"
;;

let text_value path json = Or_error.map (string path json) ~f:(fun _ -> ())

module Content_context = struct
  type t =
    | Message
    | Function_output
    | Custom_output
end

let validate_content context path json =
  let nullable =
    match context with
    | Content_context.Function_output -> true
    | Message | Custom_output -> false
  in
  let%bind.Or_error () = required json "type" text_value in
  match field json "type" with
  | Value (`String "input_text") -> required json "text" text_value
  | Value (`String "output_text") ->
    (match context with
     | Message -> required json "text" text_value
     | Function_output | Custom_output ->
       fail path "output_text is not tool-result input content")
  | Value (`String "refusal") ->
    (match context with
     | Message -> required json "refusal" text_value
     | Function_output | Custom_output ->
       fail path "refusal is not tool-result input content")
  | Value (`String "input_image") ->
    let%bind.Or_error () =
      match field json "file_id" with
      | Absent | Null -> Ok ()
      | Value _ -> fail path "provider-only file_id is not locally recoverable"
    in
    let%bind.Or_error () = required json "image_url" nonempty in
    (if nullable then optional ~nullable:true else required ~nullable:false)
      json
      "detail"
      (fun p -> enum p [ "auto"; "low"; "high"; "original" ])
  | Value (`String "input_file") ->
    (match field json "file_id" with
     | Value _ -> fail path "provider-only file_id is not locally recoverable"
     | Absent | Null ->
       let%bind.Or_error () = optional ~nullable json "filename" text_value in
       let%bind.Or_error () = optional ~nullable json "file_data" nonempty in
       let%bind.Or_error () = optional ~nullable json "file_url" nonempty in
       let%bind.Or_error () =
         optional json "detail" (fun p -> enum p [ "auto"; "low"; "high" ])
       in
       (match field json "file_data", field json "file_url" with
        | Value data, Absent | Value data, Null -> nonempty "file_data" data
        | Absent, Value url | Null, Value url -> nonempty "file_url" url
        | _ -> fail path "expected exactly one file_data or file_url"))
  | Absent | Null | Value _ -> fail path "unsupported content kind"
;;

let validate_output context path = function
  | `String _ -> Ok ()
  | `Array _ as json -> list path json (validate_content context)
  | _ -> fail path "expected string or local content array"
;;

let validate_caller path json =
  let%bind.Or_error _ = object_ path json in
  let%bind.Or_error () = required json "type" nonempty in
  match field json "type" with
  | Value (`String "program") -> required json "caller_id" nonempty
  | Absent | Null | Value _ -> Ok ()
;;

let validate_input _path json =
  let%bind.Or_error _ = object_ "input item" json in
  match field json "type" with
  | Absent | Value (`String "message") ->
    let%bind.Or_error () = optional json "id" nonempty in
    let%bind.Or_error () =
      optional json "status" (fun p ->
        enum p [ "in_progress"; "completed"; "incomplete" ])
    in
    let%bind.Or_error () =
      optional ~nullable:true json "phase" (fun p ->
        enum p [ "commentary"; "final_answer" ])
    in
    let%bind.Or_error () =
      required json "role" (fun p ->
        enum p [ "user"; "assistant"; "system"; "developer" ])
    in
    (match field json "content" with
     | Value (`String _) -> Ok ()
     | Value (`Array _ as content) -> list "content" content (validate_content Message)
     | Absent | Null | Value _ -> fail "content" "required string or content array")
  | Value (`String "function_call") | Value (`String "custom_tool_call") ->
    let%bind.Or_error () = optional ~nullable:true json "caller" validate_caller in
    let%bind.Or_error () = optional json "id" nonempty in
    let%bind.Or_error () = optional json "namespace" nonempty in
    let%bind.Or_error () = optional json "async" boolean in
    let%bind.Or_error () =
      optional json "status" (fun p ->
        enum p [ "in_progress"; "completed"; "incomplete" ])
    in
    let%bind.Or_error () = required json "call_id" nonempty in
    let%bind.Or_error () = required json "name" name in
    (match field json "type" with
     | Value (`String "function_call") -> required json "arguments" text_value
     | Absent | Null | Value _ -> required json "input" text_value)
  | Value (`String "function_call_output") | Value (`String "custom_tool_call_output") ->
    let%bind.Or_error () = optional ~nullable:true json "caller" validate_caller in
    let%bind.Or_error () = required json "call_id" nonempty in
    let context =
      match field json "type" with
      | Value (`String "function_call_output") -> Content_context.Function_output
      | Absent | Null | Value _ -> Custom_output
    in
    let nullable =
      match context with
      | Function_output -> true
      | Message | Custom_output -> false
    in
    let%bind.Or_error () = optional ~nullable json "id" nonempty in
    let%bind.Or_error () =
      match context with
      | Function_output ->
        let%bind.Or_error () = optional ~nullable:true json "name" name in
        let%bind.Or_error () = optional ~nullable:true json "namespace" nonempty in
        optional ~nullable:true json "status" (fun p ->
          enum p [ "in_progress"; "completed"; "incomplete" ])
      | Message | Custom_output -> Ok ()
    in
    required json "output" (validate_output context)
  | Value (`String "reasoning") ->
    let%bind.Or_error () =
      optional json "status" (fun p ->
        enum p [ "in_progress"; "completed"; "incomplete" ])
    in
    let%bind.Or_error () = required json "id" nonempty in
    let%bind.Or_error () =
      required json "summary" (fun p j ->
        list p j (fun _ entry ->
          let%bind.Or_error () =
            required entry "type" (fun p -> enum p [ "summary_text" ])
          in
          required entry "text" text_value))
    in
    optional ~nullable:true json "encrypted_content" text_value
  | Null | Value _ -> fail "input item" "unsupported local item kind"
;;

let validate_format path json =
  let%bind.Or_error () =
    required json "type" (fun p -> enum p [ "text"; "json_object"; "json_schema" ])
  in
  match field json "type" with
  | Value (`String "text") | Value (`String "json_object") -> closed path json [ "type" ]
  | Value (`String "json_schema") ->
    let%bind.Or_error () =
      closed path json [ "type"; "name"; "schema"; "description"; "strict" ]
    in
    let%bind.Or_error () = required json "name" name in
    let%bind.Or_error () = required json "schema" schema in
    let%bind.Or_error () = optional json "description" text_value in
    optional ~nullable:true json "strict" boolean
  | Absent | Null | Value _ -> fail path "unsupported text format"
;;

let validate_text path json =
  let%bind.Or_error () = closed path json [ "format"; "verbosity" ] in
  let%bind.Or_error () = optional json "format" validate_format in
  optional ~nullable:true json "verbosity" (fun p -> enum p [ "low"; "medium"; "high" ])
;;

let validate_reasoning path json =
  let%bind.Or_error () = closed path json [ "effort"; "summary" ] in
  let%bind.Or_error () =
    optional ~nullable:true json "effort" (fun p ->
      enum p [ "none"; "minimal"; "low"; "medium"; "high"; "xhigh"; "max" ])
  in
  optional ~nullable:true json "summary" (fun p ->
    enum p [ "auto"; "concise"; "detailed" ])
;;

let validate_cache path json =
  let%bind.Or_error () = closed path json [ "mode"; "ttl" ] in
  let%bind.Or_error () =
    optional json "mode" (fun p -> enum p [ "implicit"; "explicit" ])
  in
  optional json "ttl" (fun p -> enum p [ "30m" ])
;;

let validate_reference json =
  let%bind.Or_error () = closed "tool reference" json [ "type"; "name" ] in
  let%bind.Or_error () =
    required json "type" (fun p -> enum p [ "function"; "custom" ])
  in
  required json "name" name
;;

let validate_choice path json =
  match json with
  | `String _ -> enum path [ "none"; "auto"; "required" ] json
  | `Object _ ->
    (match field json "type" with
     | Value (`String "allowed_tools") ->
       let%bind.Or_error () = closed path json [ "type"; "mode"; "tools" ] in
       let%bind.Or_error () =
         required json "mode" (fun p -> enum p [ "auto"; "required" ])
       in
       required json "tools" (fun p j -> list p j (fun _ -> validate_reference))
     | Absent | Null | Value _ -> validate_reference json)
  | _ -> fail path "expected tool-choice string or object"
;;

let float_range minimum maximum path json =
  match json with
  | `Number number ->
    (match Or_error.try_with (fun () -> Float.of_string number) with
     | Ok value when Float.is_finite value && Float.(value >= minimum && value <= maximum)
       -> Ok ()
     | Ok _ | Error _ -> fail path "number outside permitted range")
  | _ -> fail path "expected number"
;;

let output_tokens path = function
  | `Number number ->
    (match Int.of_string_opt number with
     | Some value when value >= 16 -> Ok ()
     | Some _ | None -> fail path "expected integer at least16")
  | _ -> fail path "expected integer at least16"
;;

type t =
  { json : Jsonaf.t
  ; model : string
  ; input : Jsonaf.t list
  ; stream : bool
  }

let of_jsonaf json =
  let%bind.Or_error () = validate_json json in
  let%bind.Or_error () =
    closed
      "request"
      json
      [ "model"
      ; "input"
      ; "store"
      ; "truncation"
      ; "stream"
      ; "instructions"
      ; "max_output_tokens"
      ; "parallel_tool_calls"
      ; "temperature"
      ; "top_p"
      ; "reasoning"
      ; "text"
      ; "tools"
      ; "tool_choice"
      ; "prompt_cache_key"
      ; "prompt_cache_retention"
      ; "prompt_cache_options"
      ; "include"
      ]
  in
  let%bind.Or_error () = required json "model" nonempty in
  let%bind.Or_error model =
    match field json "model" with
    | Value value -> string "model" value
    | Absent | Null -> fail "model" "required"
  in
  let%bind.Or_error input =
    match field json "input" with
    | Value (`Array input) -> Ok input
    | Absent | Null | Value _ -> fail "input" "required full input array"
  in
  let%bind.Or_error () = list "input" (`Array input) validate_input in
  let%bind.Or_error () =
    required json "store" (fun p -> function
      | `False -> Ok ()
      | _ -> fail p "local profile requires false")
  in
  let%bind.Or_error () = optional json "stream" boolean in
  let%bind.Or_error () = optional json "truncation" (fun p -> enum p [ "disabled" ]) in
  let stream =
    match field json "stream" with
    | Value `True -> true
    | Absent | Null | Value _ -> false
  in
  let%bind.Or_error () = optional ~nullable:true json "instructions" text_value in
  let%bind.Or_error () = optional ~nullable:true json "max_output_tokens" output_tokens in
  let%bind.Or_error () = optional ~nullable:true json "parallel_tool_calls" boolean in
  let%bind.Or_error () = optional ~nullable:true json "temperature" (float_range 0. 2.) in
  let%bind.Or_error () = optional ~nullable:true json "top_p" (float_range 0. 1.) in
  let%bind.Or_error () = optional ~nullable:true json "reasoning" validate_reasoning in
  let%bind.Or_error () = optional json "text" validate_text in
  let%bind.Or_error () =
    optional json "tools" (fun p j -> list p j (fun _ -> validate_tool))
  in
  let%bind.Or_error () = optional json "tool_choice" validate_choice in
  let%bind.Or_error () = optional ~nullable:true json "prompt_cache_key" text_value in
  let%bind.Or_error () =
    optional ~nullable:true json "prompt_cache_retention" (fun p ->
      enum p [ "in_memory"; "24h" ])
  in
  let%bind.Or_error () = optional json "prompt_cache_options" validate_cache in
  let%bind.Or_error () =
    optional ~nullable:true json "include" (fun p j ->
      list p j (fun p -> enum p [ "reasoning.encrypted_content" ]))
  in
  let tools =
    match field json "tools" with
    | Value (`Array tools) -> tools
    | Absent | Null | Value _ -> []
  in
  let%bind.Or_error _ =
    List.fold_result tools ~init:String.Set.empty ~f:(fun names tool ->
      match field tool "name" with
      | Value (`String name) ->
        if Set.mem names name
        then fail "tools" "duplicate local tool name"
        else Ok (Set.add names name)
      | Absent | Null | Value _ -> fail "tools" "missing name")
  in
  let references =
    match field json "tool_choice" with
    | Value (`Object _ as choice) ->
      (match field choice "tools" with
       | Value (`Array refs) -> refs
       | Absent | Null | Value _ -> [ choice ])
    | Absent | Null | Value _ -> []
  in
  let%bind.Or_error () =
    List.fold_result references ~init:() ~f:(fun () reference ->
      let matches tool =
        List.for_all [ "type"; "name" ] ~f:(fun key ->
          Field.equal Jsonaf.exactly_equal (field tool key) (field reference key))
      in
      if List.exists tools ~f:matches
      then Ok ()
      else fail "tool_choice" "reference is not a declared local tool")
  in
  let%bind.Or_error () =
    match field json "tool_choice" with
    | Value (`String "required") when List.is_empty tools ->
      fail "tool_choice" "required needs at least one local tool"
    | Value (`Object _ as choice) ->
      (match field choice "mode", field choice "tools" with
       | Value (`String "required"), Value (`Array []) ->
         fail "tool_choice" "required allowed_tools must not be empty"
       | _ -> Ok ())
    | Absent | Null | Value _ -> Ok ()
  in
  Ok { json; model; input; stream }
;;

let jsonaf_of_t t = t.json
let to_jsonaf = jsonaf_of_t
let model t = t.model
let input t = t.input
let stream t = t.stream
let field t key = field t.json key

let effort_json = function
  | Reasoning.Effort.None -> `String "none"
  | Minimal -> `String "minimal"
  | Low -> `String "low"
  | Medium -> `String "medium"
  | High -> `String "high"
  | Xhigh -> `String "xhigh"
  | Max -> `String "max"
;;

let summary_json = function
  | Reasoning.Summary.Auto -> `String "auto"
  | Concise -> `String "concise"
  | Detailed -> `String "detailed"
;;

let reasoning_json (t : Reasoning.t) =
  `Object
    (encoded_field "effort" effort_json t.effort
     @ encoded_field "summary" summary_json t.summary)
;;

let format_json = function
  | Text.Format.Text -> `Object [ "type", `String "text" ]
  | Json_object -> `Object [ "type", `String "json_object" ]
  | Json_schema { name; schema; description; strict } ->
    `Object
      ([ "type", `String "json_schema"; "name", `String name; "schema", schema ]
       @ encoded_field "description" (fun s -> `String s) description
       @ encoded_field "strict" bool_json strict)
;;

let text_json (t : Text.t) =
  `Object
    (encoded_field "format" format_json t.format
     @ encoded_field
         "verbosity"
         (function
           | Text.Verbosity.Low -> `String "low"
           | Medium -> `String "medium"
           | High -> `String "high")
         t.verbosity)
;;

let reference_json = function
  | Tool_choice.Reference.Function name ->
    `Object [ "type", `String "function"; "name", `String name ]
  | Custom name -> `Object [ "type", `String "custom"; "name", `String name ]
;;

let choice_json = function
  | Tool_choice.None -> `String "none"
  | Auto -> `String "auto"
  | Required -> `String "required"
  | Named reference -> reference_json reference
  | Allowed { mode; tools } ->
    `Object
      [ "type", `String "allowed_tools"
      ; ( "mode"
        , `String
            (match mode with
             | Tool_choice.Auto -> "auto"
             | Required -> "required") )
      ; "tools", `Array (List.map tools ~f:reference_json)
      ]
;;

let cache_json (t : Cache.Options.t) =
  `Object
    (encoded_field
       "mode"
       (function
         | Cache.Options.Implicit -> `String "implicit"
         | Explicit -> `String "explicit")
       t.mode
     @ encoded_field
         "ttl"
         (function
           | `Minutes_30 -> `String "30m")
         t.ttl)
;;

let create
      ~model
      ~input
      ~stream
      ?(instructions = Field.Absent)
      ?(max_output_tokens = Field.Absent)
      ?(parallel_tool_calls = Field.Absent)
      ?(temperature = Field.Absent)
      ?(top_p = Field.Absent)
      ?(reasoning = Field.Absent)
      ?(text = Field.Absent)
      ?(tools = Field.Absent)
      ?(tool_choice = Field.Absent)
      ?(prompt_cache_key = Field.Absent)
      ?(prompt_cache_retention = Field.Absent)
      ?(prompt_cache_options = Field.Absent)
      ?(include_encrypted_reasoning = true)
      ()
  =
  of_jsonaf
    (`Object
        ([ "model", `String model
         ; "input", `Array input
         ; "store", `False
         ; "truncation", `String "disabled"
         ; "stream", bool_json stream
         ]
         @ encoded_field "instructions" (fun s -> `String s) instructions
         @ encoded_field
             "max_output_tokens"
             (fun n -> `Number (Int.to_string n))
             max_output_tokens
         @ encoded_field "parallel_tool_calls" bool_json parallel_tool_calls
         @ encoded_field "temperature" float_json temperature
         @ encoded_field "top_p" float_json top_p
         @ encoded_field "reasoning" reasoning_json reasoning
         @ encoded_field "text" text_json text
         @ encoded_field
             "tools"
             (fun ts -> `Array (List.map ts ~f:Tool.jsonaf_of_t))
             tools
         @ encoded_field "tool_choice" choice_json tool_choice
         @ encoded_field "prompt_cache_key" (fun s -> `String s) prompt_cache_key
         @ encoded_field
             "prompt_cache_retention"
             (function
               | Cache.Retention.In_memory -> `String "in_memory"
               | Hours_24 -> `String "24h")
             prompt_cache_retention
         @ encoded_field "prompt_cache_options" cache_json prompt_cache_options
         @
         if include_encrypted_reasoning
         then [ "include", `Array [ `String "reasoning.encrypted_content" ] ]
         else []))
;;
