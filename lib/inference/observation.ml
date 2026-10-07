open! Core
module D = Document_schema

module Error = struct
  type t =
    | Json of D.Error.t
    | Invalid_field of
        { field : string
        ; reason : string
        }
    | Conflicting_revision
    | Conflicting_scope
    | Conflicting_kind
    | Retention_limit
  [@@deriving equal, sexp_of]
end

let invalid field reason = Error (Error.Invalid_field { field; reason })
let json_error result = Result.map_error result ~f:(fun error -> Error.Json error)

let external_error field result =
  Result.map_error result ~f:(fun reason -> Error.Invalid_field { field; reason })
;;

let ( let* ) result f = Result.bind result ~f
let string value = `String value
let decimal value = string (Int64.to_string value)
let integer value = `Number (Int.to_string value)
let obj fields = `Object fields

let nullable f = function
  | None -> `Null
  | Some value -> f value
;;

let list f values = `Array (List.map values ~f)

module Admission = struct
  let limits bytes =
    Transcript.Admission.limits ~max_bytes:bytes
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  ;;

  let observation = limits (8 * 1024)
  let diagnostic = limits 1024
  let attempt = limits (64 * 1024)
end

let label field value =
  if String.is_empty value
  then invalid field "required nonempty identity"
  else
    let* bytes =
      json_error
        (D.Json.validate_and_measure ~limits:Admission.observation (`String value))
    in
    if bytes > 512 then invalid field "identity exceeds 512 encoded bytes" else Ok ()
;;

module Decode = struct
  let fields json allowed =
    match json with
    | `Object fields ->
      let allowed = Set.of_list (module String) allowed in
      (match List.find fields ~f:(fun (key, _) -> not (Set.mem allowed key)) with
       | Some (field, _) -> invalid field "unknown safe observation field"
       | None -> Ok fields)
    | `Null | `True | `False | `Number _ | `String _ | `Array _ ->
      invalid "object" "expected object"
  ;;

  let field fields name =
    match List.Assoc.find fields name ~equal:String.equal with
    | None -> invalid name "required field is absent"
    | Some value -> Ok value
  ;;

  let text = function
    | `String value -> Ok value
    | _ -> invalid "string" "expected string"
  ;;

  let bool = function
    | `True -> Ok true
    | `False -> Ok false
    | _ -> invalid "boolean" "expected boolean"
  ;;

  let array = function
    | `Array values -> Ok values
    | _ -> invalid "array" "expected array"
  ;;

  let int64 json =
    let* value = text json in
    match Int64.of_string_opt value with
    | Some count when String.equal value (Int64.to_string count) -> Ok count
    | Some _ | None -> invalid "integer" "expected canonical decimal int64 string"
  ;;

  let int json =
    match json with
    | `Number value ->
      (match Int.of_string_opt value with
       | Some number when String.equal value (Int.to_string number) -> Ok number
       | Some _ | None -> invalid "integer" "expected canonical host integer")
    | _ -> invalid "integer" "expected number"
  ;;

  let float = function
    | `Number value ->
      (match Float.of_string_opt value with
       | Some number when Float.is_finite number -> Ok number
       | Some _ | None -> invalid "number" "expected finite number")
    | _ -> invalid "number" "expected number"
  ;;

  let optional f = function
    | `Null -> Ok None
    | value -> Result.map (f value) ~f:Option.some
  ;;

  let get fields name f =
    let* value = field fields name in
    f value
  ;;

  let tag value choices =
    let* value = text value in
    match List.Assoc.find choices value ~equal:String.equal with
    | Some result -> Ok result
    | None -> invalid "kind" "unknown safe observation vocabulary"
  ;;
end

module Observation_id = struct
  module T = struct
    type t = string [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let of_string value =
    let* () = label "observation_id" value in
    Ok value
  ;;

  let to_string value = value
end

module Estimator = struct
  type method_ =
    | O200k_serialized_history
    | Utf8_heuristic
  [@@deriving equal, sexp_of]

  type t =
    { method_ : method_
    ; version : int
    }
  [@@deriving equal]

  let create ~method_ ~version =
    if version <= 0
    then invalid "estimator.version" "required positive version"
    else Ok { method_; version }
  ;;

  let method_ t = t.method_
  let version t = t.version

  let to_json t =
    obj
      [ ( "method"
        , string
            (match t.method_ with
             | O200k_serialized_history -> "o200k_serialized_history"
             | Utf8_heuristic -> "utf8_heuristic") )
      ; "version", integer t.version
      ]
  ;;

  let of_json json =
    let* fields = Decode.fields json [ "method"; "version" ] in
    let* method_ =
      Decode.get fields "method" (fun value ->
        Decode.tag
          value
          [ "o200k_serialized_history", O200k_serialized_history
          ; "utf8_heuristic", Utf8_heuristic
          ])
    in
    let* version = Decode.get fields "version" Decode.int in
    create ~method_ ~version
  ;;
end

module Count = struct
  type unknown_reason =
    | Not_reported
    | Explicit_null
    | Interrupted
    | Not_submitted
    | Before_tracking
  [@@deriving equal, sexp_of]

  type view =
    | Unknown of unknown_reason
    | Estimated of
        { tokens : int64
        ; estimator : Estimator.t
        }
    | Actual of int64

  type t = view

  let create view =
    match view with
    | Unknown _ -> Ok view
    | Estimated { tokens; estimator = _ } | Actual tokens ->
      if Int64.(tokens < zero) then invalid "tokens" "negative token count" else Ok view
  ;;

  let view (t : t) : view = t

  let equal a b =
    match a, b with
    | Unknown a, Unknown b -> equal_unknown_reason a b
    | Actual a, Actual b -> Int64.equal a b
    | Estimated a, Estimated b ->
      Int64.equal a.tokens b.tokens && Estimator.equal a.estimator b.estimator
    | (Unknown _ | Actual _ | Estimated _), _ -> false
  ;;

  let reason_to_string = function
    | Not_reported -> "not_reported"
    | Explicit_null -> "explicit_null"
    | Interrupted -> "interrupted"
    | Not_submitted -> "not_submitted"
    | Before_tracking -> "before_tracking"
  ;;

  let to_json = function
    | Unknown reason ->
      obj [ "kind", string "unknown"; "reason", string (reason_to_string reason) ]
    | Actual tokens -> obj [ "kind", string "actual"; "tokens", decimal tokens ]
    | Estimated { tokens; estimator } ->
      obj
        [ "kind", string "estimated"
        ; "tokens", decimal tokens
        ; "estimator", Estimator.to_json estimator
        ]
  ;;

  let of_json json =
    let* all = Decode.fields json [ "kind"; "reason"; "tokens"; "estimator" ] in
    let* kind = Decode.get all "kind" Decode.text in
    let* value =
      match kind with
      | "unknown" ->
        let* fields = Decode.fields json [ "kind"; "reason" ] in
        let* reason =
          Decode.get fields "reason" (fun value ->
            Decode.tag
              value
              [ "not_reported", Not_reported
              ; "explicit_null", Explicit_null
              ; "interrupted", Interrupted
              ; "not_submitted", Not_submitted
              ; "before_tracking", Before_tracking
              ])
        in
        Ok (Unknown reason)
      | "actual" ->
        let* fields = Decode.fields json [ "kind"; "tokens" ] in
        let* tokens = Decode.get fields "tokens" Decode.int64 in
        Ok (Actual tokens)
      | "estimated" ->
        let* fields = Decode.fields json [ "kind"; "tokens"; "estimator" ] in
        let* tokens = Decode.get fields "tokens" Decode.int64 in
        let* estimator = Decode.get fields "estimator" Estimator.of_json in
        Ok (Estimated { tokens; estimator })
      | _ -> invalid "count.kind" "unknown count kind"
    in
    create value
  ;;
end

module Usage = struct
  module Component = struct
    type t =
      | Input
      | Output
      | Reported_total
      | Cached_input
      | Cache_write_input
      | Reasoning_output
    [@@deriving compare, equal, sexp_of]

    let all =
      [ Input; Output; Reported_total; Cached_input; Cache_write_input; Reasoning_output ]
    ;;

    let to_string = function
      | Input -> "input"
      | Output -> "output"
      | Reported_total -> "reported_total"
      | Cached_input -> "cached_input"
      | Cache_write_input -> "cache_write_input"
      | Reasoning_output -> "reasoning_output"
    ;;

    let of_json json =
      Decode.tag json (List.map all ~f:(fun component -> to_string component, component))
    ;;
  end

  type counts =
    { input : Count.t
    ; output : Count.t
    ; reported_total : Count.t
    ; cached_input : Count.t
    ; cache_write_input : Count.t
    ; reasoning_output : Count.t
    }

  type inclusion =
    { subset : Component.t
    ; included_in : Component.t
    }
  [@@deriving compare, equal]

  type t =
    { counts : counts
    ; inclusions : inclusion list
    }

  let counts t = t.counts

  let count t = function
    | Component.Input -> t.counts.input
    | Output -> t.counts.output
    | Reported_total -> t.counts.reported_total
    | Cached_input -> t.counts.cached_input
    | Cache_write_input -> t.counts.cache_write_input
    | Reasoning_output -> t.counts.reasoning_output
  ;;

  let inclusions t = t.inclusions

  let create ~counts ~inclusions =
    if List.length inclusions > 12
    then invalid "inclusions" "at most twelve edges"
    else (
      let sorted = List.sort inclusions ~compare:compare_inclusion in
      if List.contains_dup sorted ~compare:compare_inclusion
      then invalid "inclusions" "duplicate inclusion"
      else (
        let t = { counts; inclusions = sorted } in
        let successors node =
          List.filter_map sorted ~f:(fun edge ->
            if Component.equal edge.subset node then Some edge.included_in else None)
        in
        let rec reaches ~visited node target =
          List.exists (successors node) ~f:(fun next ->
            Component.equal next target
            || ((not (List.mem visited next ~equal:Component.equal))
                && reaches ~visited:(next :: visited) next target))
        in
        if List.exists Component.all ~f:(fun node -> reaches ~visited:[ node ] node node)
        then invalid "inclusions" "cyclic inclusion"
        else (
          let invalid_actual =
            List.exists Component.all ~f:(fun subset ->
              List.exists Component.all ~f:(fun included_in ->
                reaches ~visited:[ subset ] subset included_in
                &&
                match Count.view (count t subset), Count.view (count t included_in) with
                | Actual subset, Actual container -> Int64.(subset > container)
                | (Unknown _ | Estimated _ | Actual _), _ -> false))
          in
          if invalid_actual
          then invalid "inclusions" "actual subset exceeds actual container"
          else Ok t)))
  ;;

  let equal a b =
    List.for_all Component.all ~f:(fun component ->
      Count.equal (count a component) (count b component))
    && List.equal equal_inclusion a.inclusions b.inclusions
  ;;

  let to_json t =
    obj
      [ ( "counts"
        , obj
            (List.map Component.all ~f:(fun component ->
               Component.to_string component, Count.to_json (count t component))) )
      ; ( "inclusions"
        , list
            (fun edge ->
               obj
                 [ "subset", string (Component.to_string edge.subset)
                 ; "included_in", string (Component.to_string edge.included_in)
                 ])
            t.inclusions )
      ]
  ;;

  let of_json json =
    let* fields = Decode.fields json [ "counts"; "inclusions" ] in
    let* raw_counts = Decode.field fields "counts" in
    let* counts =
      Decode.fields raw_counts (List.map Component.all ~f:Component.to_string)
    in
    let* input = Decode.get counts "input" Count.of_json in
    let* output = Decode.get counts "output" Count.of_json in
    let* reported_total = Decode.get counts "reported_total" Count.of_json in
    let* cached_input = Decode.get counts "cached_input" Count.of_json in
    let* cache_write_input = Decode.get counts "cache_write_input" Count.of_json in
    let* reasoning_output = Decode.get counts "reasoning_output" Count.of_json in
    let* raw_edges = Decode.get fields "inclusions" Decode.array in
    let* () =
      if List.length raw_edges > 12
      then invalid "inclusions" "at most twelve edges"
      else Ok ()
    in
    let* inclusions =
      Result.all
        (List.map raw_edges ~f:(fun json ->
           let* fields = Decode.fields json [ "subset"; "included_in" ] in
           let* subset = Decode.get fields "subset" Component.of_json in
           let* included_in = Decode.get fields "included_in" Component.of_json in
           Ok { subset; included_in }))
    in
    create
      ~counts:
        { input
        ; output
        ; reported_total
        ; cached_input
        ; cache_write_input
        ; reasoning_output
        }
      ~inclusions
  ;;
end

module Context_estimate = struct
  type capacity =
    | Unknown
    | Declared of int64
  [@@deriving equal]

  type t =
    { preparation_id : string
    ; count : Count.t
    ; capacity : capacity
    }

  let create ~preparation_id ~count ~capacity =
    let* () = label "preparation_id" preparation_id in
    let* () =
      match Count.view count with
      | Actual _ -> invalid "context.count" "context estimate cannot be actual usage"
      | Unknown _ | Estimated _ -> Ok ()
    in
    let* () =
      match capacity with
      | Unknown -> Ok ()
      | Declared value ->
        if Int64.(value <= zero)
        then invalid "capacity" "required positive capacity"
        else Ok ()
    in
    Ok { preparation_id; count; capacity }
  ;;

  let preparation_id t = t.preparation_id
  let count t = t.count
  let capacity t = t.capacity

  let equal a b =
    String.equal a.preparation_id b.preparation_id
    && Count.equal a.count b.count
    && equal_capacity a.capacity b.capacity
  ;;

  let to_json t =
    obj
      [ "preparation_id", string t.preparation_id
      ; "count", Count.to_json t.count
      ; ( "capacity"
        , match t.capacity with
          | Unknown -> `Null
          | Declared value -> decimal value )
      ]
  ;;

  let of_json json =
    let* fields = Decode.fields json [ "preparation_id"; "count"; "capacity" ] in
    let* preparation_id = Decode.get fields "preparation_id" Decode.text in
    let* count = Decode.get fields "count" Count.of_json in
    let* capacity =
      Decode.get fields "capacity" (function
        | `Null -> Ok Unknown
        | json -> Result.map (Decode.int64 json) ~f:(fun value -> Declared value))
    in
    create ~preparation_id ~count ~capacity
  ;;
end

module Configuration = struct
  module Name = struct
    type t =
      | Instructions
      | Max_output_tokens
      | Parallel_tool_calls
      | Temperature
      | Top_p
      | Reasoning_effort
      | Reasoning_summary
      | Text_verbosity
      | Text_format
      | Tool_choice
      | Prompt_cache_key
      | Prompt_cache_retention
    [@@deriving compare, equal, sexp_of]

    let all =
      [ Instructions
      ; Max_output_tokens
      ; Parallel_tool_calls
      ; Temperature
      ; Top_p
      ; Reasoning_effort
      ; Reasoning_summary
      ; Text_verbosity
      ; Text_format
      ; Tool_choice
      ; Prompt_cache_key
      ; Prompt_cache_retention
      ]
    ;;

    let to_string = function
      | Instructions -> "instructions"
      | Max_output_tokens -> "max_output_tokens"
      | Parallel_tool_calls -> "parallel_tool_calls"
      | Temperature -> "temperature"
      | Top_p -> "top_p"
      | Reasoning_effort -> "reasoning_effort"
      | Reasoning_summary -> "reasoning_summary"
      | Text_verbosity -> "text_verbosity"
      | Text_format -> "text_format"
      | Tool_choice -> "tool_choice"
      | Prompt_cache_key -> "prompt_cache_key"
      | Prompt_cache_retention -> "prompt_cache_retention"
    ;;

    let of_json json =
      Decode.tag json (List.map all ~f:(fun name -> to_string name, name))
    ;;
  end

  type value =
    | Tokens of int64
    | Boolean of bool
    | Temperature of float
    | Probability of float
    | Reasoning_effort of [ `None | `Minimal | `Low | `Medium | `High | `Xhigh ]
    | Reasoning_summary of [ `Auto | `Concise | `Detailed ]
    | Verbosity of [ `Low | `Medium | `High ]
    | Text_format of [ `Text | `Json_object | `Json_schema ]
    | Tool_choice of [ `Auto | `None | `Required ]
    | Cache_retention of [ `In_memory | `Hours_24 ]
  [@@deriving equal]

  type selection =
    | Omitted
    | Explicit_null
    | Value of value
    | Withheld
  [@@deriving equal]

  type setting =
    { name : Name.t
    ; selection : selection
    ; provenance : Request.Setting.provenance option
    }

  type feature =
    | Text_input
    | Image_input
    | Document_input
    | Function_tools
    | Custom_tools
    | Opaque_replay
    | Setting of Name.t
  [@@deriving compare, equal, sexp_of]

  type support =
    | Supported
    | Unsupported
    | Unknown
  [@@deriving equal, sexp_of]

  type transport =
    | Http_sse
    | Websocket
    | In_process
    | Unknown_transport
  [@@deriving equal, sexp_of]

  type t =
    { adapter : string
    ; profile : string
    ; profile_revision : string option
    ; account : string option
    ; model : string
    ; preparation_id : string
    ; transport : transport
    ; settings : setting list
    ; withheld_settings : int
    ; capabilities : (feature * support) list
    }

  let adapter t = t.adapter
  let profile t = t.profile
  let profile_revision t = t.profile_revision
  let account t = t.account
  let model t = t.model
  let preparation_id t = t.preparation_id
  let transport t = t.transport
  let settings t = t.settings
  let withheld_settings t = t.withheld_settings
  let capabilities t = t.capabilities

  let equal_setting a b =
    Name.equal a.name b.name
    && equal_selection a.selection b.selection
    && Option.equal Request.Setting.equal_provenance a.provenance b.provenance
  ;;

  let equal a b =
    String.equal a.adapter b.adapter
    && String.equal a.profile b.profile
    && Option.equal String.equal a.profile_revision b.profile_revision
    && Option.equal String.equal a.account b.account
    && String.equal a.model b.model
    && String.equal a.preparation_id b.preparation_id
    && equal_transport a.transport b.transport
    && List.equal equal_setting a.settings b.settings
    && Int.equal a.withheld_settings b.withheld_settings
    && List.equal
         (fun (af, av) (bf, bv) -> equal_feature af bf && equal_support av bv)
         a.capabilities
         b.capabilities
  ;;

  let provenance_to_json = function
    | Request.Setting.Execution_override -> string "execution_override"
    | Captured_prompt -> string "captured_prompt"
    | Profile_default -> string "profile_default"
  ;;

  let provenance_of_json value =
    Decode.tag
      value
      [ "execution_override", Request.Setting.Execution_override
      ; "captured_prompt", Captured_prompt
      ; "profile_default", Profile_default
      ]
  ;;

  let enum values json = Decode.tag json values

  let value_of_json name json =
    match name with
    | Name.Max_output_tokens ->
      let* value = Decode.int64 json in
      if Int64.(value <= zero)
      then invalid "max_output_tokens" "required positive tokens"
      else Ok (Tokens value)
    | Parallel_tool_calls -> Result.map (Decode.bool json) ~f:(fun value -> Boolean value)
    | Temperature ->
      let* value = Decode.float json in
      if Float.(value < 0. || value > 2.)
      then invalid "temperature" "outside 0..2"
      else Ok (Temperature value)
    | Top_p ->
      let* value = Decode.float json in
      if Float.(value < 0. || value > 1.)
      then invalid "top_p" "outside 0..1"
      else Ok (Probability value)
    | Reasoning_effort ->
      Result.map
        (enum
           [ "none", `None
           ; "minimal", `Minimal
           ; "low", `Low
           ; "medium", `Medium
           ; "high", `High
           ; "xhigh", `Xhigh
           ]
           json)
        ~f:(fun value -> Reasoning_effort value)
    | Reasoning_summary ->
      Result.map
        (enum [ "auto", `Auto; "concise", `Concise; "detailed", `Detailed ] json)
        ~f:(fun value -> Reasoning_summary value)
    | Text_verbosity ->
      Result.map
        (enum [ "low", `Low; "medium", `Medium; "high", `High ] json)
        ~f:(fun value -> Verbosity value)
    | Text_format ->
      Result.map
        (enum
           [ "text", `Text; "json_object", `Json_object; "json_schema", `Json_schema ]
           json)
        ~f:(fun value -> Text_format value)
    | Tool_choice ->
      Result.map
        (enum [ "auto", `Auto; "none", `None; "required", `Required ] json)
        ~f:(fun value -> Tool_choice value)
    | Prompt_cache_retention ->
      Result.map
        (enum [ "in-memory", `In_memory; "24h", `Hours_24 ] json)
        ~f:(fun value -> Cache_retention value)
    | Instructions | Prompt_cache_key ->
      invalid "setting" "private setting value must be withheld"
  ;;

  let value_to_json = function
    | Tokens value -> decimal value
    | Boolean value -> if value then `True else `False
    | Temperature value | Probability value ->
      (* [Float.to_string] emits [1.] for integral values; JSON requires a digit
         after a decimal point. These values are already finite and validated. *)
      let number = Float.to_string value in
      `Number (if String.is_suffix number ~suffix:"." then number ^ "0" else number)
    | Reasoning_effort value ->
      string
        (match value with
         | `None -> "none"
         | `Minimal -> "minimal"
         | `Low -> "low"
         | `Medium -> "medium"
         | `High -> "high"
         | `Xhigh -> "xhigh")
    | Reasoning_summary value ->
      string
        (match value with
         | `Auto -> "auto"
         | `Concise -> "concise"
         | `Detailed -> "detailed")
    | Verbosity value ->
      string
        (match value with
         | `Low -> "low"
         | `Medium -> "medium"
         | `High -> "high")
    | Text_format value ->
      string
        (match value with
         | `Text -> "text"
         | `Json_object -> "json_object"
         | `Json_schema -> "json_schema")
    | Tool_choice value ->
      string
        (match value with
         | `Auto -> "auto"
         | `None -> "none"
         | `Required -> "required")
    | Cache_retention value ->
      string
        (match value with
         | `In_memory -> "in-memory"
         | `Hours_24 -> "24h")
  ;;

  let selection_to_json = function
    | Omitted -> obj [ "kind", string "omitted" ]
    | Explicit_null -> obj [ "kind", string "null" ]
    | Withheld -> obj [ "kind", string "withheld" ]
    | Value value -> obj [ "kind", string "value"; "value", value_to_json value ]
  ;;

  let selection_of_json name json =
    let* fields = Decode.fields json [ "kind"; "value" ] in
    let* kind = Decode.get fields "kind" Decode.text in
    match kind with
    | "value" ->
      let* value = Decode.get fields "value" (value_of_json name) in
      Ok (Value value)
    | "omitted" | "null" | "withheld" ->
      let* _ = Decode.fields json [ "kind" ] in
      Ok
        (match kind with
         | "omitted" -> Omitted
         | "null" -> Explicit_null
         | _ -> Withheld)
    | _ -> invalid "selection.kind" "unknown selection"
  ;;

  let feature_to_json = function
    | Setting name ->
      obj [ "kind", string "setting"; "name", string (Name.to_string name) ]
    | Text_input -> obj [ "kind", string "text_input" ]
    | Image_input -> obj [ "kind", string "image_input" ]
    | Document_input -> obj [ "kind", string "document_input" ]
    | Function_tools -> obj [ "kind", string "function_tools" ]
    | Custom_tools -> obj [ "kind", string "custom_tools" ]
    | Opaque_replay -> obj [ "kind", string "opaque_replay" ]
  ;;

  let feature_of_json json =
    let* fields = Decode.fields json [ "kind"; "name" ] in
    let* kind = Decode.get fields "kind" Decode.text in
    if String.equal kind "setting"
    then
      let* name = Decode.get fields "name" Name.of_json in
      Ok (Setting name)
    else
      let* _ = Decode.fields json [ "kind" ] in
      Decode.tag
        (string kind)
        [ "text_input", Text_input
        ; "image_input", Image_input
        ; "document_input", Document_input
        ; "function_tools", Function_tools
        ; "custom_tools", Custom_tools
        ; "opaque_replay", Opaque_replay
        ]
  ;;

  let support_to_json = function
    | Supported -> string "supported"
    | Unsupported -> string "unsupported"
    | Unknown -> string "unknown"
  ;;

  let support_of_json json =
    Decode.tag
      json
      [ "supported", Supported; "unsupported", Unsupported; "unknown", Unknown ]
  ;;

  let transport_to_json = function
    | Http_sse -> string "http_sse"
    | Websocket -> string "websocket"
    | In_process -> string "in_process"
    | Unknown_transport -> string "unknown"
  ;;

  let transport_of_json json =
    Decode.tag
      json
      [ "http_sse", Http_sse
      ; "websocket", Websocket
      ; "in_process", In_process
      ; "unknown", Unknown_transport
      ]
  ;;

  let to_json t =
    obj
      [ "adapter", string t.adapter
      ; "profile", string t.profile
      ; "profile_revision", nullable string t.profile_revision
      ; "account", nullable string t.account
      ; "model", string t.model
      ; "preparation_id", string t.preparation_id
      ; "transport", transport_to_json t.transport
      ; ( "settings"
        , list
            (fun setting ->
               obj
                 [ "name", string (Name.to_string setting.name)
                 ; "selection", selection_to_json setting.selection
                 ; "provenance", nullable provenance_to_json setting.provenance
                 ])
            t.settings )
      ; "withheld_settings", integer t.withheld_settings
      ; ( "capabilities"
        , list
            (fun (feature, support) ->
               obj
                 [ "feature", feature_to_json feature
                 ; "support", support_to_json support
                 ])
            t.capabilities )
      ]
  ;;

  let admit t ~limits =
    let* () =
      Result.all_unit
        (List.map
           [ "adapter", t.adapter
           ; "profile", t.profile
           ; "model", t.model
           ; "preparation_id", t.preparation_id
           ]
           ~f:(fun (field, value) -> label field value))
    in
    let* () =
      Result.all_unit
        (List.filter_map
           [ "profile_revision", t.profile_revision; "account", t.account ]
           ~f:(fun (field, value) -> Option.map value ~f:(label field)))
    in
    if t.withheld_settings < 0
    then invalid "withheld_settings" "required nonnegative count"
    else if
      List.length t.capabilities > 64
      || List.contains_dup (List.map t.capabilities ~f:fst) ~compare:compare_feature
    then invalid "capabilities" "duplicate or more than 64 declarations"
    else if
      not
        (List.equal
           Name.equal
           (List.map t.settings ~f:(fun setting -> setting.name))
           Name.all)
    then invalid "settings" "required complete ordered unique safe settings"
    else
      let* () =
        Result.all_unit
          (List.map t.settings ~f:(fun setting ->
             match setting.selection, setting.provenance with
             | (Explicit_null | Value _ | Withheld), None ->
               invalid "provenance" "selected setting requires actual provenance"
             | Value value, Some _ ->
               let* decoded = value_of_json setting.name (value_to_json value) in
               if equal_value value decoded
               then Ok ()
               else invalid "selection" "value does not match setting name"
             | Omitted, (None | Some _) | (Explicit_null | Withheld), Some _ -> Ok ()))
      in
      let* () = json_error (D.Json.validate ~limits (to_json t)) in
      Ok
        { t with
          capabilities =
            List.sort t.capabilities ~compare:(fun (a, _) (b, _) -> compare_feature a b)
        }
  ;;

  let source name =
    match name with
    | Name.Reasoning_effort -> "reasoning", Some "effort"
    | Reasoning_summary -> "reasoning", Some "summary"
    | Text_verbosity -> "text", Some "verbosity"
    | Text_format -> "text", Some "format"
    | name -> Name.to_string name, None
  ;;

  let of_target target ~preparation_id ~transport ~capabilities ~limits =
    if List.length capabilities > 64
    then invalid "capabilities" "at most 64 declarations"
    else (
      let raw_settings = Request.Target.settings target in
      let known_names = List.map Name.all ~f:(fun name -> fst (source name)) in
      let settings =
        List.map Name.all ~f:(fun name ->
          let root, member = source name in
          match
            List.find raw_settings ~f:(fun setting ->
              String.equal (Request.Setting.name setting) root)
          with
          | None -> { name; selection = Omitted; provenance = None }
          | Some setting ->
            let provenance = Some (Request.Setting.provenance setting) in
            let presence =
              match Request.Setting.value setting, member with
              | History_entry.Payload.Presence.Value (`Object _ as json), Some member ->
                (match D.Json.field json ~name:member with
                 | Absent -> History_entry.Payload.Presence.Absent
                 | Null -> Null
                 | Value value -> Value value)
              | value, None -> value
              | History_entry.Payload.Presence.Absent, Some _ -> Absent
              | Null, Some _ -> Null
              | Value _, Some _ -> Value (`Object [])
            in
            let selection =
              match presence with
              | Absent -> Omitted
              | Null -> Explicit_null
              | Value value ->
                let value =
                  match name, value with
                  | Name.Max_output_tokens, `Number number ->
                    (match Int64.of_string_opt number with
                     | Some number -> decimal number
                     | None -> value)
                  | Text_format, `Object _ ->
                    (match D.Json.field value ~name:"type" with
                     | Value value -> value
                     | Absent | Null -> `Object [])
                  | _ -> value
                in
                (match value_of_json name value with
                 | Ok value -> Value value
                 | Error _ -> Withheld)
            in
            { name; selection; provenance })
      in
      admit
        { adapter = Request.Target.adapter target
        ; profile = Request.Target.profile target
        ; profile_revision = Request.Target.profile_revision target
        ; account = Request.Target.account target
        ; model = Request.Target.model target
        ; preparation_id
        ; transport
        ; settings
        ; withheld_settings =
            List.count raw_settings ~f:(fun setting ->
              not
                (List.mem known_names (Request.Setting.name setting) ~equal:String.equal))
        ; capabilities
        }
        ~limits)
  ;;

  let decode json ~limits =
    let* fields =
      Decode.fields
        json
        [ "adapter"
        ; "profile"
        ; "profile_revision"
        ; "account"
        ; "model"
        ; "preparation_id"
        ; "transport"
        ; "settings"
        ; "withheld_settings"
        ; "capabilities"
        ]
    in
    let* adapter = Decode.get fields "adapter" Decode.text in
    let* profile = Decode.get fields "profile" Decode.text in
    let* profile_revision =
      Decode.get fields "profile_revision" (Decode.optional Decode.text)
    in
    let* account = Decode.get fields "account" (Decode.optional Decode.text) in
    let* model = Decode.get fields "model" Decode.text in
    let* preparation_id = Decode.get fields "preparation_id" Decode.text in
    let* transport = Decode.get fields "transport" transport_of_json in
    let* raw_settings = Decode.get fields "settings" Decode.array in
    let* () =
      if List.length raw_settings <> List.length Name.all
      then invalid "settings" "required complete safe settings"
      else Ok ()
    in
    let* settings =
      Result.all
        (List.map raw_settings ~f:(fun json ->
           let* fields = Decode.fields json [ "name"; "selection"; "provenance" ] in
           let* name = Decode.get fields "name" Name.of_json in
           let* selection = Decode.get fields "selection" (selection_of_json name) in
           let* provenance =
             Decode.get fields "provenance" (Decode.optional provenance_of_json)
           in
           Ok { name; selection; provenance }))
    in
    let* withheld_settings = Decode.get fields "withheld_settings" Decode.int in
    let* raw_capabilities = Decode.get fields "capabilities" Decode.array in
    let* () =
      if List.length raw_capabilities > 64
      then invalid "capabilities" "at most 64 declarations"
      else Ok ()
    in
    let* capabilities =
      Result.all
        (List.map raw_capabilities ~f:(fun json ->
           let* fields = Decode.fields json [ "feature"; "support" ] in
           let* feature = Decode.get fields "feature" feature_of_json in
           let* support = Decode.get fields "support" support_of_json in
           Ok (feature, support)))
    in
    admit
      { adapter
      ; profile
      ; profile_revision
      ; account
      ; model
      ; preparation_id
      ; transport
      ; settings
      ; withheld_settings
      ; capabilities
      }
      ~limits
  ;;

  let of_json json ~limits =
    let* () = json_error (D.Json.validate ~limits json) in
    decode json ~limits
  ;;
end

module Diagnostic = struct
  type phase =
    | Preparation
    | Authentication
    | Dispatch
    | Stream
    | Observation
  [@@deriving equal, sexp_of]

  type limit_kind =
    | Request_bytes
    | Response_body_bytes
    | Transfer_framing_bytes
    | Sse_frame_bytes
    | Json_depth
    | Json_nodes
    | Json_fields
    | Observation_bytes
    | Diagnostic_entries
    | Diagnostic_bytes
  [@@deriving equal, sexp_of]

  type reason =
    | Authentication of Event.Terminal.auth_failure
    | Connection
    | Timeout
    | Http_status of int
    | Malformed_protocol
    | Unsupported_input
    | Provider_failure
    | Local_result_invalid
    | Limit of limit_kind
    | Conflicting_observation
  [@@deriving equal, sexp_of]

  type t =
    { phase : phase
    ; reason : reason
    ; delivery : Event.Terminal.delivery option
    ; elapsed_ms : int64 option
    }
  [@@deriving equal]

  let create ~phase ~reason ~(delivery : Event.Terminal.delivery option) ~elapsed_ms =
    let* () =
      match reason with
      | Http_status status when status < 100 || status > 599 ->
        invalid "http_status" "outside 100..599"
      | _ -> Ok ()
    in
    let* () =
      match elapsed_ms with
      | Some value when Int64.(value < zero) -> invalid "elapsed_ms" "negative duration"
      | Some _ | None -> Ok ()
    in
    let* () =
      match reason, delivery with
      | Authentication _, Some (Possibly_submitted | Response_started) ->
        invalid "delivery" "authentication failed before submission"
      | _ -> Ok ()
    in
    Ok { phase; reason; delivery; elapsed_ms }
  ;;

  let phase t = t.phase
  let reason t = t.reason
  let delivery t = t.delivery
  let elapsed_ms t = t.elapsed_ms

  let message t =
    match t.reason with
    | Authentication Missing -> "Inference authentication is unavailable."
    | Authentication Denied -> "Inference authentication was denied."
    | Authentication Invalid_credential -> "Inference credential was rejected."
    | Authentication Timed_out -> "Inference authentication timed out."
    | Connection -> "Inference connection failed."
    | Timeout -> "Inference deadline elapsed."
    | Http_status _ -> "Inference HTTP request failed."
    | Malformed_protocol -> "Inference response was malformed."
    | Unsupported_input -> "Inference input is unsupported."
    | Provider_failure -> "Inference provider reported a failure."
    | Local_result_invalid -> "Inference completed but the local result was invalid."
    | Limit _ -> "Inference exceeded a declared limit."
    | Conflicting_observation -> "Inference observation revision conflicted."
  ;;

  let phase_to_json = function
    | Preparation -> string "preparation"
    | Authentication -> string "authentication"
    | Dispatch -> string "dispatch"
    | Stream -> string "stream"
    | Observation -> string "observation"
  ;;

  let phase_of_json json =
    Decode.tag
      json
      [ "preparation", Preparation
      ; "authentication", Authentication
      ; "dispatch", Dispatch
      ; "stream", Stream
      ; "observation", Observation
      ]
  ;;

  let delivery_to_json = function
    | Event.Terminal.Definitely_not_submitted -> string "definitely_not_submitted"
    | Possibly_submitted -> string "possibly_submitted"
    | Response_started -> string "response_started"
  ;;

  let delivery_of_json json =
    Decode.tag
      json
      [ "definitely_not_submitted", Event.Terminal.Definitely_not_submitted
      ; "possibly_submitted", Possibly_submitted
      ; "response_started", Response_started
      ]
  ;;

  let limit_names =
    [ "request_bytes", Request_bytes
    ; "response_body_bytes", Response_body_bytes
    ; "transfer_framing_bytes", Transfer_framing_bytes
    ; "sse_frame_bytes", Sse_frame_bytes
    ; "json_depth", Json_depth
    ; "json_nodes", Json_nodes
    ; "json_fields", Json_fields
    ; "observation_bytes", Observation_bytes
    ; "diagnostic_entries", Diagnostic_entries
    ; "diagnostic_bytes", Diagnostic_bytes
    ]
  ;;

  let reason_to_json = function
    | Authentication reason ->
      obj
        [ "kind", string "authentication"
        ; ( "reason"
          , string
              (match reason with
               | Missing -> "missing"
               | Denied -> "denied"
               | Invalid_credential -> "invalid_credential"
               | Timed_out -> "timed_out") )
        ]
    | Http_status status -> obj [ "kind", string "http_status"; "status", integer status ]
    | Limit limit ->
      obj
        [ "kind", string "limit"
        ; ( "limit"
          , string
              (fst
                 (List.find_exn limit_names ~f:(fun (_, value) ->
                    equal_limit_kind limit value))) )
        ]
    | Connection -> obj [ "kind", string "connection" ]
    | Timeout -> obj [ "kind", string "timeout" ]
    | Malformed_protocol -> obj [ "kind", string "malformed_protocol" ]
    | Unsupported_input -> obj [ "kind", string "unsupported_input" ]
    | Provider_failure -> obj [ "kind", string "provider_failure" ]
    | Local_result_invalid -> obj [ "kind", string "local_result_invalid" ]
    | Conflicting_observation -> obj [ "kind", string "conflicting_observation" ]
  ;;

  let reason_of_json json =
    let* fields = Decode.fields json [ "kind"; "reason"; "status"; "limit" ] in
    let* kind = Decode.get fields "kind" Decode.text in
    match kind with
    | "authentication" ->
      let* fields = Decode.fields json [ "kind"; "reason" ] in
      let* reason =
        Decode.get fields "reason" (fun json ->
          Decode.tag
            json
            [ "missing", Event.Terminal.Missing
            ; "denied", Denied
            ; "invalid_credential", Invalid_credential
            ; "timed_out", Timed_out
            ])
      in
      Ok (Authentication reason)
    | "http_status" ->
      let* fields = Decode.fields json [ "kind"; "status" ] in
      let* status = Decode.get fields "status" Decode.int in
      Ok (Http_status status)
    | "limit" ->
      let* fields = Decode.fields json [ "kind"; "limit" ] in
      let* limit = Decode.get fields "limit" (fun json -> Decode.tag json limit_names) in
      Ok (Limit limit)
    | _ ->
      let* _ = Decode.fields json [ "kind" ] in
      Decode.tag
        (string kind)
        [ "connection", Connection
        ; "timeout", Timeout
        ; "malformed_protocol", Malformed_protocol
        ; "unsupported_input", Unsupported_input
        ; "provider_failure", Provider_failure
        ; "local_result_invalid", Local_result_invalid
        ; "conflicting_observation", Conflicting_observation
        ]
  ;;

  let to_json t =
    obj
      [ "phase", phase_to_json t.phase
      ; "reason", reason_to_json t.reason
      ; "delivery", nullable delivery_to_json t.delivery
      ; "elapsed_ms", nullable decimal t.elapsed_ms
      ]
  ;;

  let of_json json =
    let* fields = Decode.fields json [ "phase"; "reason"; "delivery"; "elapsed_ms" ] in
    let* phase = Decode.get fields "phase" phase_of_json in
    let* reason = Decode.get fields "reason" reason_of_json in
    let* delivery = Decode.get fields "delivery" (Decode.optional delivery_of_json) in
    let* elapsed_ms = Decode.get fields "elapsed_ms" (Decode.optional Decode.int64) in
    create ~phase ~reason ~delivery ~elapsed_ms
  ;;
end

module Key = struct
  module T = struct
    type t =
      { scope : Transcript.Scope.Key.t
      ; observation : Observation_id.t
      }
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type payload =
  | Usage of Usage.t
  | Context_estimate of Context_estimate.t
  | Configuration of Configuration.t
  | Diagnostic of Diagnostic.t

type t =
  { scope : Transcript.Scope.t
  ; id : Observation_id.t
  ; revision : int64
  ; payload : payload
  ; encoded_bytes : int
  }

let scope t = t.scope
let id t = t.id
let key t = { Key.scope = Transcript.Scope.key t.scope; observation = t.id }
let revision t = t.revision
let payload t = t.payload
let encoded_bytes t = t.encoded_bytes

let equal_payload a b =
  match a, b with
  | Usage a, Usage b -> Usage.equal a b
  | Context_estimate a, Context_estimate b -> Context_estimate.equal a b
  | Configuration a, Configuration b -> Configuration.equal a b
  | Diagnostic a, Diagnostic b -> Diagnostic.equal a b
  | (Usage _ | Context_estimate _ | Configuration _ | Diagnostic _), _ -> false
;;

let equal a b =
  Transcript.Scope.equal a.scope b.scope
  && Observation_id.equal a.id b.id
  && Int64.equal a.revision b.revision
  && equal_payload a.payload b.payload
;;

let payload_json = function
  | Usage value -> "usage", Usage.to_json value
  | Context_estimate value -> "context_estimate", Context_estimate.to_json value
  | Configuration value -> "configuration", Configuration.to_json value
  | Diagnostic value -> "diagnostic", Diagnostic.to_json value
;;

let to_json t =
  let kind, payload = payload_json t.payload in
  obj
    [ "schema_version", integer 1
    ; "scope", Transcript.Scope.to_json t.scope
    ; "id", string (Observation_id.to_string t.id)
    ; "revision", decimal t.revision
    ; "kind", string kind
    ; "payload", payload
    ]
;;

let create ~scope ~id ~revision ~payload ~limits =
  if Int64.(revision < zero)
  then invalid "revision" "negative revision"
  else (
    let candidate = { scope; id; revision; payload; encoded_bytes = 0 } in
    let* encoded_bytes =
      json_error (D.Json.validate_and_measure ~limits (to_json candidate))
    in
    Ok { candidate with encoded_bytes })
;;

let validate t ~limits = json_error (D.Json.validate ~limits (to_json t))

let of_json json ~limits =
  let* () = json_error (D.Json.validate ~limits json) in
  let* fields =
    Decode.fields json [ "schema_version"; "scope"; "id"; "revision"; "kind"; "payload" ]
  in
  let* version = Decode.get fields "schema_version" Decode.int in
  if version <> 1
  then invalid "schema_version" "unsupported observation version"
  else
    let* scope =
      Decode.get fields "scope" (fun json ->
        external_error "scope" (Transcript.Scope.of_json json ~limits))
    in
    let* id =
      Decode.get fields "id" (fun json ->
        let* id = Decode.text json in
        Observation_id.of_string id)
    in
    let* revision = Decode.get fields "revision" Decode.int64 in
    let* kind = Decode.get fields "kind" Decode.text in
    let* raw = Decode.field fields "payload" in
    let* payload =
      match kind with
      | "usage" -> Result.map (Usage.of_json raw) ~f:(fun value -> Usage value)
      | "context_estimate" ->
        Result.map (Context_estimate.of_json raw) ~f:(fun value -> Context_estimate value)
      | "configuration" ->
        Result.map (Configuration.decode raw ~limits) ~f:(fun value ->
          Configuration value)
      | "diagnostic" ->
        Result.map (Diagnostic.of_json raw) ~f:(fun value -> Diagnostic value)
      | _ -> invalid "kind" "unknown observation kind"
    in
    create ~scope ~id ~revision ~payload ~limits
;;

module Latest = struct
  type observation = t

  type scope_state =
    { scope : Transcript.Scope.t
    ; configuration : Configuration.t option
    }

  type t =
    { values : observation Map.M(Key).t
    ; scopes : scope_state Map.M(Transcript.Scope.Key).t
    ; max_observations : int
    ; max_retained_bytes : int
    ; retained_bytes : int
    }

  type disposition =
    | Added
    | Replaced
    | Duplicate
    | Stale
  [@@deriving equal, sexp_of]

  let create ~max_observations ~max_retained_bytes =
    if max_observations <= 0 || max_retained_bytes <= 0
    then invalid "latest.limits" "required positive bounds"
    else
      Ok
        { values = Map.empty (module Key)
        ; scopes = Map.empty (module Transcript.Scope.Key)
        ; max_observations
        ; max_retained_bytes
        ; retained_bytes = 0
        }
  ;;

  let find t key = Map.find t.values key
  let observations t = Map.data t.values
  let retained_bytes t = t.retained_bytes

  let compatible (previous : observation) (incoming : observation) =
    if not (Transcript.Scope.equal previous.scope incoming.scope)
    then Error Error.Conflicting_scope
    else (
      match previous.payload, incoming.payload with
      | Usage _, Usage _ | Diagnostic _, Diagnostic _ -> Ok ()
      | Context_estimate a, Context_estimate b ->
        if
          String.equal
            (Context_estimate.preparation_id a)
            (Context_estimate.preparation_id b)
        then Ok ()
        else Error Error.Conflicting_kind
      | Configuration a, Configuration b ->
        if Configuration.equal a b then Ok () else Error Error.Conflicting_revision
      | (Usage _ | Context_estimate _ | Configuration _ | Diagnostic _), _ ->
        Error Error.Conflicting_kind)
  ;;

  let observe t (incoming : observation) =
    let k = key incoming in
    let previous_scope = Map.find t.scopes k.scope in
    let* () =
      match previous_scope with
      | None -> Ok ()
      | Some previous ->
        if not (Transcript.Scope.equal previous.scope incoming.scope)
        then Error Error.Conflicting_scope
        else (
          match previous.configuration, incoming.payload with
          | Some a, Configuration b when not (Configuration.equal a b) ->
            Error Error.Conflicting_revision
          | ( (Some _ | None)
            , (Usage _ | Context_estimate _ | Configuration _ | Diagnostic _) ) -> Ok ())
    in
    let previous = Map.find t.values k in
    let* disposition =
      match previous with
      | None -> Ok Added
      | Some previous ->
        let* () = compatible previous incoming in
        let order = Int64.compare incoming.revision previous.revision in
        if order < 0
        then Ok Stale
        else if order > 0
        then Ok Replaced
        else if equal previous incoming
        then Ok Duplicate
        else Error Error.Conflicting_revision
    in
    match disposition with
    | Duplicate | Stale -> Ok (t, disposition)
    | Added | Replaced ->
      let base =
        t.retained_bytes - Option.value_map previous ~default:0 ~f:encoded_bytes
      in
      if
        (Option.is_none previous && Map.length t.values >= t.max_observations)
        || encoded_bytes incoming > t.max_retained_bytes - base
      then Error Error.Retention_limit
      else (
        let configuration =
          match incoming.payload with
          | Configuration configuration -> Some configuration
          | Usage _ | Context_estimate _ | Diagnostic _ ->
            Option.bind previous_scope ~f:(fun previous -> previous.configuration)
        in
        Ok
          ( { t with
              values = Map.set t.values ~key:k ~data:incoming
            ; scopes =
                Map.set
                  t.scopes
                  ~key:k.scope
                  ~data:{ scope = incoming.scope; configuration }
            ; retained_bytes = base + encoded_bytes incoming
            }
          , disposition ))
  ;;
end

module Attempt_record = struct
  type observation = t

  type interruption =
    | Cancelled
    | Host_interrupted
  [@@deriving equal, sexp_of]

  type state =
    | Prepared
    | Running
    | Terminal of Event.Terminal.t
    | Interrupted of
        { reason : interruption
        ; delivery : Event.Terminal.delivery
        }

  type t =
    { scope : Transcript.Scope.t
    ; accounting_id : Observation_id.t
    ; configuration : Configuration.t
    ; state : state
    ; observations : observation list
    ; omitted_diagnostics : int64
    ; encoded_bytes : int
    ; admitted_limits : D.Limits.t
    }

  let scope t = t.scope
  let accounting_id t = t.accounting_id
  let configuration t = t.configuration
  let state t = t.state
  let observations t = t.observations
  let omitted_diagnostics t = t.omitted_diagnostics
  let encoded_bytes t = t.encoded_bytes

  let state_to_json = function
    | Prepared -> obj [ "kind", string "prepared" ]
    | Running -> obj [ "kind", string "running" ]
    | Terminal terminal ->
      obj [ "kind", string "terminal"; "terminal", Event.Terminal.to_json terminal ]
    | Interrupted { reason; delivery } ->
      obj
        [ "kind", string "interrupted"
        ; ( "reason"
          , string
              (match reason with
               | Cancelled -> "cancelled"
               | Host_interrupted -> "host_interrupted") )
        ; "delivery", Diagnostic.delivery_to_json delivery
        ]
  ;;

  let observation_to_json = to_json
  let observation_of_json = of_json

  let to_json t =
    obj
      [ "schema_version", integer 1
      ; "scope", Transcript.Scope.to_json t.scope
      ; "accounting_id", string (Observation_id.to_string t.accounting_id)
      ; "configuration", Configuration.to_json t.configuration
      ; "state", state_to_json t.state
      ; "observations", list observation_to_json t.observations
      ; "omitted_diagnostics", decimal t.omitted_diagnostics
      ]
  ;;

  let create
        ~scope
        ~accounting_id
        ~configuration
        ~state
        ~observations
        ~omitted_diagnostics
        ~limits
    =
    let* () =
      match state with
      | Terminal terminal
        when not (Transcript.Scope.equal scope (Event.Terminal.scope terminal)) ->
        Error Error.Conflicting_scope
      | Prepared | Running | Terminal _ | Interrupted _ -> Ok ()
    in
    if Int64.(omitted_diagnostics < zero)
    then invalid "omitted_diagnostics" "negative omitted count"
    else if List.length observations > 64
    then invalid "observations" "at most 64 observations"
    else if List.contains_dup (List.map observations ~f:key) ~compare:Key.compare
    then invalid "observations" "duplicate observation identity"
    else
      let* () =
        Result.all_unit
          (List.map observations ~f:(fun observation ->
             if not (Transcript.Scope.equal scope observation.scope)
             then Error Error.Conflicting_scope
             else (
               match observation.payload with
               | Usage _ ->
                 if Observation_id.equal accounting_id observation.id
                 then Ok ()
                 else
                   invalid
                     "accounting_id"
                     "usage does not match designated accounting identity"
               | Configuration incoming ->
                 if Configuration.equal configuration incoming
                 then Ok ()
                 else Error Error.Conflicting_revision
               | Context_estimate incoming ->
                 if
                   String.equal
                     (Configuration.preparation_id configuration)
                     (Context_estimate.preparation_id incoming)
                 then Ok ()
                 else invalid "preparation_id" "context does not match actual preparation"
               | Diagnostic _ -> Ok ())))
      in
      if
        List.count observations ~f:(fun observation ->
          match observation.payload with
          | Usage _ -> true
          | _ -> false)
        > 1
      then invalid "usage" "at most one designated usage observation"
      else (
        let diagnostics =
          List.filter observations ~f:(fun observation ->
            match observation.payload with
            | Diagnostic _ -> true
            | _ -> false)
        in
        if List.length diagnostics > 16
        then invalid "diagnostics" "at most sixteen diagnostic observations"
        else
          let* _ =
            List.fold diagnostics ~init:(Ok 0) ~f:(fun acc observation ->
              let* bytes = acc in
              if observation.encoded_bytes > (16 * 1024) - bytes
              then invalid "diagnostics" "diagnostic bytes exceed 16 KiB"
              else Ok (bytes + observation.encoded_bytes))
          in
          let candidate =
            { scope
            ; accounting_id
            ; configuration
            ; state
            ; observations
            ; omitted_diagnostics
            ; encoded_bytes = 0
            ; admitted_limits = limits
            }
          in
          let* encoded_bytes =
            json_error (D.Json.validate_and_measure ~limits (to_json candidate))
          in
          Ok { candidate with encoded_bytes })
  ;;

  (* Both constructors publish only after the complete final row is admitted.
     A different profile must inspect every structural bound, not only bytes. *)
  let validate t ~limits =
    if D.Limits.equal t.admitted_limits limits
    then Ok ()
    else json_error (D.Json.validate ~limits (to_json t))
  ;;

  let state_of_json json ~limits =
    let* fields = Decode.fields json [ "kind"; "terminal"; "reason"; "delivery" ] in
    let* kind = Decode.get fields "kind" Decode.text in
    match kind with
    | "prepared" | "running" ->
      let* _ = Decode.fields json [ "kind" ] in
      Ok (if String.equal kind "prepared" then Prepared else Running)
    | "terminal" ->
      let* fields = Decode.fields json [ "kind"; "terminal" ] in
      let* terminal =
        Decode.get fields "terminal" (fun json ->
          external_error "terminal" (Event.Terminal.of_json json ~limits))
      in
      Ok (Terminal terminal)
    | "interrupted" ->
      let* fields = Decode.fields json [ "kind"; "reason"; "delivery" ] in
      let* reason =
        Decode.get fields "reason" (fun json ->
          Decode.tag json [ "cancelled", Cancelled; "host_interrupted", Host_interrupted ])
      in
      let* delivery = Decode.get fields "delivery" Diagnostic.delivery_of_json in
      Ok (Interrupted { reason; delivery })
    | _ -> invalid "state.kind" "unknown attempt state"
  ;;

  let of_json json ~limits =
    let* () = json_error (D.Json.validate ~limits json) in
    let* fields =
      Decode.fields
        json
        [ "schema_version"
        ; "scope"
        ; "accounting_id"
        ; "configuration"
        ; "state"
        ; "observations"
        ; "omitted_diagnostics"
        ]
    in
    let* version = Decode.get fields "schema_version" Decode.int in
    if version <> 1
    then invalid "schema_version" "unsupported attempt version"
    else
      let* scope =
        Decode.get fields "scope" (fun json ->
          external_error "scope" (Transcript.Scope.of_json json ~limits))
      in
      let* accounting_id =
        Decode.get fields "accounting_id" (fun json ->
          let* id = Decode.text json in
          Observation_id.of_string id)
      in
      let* configuration =
        Decode.get fields "configuration" (fun json -> Configuration.decode json ~limits)
      in
      let* state = Decode.get fields "state" (fun json -> state_of_json json ~limits) in
      let* raw_observations = Decode.get fields "observations" Decode.array in
      if List.length raw_observations > 64
      then invalid "observations" "at most 64 observations"
      else
        let* observations =
          Result.all
            (List.map raw_observations ~f:(fun json -> observation_of_json json ~limits))
        in
        let* omitted_diagnostics = Decode.get fields "omitted_diagnostics" Decode.int64 in
        create
          ~scope
          ~accounting_id
          ~configuration
          ~state
          ~observations
          ~omitted_diagnostics
          ~limits
  ;;
end
