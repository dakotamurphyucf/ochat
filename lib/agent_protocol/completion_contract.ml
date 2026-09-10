open Core

module Jsonaf = struct
  include Jsonaf

  let equal = exactly_equal
end

type t =
  { tool_name : string
  ; tool_fingerprint : string
  ; capability_pins : (string * string) list
  ; completion_schema : Jsonaf.t option
  ; max_output_bytes : int
  ; max_output_depth : int
  }
[@@deriving equal, sexp]

let invalid message = Error (Protocol_error.invalid_request message)
let valid_text value max = (not (String.is_empty value)) && String.length value <= max

let valid_pin value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    match
      valid_text t.tool_name 256
      && valid_pin t.tool_fingerprint
      && t.max_output_bytes > 0
      && t.max_output_bytes <= 1_048_576
      && t.max_output_depth > 0
      && t.max_output_depth <= 128
      && List.length t.capability_pins <= 4096
      && List.for_all t.capability_pins ~f:(fun (name, pin) ->
        valid_text name 256 && valid_pin pin)
      && Option.is_none
           (List.find_a_dup (List.map t.capability_pins ~f:fst) ~compare:String.compare)
    with
    | true -> Ok ()
    | false -> invalid "invalid standalone completion contract"
  in
  match t.completion_schema with
  | None -> Ok ()
  | Some schema -> Json_codec.validate_limits ~max_depth:128 ~max_bytes:1_048_576 schema
;;

let to_json t =
  `Object
    ([ "version", `Number "1"
     ; "tool_name", `String t.tool_name
     ; "tool_fingerprint", `String t.tool_fingerprint
     ; ( "capability_pins"
       , `Array
           (List.map t.capability_pins ~f:(fun (name, fingerprint) ->
              `Object [ "name", `String name; "fingerprint", `String fingerprint ])) )
     ; "max_output_bytes", `Number (Int.to_string t.max_output_bytes)
     ; "max_output_depth", `Number (Int.to_string t.max_output_depth)
     ]
     @ Option.to_list
         (Option.map t.completion_schema ~f:(fun schema -> "completion_schema", schema)))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Json_codec.validate_limits ~max_depth:132 ~max_bytes:(18 * 1024 * 1024) json
  in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "version"
      ; "tool_name"
      ; "tool_fingerprint"
      ; "capability_pins"
      ; "completion_schema"
      ; "max_output_bytes"
      ; "max_output_depth"
      ]
  in
  let get name decode = Json_codec.required_as fields name decode in
  let%bind _ = get "version" (Json_codec.bounded_int ~min:1 ~max:1) in
  let%bind tool_name = get "tool_name" Json_codec.string in
  let%bind tool_fingerprint = get "tool_fingerprint" Json_codec.string in
  let%bind capability_pins =
    get
      "capability_pins"
      (Json_codec.list (fun json ->
         let%bind fields = Json_codec.fields json in
         let%bind () = Extension_codec.closed fields [ "name"; "fingerprint" ] in
         let%bind name = Json_codec.required_as fields "name" Json_codec.string in
         let%map fingerprint =
           Json_codec.required_as fields "fingerprint" Json_codec.string
         in
         name, fingerprint))
  in
  let%bind completion_schema =
    Json_codec.optional_as fields "completion_schema" (fun json -> Ok json)
  in
  let%bind max_output_bytes =
    get "max_output_bytes" (Json_codec.bounded_int ~min:1 ~max:1_048_576)
  in
  let%bind max_output_depth =
    get "max_output_depth" (Json_codec.bounded_int ~min:1 ~max:128)
  in
  let t =
    { tool_name
    ; tool_fingerprint
    ; capability_pins
    ; completion_schema
    ; max_output_bytes
    ; max_output_depth
    }
  in
  let%map () = validate t in
  t
;;
