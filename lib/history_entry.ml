open! Core
module Jsonaf = Jsonaf_ext

module Id = struct
  type t =
    { namespace : string
    ; sequence : int
    }
  [@@deriving compare, hash]

  let validate_namespace namespace =
    if String.is_empty namespace
    then Error "history ID namespace must be nonempty"
    else Ok ()
  ;;

  let create ~namespace ~sequence =
    let open Result.Let_syntax in
    let%map () = validate_namespace namespace
    and () =
      if sequence < 0 then Error "history ID sequence must be nonnegative" else Ok ()
    in
    { namespace; sequence }
  ;;

  let namespace t = t.namespace
  let sequence t = t.sequence
  let equal a b = compare a b = 0
  let to_string t = sprintf "%d:%s:%d" (String.length t.namespace) t.namespace t.sequence

  let parse_int value =
    match Option.try_with (fun () -> Int.of_string value) with
    | Some value -> Ok value
    | None -> Error "history ID contains an invalid integer"
  ;;

  let of_string encoded =
    let open Result.Let_syntax in
    match String.lsplit2 encoded ~on:':' with
    | None -> Error "history ID is missing its namespace length"
    | Some (length_text, rest) ->
      let%bind namespace_length = parse_int length_text in
      if namespace_length < 0 || String.length rest <= namespace_length
      then Error "history ID has an invalid namespace length"
      else if not (Char.equal rest.[namespace_length] ':')
      then Error "history ID is missing its sequence separator"
      else (
        let namespace = String.sub rest ~pos:0 ~len:namespace_length in
        let sequence_text = String.drop_prefix rest (namespace_length + 1) in
        let%bind sequence = parse_int sequence_text in
        let%bind id = create ~namespace ~sequence in
        if String.equal encoded (to_string id)
        then Ok id
        else Error "history ID is not canonically encoded")
  ;;

  let invalid_encoded error = failwith ("invalid encoded history ID: " ^ error)

  let of_string_exn encoded =
    match of_string encoded with
    | Ok id -> id
    | Error error -> invalid_encoded error
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)

  let t_of_sexp = function
    | Sexp.Atom encoded -> of_string_exn encoded
    | sexp ->
      Sexplib.Conv.of_sexp_error
        "History_entry.Id.t must be a canonical encoded string"
        sexp
  ;;

  let jsonaf_of_t t = `String (to_string t)

  let t_of_jsonaf = function
    | `String encoded -> of_string_exn encoded
    | _ -> failwith "History_entry.Id.t must be a JSON string"
  ;;

  let bin_shape_t = Bin_prot.Shape.bin_shape_string
  let bin_size_t t = Bin_prot.Size.bin_size_string (to_string t)

  let bin_write_t buffer ~pos t =
    Bin_prot.Write.bin_write_string buffer ~pos (to_string t)
  ;;

  let bin_read_t buffer ~pos_ref =
    Bin_prot.Read.bin_read_string buffer ~pos_ref |> of_string_exn
  ;;

  let __bin_read_t__ buffer ~pos_ref _length = bin_read_t buffer ~pos_ref

  let bin_writer_t : t Bin_prot.Type_class.writer =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let bin_reader_t : t Bin_prot.Type_class.reader =
    { read = bin_read_t; vtag_read = __bin_read_t__ }
  ;;

  let bin_t : t Bin_prot.Type_class.t =
    { writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
  ;;
end

module Allocator = struct
  type t =
    { namespace : string
    ; next_sequence : int Atomic.t
    ; limit_exclusive : int option
    }

  let create_with_limit ~namespace ~next_sequence ~limit_exclusive =
    let open Result.Let_syntax in
    let%map (_ : Id.t) = Id.create ~namespace ~sequence:next_sequence in
    { namespace; next_sequence = Atomic.make next_sequence; limit_exclusive }
  ;;

  let create ~namespace ~next_sequence =
    create_with_limit ~namespace ~next_sequence ~limit_exclusive:None
  ;;

  let create_bounded ~namespace ~next_sequence ~limit_exclusive =
    if limit_exclusive < next_sequence
    then Error "history ID allocation limit precedes the next sequence"
    else
      create_with_limit ~namespace ~next_sequence ~limit_exclusive:(Some limit_exclusive)
  ;;

  let namespace t = t.namespace
  let next_sequence t = Atomic.get t.next_sequence

  let rec reserve t ~count =
    if count < 0
    then Error "history ID reservation count must be nonnegative"
    else (
      let next_sequence = Atomic.get t.next_sequence in
      if Option.exists t.limit_exclusive ~f:(fun limit -> count > limit - next_sequence)
      then Error "history ID reservation exceeds its committed allocation block"
      else if count > Int.max_value - next_sequence
      then Error "history ID sequence exhausted"
      else (
        let ids =
          List.init count ~f:(fun offset ->
            { Id.namespace = t.namespace; sequence = next_sequence + offset })
        in
        if Atomic.compare_and_set t.next_sequence next_sequence (next_sequence + count)
        then Ok ids
        else reserve t ~count))
  ;;

  let allocate t =
    Result.bind (reserve t ~count:1) ~f:(function
      | [ id ] -> Ok id
      | _ -> Error "history ID allocator violated its reservation invariant")
  ;;
end

module Payload = struct
  module Presence = struct
    type 'a t =
      | Absent
      | Null
      | Value of 'a
    [@@deriving equal, sexp, bin_io]
  end

  let fields = function
    | `Object fields -> Ok fields
    | _ -> Error "history payload must contain an object"
  ;;

  let string = function
    | `String value -> Ok value
    | _ -> Error "history payload field must be a string"
  ;;

  let boolean = function
    | `True -> Ok true
    | `False -> Ok false
    | _ -> Error "history payload field must be a Boolean"
  ;;

  let required fields name decode =
    match List.Assoc.find fields name ~equal:String.equal with
    | None -> Error ("history payload is missing " ^ name)
    | Some value -> Result.map_error (decode value) ~f:(fun error -> name ^ ": " ^ error)
  ;;

  let presence fields name decode =
    match List.Assoc.find fields name ~equal:String.equal with
    | None -> Ok Presence.Absent
    | Some `Null -> Ok Presence.Null
    | Some value -> Result.map (decode value) ~f:(fun value -> Presence.Value value)
  ;;

  let list decode = function
    | `Array values -> Result.all (List.map values ~f:decode)
    | _ -> Error "history payload field must be an array"
  ;;

  let json value = Ok value

  let presence_field name value encode =
    match value with
    | Presence.Absent -> []
    | Null -> [ name, `Null ]
    | Value value -> [ name, encode value ]
  ;;

  let json_string value = `String value
  let json_bool value = if value then `True else `False

  (* Preserve the existing root-at-zero depth policy and node bound. Byte
     admission belongs to the surrounding configured document owner. *)
  let json_limits =
    Document_schema.Limits.create
      ~max_bytes:Int.max_value
      ~max_depth:129
      ~max_fields:100_000
      ~max_nodes:100_000
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  ;;

  let validate_json value =
    Document_schema.Json.validate ~limits:json_limits value
    |> Result.map_error ~f:(fun error ->
      "history payload JSON: "
      ^ Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  ;;

  module Origin = struct
    type known =
      { adapter : string
      ; provider : string
      ; account : string option
      ; endpoint : string
      ; profile : string option
      ; model : string option
      ; replay_version : int
      }
    [@@deriving sexp, bin_io]

    type t =
      | Unavailable
      | Known of known
    [@@deriving sexp, bin_io]

    let unavailable = Unavailable

    let create ~adapter ~provider ~account ~endpoint ~profile ~model ~replay_version =
      if
        List.exists [ adapter; provider; endpoint ] ~f:String.is_empty
        || List.exists [ account; profile; model ] ~f:(Option.exists ~f:String.is_empty)
        || replay_version <= 0
      then Error "history origin has empty identity or invalid replay version"
      else
        Ok
          (Known { adapter; provider; account; endpoint; profile; model; replay_version })
    ;;

    let is_available = function
      | Unavailable -> false
      | Known _ -> true
    ;;

    let to_json = function
      | Unavailable -> `Object [ "type", `String "unavailable" ]
      | Known { adapter; provider; account; endpoint; profile; model; replay_version } ->
        let optional name = function
          | None -> []
          | Some value -> [ name, `String value ]
        in
        `Object
          ([ "type", `String "known"
           ; "adapter", `String adapter
           ; "provider", `String provider
           ; "endpoint", `String endpoint
           ; "account", Option.value_map account ~default:`Null ~f:json_string
           ; "replay_version", `String (Int.to_string replay_version)
           ]
           @ optional "profile" profile
           @ optional "model" model)
    ;;

    let of_json value =
      let open Result.Let_syntax in
      let%bind fields = fields value in
      let%bind kind = required fields "type" string in
      match kind with
      | "unavailable" -> Ok Unavailable
      | "known" ->
        let optional name =
          Result.map (presence fields name string) ~f:(function
            | Presence.Value value -> Some value
            | Absent | Null -> None)
        in
        let%bind adapter = required fields "adapter" string in
        let%bind provider = required fields "provider" string in
        let%bind endpoint = required fields "endpoint" string in
        let%bind account = optional "account" in
        let%bind profile = optional "profile" in
        let%bind model = optional "model" in
        let%bind encoded = required fields "replay_version" string in
        let%bind replay_version =
          match Int.of_string_opt encoded with
          | Some value when String.equal encoded (Int.to_string value) -> Ok value
          | _ -> Error "history origin replay version must be a canonical decimal string"
        in
        create ~adapter ~provider ~account ~endpoint ~profile ~model ~replay_version
      | _ -> Error "unknown history origin kind"
    ;;
  end

  module Role = struct
    type t =
      | System
      | Developer
      | User
      | Assistant
      | Tool
    [@@deriving equal, sexp, bin_io]

    let to_string = function
      | System -> "system"
      | Developer -> "developer"
      | User -> "user"
      | Assistant -> "assistant"
      | Tool -> "tool"
    ;;

    let of_json value =
      Result.bind (string value) ~f:(function
        | "system" -> Ok System
        | "developer" -> Ok Developer
        | "user" -> Ok User
        | "assistant" -> Ok Assistant
        | "tool" -> Ok Tool
        | _ -> Error "unknown history role")
    ;;
  end

  module Call_kind = struct
    type t =
      | Function
      | Custom
    [@@deriving equal, sexp, bin_io]

    let to_string = function
      | Function -> "function"
      | Custom -> "custom"
    ;;

    let of_json value =
      Result.bind (string value) ~f:(function
        | "function" -> Ok Function
        | "custom" -> Ok Custom
        | _ -> Error "unknown call kind")
    ;;
  end

  module Metadata = struct
    type t =
      { item_id : string Presence.t
      ; response_id : string Presence.t
      ; call_id : string Presence.t
      ; status : string Presence.t
      }
    [@@deriving equal, sexp, bin_io]

    let empty =
      { item_id = Absent; response_id = Absent; call_id = Absent; status = Absent }
    ;;

    let to_json { item_id; response_id; call_id; status } =
      `Object
        (presence_field "item_id" item_id json_string
         @ presence_field "response_id" response_id json_string
         @ presence_field "call_id" call_id json_string
         @ presence_field "status" status json_string)
    ;;

    let of_json value =
      let open Result.Let_syntax in
      let%bind fields = fields value in
      let%map item_id = presence fields "item_id" string
      and response_id = presence fields "response_id" string
      and call_id = presence fields "call_id" string
      and status = presence fields "status" string in
      { item_id; response_id; call_id; status }
    ;;
  end

  module Content = struct
    type t =
      | Text of
          { text : string
          ; annotations : Jsonaf.t list
          ; logprobs : Jsonaf.t Presence.t
          }
      | Refusal of string
      | Image of
          { uri : string
          ; detail : string Presence.t
          }
      | Unknown of
          { kind : string
          ; raw : Jsonaf.t
          }
    [@@deriving sexp, bin_io]

    let to_json = function
      | Text { text; annotations; logprobs } ->
        `Object
          ([ "type", `String "text"
           ; "text", `String text
           ; "annotations", `Array annotations
           ]
           @ presence_field "logprobs" logprobs Fn.id)
      | Refusal text -> `Object [ "type", `String "refusal"; "text", `String text ]
      | Image { uri; detail } ->
        `Object
          ([ "type", `String "image"; "uri", `String uri ]
           @ presence_field "detail" detail json_string)
      | Unknown { kind; raw } ->
        `Object [ "type", `String "unknown"; "kind", `String kind; "raw", raw ]
    ;;

    let of_json value =
      let open Result.Let_syntax in
      let%bind fields = fields value in
      let%bind kind = required fields "type" string in
      match kind with
      | "text" ->
        let%map text = required fields "text" string
        and annotations = required fields "annotations" (list json)
        and logprobs = presence fields "logprobs" json in
        Text { text; annotations; logprobs }
      | "refusal" ->
        Result.map (required fields "text" string) ~f:(fun text -> Refusal text)
      | "image" ->
        let%bind uri = required fields "uri" string in
        let%map detail = presence fields "detail" string in
        Image { uri; detail }
      | "unknown" ->
        let%bind kind = required fields "kind" string in
        let%map raw = required fields "raw" json in
        Unknown { kind; raw }
      | _ -> Error "unknown neutral content kind"
    ;;
  end

  module Output = struct
    type t =
      | Text of string
      | Content of Content.t list
    [@@deriving sexp, bin_io]

    let to_json = function
      | Text text -> `Object [ "type", `String "text"; "text", `String text ]
      | Content values ->
        `Object
          [ "type", `String "content"
          ; "content", `Array (List.map values ~f:Content.to_json)
          ]
    ;;

    let of_validated_json value =
      let open Result.Let_syntax in
      let%bind fields = fields value in
      let%bind kind = required fields "type" string in
      match kind with
      | "text" -> Result.map (required fields "text" string) ~f:(fun value -> Text value)
      | "content" ->
        Result.map
          (required fields "content" (list Content.of_json))
          ~f:(fun values -> Content values)
      | _ -> Error "unknown neutral output kind"
    ;;

    let of_json value ~limits =
      let open Result.Let_syntax in
      let%bind () =
        Document_schema.Json.validate ~limits value
        |> Result.map_error ~f:(fun error ->
          Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
      in
      of_validated_json value
    ;;
  end

  module Call_relation = struct
    type t =
      | Bound of Id.t
      | Unresolved
    [@@deriving equal, sexp, bin_io]

    let to_json = function
      | Bound id ->
        `Object [ "type", `String "bound"; "call_entry_id", Id.jsonaf_of_t id ]
      | Unresolved -> `Object [ "type", `String "unresolved" ]
    ;;

    let of_json value =
      let open Result.Let_syntax in
      let%bind fields = fields value in
      let%bind kind = required fields "type" string in
      match kind with
      | "unresolved" -> Ok Unresolved
      | "bound" ->
        Result.map
          (required fields "call_entry_id" (fun value ->
             Result.bind (string value) ~f:Id.of_string))
          ~f:(fun id -> Bound id)
      | _ -> Error "unknown call relation"
    ;;
  end

  module Semantic = struct
    type message_form =
      | Input
      | Output
    [@@deriving equal, sexp, bin_io]

    type view =
      | Message of
          { form : message_form
          ; role : Role.t
          ; content : Content.t list
          ; phase : string Presence.t
          }
      | Call of
          { kind : Call_kind.t
          ; name : string
          ; namespace : string Presence.t
          ; input_bytes : string
          ; async : bool Presence.t
          }
      | Result of
          { relation : Call_relation.t
          ; kind : Call_kind.t
          ; output : Output.t
          }
      | Reasoning of { readable_summary : string list }
      | Unknown of { provider_kind : string }
    [@@deriving sexp, bin_io]

    type t =
      { view : view
      ; metadata : Metadata.t
      }
    [@@deriving sexp, bin_io]

    let view t = t.view
    let metadata t = t.metadata

    let to_json { view; metadata } =
      let fields =
        match view with
        | Message { form; role; content; phase } ->
          [ "type", `String "message"
          ; ( "form"
            , `String
                (match form with
                 | Input -> "input"
                 | Output -> "output") )
          ; "role", `String (Role.to_string role)
          ; "content", `Array (List.map content ~f:Content.to_json)
          ]
          @ presence_field "phase" phase json_string
        | Call { kind; name; namespace; input_bytes; async } ->
          [ "type", `String "call"
          ; "kind", `String (Call_kind.to_string kind)
          ; "name", `String name
          ; "input_bytes", `String input_bytes
          ]
          @ presence_field "namespace" namespace json_string
          @ presence_field "async" async json_bool
        | Result { relation; kind; output } ->
          [ "type", `String "result"
          ; "kind", `String (Call_kind.to_string kind)
          ; "relation", Call_relation.to_json relation
          ; "output", Output.to_json output
          ]
        | Reasoning { readable_summary } ->
          [ "type", `String "reasoning"
          ; "readable_summary", `Array (List.map readable_summary ~f:json_string)
          ]
        | Unknown { provider_kind } ->
          [ "type", `String "unknown"; "provider_kind", `String provider_kind ]
      in
      `Object (fields @ [ "metadata", Metadata.to_json metadata ])
    ;;

    let validate_view view =
      match view with
      | Message { form = Output; role; _ } when not (Role.equal role Assistant) ->
        Error "observed output message must have assistant role"
      | Call { name; _ } when String.is_empty name -> Error "call name must be nonempty"
      | Unknown { provider_kind; _ } when String.is_empty provider_kind ->
        Error "unknown provider kind must be nonempty"
      | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ -> Ok ()
    ;;

    let create view ~metadata =
      let open Result.Let_syntax in
      let%bind () = validate_view view in
      let t = { view; metadata } in
      let%map () = validate_json (to_json t) in
      t
    ;;

    (* Only Payload.of_json calls this after complete JSON admission. Its
       semantic subtree and known projection are within the same JSON bounds. *)
    let of_validated_json value =
      let open Result.Let_syntax in
      let%bind fields = fields value in
      let%bind kind = required fields "type" string in
      let%bind metadata = required fields "metadata" Metadata.of_json in
      let%bind view =
        match kind with
        | "message" ->
          let%bind form =
            required fields "form" (fun value ->
              Result.bind (string value) ~f:(function
                | "input" -> Ok Input
                | "output" -> Ok Output
                | _ -> Error "unknown message form"))
          in
          let%map role = required fields "role" Role.of_json
          and content = required fields "content" (list Content.of_json)
          and phase = presence fields "phase" string in
          Message { form; role; content; phase }
        | "call" ->
          let%map kind = required fields "kind" Call_kind.of_json
          and name = required fields "name" string
          and namespace = presence fields "namespace" string
          and input_bytes = required fields "input_bytes" string
          and async = presence fields "async" boolean in
          Call { kind; name; namespace; input_bytes; async }
        | "result" ->
          let%map kind = required fields "kind" Call_kind.of_json
          and relation = required fields "relation" Call_relation.of_json
          and output = required fields "output" Output.of_validated_json in
          Result { relation; kind; output }
        | "reasoning" ->
          Result.map
            (required fields "readable_summary" (list string))
            ~f:(fun readable_summary -> Reasoning { readable_summary })
        | "unknown" ->
          Result.map (required fields "provider_kind" string) ~f:(fun provider_kind ->
            Unknown { provider_kind })
        | _ -> Error "unknown neutral semantic kind"
      in
      let%map () = validate_view view in
      { view; metadata }
    ;;
  end

  type representation =
    | Authored
    | Captured of
        { origin : Origin.t
        ; raw : Jsonaf.t
        }
    | Reconstructed of
        { provider : string
        ; raw : Jsonaf.t
        }
  [@@deriving sexp, bin_io]

  let representation_to_json = function
    | Authored -> `Object [ "type", `String "authored" ]
    | Captured { origin; raw } ->
      `Object [ "type", `String "captured"; "origin", Origin.to_json origin; "raw", raw ]
    | Reconstructed { provider; raw } ->
      `Object
        [ "type", `String "reconstructed"; "provider", `String provider; "raw", raw ]
  ;;

  let representation_of_json value =
    let open Result.Let_syntax in
    let%bind fields = fields value in
    let%bind kind = required fields "type" string in
    match kind with
    | "authored" -> Ok Authored
    | "captured" ->
      let%map origin = required fields "origin" Origin.of_json
      and raw = required fields "raw" json in
      Captured { origin; raw }
    | "reconstructed" ->
      let%bind provider = required fields "provider" string in
      if String.is_empty provider
      then Error "reconstructed provider must be nonempty"
      else
        Result.map (required fields "raw" json) ~f:(fun raw ->
          Reconstructed { provider; raw })
    | _ -> Error "unknown history representation"
  ;;

  type t =
    { document : Jsonaf.t
    ; semantic : Semantic.t
    ; representation : representation
    }

  let to_json t = t.document

  let of_json value =
    let open Result.Let_syntax in
    let%bind () = validate_json value in
    let%bind envelope = fields value in
    let%bind format = required envelope "format" string in
    let%bind kind = required envelope "kind" string in
    let%bind () =
      if String.equal format "ochat.document" && String.equal kind "history.payload"
      then Ok ()
      else Error "wrong history payload document kind or format"
    in
    let%bind () =
      required envelope "schema_version" (function
        | `Number "1" -> Ok ()
        | _ -> Error "unsupported history payload document version")
    in
    let%bind () =
      match List.Assoc.find envelope "required_semantics" ~equal:String.equal with
      | None | Some (`Array []) -> Ok ()
      | Some _ -> Error "unsupported history required semantics"
    in
    let%bind () =
      match List.Assoc.find envelope "extensions" ~equal:String.equal with
      | None | Some (`Object _) -> Ok ()
      | Some _ -> Error "history payload extensions must be a non-null object"
    in
    let%bind payload = required envelope "payload" fields in
    let%bind semantic = required payload "semantic" Semantic.of_validated_json in
    let%map representation = required payload "representation" representation_of_json in
    { document = value; semantic; representation }
  ;;

  let semantic t = t.semantic
  let representation t = t.representation

  let make_document semantic representation =
    `Object
      [ "format", `String "ochat.document"
      ; "schema_version", `Number "1"
      ; "kind", `String "history.payload"
      ; ( "payload"
        , `Object
            [ "semantic", Semantic.to_json semantic
            ; "representation", representation_to_json representation
            ] )
      ]
  ;;

  let authored semantic =
    { document = make_document semantic Authored; semantic; representation = Authored }
  ;;

  let captured semantic ~origin ~raw =
    make_document semantic (Captured { origin; raw }) |> of_json
  ;;

  let reconstructed semantic ~provider ~raw =
    make_document semantic (Reconstructed { provider; raw }) |> of_json
  ;;

  let validate t = Result.map (of_json t.document) ~f:(fun _ -> ())
  let sexp_of_t t = Jsonaf.sexp_of_t t.document
  let bin_shape_t = Jsonaf.bin_shape_t
  let bin_size_t t = Jsonaf.bin_size_t t.document
  let bin_write_t buffer ~pos t = Jsonaf.bin_write_t buffer ~pos t.document

  let bin_writer_t : t Bin_prot.Type_class.writer =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let t_of_sexp sexp = Jsonaf.t_of_sexp sexp |> of_json |> Result.ok_or_failwith

  let bin_read_t buffer ~pos_ref =
    Jsonaf.bin_read_t buffer ~pos_ref |> of_json |> Result.ok_or_failwith
  ;;

  let __bin_read_t__ buffer ~pos_ref length =
    Jsonaf.__bin_read_t__ buffer ~pos_ref length |> of_json |> Result.ok_or_failwith
  ;;

  let bin_reader_t : t Bin_prot.Type_class.reader =
    { read = bin_read_t; vtag_read = __bin_read_t__ }
  ;;

  let bin_t : t Bin_prot.Type_class.t =
    { writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
  ;;
end

type t =
  { id : Id.t
  ; payload : Payload.t
  }
[@@deriving bin_io, sexp]

type entry = t

let create ~allocator payload =
  Result.map (Allocator.allocate allocator) ~f:(fun id -> { id; payload })
;;

let create_with_id ~id payload = { id; payload }
let id t = t.id
let payload t = t.payload
let with_payload t payload = { t with payload }

let validate_relations entries =
  let indexed = Hashtbl.create (module Id) in
  List.iteri entries ~f:(fun index entry ->
    Hashtbl.set indexed ~key:entry.id ~data:(index, entry));
  List.foldi entries ~init:(Ok ()) ~f:(fun index previous entry ->
    let open Result.Let_syntax in
    let%bind () = previous in
    let semantic = Payload.semantic entry.payload in
    match Payload.Semantic.view semantic with
    | Result { relation = Bound call_id; kind; _ } ->
      (match Hashtbl.find indexed call_id with
       | None -> Ok ()
       | Some (call_index, call_entry) ->
         let call = Payload.semantic call_entry.payload in
         (match Payload.Semantic.view call with
          | Call { kind = call_kind; _ }
            when call_index < index && Payload.Call_kind.equal kind call_kind ->
            (match
               ( (Payload.Semantic.metadata semantic).call_id
               , (Payload.Semantic.metadata call).call_id )
             with
             | Value actual, Value expected when not (String.equal actual expected) ->
               Error "bound result provider metadata differs from its host call"
             | _ -> Ok ())
          | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ ->
            Error "bound result does not follow a matching host call"))
    | Message _ | Call _ | Result { relation = Unresolved; _ } | Reasoning _ | Unknown _
      -> Ok ())
;;

let validate ~allocator entries =
  let ids = Hash_set.create (module Id) in
  let next_sequence = Allocator.next_sequence allocator in
  let validated =
    List.fold_result entries ~init:() ~f:(fun () entry ->
      let open Result.Let_syntax in
      let%bind () = Payload.validate entry.payload in
      if String.is_empty (Id.namespace entry.id) || Id.sequence entry.id < 0
      then Error "history contains an invalid entry ID"
      else if Hash_set.mem ids entry.id
      then Error "history contains a duplicate entry ID"
      else (
        Hash_set.add ids entry.id;
        if
          String.equal (Id.namespace entry.id) (Allocator.namespace allocator)
          && Id.sequence entry.id >= next_sequence
        then Error "history entry sequence is not below the allocator high-water mark"
        else Ok ()))
  in
  Result.bind validated ~f:(fun () -> validate_relations entries)
;;

let tool_relation entry =
  let semantic = Payload.semantic entry.payload in
  let metadata = Payload.Semantic.metadata semantic in
  match Payload.Semantic.view semantic with
  | Call { kind; _ } ->
    Some (kind, metadata.call_id, true, Payload.Call_relation.Unresolved)
  | Result { kind; relation; _ } -> Some (kind, metadata.call_id, false, relation)
  | Message _ | Reasoning _ | Unknown _ -> None
;;

let paired_id entries index selected =
  Option.bind (tool_relation selected) ~f:(fun (family, call_id, is_call, relation) ->
    let following = List.drop entries (index + 1) in
    let explicit =
      if is_call
      then
        List.find_map following ~f:(fun entry ->
          match tool_relation entry with
          | Some (other, _, false, Bound id)
            when Id.equal id selected.id && Payload.Call_kind.equal family other ->
            Some entry.id
          | Some _ | None -> None)
      else None
    in
    match relation, explicit with
    | Bound call_id, _ ->
      List.find_map entries ~f:(fun entry ->
        if Id.equal entry.id call_id then Some entry.id else None)
    | Unresolved, Some id -> Some id
    | Unresolved, None ->
      let candidates =
        if is_call then following else List.take entries index |> List.rev
      in
      List.find candidates ~f:(fun entry ->
        Option.exists (tool_relation entry) ~f:(fun (other, key, _, other_relation) ->
          match other_relation with
          | Bound _ -> false
          | Unresolved ->
            Payload.Call_kind.equal family other
            && Payload.Presence.equal String.equal call_id key))
      |> Option.bind ~f:(fun entry ->
        match tool_relation entry with
        | Some (_, _, other_is_call, _) when Bool.(is_call <> other_is_call) ->
          Some (id entry)
        | Some _ | None -> None))
;;

let remove_with_tool_pair entries ~entry_id =
  match List.findi entries ~f:(fun _ entry -> Id.equal (id entry) entry_id) with
  | None -> Error "canonical history entry not found"
  | Some (index, selected) ->
    let paired = paired_id entries index selected in
    Ok
      (List.filter entries ~f:(fun entry ->
         not
           (Id.equal (id entry) entry_id || Option.exists paired ~f:(Id.equal (id entry)))))
;;

module Id_source = struct
  type t =
    { namespace : string
    ; allocate : unit -> (Id.t, string) result
    ; validate : entry list -> (unit, string) result
    }

  let create ~namespace ~allocate ~validate = { namespace; allocate; validate }

  let of_allocator allocator =
    create
      ~namespace:(Allocator.namespace allocator)
      ~allocate:(fun () -> Allocator.allocate allocator)
      ~validate:(fun entries -> validate ~allocator entries)
  ;;

  let namespace t = t.namespace
  let allocate t = t.allocate ()
  let validate t entries = t.validate entries
end
