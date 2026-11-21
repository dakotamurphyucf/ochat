open! Core
module P = Agent_protocol
module J = P.Json_codec
module Catalog = Recovery_notes.Catalog
module Digest = Chatml_source.Digest

let name = "context_checkpoint"

let implementation_revision =
  Chatmd_shell_spec.Source_ref.digest ("ochat.recovery.context.native.v1:" ^ name)
;;

let invalid message = Error (P.Error.invalid_request message)
let nonnegative = J.bounded_int64 ~min:0L ~max:Int64.max_value
let integer value = `Number (Int64.to_string value)
let string value = `String value
let key_json value = string (Digest.to_hex value)

let key_of_json value =
  let open Result.Let_syntax in
  let%bind hex = J.string value in
  Digest.of_hex hex
  |> Result.map_error ~f:(fun _ ->
    P.Error.invalid_request "invalid checkpoint key digest")
;;

module Request = struct
  type action =
    | Register of Catalog.selection list
    | Readback

  type t =
    { window : P.Id.Context_window.t
    ; key : Digest.t
    ; action : action
    }

  let positive_text value =
    match value with
    | `String number when String.length number <= 19 ->
      (match Int64.of_string number with
       | n when Int64.(n > 0L) && String.equal number (Int64.to_string n) -> Ok n
       | _ | (exception _) -> invalid "invalid checkpoint note selector")
    | _ -> invalid "invalid checkpoint note selector"
  ;;

  let selection json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    if List.length (J.to_alist fields) <> 2
    then invalid "invalid checkpoint note selector"
    else (
      let%bind id =
        J.required_as fields "id" positive_text >>= Catalog.Note_id.of_int64
      in
      let%map note_revision =
        J.required_as fields "revision" positive_text >>= Catalog.Note_revision.of_int64
      in
      Catalog.{ id; revision = note_revision })
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = J.validate_limits ~max_depth:8 ~max_bytes:16_384 json in
    let%bind fields = J.fields json in
    if List.length (J.to_alist fields) <> 4
    then invalid "invalid checkpoint request"
    else (
      let%bind window =
        J.required_as fields "expected_window" P.Id.Context_window.of_json
      in
      let%bind key = J.required_as fields "key" P.Idempotency_key.of_json in
      let%bind mode = J.required_as fields "mode" J.string in
      let%bind notes = J.required fields "notes" in
      let%map action =
        match mode, notes with
        | "register", `Array (_ :: _ as items)
          when Stdlib.List.compare_length_with items Catalog.max_notes <= 0 ->
          Result.map
            (List.map items ~f:selection |> Result.all)
            ~f:(fun notes -> Register notes)
        | "readback", `Array [] -> Ok Readback
        | _ -> invalid "invalid checkpoint action or notes"
      in
      let key = Digest.of_contents (P.Idempotency_key.to_string key) ~checkpoint:ignore in
      { window; key; action })
  ;;

  let object_ properties =
    `Object
      [ "type", string "object"
      ; "properties", `Object properties
      ; "required", `Array (List.map properties ~f:(fun (name, _) -> string name))
      ; "additionalProperties", `False
      ]
  ;;

  let parameters =
    let text = `Object [ "type", string "string" ] in
    let mode =
      `Object
        [ "type", string "string"
        ; "enum", `Array [ string "register"; string "readback" ]
        ]
    in
    let notes =
      `Object
        [ "type", string "array"; "items", object_ [ "id", text; "revision", text ] ]
    in
    object_ [ "expected_window", text; "key", text; "mode", mode; "notes", notes ]
  ;;
end

module Receipt = struct
  type t =
    | Accepted
    | Stale
    | Absent
    | Uncertain
  [@@deriving equal]

  let to_json value =
    let status =
      match value with
      | Accepted -> "accepted"
      | Stale -> "stale"
      | Absent -> "absent"
      | Uncertain -> "uncertain"
    in
    `Object [ "status", string status ]
  ;;
end

type t =
  { principal : P.Id.Principal.t
  ; key : Digest.t
  ; operation : P.Id.Operation.t
  ; invocation : P.Id.Invocation.t
  ; call : P.History.Id.t
  ; attempt_ordinal : int64
  ; source_revision : int64
  ; configuration_revision : int64
  ; prompt_revision : P.Id.Prompt_revision.t
  ; moderator : Digest.t option
  ; catalog_revision : Catalog.Catalog_revision.t
  ; pending_revision : P.Pending_input.Revision.t
  ; instruction_revision : int64 option
  ; instruction_digest : Chatml_source.Digest.t option
  ; covered_rows : int
  }
[@@deriving equal, sexp_of]

let create
      ~principal
      ~key
      ~operation
      ~invocation
      ~call
      ~attempt_ordinal
      ~source_revision
      ~configuration_revision
      ~prompt_revision
      ~moderator
      ~catalog_revision
      ~pending_revision
      ~instruction_revision
      ~instruction_digest
      ~covered_rows
  =
  if
    Int64.(attempt_ordinal < 0L || source_revision < 0L || configuration_revision < 0L)
    || Option.exists instruction_revision ~f:(fun revision -> Int64.(revision < 0L))
    || (not
          (Bool.equal
             (Option.is_some instruction_revision)
             (Option.is_some instruction_digest)))
    || covered_rows < 1
    || covered_rows > Recovery_window_record.max_members
  then invalid "invalid authored checkpoint basis"
  else
    Ok
      { principal
      ; key
      ; operation
      ; invocation
      ; call
      ; attempt_ordinal
      ; source_revision
      ; configuration_revision
      ; prompt_revision
      ; moderator
      ; catalog_revision
      ; pending_revision
      ; instruction_revision
      ; instruction_digest
      ; covered_rows
      }
;;

let same_key t ~principal ~key =
  P.Id.Principal.equal t.principal principal && Digest.equal t.key key
;;

let selected_invocation (invocation : P.Invocation.t) =
  let context = invocation.context in
  P.Invocation.equal_origin context.origin Model
  && Option.is_some context.call_entry_id
  && Option.is_none context.parent_invocation
  && Option.is_none context.parent_job
  && Option.is_none invocation.parent_event
  && String.equal context.tool_name name
  && String.equal context.implementation_revision implementation_revision
  && Option.exists invocation.routing ~f:(fun routing ->
    String.equal routing.original_name name
    && P.Invocation.equal_preparation routing.preparation Passed)
;;

let matches_invocation t (invocation : P.Invocation.t) =
  P.Id.Invocation.equal invocation.context.id t.invocation
  && Option.equal P.History.Id.equal invocation.context.call_entry_id (Some t.call)
  && selected_invocation invocation
;;

let to_json t =
  `Object
    [ "principal", P.Id.Principal.to_json t.principal
    ; "key_digest", key_json t.key
    ; "operation", P.Id.Operation.to_json t.operation
    ; "invocation", P.Id.Invocation.to_json t.invocation
    ; "call", P.History.Id.to_json t.call
    ; "attempt_ordinal", integer t.attempt_ordinal
    ; "source_revision", integer t.source_revision
    ; "configuration_revision", integer t.configuration_revision
    ; "prompt_revision", P.Id.Prompt_revision.to_json t.prompt_revision
    ; "moderator", Option.value_map t.moderator ~default:`Null ~f:key_json
    ; "catalog_revision", integer (Catalog.Catalog_revision.to_int64 t.catalog_revision)
    ; "pending_revision", P.Pending_input.Revision.to_json t.pending_revision
    ; ( "instruction_revision"
      , Option.value_map t.instruction_revision ~default:`Null ~f:integer )
    ; ( "instruction_digest"
      , Option.value_map t.instruction_digest ~default:`Null ~f:key_json )
    ; "covered_rows", `Number (Int.to_string t.covered_rows)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind principal = J.required_as fields "principal" P.Id.Principal.of_json in
  let%bind key = J.required_as fields "key_digest" key_of_json in
  let%bind operation = J.required_as fields "operation" P.Id.Operation.of_json in
  let%bind invocation = J.required_as fields "invocation" P.Id.Invocation.of_json in
  let%bind call = J.required_as fields "call" P.History.Id.of_json in
  let%bind attempt_ordinal = J.required_as fields "attempt_ordinal" nonnegative in
  let%bind source_revision = J.required_as fields "source_revision" nonnegative in
  let%bind configuration_revision =
    J.required_as fields "configuration_revision" nonnegative
  in
  let%bind prompt_revision =
    J.required_as fields "prompt_revision" P.Id.Prompt_revision.of_json
  in
  let nullable decode = function
    | `Null -> Ok None
    | value -> Result.map (decode value) ~f:Option.some
  in
  let%bind moderator = J.required_as fields "moderator" (nullable key_of_json) in
  let%bind catalog_revision =
    J.required_as fields "catalog_revision" nonnegative
    >>= Catalog.Catalog_revision.of_int64
  in
  let%bind pending_revision =
    J.required_as fields "pending_revision" P.Pending_input.Revision.of_json
  in
  let%bind instruction_revision =
    J.required_as fields "instruction_revision" (nullable nonnegative)
  in
  let%bind instruction_digest =
    J.required_as fields "instruction_digest" (nullable key_of_json)
  in
  let%bind covered_rows =
    J.required_as
      fields
      "covered_rows"
      (J.bounded_int ~min:1 ~max:Recovery_window_record.max_members)
  in
  create
    ~principal
    ~key
    ~operation
    ~invocation
    ~call
    ~attempt_ordinal
    ~source_revision
    ~configuration_revision
    ~prompt_revision
    ~moderator
    ~catalog_revision
    ~pending_revision
    ~instruction_revision
    ~instruction_digest
    ~covered_rows
;;
