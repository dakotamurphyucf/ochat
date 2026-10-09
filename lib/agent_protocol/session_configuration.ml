open! Core
module Error = Protocol_error
module J = Json_codec
module C = Inference.Observation.Configuration

let nonnegative = J.bounded_int64 ~min:0L ~max:Int64.max_value
let number n = `Number (Int64.to_string n)
let invalid = Error.invalid_request

let label text =
  if
    String.is_empty text
    || String.length text > 512
    || not (Stdlib.String.is_valid_utf_8 text)
  then Error (invalid "configuration label must be nonempty bounded UTF-8")
  else Ok text
;;

module Patch = struct
  type t =
    { model : string option
    ; profile : string option
    ; settings : Inference.Request.Setting.t list
    }

  let allowed =
    String.Set.of_list
      [ "max_output_tokens"
      ; "temperature"
      ; "top_p"
      ; "reasoning"
      ; "text"
      ; "parallel_tool_calls"
      ; "tool_choice"
      ; "prompt_cache_key"
      ; "prompt_cache_retention"
      ]
  ;;

  let create ?model ?profile ~settings () =
    let open Result.Let_syntax in
    let%bind model =
      Option.map model ~f:label
      |> Option.value_map ~default:(Ok None) ~f:(Result.map ~f:Option.some)
    in
    let%bind profile =
      Option.map profile ~f:label
      |> Option.value_map ~default:(Ok None) ~f:(Result.map ~f:Option.some)
    in
    let%bind () =
      if Option.is_none model && Option.is_none profile && List.is_empty settings
      then Error (invalid "empty configuration patch")
      else if List.length settings > 32
      then Error (invalid "too many configuration settings")
      else Ok ()
    in
    let names = List.map settings ~f:Inference.Request.Setting.name in
    let%bind () =
      if List.contains_dup names ~compare:String.compare
      then Error (invalid "duplicate configuration setting")
      else Ok ()
    in
    let%bind () =
      if List.for_all names ~f:(Set.mem allowed)
      then Ok ()
      else Error (invalid "unsupported explicit configuration setting")
    in
    Ok { model; profile; settings }
  ;;

  let model t = t.model
  let profile t = t.profile
  let settings t = t.settings

  let to_json t =
    `Object
      (Option.to_list (Option.map t.model ~f:(fun x -> "model", `String x))
       @ Option.to_list (Option.map t.profile ~f:(fun x -> "profile", `String x))
       @ [ "settings", `Array (List.map t.settings ~f:Inference.Request.Setting.to_json) ]
      )
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = J.validate_limits ~max_depth:32 ~max_bytes:65536 json in
    let%bind fields = J.fields json in
    let%bind model = J.optional_as fields "model" J.string in
    let%bind profile = J.optional_as fields "profile" J.string in
    let%bind settings =
      J.required_as
        fields
        "settings"
        (J.list (fun value ->
           Inference.Request.Setting.of_json value ~limits:Document_schema.Limits.default
           |> Result.map_error ~f:(fun _ -> invalid "invalid configuration setting")))
    in
    create ?model ?profile ~settings ()
  ;;

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error _ -> Sexplib.Conv.of_sexp_error "invalid configuration patch" sexp
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)
end

type phase =
  | Preparing
  | Effective
  | Retained
[@@deriving equal, sexp]

type capture =
  { operation_id : Id.Operation.t
  ; revision : int64
  ; phase : phase
  ; configuration : (C.t[@sexp.opaque])
  }
[@@deriving sexp]

type t =
  { revision : int64
  ; selected : (C.t option[@sexp.opaque])
  ; capture : capture option
  ; pending : bool
  }
[@@deriving sexp]

let capture_json c =
  `Object
    [ "operation_id", Id.Operation.to_json c.operation_id
    ; "revision", number c.revision
    ; ( "phase"
      , `String
          (match c.phase with
           | Preparing -> "preparing"
           | Effective -> "effective"
           | Retained -> "retained") )
    ; "configuration", C.to_json c.configuration
    ]
;;

let configuration json =
  C.of_json json ~limits:Document_schema.Limits.default
  |> Result.map_error ~f:(fun _ -> invalid "invalid safe configuration")
;;

let capture_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind operation_id = J.required_as fields "operation_id" Id.Operation.of_json in
  let%bind revision = J.required_as fields "revision" nonnegative in
  let%bind phase =
    J.required_as
      fields
      "phase"
      (J.enum
         ~name:"capture phase"
         [ "preparing", Preparing; "effective", Effective; "retained", Retained ])
  in
  let%map configuration = J.required_as fields "configuration" configuration in
  { operation_id; revision; phase; configuration }
;;

let optional decode = function
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

let to_json t =
  `Object
    [ "revision", number t.revision
    ; "selected", Option.value_map t.selected ~default:`Null ~f:C.to_json
    ; "capture", Option.value_map t.capture ~default:`Null ~f:capture_json
    ; ("pending", if t.pending then `True else `False)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind revision = J.required_as fields "revision" nonnegative in
  let%bind selected = J.required_as fields "selected" (optional configuration) in
  let%bind capture = J.required_as fields "capture" (optional capture_of_json) in
  let%bind pending = J.required_as fields "pending" J.bool in
  let%bind () =
    if Option.exists capture ~f:(fun c -> Int64.(c.revision > revision))
    then Error (invalid "capture revision exceeds selected revision")
    else Ok ()
  in
  Ok { revision; selected; capture; pending }
;;

module Get_request = struct
  type t = { session_id : Id.Session.t } [@@deriving sexp]

  let to_json t = `Object [ "session_id", Id.Session.to_json t.session_id ]

  let of_json json =
    Result.bind (J.fields json) ~f:(fun fields ->
      Result.map
        (J.required_as fields "session_id" Id.Session.of_json)
        ~f:(fun session_id -> { session_id }))
  ;;
end

module Update_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "expected_generation", `Number (Int.to_string t.expected_generation)
      ; "expected_revision", number t.expected_revision
      ; "patch", Patch.to_json t.patch
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind session_id = J.required_as fields "session_id" Id.Session.of_json in
    let%bind attachment_id = J.required_as fields "attachment_id" Id.Attachment.of_json in
    let%bind expected_generation =
      J.required_as fields "expected_generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind expected_revision = J.required_as fields "expected_revision" nonnegative in
    let%bind patch = J.required_as fields "patch" Patch.of_json in
    let%map idempotency_key =
      J.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    { session_id
    ; attachment_id
    ; expected_generation
    ; expected_revision
    ; patch
    ; idempotency_key
    }
  ;;
end
