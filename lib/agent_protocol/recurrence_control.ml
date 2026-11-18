open! Core
module J = Json_codec
module W = Recurrence_wire

module Features = struct
  let control = "sessions.recurrence_control"
end

let method_ = "session.recurrence.command"
let max_input_bytes = 256 * 1024
let max_request_bytes = max_input_bytes + (80 * 1024)

let validate_request json =
  Extension_codec.validate_json ~max_bytes:max_request_bytes ~max_depth:32 json
;;

module Source_basis = struct
  type t =
    { source : Run_source.t
    ; expected_lifecycle_revision : Session_lifecycle.Revision.t
    }

  let fields t =
    [ "source", Run_source.to_json t.source
    ; "expected_lifecycle_revision", W.lifecycle_json t.expected_lifecycle_revision
    ]
  ;;

  let decode fields =
    let open Result.Let_syntax in
    let%bind source = J.required_as fields "source" W.source in
    let%map expected_lifecycle_revision =
      J.required_as fields "expected_lifecycle_revision" W.lifecycle
    in
    { source; expected_lifecycle_revision }
  ;;
end

module Goal_owner = struct
  type t =
    { goal_id : Id.Goal.t
    ; expected_goal_revision : int64
    }

  let create ~goal_id ~expected_goal_revision =
    let%bind.Result () =
      Extension_codec.validate_id Id.Goal.to_json Id.Goal.of_json goal_id
    in
    if Int64.(expected_goal_revision < 0L)
    then W.invalid "negative recurrence Goal revision"
    else Ok { goal_id; expected_goal_revision }
  ;;

  let to_json t =
    `Object
      [ "goal_id", Id.Goal.to_json t.goal_id
      ; "expected_goal_revision", W.counter_json t.expected_goal_revision
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "goal_id"; "expected_goal_revision" ] in
    let%bind goal_id = J.required_as fields "goal_id" Id.Goal.of_json in
    let%bind expected_goal_revision =
      J.required_as fields "expected_goal_revision" W.counter
    in
    create ~goal_id ~expected_goal_revision
  ;;
end

module Capture = struct
  type selector =
    | Existing of
        { session : Session_ref.t
        ; input : Session.Message_content.t
        }
    | Authored_child of
        { name : string
        ; mode : Run.Mode.t
        }

  type t =
    { schedule : Recurrence_schedule.t
    ; policy : Recurrence_policy.t
    ; selector : selector
    ; goal_owner : Goal_owner.t option
    }

  let input_of_json json =
    let open Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_json ~max_bytes:max_input_bytes ~max_depth:12 json
    in
    let%bind fields = W.fields json [ "kind"; "text"; "attachments" ] in
    let check_blob blob =
      let%bind fields = J.fields blob in
      let%bind source = J.required_as fields "source" J.string in
      let common =
        [ "kind"; "media_type"; "byte_length"; "digest"; "display_name"; "source" ]
      in
      match source with
      | "stored" -> Extension_codec.closed fields ("blob_id" :: common)
      | "inline_base64" -> Extension_codec.closed fields ("inline_base64" :: common)
      | _ -> W.invalid "unsupported recurrence input attachment"
    in
    let%bind (_ : unit list) = J.required_as fields "attachments" (J.list check_blob) in
    Session.Message_content.of_json json
  ;;

  let valid_name name =
    (not (String.is_empty name))
    && String.length name <= 1024
    && not (String.exists name ~f:(fun ch -> Char.to_int ch < 32 || Char.equal ch '\127'))
  ;;

  let create ~schedule ~policy ~selector ?goal_owner () =
    let open Result.Let_syntax in
    let%map selector =
      match selector with
      | Existing _ when Option.is_some goal_owner ->
        W.invalid "recurrence Goal ownership requires a declared authored child"
      | Existing { session; input } ->
        let%bind _ = W.session (Session_ref.to_json session) in
        let%map input = input_of_json (Session.Message_content.to_json input) in
        Existing { session; input }
      | Authored_child { name; mode } ->
        if valid_name name
        then Ok (Authored_child { name; mode })
        else W.invalid "invalid authored recurrence worker name"
    in
    { schedule; policy; selector; goal_owner }
  ;;

  let selector_json = function
    | Existing { session; input } ->
      W.tagged
        "existing_session"
        [ "session", Session_ref.to_json session
        ; "input", Session.Message_content.to_json input
        ]
    | Authored_child { name; mode } ->
      W.tagged "authored_child" [ "name", `String name; "mode", Run.Mode.to_json mode ]
  ;;

  let selector_of_json json =
    let open Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "existing_session" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "session"; "input" ] in
      let%bind session = J.required_as fields "session" W.session in
      let%map input = J.required_as fields "input" input_of_json in
      Existing { session; input }
    | "authored_child" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "name"; "mode" ] in
      let%bind name = J.required_as fields "name" J.string in
      let%map mode = J.required_as fields "mode" Run.Mode.of_json in
      Authored_child { name; mode }
    | _ -> W.invalid "unsupported recurrence target selector"
  ;;

  let to_json t =
    `Object
      [ "schedule", Recurrence_schedule.to_json t.schedule
      ; "policy", Recurrence_policy.to_json t.policy
      ; "target", selector_json t.selector
      ; "goal_owner", W.option_json Goal_owner.to_json t.goal_owner
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "schedule"; "policy"; "target"; "goal_owner" ] in
    let%bind schedule =
      J.required_as fields "schedule" Recurrence_schedule.of_public_json
    in
    let%bind policy = J.required_as fields "policy" Recurrence_policy.of_public_json in
    let%bind selector = J.required_as fields "target" selector_of_json in
    let%bind goal_owner =
      J.required_as fields "goal_owner" (W.nullable Goal_owner.of_json)
    in
    create ~schedule ~policy ~selector ?goal_owner ()
  ;;
end

module Verb = struct
  type t =
    | Create
    | Edit
    | Enable
    | Pause
    | Retire
  [@@deriving equal, compare, sexp_of]

  let names =
    [ "create", Create; "edit", Edit; "enable", Enable; "pause", Pause; "retire", Retire ]
  ;;

  let to_string = function
    | Create -> "create"
    | Edit -> "edit"
    | Enable -> "enable"
    | Pause -> "pause"
    | Retire -> "retire"
  ;;

  let to_json verb = `String (to_string verb)
  let of_json json = J.enum ~name:"recurrence action" names json
end

module Action = struct
  type t =
    | Create of
        { id : Id.Recurrence.t
        ; basis : Source_basis.t
        ; capture : Capture.t
        }
    | Edit of
        { id : Id.Recurrence.t
        ; expected_control_revision : int64
        ; basis : Source_basis.t
        ; capture : Capture.t
        }
    | Enable of
        { id : Id.Recurrence.t
        ; expected_control_revision : int64
        ; basis : Source_basis.t
        }
    | Pause of
        { id : Id.Recurrence.t
        ; expected_control_revision : int64
        }
    | Retire of
        { id : Id.Recurrence.t
        ; expected_control_revision : int64
        }

  let id = function
    | Create { id; _ }
    | Edit { id; _ }
    | Enable { id; _ }
    | Pause { id; _ }
    | Retire { id; _ } -> id
  ;;

  let verb : t -> Verb.t = function
    | Create _ -> Create
    | Edit _ -> Edit
    | Enable _ -> Enable
    | Pause _ -> Pause
    | Retire _ -> Retire
  ;;

  let expected_control_revision = function
    | Create _ -> None
    | Edit { expected_control_revision; _ }
    | Enable { expected_control_revision; _ }
    | Pause { expected_control_revision; _ }
    | Retire { expected_control_revision; _ } -> Some expected_control_revision
  ;;

  let basis = function
    | Create { basis; _ } | Edit { basis; _ } | Enable { basis; _ } -> Some basis
    | Pause _ | Retire _ -> None
  ;;

  let capture = function
    | Create { capture; _ } | Edit { capture; _ } -> Some capture
    | Enable _ | Pause _ | Retire _ -> None
  ;;

  let to_json t =
    W.tagged
      (Verb.to_string (verb t))
      ([ "recurrence_id", Id.Recurrence.to_json (id t) ]
       @ Option.value_map (expected_control_revision t) ~default:[] ~f:(fun revision ->
         [ "expected_control_revision", W.counter_json revision ])
       @ Option.value_map (basis t) ~default:[] ~f:Source_basis.fields
       @ Option.value_map (capture t) ~default:[] ~f:(fun capture ->
         [ "capture", Capture.to_json capture ]))
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind verb = J.required_as fields "kind" Verb.of_json in
    let identity = [ "kind"; "recurrence_id" ] in
    let with_control = "expected_control_revision" :: identity in
    let with_source fields = "source" :: "expected_lifecycle_revision" :: fields in
    let allowed =
      match verb with
      | Create -> with_source ("capture" :: identity)
      | Edit -> with_source ("capture" :: with_control)
      | Enable -> with_source with_control
      | Pause | Retire -> with_control
    in
    let%bind () = Extension_codec.closed fields allowed in
    let%bind id = J.required_as fields "recurrence_id" Id.Recurrence.of_json in
    let decode_control () = J.required_as fields "expected_control_revision" W.counter in
    let decode_capture () = J.required_as fields "capture" Capture.of_json in
    match verb with
    | Create ->
      let%bind basis = Source_basis.decode fields in
      let%map capture = decode_capture () in
      Create { id; basis; capture }
    | Edit ->
      let%bind expected_control_revision = decode_control () in
      let%bind basis = Source_basis.decode fields in
      let%map capture = decode_capture () in
      Edit { id; expected_control_revision; basis; capture }
    | Enable ->
      let%bind expected_control_revision = decode_control () in
      let%map basis = Source_basis.decode fields in
      Enable { id; expected_control_revision; basis }
    | Pause ->
      let%map expected_control_revision = decode_control () in
      Pause { id; expected_control_revision }
    | Retire ->
      let%map expected_control_revision = decode_control () in
      Retire { id; expected_control_revision }
  ;;
end

module Intent = struct
  type t =
    { generation : int
    ; expected_catalog_revision : int64
    ; key : Idempotency_key.t
    ; action : Action.t
    }

  let to_json t =
    `Object
      [ "generation", W.integer_json t.generation
      ; "expected_catalog_revision", W.counter_json t.expected_catalog_revision
      ; "key", Idempotency_key.to_json t.key
      ; "action", Action.to_json t.action
      ]
  ;;

  let create ~generation ~expected_catalog_revision ~key action =
    let open Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_id
        Id.Recurrence.to_json
        Id.Recurrence.of_json
        (Action.id action)
    in
    let%bind () =
      Extension_codec.validate_id Idempotency_key.to_json Idempotency_key.of_json key
    in
    let%bind () =
      Option.value_map (Action.basis action) ~default:(Ok ()) ~f:(fun basis ->
        let%bind () = Run_source.validate basis.source in
        if Int.equal generation basis.source.generation
        then Ok ()
        else W.invalid "recurrence controller source and generation differ")
    in
    if
      generation < 0
      || Int64.(expected_catalog_revision < 0L)
      || Option.exists (Action.expected_control_revision action) ~f:(fun revision ->
        Int64.(revision < 0L))
    then W.invalid "negative recurrence revision or generation"
    else (
      let t = { generation; expected_catalog_revision; key; action } in
      let%map () = validate_request (to_json t) in
      t)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields json [ "generation"; "expected_catalog_revision"; "key"; "action" ]
    in
    let%bind generation = J.required_as fields "generation" W.integer in
    let%bind expected_catalog_revision =
      J.required_as fields "expected_catalog_revision" W.counter
    in
    let%bind key = J.required_as fields "key" Idempotency_key.of_json in
    let%bind action = J.required_as fields "action" Action.of_json in
    create ~generation ~expected_catalog_revision ~key action
  ;;

  let check_session t session =
    let open Result.Let_syntax in
    let%bind _ = W.session (Session_ref.to_json session) in
    match Action.capture t.action with
    | Some { selector = Existing { session = target; _ }; _ }
      when not
             (Id.Server.equal
                (Session_ref.server_id target)
                (Session_ref.server_id session)) ->
      W.invalid "recurrence existing target belongs to another host"
    | None | Some _ -> Ok ()
  ;;

  let digest t ~session =
    let open Result.Let_syntax in
    let%bind () = check_session t session in
    let%map canonical =
      J.canonical_string
        (`Object
            [ "format", `String "ochat.recurrence.operator_intent.v1"
            ; "session", Session_ref.to_json session
            ; "intent", to_json t
            ])
    in
    Digestif.SHA256.(digest_string canonical |> to_hex)
  ;;
end

module Request = struct
  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t
    ; intent : Intent.t
    }

  let methods = [ method_ ]
  let method_name (_ : t) = method_

  let params t =
    `Object
      [ "session", Session_ref.to_json t.session
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "intent", Intent.to_json t.intent
      ]
  ;;

  let create ~session ~attachment_id intent =
    let open Result.Let_syntax in
    let%bind () = Intent.check_session intent session in
    let%bind () =
      Extension_codec.validate_id
        Id.Attachment.to_json
        Id.Attachment.of_json
        attachment_id
    in
    let t = { session; attachment_id; intent } in
    let%map () = validate_request (params t) in
    t
  ;;

  let of_method_and_params ~method_:requested ~params =
    let open Result.Let_syntax in
    if not (String.equal method_ requested)
    then W.invalid "unknown recurrence control method"
    else (
      let%bind () = validate_request params in
      let%bind fields = W.fields params [ "session"; "attachment_id"; "intent" ] in
      let%bind session = J.required_as fields "session" W.session in
      let%bind attachment_id =
        J.required_as fields "attachment_id" Id.Attachment.of_json
      in
      let%bind intent = J.required_as fields "intent" Intent.of_json in
      create ~session ~attachment_id intent)
  ;;
end

module Receipt = struct
  type t =
    { session : Session_ref.t
    ; generation : int
    ; principal_id : Id.Principal.t
    ; key : Idempotency_key.t
    ; intent_sha256 : string
    ; recurrence_id : Id.Recurrence.t
    ; verb : Verb.t
    ; definition_revision : int64
    ; control_revision : int64
    ; catalog_revision : int64
    ; accepted_at : Timestamp.t
    }

  let to_json t =
    `Object
      [ "session", Session_ref.to_json t.session
      ; "generation", W.integer_json t.generation
      ; "principal_id", Id.Principal.to_json t.principal_id
      ; "key", Idempotency_key.to_json t.key
      ; "intent_sha256", `String t.intent_sha256
      ; "recurrence_id", Id.Recurrence.to_json t.recurrence_id
      ; "verb", Verb.to_json t.verb
      ; "definition_revision", W.counter_json t.definition_revision
      ; "control_revision", W.counter_json t.control_revision
      ; "catalog_revision", W.counter_json t.catalog_revision
      ; "accepted_at", Timestamp.to_json t.accepted_at
      ]
  ;;

  let validate t =
    let%bind.Result () =
      Extension_codec.validate_id Id.Principal.to_json Id.Principal.of_json t.principal_id
    in
    let valid_digest =
      String.length t.intent_sha256 = 64
      && String.for_all t.intent_sha256 ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    in
    if
      (not valid_digest)
      || t.generation < 0
      || Int64.(
           t.definition_revision < 0L
           || t.control_revision < t.definition_revision
           || t.catalog_revision <= 0L)
    then W.invalid "invalid recurrence command receipt"
    else Ok t
  ;;

  let create
        ~session
        ~generation
        ~principal_id
        ~key
        ~intent_sha256
        ~recurrence_id
        ~verb
        ~definition_revision
        ~control_revision
        ~catalog_revision
        ~accepted_at
    =
    validate
      { session
      ; generation
      ; principal_id
      ; key
      ; intent_sha256
      ; recurrence_id
      ; verb
      ; definition_revision
      ; control_revision
      ; catalog_revision
      ; accepted_at
      }
  ;;

  let next_is before after = Int64.(before < max_value && equal (succ before) after)

  let check_intent t ~session ~principal_id ~(intent : Intent.t) =
    let%bind.Result digest = Intent.digest intent ~session in
    let control_matches =
      let matches expected =
        Int64.equal t.control_revision expected || next_is expected t.control_revision
      in
      match intent.action with
      | Create _ -> Int64.(equal t.definition_revision 0L && equal t.control_revision 0L)
      | Edit { expected_control_revision; _ } ->
        Int64.(t.definition_revision > 0L)
        && next_is expected_control_revision t.control_revision
      | Enable { expected_control_revision; _ }
      | Pause { expected_control_revision; _ }
      | Retire { expected_control_revision; _ } -> matches expected_control_revision
    in
    if
      Session_ref.equal t.session session
      && Int.equal t.generation intent.generation
      && Id.Principal.equal t.principal_id principal_id
      && Idempotency_key.equal t.key intent.key
      && Id.Recurrence.equal t.recurrence_id (Action.id intent.action)
      && Verb.equal t.verb (Action.verb intent.action)
      && String.equal t.intent_sha256 digest
      && next_is intent.expected_catalog_revision t.catalog_revision
      && control_matches
    then Ok ()
    else W.invalid "recurrence receipt does not match the original command"
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Extension_codec.validate_json ~max_bytes:4096 ~max_depth:8 json in
    let%bind fields =
      W.fields
        json
        [ "session"
        ; "generation"
        ; "principal_id"
        ; "key"
        ; "intent_sha256"
        ; "recurrence_id"
        ; "verb"
        ; "definition_revision"
        ; "control_revision"
        ; "catalog_revision"
        ; "accepted_at"
        ]
    in
    let get name decode = J.required_as fields name decode in
    let%bind session = get "session" W.session in
    let%bind generation = get "generation" W.integer in
    let%bind principal_id = get "principal_id" Id.Principal.of_json in
    let%bind key = get "key" Idempotency_key.of_json in
    let%bind intent_sha256 = get "intent_sha256" J.string in
    let%bind recurrence_id = get "recurrence_id" Id.Recurrence.of_json in
    let%bind verb = get "verb" Verb.of_json in
    let%bind definition_revision = get "definition_revision" W.counter in
    let%bind control_revision = get "control_revision" W.counter in
    let%bind catalog_revision = get "catalog_revision" W.counter in
    let%bind accepted_at = get "accepted_at" Timestamp.of_json in
    validate
      { session
      ; generation
      ; principal_id
      ; key
      ; intent_sha256
      ; recurrence_id
      ; verb
      ; definition_revision
      ; control_revision
      ; catalog_revision
      ; accepted_at
      }
  ;;
end

module Result = struct
  type t = Receipt.t

  let method_name (_ : t) = method_
  let payload = Receipt.to_json

  let of_method_and_result ~method_:requested ~result =
    if String.equal method_ requested
    then Receipt.of_json result
    else W.invalid "unknown recurrence control result method"
  ;;
end
