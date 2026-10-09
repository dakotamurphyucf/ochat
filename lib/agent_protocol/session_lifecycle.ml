open! Core

module Revision = struct
  type t = int64 [@@deriving compare, equal, sexp]

  let zero = 0L
  let one = 1L

  let of_int64 value =
    if Int64.(value < zero)
    then Error (Protocol_error.invalid_request "negative lifecycle revision")
    else Ok value
  ;;

  let t_of_sexp sexp =
    match of_int64 (Int64.t_of_sexp sexp) with
    | Ok t -> t
    | Error failure -> Sexplib.Conv.of_sexp_error failure.message sexp
  ;;

  let to_int64 t = t

  let succ t =
    if Int64.equal t Int64.max_value
    then Error (Protocol_error.invalid_request "lifecycle revision exhausted")
    else Ok Int64.(t + 1L)
  ;;
end

module Expected = struct
  type t =
    { reference : Session_ref.t
    ; generation : int
    ; session_revision : int64
    ; lifecycle_revision : Revision.t
    }
  [@@deriving equal, sexp]

  let decoded_of_sexp = t_of_sexp

  let create ~reference ~generation ~session_revision ~lifecycle_revision =
    if generation < 0 || Int64.(session_revision < 0L)
    then Error (Protocol_error.invalid_request "negative lifecycle canonical anchor")
    else Ok { reference; generation; session_revision; lifecycle_revision }
  ;;

  let t_of_sexp sexp =
    let decoded = decoded_of_sexp sexp in
    match
      create
        ~reference:decoded.reference
        ~generation:decoded.generation
        ~session_revision:decoded.session_revision
        ~lifecycle_revision:decoded.lifecycle_revision
    with
    | Ok t -> t
    | Error failure -> Sexplib.Conv.of_sexp_error failure.message sexp
  ;;

  let reference t = t.reference
  let generation t = t.generation
  let session_revision t = t.session_revision
  let lifecycle_revision t = t.lifecycle_revision

  let to_json t =
    `Object
      [ "reference", Session_ref.to_json t.reference
      ; "generation", `Number (Int.to_string t.generation)
      ; "session_revision", `Number (Int64.to_string t.session_revision)
      ; ( "lifecycle_revision"
        , `Number (Int64.to_string (Revision.to_int64 t.lifecycle_revision)) )
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind reference = Json_codec.required_as fields "reference" Session_ref.of_json in
    let%bind generation =
      Json_codec.required_as
        fields
        "generation"
        (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind session_revision =
      Json_codec.required_as
        fields
        "session_revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
    in
    let%bind lifecycle_revision =
      Json_codec.required_as fields "lifecycle_revision" (fun json ->
        Result.bind
          (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value json)
          ~f:Revision.of_int64)
    in
    create ~reference ~generation ~session_revision ~lifecycle_revision
  ;;
end

module Request = struct
  type t =
    { expected : Expected.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let create ~expected ~idempotency_key = { expected; idempotency_key }
  let expected t = t.expected
  let idempotency_key t = t.idempotency_key

  let to_json t =
    `Object
      [ "expected", Expected.to_json t.expected
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind expected = Json_codec.required_as fields "expected" Expected.of_json in
    let%map idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    create ~expected ~idempotency_key
  ;;
end

module Result = struct
  module Status = struct
    type t =
      | Active
      | Archived
      | Removed
    [@@deriving equal, sexp]

    let to_json = function
      | Active -> `String "active"
      | Archived -> `String "archived"
      | Removed -> `String "removed"
    ;;

    let of_json =
      Json_codec.enum
        ~name:"lifecycle status"
        [ "active", Active; "archived", Archived; "removed", Removed ]
    ;;
  end

  module Admission = struct
    type t =
      | Automatic
      | Explicit_resume_required
    [@@deriving equal, sexp]

    let to_json = function
      | Automatic -> `String "automatic"
      | Explicit_resume_required -> `String "explicit_resume_required"
    ;;

    let of_json =
      Json_codec.enum
        ~name:"lifecycle admission"
        [ "automatic", Automatic; "explicit_resume_required", Explicit_resume_required ]
    ;;
  end

  module Action = struct
    type t =
      | Archive
      | Restore
      | Resume
      | Remove
    [@@deriving equal, sexp]

    let to_json = function
      | Archive -> `String "archive"
      | Restore -> `String "restore"
      | Resume -> `String "resume"
      | Remove -> `String "remove"
    ;;

    let of_json =
      Json_codec.enum
        ~name:"lifecycle action"
        [ "archive", Archive; "restore", Restore; "resume", Resume; "remove", Remove ]
    ;;
  end

  module Disposition = struct
    type t =
      | Applied
      | Already_current
    [@@deriving equal, sexp]

    let to_json = function
      | Applied -> `String "applied"
      | Already_current -> `String "already_current"
    ;;

    let of_json =
      Json_codec.enum
        ~name:"lifecycle disposition"
        [ "applied", Applied; "already_current", Already_current ]
    ;;
  end

  type t =
    { expected : Expected.t
    ; latest_event_sequence : int64
    ; status : Status.t
    ; admission : Admission.t
    ; action : Action.t
    ; disposition : Disposition.t
    ; completed_at : Timestamp.t
    }
  [@@deriving sexp]

  let decoded_of_sexp = t_of_sexp
  let expected t = t.expected
  let latest_event_sequence t = t.latest_event_sequence
  let status t = t.status
  let admission t = t.admission
  let action t = t.action
  let disposition t = t.disposition
  let completed_at t = t.completed_at

  let create
        ~reference
        ~generation
        ~session_revision
        ~latest_event_sequence
        ~lifecycle_revision
        ~status
        ~admission
        ~action
        ~disposition
        ~completed_at
    =
    let open Core.Result.Let_syntax in
    let%bind expected =
      Expected.create ~reference ~generation ~session_revision ~lifecycle_revision
    in
    let valid_status =
      match status, admission with
      | Status.Active, (Admission.Automatic | Explicit_resume_required) -> true
      | (Archived | Removed), Explicit_resume_required ->
        not (Revision.equal lifecycle_revision Revision.zero)
      | (Archived | Removed), Automatic -> false
    in
    let valid_action =
      match action, status, admission with
      | Action.Archive, Status.Archived, Admission.Explicit_resume_required
      | Remove, Removed, Explicit_resume_required
      | Restore, Active, Explicit_resume_required
      | Resume, Active, Automatic -> true
      | Archive, (Active | Removed), _
      | Archive, Archived, Automatic
      | Remove, (Active | Archived), _
      | Remove, Removed, Automatic
      | Restore, (Archived | Removed), _
      | Resume, (Archived | Removed), _
      | Resume, Active, Explicit_resume_required -> false
      | Restore, Active, Automatic -> Disposition.equal disposition Already_current
    in
    if Int64.(latest_event_sequence < 0L) || (not valid_status) || not valid_action
    then Error (Protocol_error.invalid_request "invalid lifecycle result")
    else
      Ok
        { expected
        ; latest_event_sequence
        ; status
        ; admission
        ; action
        ; disposition
        ; completed_at
        }
  ;;

  let t_of_sexp sexp =
    let decoded = decoded_of_sexp sexp in
    match
      create
        ~reference:(Expected.reference decoded.expected)
        ~generation:(Expected.generation decoded.expected)
        ~session_revision:(Expected.session_revision decoded.expected)
        ~latest_event_sequence:decoded.latest_event_sequence
        ~lifecycle_revision:(Expected.lifecycle_revision decoded.expected)
        ~status:decoded.status
        ~admission:decoded.admission
        ~action:decoded.action
        ~disposition:decoded.disposition
        ~completed_at:decoded.completed_at
    with
    | Ok t -> t
    | Error failure -> Sexplib.Conv.of_sexp_error failure.message sexp
  ;;

  let to_json t =
    `Object
      [ "expected", Expected.to_json t.expected
      ; "latest_event_sequence", `Number (Int64.to_string t.latest_event_sequence)
      ; "status", Status.to_json t.status
      ; "admission", Admission.to_json t.admission
      ; "action", Action.to_json t.action
      ; "disposition", Disposition.to_json t.disposition
      ; "completed_at", Timestamp.to_json t.completed_at
      ]
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind expected = Json_codec.required_as fields "expected" Expected.of_json in
    let%bind latest_event_sequence =
      Json_codec.required_as
        fields
        "latest_event_sequence"
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
    in
    let%bind status = Json_codec.required_as fields "status" Status.of_json in
    let%bind admission = Json_codec.required_as fields "admission" Admission.of_json in
    let%bind action = Json_codec.required_as fields "action" Action.of_json in
    let%bind disposition =
      Json_codec.required_as fields "disposition" Disposition.of_json
    in
    let%bind completed_at =
      Json_codec.required_as fields "completed_at" Timestamp.of_json
    in
    create
      ~reference:(Expected.reference expected)
      ~generation:(Expected.generation expected)
      ~session_revision:(Expected.session_revision expected)
      ~latest_event_sequence
      ~lifecycle_revision:(Expected.lifecycle_revision expected)
      ~status
      ~admission
      ~action
      ~disposition
      ~completed_at
  ;;
end

module Observation = struct
  type t =
    { expected : Expected.t
    ; status : Result.Status.t
    ; admission : Result.Admission.t
    }
  [@@deriving sexp]

  let decoded_of_sexp = t_of_sexp

  let create ~expected ~status ~admission =
    let revision_zero =
      Revision.equal (Expected.lifecycle_revision expected) Revision.zero
    in
    let valid =
      match status, admission with
      | Result.Status.Active, Result.Admission.Automatic -> true
      | (Active | Archived | Removed), Explicit_resume_required -> not revision_zero
      | (Archived | Removed), Automatic -> false
    in
    if valid
    then Ok { expected; status; admission }
    else Error (Protocol_error.invalid_request "invalid lifecycle observation")
  ;;

  let t_of_sexp sexp =
    let decoded = decoded_of_sexp sexp in
    match
      create
        ~expected:decoded.expected
        ~status:decoded.status
        ~admission:decoded.admission
    with
    | Ok t -> t
    | Error failure -> Sexplib.Conv.of_sexp_error failure.message sexp
  ;;

  let expected t = t.expected
  let status t = t.status
  let admission t = t.admission

  let matches_session t (session : Session.t) =
    Id.Session.equal (Session_ref.session_id (Expected.reference t.expected)) session.id
    && Int.equal (Expected.generation t.expected) session.generation
    && Int64.equal (Expected.session_revision t.expected) session.revision
  ;;

  let to_json t =
    `Object
      [ "expected", Expected.to_json t.expected
      ; "status", Result.Status.to_json t.status
      ; "admission", Result.Admission.to_json t.admission
      ]
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind expected = Json_codec.required_as fields "expected" Expected.of_json in
    let%bind status = Json_codec.required_as fields "status" Result.Status.of_json in
    let%bind admission =
      Json_codec.required_as fields "admission" Result.Admission.of_json
    in
    create ~expected ~status ~admission
  ;;
end
