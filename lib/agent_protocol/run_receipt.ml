open! Core
module J = Json_codec

module Kind = struct
  type t =
    | Admission
    | Action
    | Terminal
  [@@deriving compare, equal, sexp]

  let to_json = function
    | Admission -> `String "admission"
    | Action -> `String "action"
    | Terminal -> `String "terminal"
  ;;

  let of_json = function
    | `String "admission" -> Ok Admission
    | `String "action" -> Ok Action
    | `String "terminal" -> Ok Terminal
    | _ -> Error (Protocol_error.invalid_request "unsupported run receipt kind")
  ;;
end

type t =
  { run_id : Id.Run.t
  ; principal_id : Id.Principal.t
  ; source : Run_source.t
  ; key : Idempotency_key.t
  ; request_sha256 : string
  ; kind : Kind.t
  ; run_revision : int64
  ; session_revision : int64
  ; committed_at : Timestamp.t
  }
[@@deriving equal]

let create
      ~run_id
      ~principal_id
      ~source
      ~key
      ~request_sha256
      ~kind
      ~run_revision
      ~session_revision
      ~committed_at
  =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_id Id.Run.to_json Id.Run.of_json run_id in
  let%bind () =
    Extension_codec.validate_id Id.Principal.to_json Id.Principal.of_json principal_id
  in
  let%bind () = Run_source.validate source in
  let%bind _ = Idempotency_key.of_string (Idempotency_key.to_string key) in
  let valid_digest =
    String.length request_sha256 = 64
    && String.for_all request_sha256 ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  in
  if (not valid_digest) || Int64.(run_revision < 0L || session_revision < 0L)
  then Error (Protocol_error.invalid_request "invalid run receipt digest or revision")
  else
    Ok
      { run_id
      ; principal_id
      ; source
      ; key
      ; request_sha256
      ; kind
      ; run_revision
      ; session_revision
      ; committed_at
      }
;;

let to_json t =
  `Object
    [ "run_id", Id.Run.to_json t.run_id
    ; "principal_id", Id.Principal.to_json t.principal_id
    ; "source", Run_source.to_json t.source
    ; "key", Idempotency_key.to_json t.key
    ; "request_sha256", `String t.request_sha256
    ; "kind", Kind.to_json t.kind
    ; "run_revision", `String (Int64.to_string t.run_revision)
    ; "session_revision", `String (Int64.to_string t.session_revision)
    ; "committed_at", Timestamp.to_json t.committed_at
    ]
;;

let revision_of_json = function
  | `String encoded ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value >= 0L) && String.equal encoded (Int64.to_string value)
       -> Ok value
     | Some _ | None ->
       Error (Protocol_error.invalid_request "invalid run receipt revision"))
  | _ ->
    Error (Protocol_error.invalid_request "run receipt revision must be decimal string")
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Run_limits.max_document_bytes
      ~max_depth:Run_limits.max_depth
      json
  in
  let%bind fields = J.fields json in
  let%bind run_id = J.required_as fields "run_id" Id.Run.of_json in
  let%bind principal_id = J.required_as fields "principal_id" Id.Principal.of_json in
  let%bind source = J.required_as fields "source" Run_source.of_json in
  let%bind key = J.required_as fields "key" Idempotency_key.of_json in
  let%bind request_sha256 = J.required_as fields "request_sha256" J.string in
  let%bind kind = J.required_as fields "kind" Kind.of_json in
  let%bind run_revision = J.required_as fields "run_revision" revision_of_json in
  let%bind session_revision = J.required_as fields "session_revision" revision_of_json in
  let%bind committed_at = J.required_as fields "committed_at" Timestamp.of_json in
  create
    ~run_id
    ~principal_id
    ~source
    ~key
    ~request_sha256
    ~kind
    ~run_revision
    ~session_revision
    ~committed_at
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;
