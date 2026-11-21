open! Core
module J = Json_codec
module W = Recovery_wire

let feature = "recovery.note_share_control"
let max_envelope_bytes = 4096
let inspect_method = "session.recovery.notes.inspect"
let share_method = "session.recovery.notes.share"
let revoke_method = "session.recovery.notes.revoke_share"
let invalid message = Error (Protocol_error.invalid_request message)

let session_ref_of_json json =
  let%bind.Result _ = W.fields json [ "server_id"; "session_id" ] in
  Session_ref.of_json json
;;

module Path = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let valid_component component =
    let length = String.length component in
    length > 0
    && length <= 63
    && (not (String.equal component "." || String.equal component ".."))
    && String.for_all component ~f:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' -> true
      | _ -> false)
  ;;

  let of_string path =
    if String.length path > 240
    then invalid "recovery note path exceeds 240 bytes"
    else (
      let components = String.split path ~on:'/' in
      if List.length components <= 8 && List.for_all components ~f:valid_component
      then Ok path
      else invalid "invalid recovery note path component")
  ;;

  let to_string t = t
  let to_json t = `String t
  let of_json json = Result.bind (J.string json) ~f:of_string
end

module Selection = struct
  type t =
    { id : int64
    ; revision : int64
    }

  let create ~id ~revision =
    if Int64.(id > 0L && revision > 0L)
    then Ok { id; revision }
    else invalid "invalid recovery note selection"
  ;;

  let to_json t =
    `Object [ "id", W.revision_to_json t.id; "revision", W.revision_to_json t.revision ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "id"; "revision" ] in
    let%bind id = J.required_as fields "id" W.revision_of_json in
    let%bind revision = J.required_as fields "revision" W.revision_of_json in
    create ~id ~revision
  ;;
end

module Reference = struct
  type t =
    { source : Session_ref.t
    ; nonce : Id.Session_incarnation.t
    ; id : int64
    }
  [@@deriving equal]

  let create source ~nonce ~id =
    let open Result.Let_syntax in
    let%bind source = Session_ref.of_json (Session_ref.to_json source) in
    let%bind nonce =
      Id.Session_incarnation.of_json (Id.Session_incarnation.to_json nonce)
    in
    if Int64.(id > 0L) then Ok { source; nonce; id } else invalid "invalid note share ID"
  ;;

  let source t = t.source
  let id t = t.id

  let matches t ~source ~nonce =
    Session_ref.equal t.source source && Id.Session_incarnation.equal t.nonce nonce
  ;;

  let to_json t =
    `Object
      [ "source", Session_ref.to_json t.source
      ; "nonce", Id.Session_incarnation.to_json t.nonce
      ; "id", W.revision_to_json t.id
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "source"; "nonce"; "id" ] in
    let%bind source = J.required_as fields "source" session_ref_of_json in
    let%bind nonce = J.required_as fields "nonce" Id.Session_incarnation.of_json in
    let%bind id = J.required_as fields "id" W.revision_of_json in
    create source ~nonce ~id
  ;;
end

module Source = struct
  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    }

  let create session ~attachment_id ~expected_generation =
    let open Result.Let_syntax in
    let%bind session = Session_ref.of_json (Session_ref.to_json session) in
    let%bind attachment_id =
      Id.Attachment.of_json (Id.Attachment.to_json attachment_id)
    in
    if expected_generation >= 0
    then Ok { session; attachment_id; expected_generation }
    else invalid "invalid recovery note source generation"
  ;;

  let names = [ "source"; "attachment_id"; "expected_generation" ]

  let fields t =
    [ "source", Session_ref.to_json t.session
    ; "attachment_id", Id.Attachment.to_json t.attachment_id
    ; "expected_generation", `Number (Int.to_string t.expected_generation)
    ]
  ;;

  let of_fields fields =
    let open Result.Let_syntax in
    let%bind session = J.required_as fields "source" session_ref_of_json in
    let%bind attachment_id = J.required_as fields "attachment_id" Id.Attachment.of_json in
    let%bind expected_generation =
      J.required_as fields "expected_generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    create session ~attachment_id ~expected_generation
  ;;
end

module Basis = struct
  type t =
    { share_revision : int64
    ; catalog_revision : int64
    }

  let create ~share_revision ~catalog_revision =
    if Int64.(share_revision >= 0L && catalog_revision >= 0L)
    then Ok { share_revision; catalog_revision }
    else invalid "invalid recovery note share basis"
  ;;

  let to_json t =
    `Object
      [ "share_revision", W.revision_to_json t.share_revision
      ; "catalog_revision", W.revision_to_json t.catalog_revision
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "share_revision"; "catalog_revision" ] in
    let%bind share_revision = J.required_as fields "share_revision" W.revision_of_json in
    let%bind catalog_revision =
      J.required_as fields "catalog_revision" W.revision_of_json
    in
    create ~share_revision ~catalog_revision
  ;;
end

module Request = struct
  type t =
    | Inspect of
        { source : Source.t
        ; path : Path.t option
        }
    | Share of
        { source : Source.t
        ; expected : Basis.t
        ; selection : Selection.t
        ; recipient : Session_ref.t
        ; expires_at : Timestamp.t
        ; key : Idempotency_key.t
        }
    | Revoke of
        { source : Source.t
        ; expected_share_revision : int64
        ; reference : Reference.t
        ; key : Idempotency_key.t
        }

  let source = function
    | Inspect x -> x.source
    | Share x -> x.source
    | Revoke x -> x.source
  ;;

  let methods = [ inspect_method; share_method; revoke_method ]

  let method_name = function
    | Inspect _ -> inspect_method
    | Share _ -> share_method
    | Revoke _ -> revoke_method
  ;;

  let to_json t =
    let fields =
      match t with
      | Inspect { path; _ } ->
        Option.to_list (Option.map path ~f:(fun value -> "path", Path.to_json value))
      | Share { expected; selection; recipient; expires_at; key; _ } ->
        [ "expected", Basis.to_json expected
        ; "selection", Selection.to_json selection
        ; "recipient", Session_ref.to_json recipient
        ; "expires_at", Timestamp.to_json expires_at
        ; "key", Idempotency_key.to_json key
        ]
      | Revoke { expected_share_revision; reference; key; _ } ->
        [ "expected_share_revision", W.revision_to_json expected_share_revision
        ; "reference", Reference.to_json reference
        ; "key", Idempotency_key.to_json key
        ]
    in
    `Object (Source.fields (source t) @ fields)
  ;;

  let decode json names =
    let open Result.Let_syntax in
    let limits = W.limits ~max_bytes:max_envelope_bytes ~max_fields:48 ~max_nodes:96 in
    let%bind _ = W.measure ~limits json in
    W.fields json names
  ;;

  let of_method_and_params ~method_ json =
    let open Result.Let_syntax in
    let fields names = decode json (Source.names @ names) in
    match method_ with
    | name when String.equal name inspect_method ->
      let%bind fields = fields [ "path" ] in
      let%bind source = Source.of_fields fields in
      let%map path = J.optional_as fields "path" Path.of_json in
      Inspect { source; path }
    | name when String.equal name share_method ->
      let%bind fields =
        fields [ "expected"; "selection"; "recipient"; "expires_at"; "key" ]
      in
      let%bind source = Source.of_fields fields in
      let%bind expected = J.required_as fields "expected" Basis.of_json in
      let%bind selection = J.required_as fields "selection" Selection.of_json in
      let%bind recipient = J.required_as fields "recipient" session_ref_of_json in
      let%bind expires_at = J.required_as fields "expires_at" Timestamp.of_json in
      let%map key = J.required_as fields "key" Idempotency_key.of_json in
      Share { source; expected; selection; recipient; expires_at; key }
    | name when String.equal name revoke_method ->
      let%bind fields = fields [ "expected_share_revision"; "reference"; "key" ] in
      let%bind source = Source.of_fields fields in
      let%bind expected_share_revision =
        J.required_as fields "expected_share_revision" W.revision_of_json
      in
      let%bind reference = J.required_as fields "reference" Reference.of_json in
      let%bind key = J.required_as fields "key" Idempotency_key.of_json in
      if Session_ref.equal source.session (Reference.source reference)
      then Ok (Revoke { source; expected_share_revision; reference; key })
      else invalid "recovery share reference belongs to another source"
    | _ -> invalid "unknown recovery note share method"
  ;;
end

module Metadata = struct
  type t =
    { basis : Basis.t
    ; selection : Selection.t option
    }

  let to_json t =
    `Object
      (("basis", Basis.to_json t.basis)
       :: Option.to_list
            (Option.map t.selection ~f:(fun note -> "selection", Selection.to_json note))
      )
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Request.decode json [ "basis"; "selection" ] in
    let%bind basis = J.required_as fields "basis" Basis.of_json in
    let%map selection = J.optional_as fields "selection" Selection.of_json in
    { basis; selection }
  ;;
end

module Receipt = struct
  type t =
    { reference : Reference.t
    ; share_revision : int64
    }

  let create reference ~share_revision =
    if Int64.(share_revision >= Reference.id reference)
    then Ok { reference; share_revision }
    else invalid "invalid recovery note share receipt revision"
  ;;

  let to_json t =
    `Object
      [ "reference", Reference.to_json t.reference
      ; "share_revision", W.revision_to_json t.share_revision
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Request.decode json [ "reference"; "share_revision" ] in
    let%bind reference = J.required_as fields "reference" Reference.of_json in
    let%bind share_revision = J.required_as fields "share_revision" W.revision_of_json in
    create reference ~share_revision
  ;;
end

module Response = struct
  type t =
    | Inspected of Metadata.t
    | Shared of Receipt.t
    | Revoked of Receipt.t

  let method_name = function
    | Inspected _ -> inspect_method
    | Shared _ -> share_method
    | Revoked _ -> revoke_method
  ;;

  let to_json = function
    | Inspected metadata -> Metadata.to_json metadata
    | Shared receipt | Revoked receipt -> Receipt.to_json receipt
  ;;

  let of_method_and_json ~method_ json =
    match method_ with
    | name when String.equal name inspect_method ->
      Result.map (Metadata.of_json json) ~f:(fun metadata -> Inspected metadata)
    | name when String.equal name share_method ->
      Result.map (Receipt.of_json json) ~f:(fun receipt -> Shared receipt)
    | name when String.equal name revoke_method ->
      Result.map (Receipt.of_json json) ~f:(fun receipt -> Revoked receipt)
    | _ -> invalid "unknown recovery note share result"
  ;;

  let validate t ~request =
    let matches =
      match t, request with
      | Inspected metadata, Request.Inspect { path; _ } ->
        Option.is_some path || Option.is_none metadata.selection
      | Shared receipt, Share { source; expected; _ } ->
        Session_ref.equal source.session (Reference.source receipt.reference)
        && Int64.(
             receipt.share_revision > expected.share_revision
             && equal receipt.share_revision (succ expected.share_revision)
             && equal receipt.share_revision (Reference.id receipt.reference))
      | Revoked receipt, Revoke { expected_share_revision; reference; _ } ->
        Reference.equal receipt.reference reference
        && Int64.(
             receipt.share_revision > expected_share_revision
             && equal receipt.share_revision (succ expected_share_revision))
      | (Inspected _ | Shared _ | Revoked _), _ -> false
    in
    if matches then Ok () else invalid "recovery note share result differs from request"
  ;;
end
