open! Core
module C = Recovery_notes.Catalog
module P = Agent_protocol
module J = P.Json_codec

let invalid = P.Error.invalid_request
let counter_to_json counter = `String (Int64.to_string counter)

let counter_of_json decode json =
  let open Result.Let_syntax in
  let%bind text = J.string json in
  match Int64.of_string_opt text with
  | Some value when String.equal (Int64.to_string value) text -> decode value
  | Some _ | None -> Error (invalid "invalid recovery note share counter")
;;

module Incarnation = struct
  type t =
    { session : P.Session_ref.t
    ; nonce : P.Id.Session_incarnation.t
    ; generation : int
    }
  [@@deriving equal]

  let create ~session ~nonce ~generation =
    let open Result.Let_syntax in
    let%bind session = P.Session_ref.of_json (P.Session_ref.to_json session) in
    let%bind nonce =
      P.Id.Session_incarnation.of_string (P.Id.Session_incarnation.to_string nonce)
    in
    if generation < 0
    then Error (invalid "invalid recovery note share authority generation")
    else Ok { session; nonce; generation }
  ;;

  let session t = t.session

  let to_json t =
    `Object
      [ "session", P.Session_ref.to_json t.session
      ; "nonce", P.Id.Session_incarnation.to_json t.nonce
      ; "generation", `Number (Int.to_string t.generation)
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind session = J.required_as fields "session" P.Session_ref.of_json in
    let%bind nonce = J.required_as fields "nonce" P.Id.Session_incarnation.of_json in
    let%bind generation =
      J.required_as fields "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    create ~session ~nonce ~generation
  ;;
end

module Revision = struct
  type t = int64 [@@deriving compare, equal]

  let initial = 0L

  let of_int64 value =
    if Int64.(value >= initial)
    then Ok value
    else Error (invalid "invalid recovery note share revision")
  ;;

  let to_int64 t = t
end

module Reference = struct
  include P.Recovery_note_share_control.Reference

  (* Older canonical documents permit additive fields inside their carrier. *)
  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind source = J.required_as fields "source" P.Session_ref.of_json in
    let%bind nonce = J.required_as fields "nonce" P.Id.Session_incarnation.of_json in
    let%bind id = J.required_as fields "id" (counter_of_json Revision.of_int64) in
    create source ~nonce ~id:(Revision.to_int64 id)
  ;;
end

module Receipt = struct
  type t =
    { reference : Reference.t
    ; revision : Revision.t
    }

  let reference t = t.reference
  let revision t = t.revision
end

type create =
  { expected_revision : Revision.t
  ; expected_catalog_revision : C.Catalog_revision.t
  ; selection : C.selection
  ; expires_at : P.Timestamp.t
  }

type revoke =
  { expected_revision : Revision.t
  ; reference : Reference.t
  }

let selection_to_json { C.id; revision } =
  `Object
    [ "id", counter_to_json (C.Note_id.to_int64 id)
    ; "revision", counter_to_json (C.Note_revision.to_int64 revision)
    ]
;;

let selection_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind id = J.required_as fields "id" (counter_of_json C.Note_id.of_int64) in
  let%map revision =
    J.required_as fields "revision" (counter_of_json C.Note_revision.of_int64)
  in
  { C.id; revision }
;;

let expires_at_limit_ms = 7 * 24 * 60 * 60 * 1000

let valid_ttl ~now expires_at =
  let remaining = P.Timestamp.diff_ns expires_at now in
  Int64.(remaining > 0L && remaining <= of_int expires_at_limit_ms * 1_000_000L)
;;

let same_host a b =
  P.Id.Server.equal (P.Session_ref.server_id a) (P.Session_ref.server_id b)
;;

let recipient_matches_owner owner recipient =
  let session = Incarnation.session recipient in
  same_host owner session && not (P.Session_ref.equal owner session)
;;

module Mutation = struct
  type t =
    | Create of Incarnation.t * create
    | Revoke of revoke

  let to_json = function
    | Create (recipient, request) ->
      `Object
        [ "kind", `String "create"
        ; "recipient", Incarnation.to_json recipient
        ; "expected_revision", counter_to_json request.expected_revision
        ; ( "expected_catalog_revision"
          , counter_to_json
              (C.Catalog_revision.to_int64 request.expected_catalog_revision) )
        ; "selection", selection_to_json request.selection
        ; "expires_at", P.Timestamp.to_json request.expires_at
        ]
    | Revoke request ->
      `Object
        [ "kind", `String "revoke"
        ; "expected_revision", counter_to_json request.expected_revision
        ; "reference", Reference.to_json request.reference
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind expected_revision =
      J.required_as fields "expected_revision" (counter_of_json Revision.of_int64)
    in
    match%bind J.required_as fields "kind" J.string with
    | "create" ->
      let%bind recipient = J.required_as fields "recipient" Incarnation.of_json in
      let%bind expected_catalog_revision =
        J.required_as
          fields
          "expected_catalog_revision"
          (counter_of_json C.Catalog_revision.of_int64)
      in
      let%bind selection = J.required_as fields "selection" selection_of_json in
      let%map expires_at = J.required_as fields "expires_at" P.Timestamp.of_json in
      Create
        ( recipient
        , { expected_revision; expected_catalog_revision; selection; expires_at } )
    | "revoke" ->
      let%map reference = J.required_as fields "reference" Reference.of_json in
      Revoke { expected_revision; reference }
    | _ -> Error (invalid "unknown recovery note share operation")
  ;;
end
