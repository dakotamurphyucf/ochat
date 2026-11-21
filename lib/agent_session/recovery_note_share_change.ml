open! Core
module P = Agent_protocol
module S = Recovery_note_share
module J = P.Json_codec

type t =
  | Establish of
      { owner : P.Session_ref.t
      ; nonce : P.Id.Session_incarnation.t
      }
  | Mutate of
      { source : S.Incarnation.t
      ; principal : P.Id.Principal.t
      ; key : P.Idempotency_key.t
      ; observed_at : P.Timestamp.t
      ; mutation : S.Journal.mutation
      }

let establish ~owner ~nonce = Establish { owner; nonce }

let create ~source ~recipient ~principal ~key ~now request =
  Mutate
    { source; principal; key; observed_at = now; mutation = Create (recipient, request) }
;;

let revoke ~source ~principal ~key ~now request =
  Mutate { source; principal; key; observed_at = now; mutation = Revoke request }
;;

type outcome =
  | Established of S.t
  | Applied of S.t * S.Receipt.t
  | Replayed of S.Receipt.t

let invalid = P.Error.invalid_request

let evaluate t ~previous ~catalog ~session_id ~generation =
  let open Result.Let_syntax in
  match t, previous with
  | Establish { owner; nonce }, None
    when P.Id.Session.equal (P.Session_ref.session_id owner) session_id ->
    let%map ledger = S.Journal.establish ~owner ~nonce in
    Established ledger
  | Mutate { source; principal; key; observed_at; mutation }, Some ledger ->
    let%bind actual = S.incarnation ledger ~generation in
    if
      P.Id.Session.equal (P.Session_ref.session_id (S.owner ledger)) session_id
      && S.Incarnation.equal source actual
    then (
      match%map
        S.Journal.mutate ledger ~source ~catalog ~principal ~key ~now:observed_at mutation
      with
      | `Applied (ledger, receipt) -> Applied (ledger, receipt)
      | `Replayed receipt -> Replayed receipt)
    else Error (invalid "recovery note share source incarnation changed")
  | Establish _, (None | Some _) | Mutate _, None ->
    Error (invalid "invalid recovery note share custody change")
;;

let apply t ~previous ~catalog ~session_id ~generation =
  match%bind.Result evaluate t ~previous ~catalog ~session_id ~generation with
  | Established ledger | Applied (ledger, _) -> Ok ledger
  | Replayed _ ->
    Error (invalid "recovery note share journal operation was already applied")
;;

let to_json = function
  | Establish { owner; nonce } ->
    `Object
      [ "kind", `String "establish"
      ; "owner", P.Session_ref.to_json owner
      ; "nonce", P.Id.Session_incarnation.to_json nonce
      ]
  | Mutate { source; principal; key; observed_at; mutation } ->
    `Object
      [ "kind", `String "mutate"
      ; "source", S.Incarnation.to_json source
      ; "principal", P.Id.Principal.to_json principal
      ; "key", P.Idempotency_key.to_json key
      ; "observed_at", P.Timestamp.to_json observed_at
      ; "mutation", S.Journal.mutation_to_json mutation
      ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  match%bind J.required_as fields "kind" J.string with
  | "establish" ->
    let%bind owner = J.required_as fields "owner" P.Session_ref.of_json in
    let%map nonce = J.required_as fields "nonce" P.Id.Session_incarnation.of_json in
    establish ~owner ~nonce
  | "mutate" ->
    let%bind source = J.required_as fields "source" S.Incarnation.of_json in
    let%bind principal = J.required_as fields "principal" P.Id.Principal.of_json in
    let%bind key = J.required_as fields "key" P.Idempotency_key.of_json in
    let%bind observed_at = J.required_as fields "observed_at" P.Timestamp.of_json in
    let%map mutation = J.required_as fields "mutation" S.Journal.mutation_of_json in
    Mutate { source; principal; key; observed_at; mutation }
  | _ -> Error (invalid "unknown recovery note share journal operation")
;;

let shape =
  let module X = Persistence_codec in
  let value = Document_schema.Shape.value in
  let session = X.fields_shape [ "server_id"; "session_id" ] in
  let incarnation =
    X.shape_exn [ "session", session; "nonce", value; "generation", value ]
  in
  let reference = X.shape_exn [ "source", session; "nonce", value; "id", value ] in
  let mutation =
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ ( "create"
        , X.shape_exn
            [ "kind", value
            ; "recipient", incarnation
            ; "expected_revision", value
            ; "expected_catalog_revision", value
            ; "selection", X.fields_shape [ "id"; "revision" ]
            ; "expires_at", value
            ] )
      ; ( "revoke"
        , X.shape_exn
            [ "kind", value; "reference", reference; "expected_revision", value ] )
      ]
  in
  X.tagged_shape_exn
    ~discriminator:"kind"
    [ "establish", X.shape_exn [ "kind", value; "owner", session; "nonce", value ]
    ; ( "mutate"
      , X.shape_exn
          [ "kind", value
          ; "source", incarnation
          ; "principal", value
          ; "key", value
          ; "observed_at", value
          ; "mutation", mutation
          ] )
    ]
;;
