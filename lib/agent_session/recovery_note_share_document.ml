open! Core
include Recovery_note_share_value
module D = Document_schema

type grant =
  { id : Revision.t
  ; source_generation : int
  ; recipient : Incarnation.t
  ; selection : C.selection
  ; issued_at : P.Timestamp.t
  ; expires_at : P.Timestamp.t
  }

type recorded_receipt =
  { principal : P.Id.Principal.t
  ; key : P.Idempotency_key.t
  ; digest : string
  ; grant_id : Revision.t
  ; revision : Revision.t
  }

type state =
  { owner : P.Session_ref.t
  ; nonce : P.Id.Session_incarnation.t
  ; revision : Revision.t
  ; grants : grant list
  ; receipts : recorded_receipt list
  }

type t = state D.Extension_carrier.t

let max_grants = 128
let max_receipts = 128
let owner t = (D.Extension_carrier.value t).owner
let revision t = (D.Extension_carrier.value t).revision

let grant_to_json (grant : grant) =
  `Object
    [ "id", counter_to_json grant.id
    ; "source_generation", `Number (Int.to_string grant.source_generation)
    ; "recipient", Incarnation.to_json grant.recipient
    ; "selection", selection_to_json grant.selection
    ; "issued_at", P.Timestamp.to_json grant.issued_at
    ; "expires_at", P.Timestamp.to_json grant.expires_at
    ]
;;

let grant_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind id = J.required_as fields "id" (counter_of_json Revision.of_int64) in
  let%bind source_generation =
    J.required_as fields "source_generation" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind recipient = J.required_as fields "recipient" Incarnation.of_json in
  let%bind selection = J.required_as fields "selection" selection_of_json in
  let%bind issued_at = J.required_as fields "issued_at" P.Timestamp.of_json in
  let%map expires_at = J.required_as fields "expires_at" P.Timestamp.of_json in
  { id; source_generation; recipient; selection; issued_at; expires_at }
;;

let receipt_to_json (receipt : recorded_receipt) =
  `Object
    [ "principal", P.Id.Principal.to_json receipt.principal
    ; "key", P.Idempotency_key.to_json receipt.key
    ; "digest", `String receipt.digest
    ; "grant_id", counter_to_json receipt.grant_id
    ; "revision", counter_to_json receipt.revision
    ]
;;

let receipt_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind principal = J.required_as fields "principal" P.Id.Principal.of_json in
  let%bind key = J.required_as fields "key" P.Idempotency_key.of_json in
  let%bind digest = J.required_as fields "digest" J.string in
  let%bind grant_id =
    J.required_as fields "grant_id" (counter_of_json Revision.of_int64)
  in
  let%map revision =
    J.required_as fields "revision" (counter_of_json Revision.of_int64)
  in
  { principal; key; digest; grant_id; revision }
;;

let state_to_json state =
  `Object
    [ "owner", P.Session_ref.to_json state.owner
    ; "nonce", P.Id.Session_incarnation.to_json state.nonce
    ; "revision", counter_to_json state.revision
    ; "grants", `Array (List.map state.grants ~f:grant_to_json)
    ; "receipts", `Array (List.map state.receipts ~f:receipt_to_json)
    ]
;;

let bounded_list max_length decode = function
  | `Array values when Stdlib.List.compare_length_with values max_length <= 0 ->
    Result.all (List.map values ~f:decode)
  | _ -> Error (invalid "invalid recovery note share record count")
;;

let state_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind owner = J.required_as fields "owner" P.Session_ref.of_json in
  let%bind nonce = J.required_as fields "nonce" P.Id.Session_incarnation.of_json in
  let%bind revision =
    J.required_as fields "revision" (counter_of_json Revision.of_int64)
  in
  let%bind grants =
    J.required_as fields "grants" (bounded_list max_grants grant_of_json)
  in
  let%map receipts =
    J.required_as fields "receipts" (bounded_list max_receipts receipt_of_json)
  in
  { owner; nonce; revision; grants; receipts }
;;

let valid_digest digest =
  String.length digest = 64
  && String.for_all digest ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let receipt_identity ~principal ~key =
  P.Id.Principal.to_string principal ^ "\000" ^ P.Idempotency_key.to_string key
;;

let validate_state state =
  let open Result.Let_syntax in
  let valid_grant (grant : grant) =
    Int64.(grant.id > 0L && grant.id <= state.revision)
    && recipient_matches_owner state.owner grant.recipient
    && valid_ttl ~now:grant.issued_at grant.expires_at
  in
  let%bind () =
    if
      Stdlib.List.compare_length_with state.grants max_grants <= 0
      && List.for_all state.grants ~f:valid_grant
      && not
           (List.contains_dup state.grants ~compare:(fun a b ->
              Revision.compare a.id b.id))
    then Ok ()
    else Error (invalid "invalid recovery note share grants")
  in
  let retained = Int64.(min (of_int max_receipts) state.revision |> to_int_exn) in
  if not (Int.equal (List.length state.receipts) retained)
  then Error (invalid "invalid recovery note share receipt count")
  else
    List.foldi
      state.receipts
      ~init:(Ok String.Set.empty)
      ~f:(fun index identities receipt ->
        let%bind identities = identities in
        let identity = receipt_identity ~principal:receipt.principal ~key:receipt.key in
        if
          Int64.(receipt.revision = state.revision - of_int index)
          && Int64.(receipt.grant_id > 0L && receipt.grant_id <= receipt.revision)
          && valid_digest receipt.digest
          && not (Set.mem identities identity)
        then Ok (Set.add identities identity)
        else Error (invalid "invalid recovery note share receipt"))
    |> Result.map ~f:ignore
;;

let document_error (error : P.Error.t) =
  D.Error.Invalid_field { path = []; reason = error.message }
;;

let schema_exn = function
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (D.Error.sexp_of_t error))
;;

let limits =
  schema_exn
    (D.Limits.create
       ~max_bytes:(512 * 1024)
       ~max_depth:32
       ~max_fields:8192
       ~max_nodes:16384)
;;

let codec =
  let v = D.Shape.value in
  let obj fields = schema_exn (D.Shape.object_ fields) in
  let fields names = obj (List.map names ~f:(fun name -> name, v)) in
  let array field element =
    schema_exn (D.Shape.array element ~identity_field:(Some field))
  in
  let session = fields [ "server_id"; "session_id" ] in
  let shape =
    obj
      [ "owner", session
      ; "nonce", v
      ; "revision", v
      ; ( "grants"
        , array
            "id"
            (obj
               [ "id", v
               ; "source_generation", v
               ; "recipient", obj [ "session", session; "nonce", v; "generation", v ]
               ; "selection", fields [ "id"; "revision" ]
               ; "issued_at", v
               ; "expires_at", v
               ]) )
      ; ( "receipts"
        , array
            "revision"
            (fields [ "principal"; "key"; "digest"; "grant_id"; "revision" ]) )
      ]
  in
  schema_exn
    (D.Domain_codec.create_validated
       ~limits
       ~kind:"ochat.recovery-note-shares"
       ~version:1
       ~shape
       ~supported_semantics:[]
       ~validate:(fun state -> Result.map_error (validate_state state) ~f:document_error)
       ~decode:(fun json ->
         let decoded =
           let open Result.Let_syntax in
           let%bind state = state_of_json json in
           let%map () = validate_state state in
           state
         in
         Result.map_error decoded ~f:document_error)
       ~encode:(fun state -> Ok (state_to_json state)))
;;

let document_result result =
  Result.map_error result ~f:(fun _ ->
    invalid "invalid durable recovery note share ledger")
;;

let to_json t =
  D.Domain_codec.encode codec t |> document_result |> Result.map ~f:D.Document.json
;;

let of_json json ~session_id =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits json |> document_result in
  let%bind ledger = D.Domain_codec.decode codec document |> document_result in
  if P.Id.Session.equal (P.Session_ref.session_id (owner ledger)) session_id
  then Ok ledger
  else Error (invalid "recovery note share ledger belongs to another session")
;;

let validate_encoded json = D.Document.inspect ~limits json |> Result.map ~f:ignore

let rows_by_id payload field identity =
  let open Result.Let_syntax in
  let%bind rows = J.required_as payload field (J.list Result.return) in
  List.fold_result rows ~init:String.Map.empty ~f:(fun map row ->
    let%bind fields = J.fields row in
    let%map id = J.required_as fields identity J.string in
    Map.set map ~key:id ~data:row)
;;

let retain_rows new_rows previous ~identity =
  let open Result.Let_syntax in
  List.fold_result new_rows ~init:[] ~f:(fun rows row ->
    let%bind fields = J.fields row in
    let%map id = J.required_as fields identity J.string in
    let raw = Map.find previous id |> Option.value ~default:row in
    raw :: rows)
  |> Result.map ~f:(fun rows -> `Array (List.rev rows))
;;

(* Ledger rows are immutable. A sanctioned revocation, expiry cleanup or old
   receipt eviction removes the whole row. Preserve the exact raw rows for
   surviving identities, plus all unrelated payload and envelope extensions. *)
let retire_rows template next =
  let open Result.Let_syntax in
  let%bind payload = J.fields (D.Document.payload template) in
  let%bind envelope = J.fields (D.Document.json template) in
  let%bind previous_grants = rows_by_id payload "grants" "id" in
  let%bind previous_receipts = rows_by_id payload "receipts" "revision" in
  let%bind grants =
    retain_rows (List.map next.grants ~f:grant_to_json) previous_grants ~identity:"id"
  in
  let%bind receipts =
    retain_rows
      (List.map next.receipts ~f:receipt_to_json)
      previous_receipts
      ~identity:"revision"
  in
  let updated =
    J.to_alist payload
    |> List.map ~f:(function
      | "grants", _ -> "grants", grants
      | "receipts", _ -> "receipts", receipts
      | "revision", _ -> "revision", counter_to_json next.revision
      | unchanged -> unchanged)
  in
  let envelope =
    List.map (J.to_alist envelope) ~f:(function
      | "payload", _ -> "payload", `Object updated
      | unchanged -> unchanged)
  in
  let%bind document = D.Document.inspect ~limits (`Object envelope) |> document_result in
  D.Domain_codec.decode codec document |> document_result
;;

let with_state current next =
  let candidate = D.Extension_carrier.with_value current next in
  match D.Domain_codec.encode codec candidate with
  | Ok _ -> Ok candidate
  | Error (D.Error.Extension_conflict _) ->
    let%bind.Result template =
      Result.of_option
        (D.Extension_carrier.template current)
        ~error:(invalid "missing recovery note share extension carrier")
    in
    retire_rows template next
  | Error error -> document_result (Error error)
;;
