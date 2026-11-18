open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module R = Recurrence_child_reservation
module Store = Agent_store.Delegation_store

type t =
  { occurrence : Recurrence_identity.Occurrence.t
  ; request_sha256 : string
  ; reference : Store.Reference.t
  ; json : Jsonaf.t
  }

let occurrence t = t.occurrence
let reference t = t.reference
let equal a b = D.Json.equal a.json b.json
let to_json t = t.json
let invalid message = Error (P.Error.invalid_request message)

let store_result result =
  Result.map_error result ~f:Agent_store.Store_error.to_protocol_error
;;

let validate t ~reservation =
  let open Result.Let_syntax in
  let%bind key = R.delegation_key reservation in
  let%bind () = Store.validate_reference t.reference |> store_result in
  if
    Recurrence_identity.Occurrence.equal t.occurrence (R.occurrence reservation)
    && String.equal t.request_sha256 (R.request_sha256 reservation)
    && Store.Key.equal t.reference.key key
    && P.Id.Session.equal t.reference.child_session_id (R.child_session_id reservation)
    && P.Id.Prompt_revision.equal
         t.reference.revision_id
         (R.artifact_revision_id reservation)
    && String.equal t.reference.request_sha256 t.request_sha256
  then Ok ()
  else invalid "child association differs from immutable reservation"
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = R.Association_capacity.validate json in
  let%bind fields = X.object_ json in
  let%bind () =
    X.required
      fields
      "kind"
      (P.Json_codec.enum ~name:"child association" [ "captured_child_reference", () ])
  in
  let%bind occurrence =
    X.required fields "occurrence" Recurrence_identity.Occurrence.of_json
  in
  let%bind request_sha256 = X.required fields "request_sha256" P.Json_codec.string in
  let%bind reference =
    X.required fields "reference" (fun json ->
      Store.reference_of_jsonaf json |> store_result)
  in
  if String.equal request_sha256 reference.request_sha256
  then Ok { occurrence; request_sha256; reference; json }
  else invalid "child association request differs from its reference"
;;

let create ~reservation ~reference =
  let open Result.Let_syntax in
  let json =
    `Object
      [ "kind", `String "captured_child_reference"
      ; "occurrence", Recurrence_identity.Occurrence.to_json (R.occurrence reservation)
      ; "request_sha256", `String (R.request_sha256 reservation)
      ; "reference", Store.reference_to_jsonaf reference
      ]
  in
  let%bind t = of_json json in
  let%map () = validate t ~reservation in
  t
;;
