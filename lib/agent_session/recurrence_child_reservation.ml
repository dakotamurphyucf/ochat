open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module F = Recurrence_definition
module I = Recurrence_identity

(* A single immutable fact. Its carrier owns all child JSON, including the exact
   selected nominal. A decoded fact deliberately has no issuer/permit field. *)
type projection =
  { nominal : Recurrence_nominal.t
  ; nominal_json : Jsonaf.t
  ; control_revision : int64
  ; principal_id : P.Id.Principal.t
  ; child_session_id : P.Id.Session.t
  ; transaction_id : P.Id.Transaction.t
  ; artifact_revision_id : P.Id.Prompt_revision.t
  ; manifest_sha256 : string
  ; target_sha256 : string
  ; source : P.Run_source.t
  ; birth : P.Session_lifecycle.Revision.t
  ; request_sha256 : string
  }

type t =
  { projection : projection
  ; json : Jsonaf.t
  }

let nominal t = t.projection.nominal
let nominal_json t = t.projection.nominal_json
let occurrence t = Recurrence_nominal.key (nominal t)
let control_revision t = t.projection.control_revision
let principal_id t = t.projection.principal_id
let child_session_id t = t.projection.child_session_id
let transaction_id t = t.projection.transaction_id
let artifact_revision_id t = t.projection.artifact_revision_id
let manifest_sha256 t = t.projection.manifest_sha256
let request_sha256 t = t.projection.request_sha256
let to_json t = t.json
let equal a b = D.Json.equal a.json b.json
let invalid message = Error (P.Error.invalid_request message)

let limits =
  D.Limits.create ~max_bytes:65536 ~max_depth:32 ~max_fields:512 ~max_nodes:1024
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let protocol r =
  Result.map_error r ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
;;

let digest json =
  let%map.Result canonical = P.Json_codec.canonical_string json in
  Digestif.SHA256.(digest_string canonical |> to_hex)
;;

let digest_of_json json =
  let%bind.Result text = P.Json_codec.string json in
  if
    String.length text = 64
    && String.for_all text ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  then Ok text
  else invalid "invalid recurrence creation digest"
;;

let request_json (p : projection) =
  `Object
    [ "kind", `String "fresh_child_reservation"
    ; "nominal", p.nominal_json
    ; "control_revision", X.int64_json p.control_revision
    ; "principal_id", P.Id.Principal.to_json p.principal_id
    ; "child_session_id", P.Id.Session.to_json p.child_session_id
    ; "transaction_id", P.Id.Transaction.to_json p.transaction_id
    ; "artifact_revision_id", P.Id.Prompt_revision.to_json p.artifact_revision_id
    ; "manifest_sha256", `String p.manifest_sha256
    ; "target_sha256", `String p.target_sha256
    ; "source", P.Run_source.to_json p.source
    ; ( "birth_lifecycle_revision"
      , X.int64_json (P.Session_lifecycle.Revision.to_int64 p.birth) )
    ]
;;

let encode p =
  match request_json p with
  | `Object fields -> `Object (fields @ [ "request_sha256", `String p.request_sha256 ])
  | _ -> assert false
;;

let decode json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind _ =
    X.required
      f
      "kind"
      (P.Json_codec.enum ~name:"recurrence creation" [ "fresh_child_reservation", () ])
  in
  let%bind nominal_json = X.required f "nominal" X.raw in
  let%bind nominal = Recurrence_nominal.of_json nominal_json in
  let%bind control_revision = X.required f "control_revision" X.nonnegative_int64 in
  let%bind principal_id = X.required f "principal_id" P.Id.Principal.of_json in
  let%bind child_session_id = X.required f "child_session_id" P.Id.Session.of_json in
  let%bind transaction_id = X.required f "transaction_id" P.Id.Transaction.of_json in
  let%bind artifact_revision_id =
    X.required f "artifact_revision_id" P.Id.Prompt_revision.of_json
  in
  let%bind manifest_sha256 = X.required f "manifest_sha256" digest_of_json in
  let%bind target_sha256 = X.required f "target_sha256" digest_of_json in
  let%bind source = X.required f "source" P.Run_source.of_json in
  let%bind birth =
    X.required f "birth_lifecycle_revision" (fun json ->
      let%bind n = X.nonnegative_int64 json in
      P.Session_lifecycle.Revision.of_int64 n)
  in
  let%bind request_sha256 = X.required f "request_sha256" digest_of_json in
  let p =
    { nominal
    ; nominal_json
    ; control_revision
    ; principal_id
    ; child_session_id
    ; transaction_id
    ; artifact_revision_id
    ; manifest_sha256
    ; target_sha256
    ; source
    ; birth
    ; request_sha256
    }
  in
  let%bind expected = digest (request_json p) in
  if String.equal expected request_sha256
  then Ok p
  else invalid "recurrence creation request digest mismatch"
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.recurrence_child_reservation"
    ~shape:
      (X.fields_shape
         [ "kind"
         ; "nominal"
         ; "control_revision"
         ; "principal_id"
         ; "child_session_id"
         ; "transaction_id"
         ; "artifact_revision_id"
         ; "manifest_sha256"
         ; "target_sha256"
         ; "source"
         ; "birth_lifecycle_revision"
         ; "request_sha256"
         ])
    ~decode
    ~encode:(fun p -> Ok (encode p))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate ~limits json |> protocol in
  let%bind document = X.inspect json |> protocol in
  let%map carrier = D.Domain_codec.decode codec document |> protocol in
  { projection = D.Extension_carrier.value carrier; json }
;;

let validate t ~definition =
  let open Result.Let_syntax in
  let p = t.projection in
  let%bind () = Recurrence_nominal.validate p.nominal ~definition in
  let%bind target_sha256 = Recurrence_target.digest (F.target definition) in
  match Recurrence_target.view (F.target definition) with
  | Existing_session _ ->
    invalid "creation reservation requires captured fresh-child target"
  | Fresh_child { artifact; definition = _; parent = _ } ->
    let artifact = Agent_store.Captured_prompt_artifact.value artifact in
    if
      String.equal p.target_sha256 target_sha256
      && P.Run_source.equal p.source (F.source definition)
      && P.Session_lifecycle.Revision.equal
           p.birth
           (F.birth_lifecycle_revision definition)
      && P.Id.Prompt_revision.equal p.artifact_revision_id artifact.revision_id
      && String.equal p.manifest_sha256 artifact.manifest_sha256
      && not
           (P.Id.Session.equal
              p.child_session_id
              (P.Session_ref.session_id (I.Definition.controller (F.key definition))))
    then Ok ()
    else invalid "creation reservation differs from captured target/source/artifact"
;;

let create
      ~definition
      ~control_revision
      ~principal_id
      ~child_session_id
      ~transaction_id
      ~nominal_json
  =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json principal_id) in
  let%bind _ = P.Id.Session.of_json (P.Id.Session.to_json child_session_id) in
  let%bind _ = P.Id.Transaction.of_json (P.Id.Transaction.to_json transaction_id) in
  let%bind () =
    if Int64.(control_revision >= 0L)
    then Ok ()
    else invalid "negative creation control revision"
  in
  let%bind nominal = Recurrence_nominal.of_json nominal_json in
  let%bind target_sha256 = Recurrence_target.digest (F.target definition) in
  match Recurrence_target.view (F.target definition) with
  | Existing_session _ ->
    invalid "creation reservation requires captured fresh-child target"
  | Fresh_child { artifact; definition = _; parent = _ } ->
    let artifact = Agent_store.Captured_prompt_artifact.value artifact in
    let p =
      { nominal
      ; nominal_json
      ; control_revision
      ; principal_id
      ; child_session_id
      ; transaction_id
      ; artifact_revision_id = artifact.revision_id
      ; manifest_sha256 = artifact.manifest_sha256
      ; target_sha256
      ; source = F.source definition
      ; birth = F.birth_lifecycle_revision definition
      ; request_sha256 = ""
      }
    in
    let%bind request_sha256 = digest (request_json p) in
    let p = { p with request_sha256 } in
    let%bind document =
      D.Domain_codec.encode codec (D.Extension_carrier.of_authored_value p) |> protocol
    in
    let%bind t = of_json (D.Document.json document) in
    let%map () = validate t ~definition in
    t
;;

module Association_capacity = struct
  let limits =
    D.Limits.create ~max_bytes:4096 ~max_depth:24 ~max_fields:64 ~max_nodes:128
    |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
    |> Result.ok_or_failwith
  ;;

  let validate json = D.Json.validate ~limits json |> protocol

  let reservation_projection t =
    `Object
      [ "reservation", to_json t
      ; ( "storage_link_growth"
        , `Object
            [ "bytes", `String (String.make (D.Limits.max_bytes limits) 'x')
            ; ( "fields"
              , `Object
                  (List.init (D.Limits.max_fields limits) ~f:(fun i ->
                     Int.to_string i, `Null)) )
            ; "nodes", `Array (List.init (D.Limits.max_nodes limits) ~f:(fun _ -> `Null))
            ] )
      ]
  ;;
end
