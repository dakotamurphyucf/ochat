open Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol
module S = Organization_state

let configuration_exn result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let limits = F.limits ~max_bytes:(16 * 1024 * 1024) |> configuration_exn
let kind = "host.organization"

let key_json (key : Idempotency_store.Key.t) =
  `Object
    [ "principal_id", P.Id.Principal.to_json key.principal_id
    ; "method_name", `String key.method_name
    ; "idempotency_key", P.Idempotency_key.to_json key.idempotency_key
    ]
;;

let receipt_id key =
  P.Json_codec.canonical_string (key_json key)
  |> F.protocol
  |> Result.map ~f:(fun encoded ->
    Digestif.SHA256.digest_string encoded |> Digestif.SHA256.to_hex)
;;

let key_decode json =
  let open Result.Let_syntax in
  let%bind principal_id =
    F.required json "principal_id" (fun j -> P.Id.Principal.of_json j |> F.protocol)
  in
  let%bind method_name = F.required json "method_name" F.string in
  let%map idempotency_key =
    F.required json "idempotency_key" (fun j -> P.Idempotency_key.of_json j |> F.protocol)
  in
  Idempotency_store.Key.{ principal_id; session_id = None; method_name; idempotency_key }
;;

let timestamp json = P.Timestamp.of_json json |> F.protocol

let nullable decode = function
  | `Null -> Ok None
  | value -> Result.map (decode value) ~f:Option.some
;;

let project_decode json =
  let open Result.Let_syntax in
  let%bind group =
    F.required json "group" (fun j ->
      P.Organization_group.Project.of_json j |> F.protocol)
  in
  let%bind id = F.required json "id" (fun j -> P.Id.Project.of_json j |> F.protocol) in
  let%bind deleted_at = F.required json "deleted_at" (nullable timestamp) in
  if P.Id.Project.equal id group.id
  then Ok S.Project_entry.{ group; deleted_at }
  else F.invalid "id" "project entry identity differs"
;;

let collection_decode json =
  let open Result.Let_syntax in
  let%bind group =
    F.required json "group" (fun j ->
      P.Organization_group.Collection.of_json j |> F.protocol)
  in
  let%bind id = F.required json "id" (fun j -> P.Id.Collection.of_json j |> F.protocol) in
  let%bind deleted_at = F.required json "deleted_at" (nullable timestamp) in
  if P.Id.Collection.equal id group.id
  then Ok S.Collection_entry.{ group; deleted_at }
  else F.invalid "id" "collection entry identity differs"
;;

let receipt_decode json =
  let open Result.Let_syntax in
  let%bind key = F.required json "key" key_decode in
  let%bind identity = F.required json "id" F.string in
  let%bind expected = receipt_id key in
  if not (String.equal identity expected)
  then F.invalid "id" "receipt identity differs"
  else (
    let%bind request_digest = F.required json "request_digest" F.string in
    let%bind result =
      F.required json "result" (fun j -> P.Organization_result.of_json j |> F.protocol)
    in
    let%bind created_at = F.required json "created_at" timestamp in
    let%map expires_at = F.required json "expires_at" timestamp in
    S.Receipt.{ key; request_digest; result; created_at; expires_at })
;;

let decode_list f json =
  Result.bind (F.array json) ~f:(fun values -> List.map values ~f |> Result.all)
;;

let decode json =
  let open Result.Let_syntax in
  let%bind server_id =
    F.required json "server_id" (fun j -> P.Id.Server.of_json j |> F.protocol)
  in
  let%bind revision = F.required json "revision" F.decimal in
  let%bind projects = F.required json "projects" (decode_list project_decode) in
  let%bind collections = F.required json "collections" (decode_list collection_decode) in
  let%bind receipts = F.required json "receipts" (decode_list receipt_decode) in
  S.restore ~server_id ~revision ~projects ~collections ~receipts |> F.protocol
;;

let encode_state state =
  let open Result.Let_syntax in
  let%bind _ =
    S.restore
      ~server_id:(S.server_id state)
      ~revision:(S.revision state)
      ~projects:(S.projects state)
      ~collections:(S.collections state)
      ~receipts:(S.receipts state)
    |> F.protocol
  in
  let%map receipts =
    List.map (S.receipts state) ~f:(fun r ->
      let%map id = receipt_id r.S.Receipt.key in
      `Object
        [ "id", `String id
        ; "key", key_json r.key
        ; "request_digest", `String r.request_digest
        ; "result", P.Organization_result.to_json r.result
        ; "created_at", P.Timestamp.to_json r.created_at
        ; "expires_at", P.Timestamp.to_json r.expires_at
        ])
    |> Result.all
  in
  `Object
    [ "server_id", P.Id.Server.to_json (S.server_id state)
    ; "revision", F.decimal_json (S.revision state)
    ; ( "projects"
      , `Array
          (List.map (S.projects state) ~f:(fun e ->
             `Object
               [ "id", P.Id.Project.to_json e.S.Project_entry.group.id
               ; "group", P.Organization_group.Project.to_json e.group
               ; "deleted_at", F.option_json e.deleted_at ~f:P.Timestamp.to_json
               ])) )
    ; ( "collections"
      , `Array
          (List.map (S.collections state) ~f:(fun e ->
             `Object
               [ "id", P.Id.Collection.to_json e.S.Collection_entry.group.id
               ; "group", P.Organization_group.Collection.to_json e.group
               ; "deleted_at", F.option_json e.deleted_at ~f:P.Timestamp.to_json
               ])) )
    ; "receipts", `Array receipts
    ]
;;

let group_shape =
  F.shape
    [ "id", D.Shape.value
    ; "creator_principal_id", D.Shape.value
    ; "name", D.Shape.value
    ; "revision", D.Shape.value
    ; "created_at", D.Shape.value
    ; "updated_at", D.Shape.value
    ]
;;

let entry_shape =
  F.shape [ "id", D.Shape.value; "group", group_shape; "deleted_at", D.Shape.value ]
;;

let deleted_shape =
  F.shape [ "id", D.Shape.value; "revision", D.Shape.value; "deleted_at", D.Shape.value ]
;;

let result_shape =
  D.Shape.tagged_object
    ~discriminator:"kind"
    (List.map
       [ "project.created"
       ; "project.updated"
       ; "project.deleted"
       ; "collection.created"
       ; "collection.updated"
       ; "collection.deleted"
       ]
       ~f:(fun tag ->
         ( tag
         , F.shape
             [ "kind", D.Shape.value
             ; ( "value"
               , if String.is_suffix tag ~suffix:".deleted"
                 then deleted_shape
                 else group_shape )
             ] )))
  |> configuration_exn
;;

let receipt_shape =
  F.shape
    [ "id", D.Shape.value
    ; ( "key"
      , F.shape
          [ "principal_id", D.Shape.value
          ; "method_name", D.Shape.value
          ; "idempotency_key", D.Shape.value
          ] )
    ; "request_digest", D.Shape.value
    ; "result", result_shape
    ; "created_at", D.Shape.value
    ; "expires_at", D.Shape.value
    ]
;;

let shape =
  F.shape
    [ "server_id", D.Shape.value
    ; "revision", D.Shape.value
    ; ( "projects"
      , D.Shape.array entry_shape ~identity_field:(Some "id") |> configuration_exn )
    ; ( "collections"
      , D.Shape.array entry_shape ~identity_field:(Some "id") |> configuration_exn )
    ; ( "receipts"
      , D.Shape.array receipt_shape ~identity_field:(Some "id") |> configuration_exn )
    ]
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~decode
    ~encode:encode_state
  |> configuration_exn
;;

let restore document = D.Domain_codec.decode codec document |> F.store
let encode carrier = D.Domain_codec.encode codec carrier |> F.store

let with_state carrier state ~now =
  let open Result.Let_syntax in
  let%bind () =
    S.validate_retention (D.Extension_carrier.value carrier) ~next_state:state ~now
    |> F.protocol
    |> F.store
  in
  match D.Extension_carrier.template carrier with
  | None -> Ok (D.Extension_carrier.with_value carrier state)
  | Some document ->
    let open Result.Let_syntax in
    (* A renewed expired key has the same stable ID but a fresh lifetime.
       Retire its prior template as well as expired IDs absent from next state. *)
    let next_receipts =
      Map.of_alist_exn
        (module Idempotency_store.Key)
        (List.map (S.receipts state) ~f:(fun receipt -> receipt.S.Receipt.key, receipt))
    in
    let retiring =
      List.filter
        (S.receipts (D.Extension_carrier.value carrier))
        ~f:(fun previous ->
          match Map.find next_receipts previous.S.Receipt.key with
          | Some next when S.Receipt.equal previous next -> false
          | Some _ | None -> true)
    in
    let%bind ids =
      List.map retiring ~f:(fun receipt -> receipt_id receipt.S.Receipt.key)
      |> Result.all
      |> F.store
    in
    let ids = String.Set.of_list ids in
    let payload =
      match D.Document.payload document with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if String.equal name "receipts"
             then
               ( name
               , match value with
                 | `Array values ->
                   `Array
                     (List.filter values ~f:(fun json ->
                        match D.Json.field json ~name:"id" with
                        | Value (`String id) -> not (Set.mem ids id)
                        | Absent | Null | Value _ -> true))
                 | json -> json )
             else name, value))
      | json -> json
    in
    let envelope =
      match D.Document.json document with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             name, if String.equal name "payload" then payload else value))
      | json -> json
    in
    let%bind document = D.Document.inspect ~limits envelope |> F.store in
    let%map restored = restore document in
    D.Extension_carrier.with_value restored state
;;
