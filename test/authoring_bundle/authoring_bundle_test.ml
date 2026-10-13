open! Core
module P = Agent_protocol
module A = P.Authoring
module B = P.Authoring_bundle.Bundle
module W = A.Wire

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let check label actual =
  if not actual then raise_s [%sexp "bundle invariant failed", (label : string)]
;;

let source_id namespace path =
  Chatml_source.Id.create ~package_namespace:namespace ~relative_path:path
  |> Or_error.ok_exn
;;

let digest bytes = Chatml_source.Digest.of_contents bytes ~checkpoint:Fn.id

let member namespace path length =
  let source =
    A.Source.create
      ~id:(source_id namespace path)
      ~digest:(digest "fixed test bytes")
      ~byte_length:length
      ~editor_revision:None
    |> ok
  in
  B.Member.create ~source |> ok
;;

let root = source_id "pkg" "chatml.project.json"
let root_member = member "pkg" "chatml.project.json" 8
let script_member = member "pkg" "src/main.chatml" 12

let selection =
  A.Context.Selection.create
    ~target:(A.Context.Selection.Entry "main")
    ~environment:(Some "dev")
  |> ok
;;

let manifest members = B.Manifest.create ~project_source:root ~selection ~members

let%expect_test "manifest identity is canonical and rejects ambiguous inventories" =
  let forward = manifest [ root_member; script_member ] |> ok in
  let reverse = manifest [ script_member; root_member ] |> ok in
  check
    "canonical permutation"
    (Chatml_source.Digest.equal
       (B.Manifest.fingerprint forward)
       (B.Manifest.fingerprint reverse));
  let roundtrip = B.Manifest.of_json (B.Manifest.to_json reverse) |> ok in
  check "canonical roundtrip" (B.Manifest.equal forward roundtrip);
  let different_selection =
    A.Context.Selection.create
      ~target:(A.Context.Selection.Entry "main")
      ~environment:(Some "prod")
    |> ok
  in
  let different =
    B.Manifest.create
      ~project_source:root
      ~selection:different_selection
      ~members:[ root_member; script_member ]
    |> ok
  in
  check
    "selection changes fingerprint"
    (not
       (Chatml_source.Digest.equal
          (B.Manifest.fingerprint forward)
          (B.Manifest.fingerprint different)));
  let duplicate = manifest [ root_member; root_member ] in
  let case_collision =
    manifest [ root_member; member "pkg" "SRC/main.chatml" 1; script_member ]
  in
  let prefix_collision = manifest [ root_member; member "pkg" "src" 1; script_member ] in
  let different_namespace =
    manifest [ root_member; member "other" "chatml.project.json" 1 ]
  in
  let missing_root = manifest [ script_member ] in
  let oversized_declared =
    List.init 17 ~f:(fun index ->
      member
        "pkg"
        (if Int.equal index 0 then "chatml.project.json" else Int.to_string index ^ ".ml")
        B.Wire_limits.max_member_bytes)
    |> manifest
  in
  List.iter
    [ "duplicate", duplicate, false
    ; "case collision", case_collision, false
    ; "file-directory collision", prefix_collision, false
    ; "different namespaces", different_namespace, true
    ; "missing root", missing_root, false
    ; "declared total", oversized_declared, false
    ]
    ~f:(fun (label, result, expected) ->
      check label (Bool.equal (Result.is_ok result) expected));
  printf
    "canonical members: %d; selection changes identity\n"
    (List.length (B.Manifest.members forward));
  [%expect {| canonical members: 2; selection changes identity |}]
;;

let upload_id = P.Id.Authoring_upload.of_string "aupl_test" |> ok
let other_upload_id = P.Id.Authoring_upload.of_string "aupl_other" |> ok
let bundle_id = P.Id.Authoring_bundle.of_string "abun_test" |> ok
let host = P.Id.Server.of_string "srv_bundle" |> ok
let principal_id = P.Id.Principal.of_string "pri_bundle" |> ok
let other_principal_id = P.Id.Principal.of_string "pri_other" |> ok
let owner = B.Owner.create ~host ~principal:principal_id
let foreign_owner = B.Owner.create ~host ~principal:other_principal_id
let begin_key = P.Idempotency_key.of_string "begin-key" |> ok
let commit_key = P.Idempotency_key.of_string "commit-key" |> ok
let expiry = P.Timestamp.of_string "2026-10-14T00:00:00Z" |> ok

let receipt =
  B.Receipt.create
    ~owner
    ~upload_id
    ~bundle_id
    ~manifest_fingerprint:(B.Manifest.fingerprint (manifest [ root_member ] |> ok))
    ~project_source:root
    ~graph:(digest "graph")
    ~compiler_contract:(digest "contract")
    ~environment:"dev"
    ~expires_at:expiry
  |> ok
;;

let%expect_test "binary chunks and publication receipts preserve owner and bounds" =
  let bytes = "\000\255\r\n\128" in
  let chunk =
    B.Chunk.create_request ~upload_id ~source:root ~byte_offset:7 ~bytes |> ok
  in
  let decoded = B.Chunk.request_of_json (B.Chunk.request_to_json chunk) |> ok in
  check "binary bytes roundtrip" (String.equal bytes decoded.bytes);
  check
    "last admitted chunk byte"
    (Result.is_ok
       (B.Chunk.create_request
          ~upload_id
          ~source:root
          ~byte_offset:(B.Wire_limits.max_member_bytes - String.length bytes)
          ~bytes));
  check
    "chunk one byte past limit"
    (Result.is_error
       (B.Chunk.create_request
          ~upload_id
          ~source:root
          ~byte_offset:(B.Wire_limits.max_member_bytes - String.length bytes + 1)
          ~bytes));
  let begin_request =
    B.Begin.create_request
      ~idempotency_key:begin_key
      ~manifest:(manifest [ root_member ] |> ok)
      ()
  in
  check
    "begin key roundtrip"
    (P.Idempotency_key.equal
       begin_key
       (B.Begin.request_of_json (B.Begin.request_to_json begin_request) |> ok)
         .idempotency_key);
  let published = B.Status.create ~owner ~upload_id ~state:(Published receipt) |> ok in
  check
    "published status roundtrip"
    (Result.is_ok (B.Status.of_json (B.Status.to_json published)));
  let mismatched_wire_owner =
    match B.Status.to_json published with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           if String.equal name "owner"
           then name, B.Owner.to_json foreign_owner
           else name, value))
    | _ -> assert false
  in
  check "published wire owner" (Result.is_error (B.Status.of_json mismatched_wire_owner));
  check
    "published constructor owner"
    (Result.is_error
       (B.Status.create ~owner:foreign_owner ~upload_id ~state:(Published receipt)));
  check
    "published commit upload"
    (Result.is_error
       (B.Commit.create_response
          ~owner
          ~upload_id:other_upload_id
          ~idempotency_key:commit_key
          ~outcome:(Published receipt)));
  let progress = B.Status.Progress.create ~source:root ~accepted_until_byte:8 |> ok in
  check
    "duplicate progress"
    (Result.is_error
       (B.Status.create ~owner ~upload_id ~state:(Staging [ progress; progress ])));
  printf
    "binary payload: %d exact bytes; mismatched publication rejected\n"
    (String.length bytes);
  [%expect {| binary payload: 5 exact bytes; mismatched publication rejected |}]
;;

let%test_unit "all bundle requests and progress outcomes roundtrip canonically" =
  let canonical decoder encoder json = Result.map (decoder json) ~f:encoder in
  let roundtrip label json decoder =
    let encoded = decoder json |> ok in
    check label (String.equal (Jsonaf.to_string json) (Jsonaf.to_string encoded))
  in
  let manifest = manifest [ root_member ] |> ok in
  let begin_request = B.Begin.create_request ~idempotency_key:begin_key ~manifest () in
  let begin_response =
    B.Begin.create_response
      ~owner
      ~upload_id
      ~idempotency_key:begin_key
      ~manifest_fingerprint:(B.Manifest.fingerprint manifest)
      ~parent_context:None
      ~max_chunk_bytes:4096
      ~expires_at:expiry
    |> ok
  in
  let chunk_request =
    B.Chunk.create_request ~upload_id ~source:root ~byte_offset:0 ~bytes:"\000\255" |> ok
  in
  let chunk_response =
    B.Chunk.create_response ~owner ~upload_id ~source:root ~accepted_until_byte:2 |> ok
  in
  let commit_request = B.Commit.create_request ~upload_id ~idempotency_key:commit_key in
  let status_request = B.Status.create_request ~upload_id in
  let cases =
    [ "member", B.Member.to_json root_member, canonical B.Member.of_json B.Member.to_json
    ; ( "manifest"
      , B.Manifest.to_json manifest
      , canonical B.Manifest.of_json B.Manifest.to_json )
    ; "owner", B.Owner.to_json owner, canonical B.Owner.of_json B.Owner.to_json
    ; ( "begin request"
      , B.Begin.request_to_json begin_request
      , canonical B.Begin.request_of_json B.Begin.request_to_json )
    ; ( "begin response"
      , B.Begin.response_to_json begin_response
      , canonical B.Begin.response_of_json B.Begin.response_to_json )
    ; ( "chunk request"
      , B.Chunk.request_to_json chunk_request
      , canonical B.Chunk.request_of_json B.Chunk.request_to_json )
    ; ( "chunk response"
      , B.Chunk.response_to_json chunk_response
      , canonical B.Chunk.response_of_json B.Chunk.response_to_json )
    ; ( "commit request"
      , B.Commit.request_to_json commit_request
      , canonical B.Commit.request_of_json B.Commit.request_to_json )
    ; ( "status request"
      , B.Status.request_to_json status_request
      , canonical B.Status.request_of_json B.Status.request_to_json )
    ; "receipt", B.Receipt.to_json receipt, canonical B.Receipt.of_json B.Receipt.to_json
    ]
  in
  List.iter cases ~f:(fun (label, json, decoder) -> roundtrip label json decoder);
  let progress = B.Status.Progress.create ~source:root ~accepted_until_byte:2 |> ok in
  let statuses =
    [ "staging", B.Status.Staging [ progress ]
    ; "validating", B.Status.Validating
    ; "published", B.Status.Published receipt
    ; "rejected", B.Status.Rejected B.Rejection.Invalid_closure
    ; "expired", B.Status.Expired
    ]
  in
  List.iter statuses ~f:(fun (label, state) ->
    let value = B.Status.create ~owner ~upload_id ~state |> ok in
    roundtrip
      ("status " ^ label)
      (B.Status.to_json value)
      (canonical B.Status.of_json B.Status.to_json));
  let outcomes =
    [ "accepted", B.Commit.Accepted_for_validation
    ; "published", B.Commit.Published receipt
    ; "rejected", B.Commit.Rejected B.Rejection.Resource_limit
    ]
  in
  List.iter outcomes ~f:(fun (label, outcome) ->
    let value =
      B.Commit.create_response ~owner ~upload_id ~idempotency_key:commit_key ~outcome
      |> ok
    in
    roundtrip
      ("commit " ^ label)
      (B.Commit.response_to_json value)
      (canonical B.Commit.response_of_json B.Commit.response_to_json))
;;

let%expect_test "upload-only client rejects foreign and renegotiated replies" =
  Eio_main.run (fun _ ->
    let version = P.Version.create ~major:1 ~minor:0 |> ok in
    let implementation =
      P.Initialize.Implementation.create ~name:"test" ~version:"1" |> ok
    in
    let principal =
      P.Principal.create
        ~id:principal_id
        ~authentication_kind:"test"
        ~scopes:(Set.empty (module P.Scope))
        ~attributes:[]
      |> ok
    in
    let initialization =
      P.Initialize.Response.create
        ~protocol_name:"ochat.agent"
        ~selected_version:version
        ~implementation
        ~server_id:host
        ~enabled_features:[ A.Features.bundle ]
        ~extensions:None
        ~principal
        ~limits:
          { max_request_bytes = 16 * 1024 * 1024
          ; max_event_bytes = 1024
          ; max_page_size = 100
          ; max_attachments_per_connection = 1
          }
        ~event_retention:
          { minimum_age_ms = 0; maximum_events = 1; oldest_replayable_sequence = None }
        ~timing:
          { heartbeat_interval_ms = 1000
          ; owner_lease_duration_ms = 10000
          ; owner_renew_after_ms = 5000
          ; disconnect_grace_default_ms = 0
          }
        ~server_time:(P.Timestamp.of_string "2026-10-13T00:00:00Z" |> ok)
      |> ok
    in
    let current_initialization = ref initialization in
    let initialize_request =
      P.Initialize.Request.create
        ~implementation
        ~protocol_min:version
        ~protocol_max:version
        ~features:[ A.Features.bundle ]
        ~event_encodings:[ Json ]
        ~max_inbound_event_bytes:1024
        ()
      |> ok
    in
    let limits =
      A.Limits.create
        ~max_sources:1
        ~max_source_bytes:1024
        ~max_context_source_bytes:1024
        ~max_request_bytes:16384
        ~max_result_bytes:16384
        ~max_path_candidates:1
      |> ok
    in
    let capabilities =
      A.Capabilities.create
        ~methods:
          [ "authoring.capabilities"
          ; "authoring.bundle.begin"
          ; "authoring.bundle.chunk"
          ; "authoring.bundle.commit"
          ; "authoring.bundle.status"
          ]
        ~encodings:[]
        ~limits
      |> ok
    in
    check
      "core method requires source encoding"
      (Result.is_error
         (A.Capabilities.create
            ~methods:[ "authoring.capabilities"; "authoring.source.read" ]
            ~encodings:[]
            ~limits));
    let manifest = manifest [ root_member ] |> ok in
    let begin_request = B.Begin.create_request ~idempotency_key:begin_key ~manifest () in
    let begin_response =
      B.Begin.create_response
        ~owner
        ~upload_id
        ~idempotency_key:begin_key
        ~manifest_fingerprint:(B.Manifest.fingerprint manifest)
        ~parent_context:None
        ~max_chunk_bytes:4096
        ~expires_at:expiry
      |> ok
    in
    let begin_reply = ref begin_response in
    let chunk_reply =
      B.Chunk.create_response ~owner ~upload_id ~source:root ~accepted_until_byte:8 |> ok
    in
    let current_chunk_reply = ref chunk_reply in
    let commit_reply =
      B.Commit.create_response
        ~owner
        ~upload_id
        ~idempotency_key:commit_key
        ~outcome:Accepted_for_validation
      |> ok
    in
    let current_commit_reply = ref commit_reply in
    let status_reply = B.Status.create ~owner ~upload_id ~state:Validating |> ok in
    let current_status_reply = ref status_reply in
    let return response =
      P.Public.Result.Non_history
        (P.Public.Result.Non_history.of_internal (P.Method_result.Authoring response)
         |> ok)
      |> Result.return
    in
    let transport =
      Agent_client.Transport.create
        ~request:(function
          | P.Command.Protocol_initialize _ ->
            P.Public.Result.Non_history
              (P.Public.Result.Non_history.of_internal
                 (P.Method_result.Protocol_initialize !current_initialization)
               |> ok)
            |> Result.return
          | P.Command.Authoring W.Request.Capabilities ->
            return (W.Response.Capabilities capabilities)
          | P.Command.Authoring (W.Request.Bundle_begin _) ->
            return (W.Response.Bundle_begin !begin_reply)
          | P.Command.Authoring (W.Request.Bundle_chunk _) ->
            return (W.Response.Bundle_chunk !current_chunk_reply)
          | P.Command.Authoring (W.Request.Bundle_commit _) ->
            return (W.Response.Bundle_commit !current_commit_reply)
          | P.Command.Authoring (W.Request.Bundle_status _) ->
            return (W.Response.Bundle_status !current_status_reply)
          | _ -> Error (P.Error.invalid_request "unexpected mock request"))
        ~next_notification:(fun () -> None)
        ~close:Fn.id
    in
    let connection = Agent_client.Connection.create transport in
    ignore
      (Agent_client.Connection.request
         connection
         (P.Command.Protocol_initialize initialize_request)
       |> ok);
    check
      "bundle-only negotiation excludes core client"
      (Result.is_error (Agent_client.Authoring.create connection));
    let client = Agent_client.Authoring_bundle.create connection |> ok in
    check
      "begin accepted"
      (Result.is_ok (Agent_client.Authoring_bundle.begin_upload client begin_request));
    begin_reply
    := B.Begin.create_response
         ~owner:foreign_owner
         ~upload_id
         ~idempotency_key:begin_key
         ~manifest_fingerprint:(B.Manifest.fingerprint manifest)
         ~parent_context:None
         ~max_chunk_bytes:4096
         ~expires_at:expiry
       |> ok;
    check
      "foreign begin owner rejected"
      (Result.is_error (Agent_client.Authoring_bundle.begin_upload client begin_request));
    begin_reply
    := B.Begin.create_response
         ~owner
         ~upload_id
         ~idempotency_key:begin_key
         ~manifest_fingerprint:(digest "different manifest")
         ~parent_context:None
         ~max_chunk_bytes:4096
         ~expires_at:expiry
       |> ok;
    check
      "wrong manifest fingerprint rejected"
      (Result.is_error (Agent_client.Authoring_bundle.begin_upload client begin_request));
    let chunk_request =
      B.Chunk.create_request ~upload_id ~source:root ~byte_offset:0 ~bytes:"1234" |> ok
    in
    check
      "chunk accepted"
      (Result.is_ok (Agent_client.Authoring_bundle.upload_chunk client chunk_request));
    current_chunk_reply
    := B.Chunk.create_response
         ~owner
         ~upload_id
         ~source:(source_id "pkg" "other.chatml")
         ~accepted_until_byte:8
       |> ok;
    check
      "wrong chunk source rejected"
      (Result.is_error (Agent_client.Authoring_bundle.upload_chunk client chunk_request));
    let commit_request = B.Commit.create_request ~upload_id ~idempotency_key:commit_key in
    check
      "commit accepted"
      (Result.is_ok (Agent_client.Authoring_bundle.commit client commit_request));
    current_commit_reply
    := B.Commit.create_response
         ~owner
         ~upload_id
         ~idempotency_key:begin_key
         ~outcome:Accepted_for_validation
       |> ok;
    check
      "wrong commit key rejected"
      (Result.is_error (Agent_client.Authoring_bundle.commit client commit_request));
    check
      "status accepted"
      (Result.is_ok
         (Agent_client.Authoring_bundle.status
            client
            (B.Status.create_request ~upload_id)));
    current_status_reply
    := B.Status.create ~owner ~upload_id:other_upload_id ~state:Validating |> ok;
    check
      "wrong status upload rejected"
      (Result.is_error
         (Agent_client.Authoring_bundle.status
            client
            (B.Status.create_request ~upload_id)));
    current_initialization := { initialization with enabled_features = [] };
    ignore
      (Agent_client.Connection.request
         connection
         (P.Command.Protocol_initialize { initialize_request with features = [] })
       |> ok);
    check
      "renegotiated feature rejects old client"
      (Result.is_error (Agent_client.Authoring_bundle.begin_upload client begin_request)));
  [%expect {| |}]
;;
