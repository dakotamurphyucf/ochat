open! Core
module M = Credential_registry_model

let ok = function
  | Ok value -> value
  | Error error -> raise_s (M.Error.sexp_of_t error)
;;

let id value = M.Id.create value |> ok
let host = id "host_registry_test"
let binding = id "binding_account_a"

let identity =
  M.Identity.api_key
    ~host
    ~provider:"openai"
    ~billing:"api"
    ~account:(Some "account_a")
    ~key_reference:(id "key_account_a")
  |> ok
;;

let initial () = M.initialize ~incarnation:(id "incarnation_a") ~host |> ok

let install registry operation =
  let registry, expected =
    M.begin_candidate
      registry
      ~binding
      ~operation
      ~expectation:(M.Expectation.exact identity)
    |> ok
  in
  let registry =
    M.stage_candidate registry ~binding ~expected ~revision:operation |> ok
  in
  M.commit_candidate
    registry
    ~binding
    ~expected
    ~identity
    ~source:(Protected_revision operation)
    ~grant:None
  |> ok
;;

let%expect_test "failed replacement preserves working active identity and revision" =
  let registry = install (initial ()) (id "login_a") in
  let registry, candidate =
    M.begin_candidate
      registry
      ~binding
      ~operation:(id "replacement_a")
      ~expectation:(M.Expectation.exact identity)
    |> ok
  in
  let registry =
    M.stage_candidate registry ~binding ~expected:candidate ~revision:(id "replacement_a")
    |> ok
  in
  let registry = M.discard_candidate registry ~binding ~expected:candidate |> ok in
  let snapshot = M.find registry ~binding |> ok in
  let active = M.Snapshot.active snapshot |> Option.value_exn in
  assert (Int64.equal (M.Epoch.to_int64 (M.Active.epoch active)) 1L);
  (match M.Active.source active with
   | Protected_revision revision -> assert (M.Id.equal revision (id "login_a"))
   | Environment_reference _ -> failwith "unexpected source");
  assert (Int.equal (List.length (M.retired registry ~binding |> ok)) 1);
  print_endline "active login retained; only rejected candidate retired";
  [%expect {| active login retained; only rejected candidate retired |}]
;;

let%expect_test "logout tombstone rejects late candidate publication" =
  let registry = install (initial ()) (id "login_a") in
  let registry, expected =
    M.begin_candidate
      registry
      ~binding
      ~operation:(id "replacement_a")
      ~expectation:(M.Expectation.exact identity)
    |> ok
  in
  let registry =
    M.stage_candidate registry ~binding ~expected ~revision:(id "replacement_a") |> ok
  in
  let registry =
    M.disable registry ~binding ~operation:(id "logout_a") ~reason:Logout |> ok
  in
  assert (Option.is_none (M.Snapshot.active (M.find registry ~binding |> ok)));
  assert (
    Int64.equal (M.Epoch.to_int64 (M.Snapshot.epoch (M.find registry ~binding |> ok))) 2L);
  (match
     M.commit_candidate
       registry
       ~binding
       ~expected
       ~identity
       ~source:(Protected_revision (id "replacement_a"))
       ~grant:None
   with
   | Error Stale_epoch -> ()
   | _ -> failwith "late login must lose to tombstone");
  let operation = M.operation registry ~binding ~operation:(id "logout_a") |> ok in
  (match M.Operation.result operation with
   | Committed -> ()
   | _ -> failwith "missing logout receipt");
  print_endline "disabled epoch authoritative; late revision cannot resurrect";
  [%expect {| disabled epoch authoritative; late revision cannot resurrect |}]
;;

let%expect_test "metadata unknown fields survive candidate and tombstone edits" =
  let document = M.to_document (initial ()) |> ok in
  let json =
    match Document_schema.Document.json document with
    | `Object fields ->
      `Object
        (List.Assoc.add
           fields
           ~equal:String.equal
           "unknown_future"
           (`Object [ "retained", `True ]))
    | _ -> failwith "expected object"
  in
  let document =
    Document_schema.Document.inspect ~limits:Document_schema.Limits.default json
    |> Result.map_error ~f:(fun _ -> M.Error.Invalid_document)
    |> ok
  in
  let registry = M.of_document document |> ok in
  let registry = install registry (id "login_a") in
  let registry =
    M.disable registry ~binding ~operation:(id "logout_a") ~reason:Logout |> ok
  in
  let roundtrip = M.to_document registry |> ok |> Document_schema.Document.json in
  (match Document_schema.Json.field roundtrip ~name:"unknown_future" with
   | Value value ->
     assert (Document_schema.Json.equal value (`Object [ "retained", `True ]))
   | Absent | Null -> failwith "lost unknown field");
  print_endline "unknown universal envelope retained through transitions";
  [%expect {| unknown universal envelope retained through transitions |}]
;;

let%expect_test "revision reservation belongs to one binding globally" =
  let registry = install (initial ()) (id "login_a") in
  let other = id "binding_account_b" in
  (match
     M.begin_candidate
       registry
       ~binding:other
       ~operation:(id "login_a")
       ~expectation:(M.Expectation.exact identity)
   with
   | Error Stale_revision -> ()
   | _ -> failwith "cross-binding active revision must never reserve a candidate");
  print_endline "global reservation rejects collision before backend effects";
  [%expect {| global reservation rejects collision before backend effects |}]
;;

let%expect_test "reenrollment waits for persisted drain and owned cleanup" =
  let registry = install (initial ()) (id "login_a") in
  let logout = id "logout_a" in
  let registry = M.disable registry ~binding ~operation:logout ~reason:Logout |> ok in
  let registry = M.to_document registry |> ok |> M.of_document |> ok in
  let removal = M.removal registry ~binding |> ok |> Option.value_exn in
  (match M.Removal.drain removal with
   | Pending -> ()
   | Drained -> failwith "restart invented drain");
  let replacement = id "login_b" in
  let registry, expected =
    M.begin_candidate
      registry
      ~binding
      ~operation:replacement
      ~expectation:(M.Expectation.exact identity)
    |> ok
  in
  let registry =
    M.stage_candidate registry ~binding ~expected ~revision:replacement |> ok
  in
  let commit registry =
    M.commit_candidate
      registry
      ~binding
      ~expected
      ~identity
      ~source:(Protected_revision replacement)
      ~grant:None
  in
  (match commit registry with
   | Error Stale_operation -> ()
   | _ -> failwith "pending drain admitted login");
  let registry =
    M.record_removal
      registry
      ~binding
      ~operation:logout
      ~drain:Drained
      ~revocation:Not_requested
    |> ok
  in
  (match commit registry with
   | Error Stale_operation -> ()
   | _ -> failwith "pending cleanup admitted login");
  let registry =
    M.record_cleanup
      registry
      ~binding
      ~operation:(id "login_a")
      ~revision:(id "login_a")
      ~deletion:Confirmed_removed
    |> ok
  in
  let registry = commit registry |> ok in
  let registry =
    M.disable registry ~binding ~operation:(id "logout_b") ~reason:Logout |> ok
  in
  let removal = M.removal registry ~binding |> ok |> Option.value_exn in
  assert (M.Id.equal (M.Removal.operation removal) (id "logout_b"));
  print_endline
    "restart retains pending drain; reenrollment waits; next tombstone is distinct";
  [%expect
    {| restart retains pending drain; reenrollment waits; next tombstone is distinct |}]
;;

let%expect_test "optional remote uncertainty never blocks subsequent local disable" =
  let registry = install (initial ()) (id "login_a") in
  let logout = id "logout_a" in
  let registry = M.disable registry ~binding ~operation:logout ~reason:Logout |> ok in
  let registry =
    M.record_removal
      registry
      ~binding
      ~operation:logout
      ~drain:Drained
      ~revocation:Possibly_sent
    |> ok
  in
  let registry =
    M.record_cleanup
      registry
      ~binding
      ~operation:(id "login_a")
      ~revision:(id "login_a")
      ~deletion:Confirmed_removed
    |> ok
  in
  let registry = install registry (id "login_b") in
  let registry =
    M.disable registry ~binding ~operation:(id "logout_b") ~reason:Logout |> ok
  in
  assert (Option.is_none (M.Snapshot.active (M.find registry ~binding |> ok)));
  let history = M.revocation_history registry ~binding |> ok in
  assert (Int.equal (List.length history) 1);
  let original = List.hd_exn history in
  assert (M.Id.equal (M.Removal.operation original) logout);
  (match M.Removal.revocation original with
   | Possibly_sent -> ()
   | _ -> failwith "false remote resolution");
  print_endline "new local tombstone succeeds; prior remote uncertainty stays nonsecret";
  [%expect {| new local tombstone succeeds; prior remote uncertainty stays nonsecret |}]
;;

let%expect_test "terminal receipt rollover permits repeated login and logout" =
  let registry = ref (initial ()) in
  for cycle = 1 to 80 do
    let login = id (sprintf "login_%d" cycle) in
    let logout = id (sprintf "logout_%d" cycle) in
    registry := install !registry login;
    registry := M.disable !registry ~binding ~operation:logout ~reason:Logout |> ok;
    registry
    := M.record_removal
         !registry
         ~binding
         ~operation:logout
         ~drain:Drained
         ~revocation:Not_requested
       |> ok;
    registry
    := M.record_cleanup
         !registry
         ~binding
         ~operation:login
         ~revision:login
         ~deletion:Confirmed_removed
       |> ok
  done;
  assert (List.is_empty (M.retired !registry ~binding |> ok));
  (match
     M.operation !registry ~binding ~operation:(id "logout_80")
     |> ok
     |> M.Operation.result
   with
   | Committed -> ()
   | _ -> failwith "lost newest tombstone proof");
  print_endline "160 terminal operations rollover; newest removal proof retained";
  [%expect {| 160 terminal operations rollover; newest removal proof retained |}]
;;

let%expect_test "restart explicit original-operation cancellation preserves active login" =
  let registry = install (initial ()) (id "login_a") in
  let original = id "candidate_after_crash" in
  let registry, expected =
    M.begin_candidate
      registry
      ~binding
      ~operation:original
      ~expectation:(M.Expectation.exact identity)
    |> ok
  in
  let registry = M.stage_candidate registry ~binding ~expected ~revision:original |> ok in
  let registry = M.to_document registry |> ok |> M.of_document |> ok in
  assert (
    Option.equal
      M.Id.equal
      (M.pending_candidate_operation registry ~binding |> ok)
      (Some original));
  (match
     M.cancel_pending_candidate registry ~binding ~operation:(id "unrelated_flow")
   with
   | Error Stale_operation -> ()
   | _ -> failwith "unrelated operation cancelled intent");
  let registry = M.cancel_pending_candidate registry ~binding ~operation:original |> ok in
  let active = M.find registry ~binding |> ok |> M.Snapshot.active |> Option.value_exn in
  (match M.Active.source active with
   | Protected_revision revision -> assert (M.Id.equal revision (id "login_a"))
   | _ -> failwith "active source changed");
  let retired = M.retired registry ~binding |> ok |> List.hd_exn in
  assert (M.Id.equal (M.Cleanup.owned_by retired) original);
  print_endline
    "original pending identity recovered; exact cancel retires only owned candidate";
  [%expect
    {| original pending identity recovered; exact cancel retires only owned candidate |}]
;;

let%expect_test
    "raw grant presence agrees with explicit effective verified scope and expiry"
  =
  let oauth =
    M.Identity.oauth
      ~host
      ~provider:"synthetic"
      ~billing:"subscription"
      ~issuer:"https://issuer.example"
      ~client_registration:"client_a"
      ~resource:"https://resource.example"
      ~account:"account_a"
      ~verified_subject:"subject_a"
      ~required_scopes:[ "inference" ]
    |> ok
  in
  let effective : M.Grant.effective =
    { scopes = [ "inference" ]
    ; scopes_provenance = Declared
    ; expiry = Known { at_ms = 123L; provenance = Declared }
    ; unknown_expiry_policy = Reject_unknown
    }
  in
  let grant scopes expires_at_ms effective =
    M.Grant.create
      ~identity:oauth
      ~scopes
      ~expires_at_ms
      ~refresh_policy:Require_rotated
      ~effective
  in
  let reject result =
    match result with
    | Error M.Error.Invalid_grant -> ()
    | _ -> failwith "contradictory grant accepted"
  in
  ignore (grant (Value [ "inference" ]) (Value 123L) effective |> ok : M.Grant.t);
  reject (grant (Value [ "inference"; "inference" ]) (Value 123L) effective);
  reject (grant (Value [ "other" ]) (Value 123L) effective);
  reject (grant (Value [ "inference" ]) (Value 124L) effective);
  reject (grant Absent (Value 123L) effective);
  reject (grant (Value [ "inference" ]) Null effective);
  let preserved : M.Grant.effective =
    { effective with
      scopes_provenance = Qualified_prior_exact
    ; expiry = Known { at_ms = 123L; provenance = Qualified_prior_exact }
    }
  in
  ignore (grant Absent Null preserved |> ok : M.Grant.t);
  let unknown : M.Grant.effective =
    { effective with
      scopes_provenance = Qualified_request
    ; expiry = Unknown
    ; unknown_expiry_policy = Reject_unknown
    }
  in
  ignore (grant Null Absent unknown |> ok : M.Grant.t);
  reject (grant (Value [ "inference" ]) (Value 123L) { effective with scopes = [] });
  print_endline
    "raw value consistency and required scopes enforced; omitted/null need explicit \
     provenance and expiry policy";
  [%expect
    {| raw value consistency and required scopes enforced; omitted/null need explicit provenance and expiry policy |}]
;;

let%expect_test
    "full secret capacity reserves candidate and rotating revision before external \
     effects"
  =
  let oauth =
    M.Identity.oauth
      ~host
      ~provider:"synthetic"
      ~billing:"subscription"
      ~issuer:"https://issuer.example"
      ~client_registration:"client_a"
      ~resource:"https://resource.example"
      ~account:"account_a"
      ~verified_subject:"subject_a"
      ~required_scopes:[ "inference" ]
    |> ok
  in
  let grant =
    M.Grant.create
      ~identity:oauth
      ~scopes:(Value [ "inference" ])
      ~expires_at_ms:(Value 0L)
      ~refresh_policy:Require_rotated
      ~effective:
        { scopes = [ "inference" ]
        ; scopes_provenance = Declared
        ; expiry = Known { at_ms = 0L; provenance = Declared }
        ; unknown_expiry_policy = Reject_unknown
        }
    |> ok
  in
  let registry = ref (initial ()) in
  for iteration = 1 to 64 do
    let operation = id (sprintf "revision_%d" iteration) in
    let next, expected =
      M.begin_candidate
        !registry
        ~binding
        ~operation
        ~expectation:(M.Expectation.exact oauth)
      |> ok
    in
    let next = M.stage_candidate next ~binding ~expected ~revision:operation |> ok in
    registry
    := M.commit_candidate
         next
         ~binding
         ~expected
         ~identity:oauth
         ~source:(Protected_revision operation)
         ~grant:(Some grant)
       |> ok
  done;
  assert (Int.equal (List.length (M.retired !registry ~binding |> ok)) 63);
  (match
     M.begin_candidate
       !registry
       ~binding
       ~operation:(id "unreserved_login")
       ~expectation:(M.Expectation.exact oauth)
   with
   | Error Capacity -> ()
   | _ -> failwith "login started without prospective revision space");
  (match M.begin_refresh !registry ~binding ~operation:(id "unreserved_rotation") with
   | Error Capacity -> ()
   | _ -> failwith "rotation started without prospective revision space");
  let disabled =
    M.disable !registry ~binding ~operation:(id "logout_at_capacity") ~reason:Logout |> ok
  in
  assert (Option.is_none (M.Snapshot.active (M.find disabled ~binding |> ok)));
  assert (Int.equal (List.length (M.retired disabled ~binding |> ok)) 64);
  print_endline
    "full capacity rejects begin before external effects; active login remains \
     disableable";
  [%expect
    {| full capacity rejects begin before external effects; active login remains disableable |}]
;;

let%expect_test
    "qualified authenticated token scope preserves absent response scope and codec \
     provenance"
  =
  let identity =
    M.Identity.oauth
      ~host
      ~provider:"synthetic"
      ~billing:"subscription"
      ~issuer:"https://issuer.example"
      ~client_registration:"client_a"
      ~resource:"https://resource.example"
      ~account:"account_a"
      ~verified_subject:"subject_a"
      ~required_scopes:[ "inference" ]
    |> ok
  in
  let effective : M.Grant.effective =
    { scopes = [ "inference" ]
    ; scopes_provenance = Qualified_token_claim
    ; expiry = Known { at_ms = 123L; provenance = Declared }
    ; unknown_expiry_policy = Reject_unknown
    }
  in
  let grant =
    M.Grant.create
      ~identity
      ~scopes:Absent
      ~expires_at_ms:(Value 123L)
      ~refresh_policy:Require_rotated
      ~effective
    |> ok
  in
  (match
     M.Grant.create
       ~identity
       ~scopes:(Value [ "inference" ])
       ~expires_at_ms:(Value 123L)
       ~refresh_policy:Require_rotated
       ~effective
   with
   | Error Invalid_grant -> ()
   | _ -> failwith "raw response value mislabeled token claim");
  (match
     M.Grant.create
       ~identity
       ~scopes:Null
       ~expires_at_ms:(Value 123L)
       ~refresh_policy:Require_rotated
       ~effective
   with
   | Error Invalid_grant -> ()
   | _ -> failwith "explicit null response scope inferred from token");
  (match
     M.Grant.create
       ~identity
       ~scopes:Absent
       ~expires_at_ms:Absent
       ~refresh_policy:Require_rotated
       ~effective:
         { effective with
           expiry = Known { at_ms = 123L; provenance = Qualified_token_claim }
         }
   with
   | Error Invalid_grant -> ()
   | _ -> failwith "canonical token expiry must be Value/Declared");
  let operation = id "qualified_token_login" in
  let registry, expected =
    M.begin_candidate
      (initial ())
      ~binding
      ~operation
      ~expectation:(M.Expectation.exact identity)
    |> ok
  in
  let registry =
    M.stage_candidate registry ~binding ~expected ~revision:operation |> ok
  in
  let registry =
    M.commit_candidate
      registry
      ~binding
      ~expected
      ~identity
      ~source:(Protected_revision operation)
      ~grant:(Some grant)
    |> ok
  in
  let decoded = M.to_document registry |> ok |> M.of_document |> ok in
  let grant =
    M.find decoded ~binding
    |> ok
    |> M.Snapshot.active
    |> Option.value_exn
    |> M.Active.grant
    |> Option.value_exn
  in
  (match M.Grant.scopes grant with
   | Absent -> ()
   | _ -> failwith "raw omission normalized");
  assert (
    M.Grant.equal_provenance
      (M.Grant.effective grant).scopes_provenance
      Qualified_token_claim);
  print_endline
    "trusted token-claim provenance roundtrips; response omission stays absent";
  [%expect
    {| trusted token-claim provenance roundtrips; response omission stays absent |}]
;;
