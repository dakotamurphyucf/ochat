open Core
module P = Agent_protocol
module S = Agent_store.Organization_state
module D = Document_schema

let ok = function
  | Ok value -> value
  | Error _ -> failwith "fixture rejected"
;;

let host = P.Id.Server.of_string "srv_organization_test" |> ok
let id = P.Id.Project.of_string "prj_first" |> ok
let now = P.Timestamp.of_string "2026-10-08T12:00:00Z" |> ok

let principal ?(scopes = [ P.Scope.View_organization; Manage_organization ]) value =
  P.Principal.create
    ~id:(P.Id.Principal.of_string value |> ok)
    ~authentication_kind:"test"
    ~scopes:(P.Scope.Set.of_list scopes)
    ~attributes:[]
  |> ok
;;

let owner = principal "pri_owner"
let other = principal "pri_other"
let name value = P.Organization_group.Name.create value |> ok
let key value = P.Idempotency_key.of_string value |> ok

let audit principal method_name idempotency_key _digest =
  let mutation =
    match method_name with
    | "project.create" ->
      S.Mutation.Create_project
        P.Organization_request.Create.
          { host_id = host; name = name "First"; idempotency_key }
    | "project.update" ->
      S.Mutation.Update_project
        P.Organization_request.Project.Update.
          { host_id = host
          ; id
          ; expected_revision = 0L
          ; name = name "Renamed"
          ; idempotency_key
          }
    | "project.delete" ->
      S.Mutation.Delete_project
        P.Organization_request.Project.Delete.
          { host_id = host; id; expected_revision = 0L; idempotency_key }
    | _ -> failwith "unknown fixture method"
  in
  Agent_store.Idempotency_store.Command_audit.
    { key =
        { principal_id = principal.P.Principal.id
        ; session_id = None
        ; method_name
        ; idempotency_key
        }
    ; request_digest = S.request_digest mutation |> ok
    ; protected_record = false
    }
;;

let status = function
  | Ok _ -> "ok"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let create =
  P.Organization_request.Create.
    { host_id = host; name = name "First"; idempotency_key = key "create" }
;;

let created () =
  S.apply
    (S.empty ~server_id:host)
    ~principal:owner
    ~audit:(audit owner "project.create" create.idempotency_key 'a')
    ~now
    ~candidate:(Some (S.Candidate.Project id))
    (S.Mutation.Create_project create)
  |> ok
;;

let%expect_test "host and principal admission, exact receipt replay and stale revision" =
  let state, result = created () in
  let rename =
    P.Organization_request.Project.Update.
      { host_id = host
      ; id
      ; expected_revision = 0L
      ; name = name "Renamed"
      ; idempotency_key = key "rename"
      }
  in
  let change principal state =
    S.apply
      state
      ~principal
      ~audit:(audit principal "project.update" rename.idempotency_key 'b')
      ~now
      ~candidate:None
      (S.Mutation.Update_project rename)
  in
  print_endline (status (S.visible_project state ~principal:other id));
  print_endline (status (change other state));
  let renamed, receipt = change owner state |> ok in
  print_endline
    (status
       (S.apply
          renamed
          ~principal:owner
          ~audit:(audit owner "project.update" (key "new-key") 'c')
          ~now
          ~candidate:None
          (S.Mutation.Update_project { rename with idempotency_key = key "new-key" })));
  let replay_state, replay = change owner renamed |> ok in
  print_s
    [%sexp
      (P.Organization_result.equal receipt replay : bool)
    , (Int64.equal (S.revision replay_state) (S.revision renamed) : bool)];
  let original =
    S.lookup_receipt
      renamed
      ~principal:owner
      ~now
      ~key:(audit owner "project.create" create.idempotency_key 'a').key
      ~request_digest:
        (audit owner "project.create" create.idempotency_key 'a').request_digest
    |> ok
    |> Option.value_exn
  in
  print_s [%sexp (P.Organization_result.equal result original : bool)];
  print_endline
    (status
       (S.lookup_receipt
          renamed
          ~principal:(principal ~scopes:[ View_organization ] "pri_owner")
          ~now
          ~key:(audit owner "project.create" create.idempotency_key 'a').key
          ~request_digest:
            (audit owner "project.create" create.idempotency_key 'a').request_digest));
  print_endline
    (status
       (S.apply
          state
          ~principal:owner
          ~audit:(audit owner "project.create" create.idempotency_key 'a')
          ~now
          ~candidate:None
          (S.Mutation.Create_project
             { create with host_id = P.Id.Server.of_string "srv_other" |> ok })));
  [%expect
    {|
    organization_not_found
    organization_not_found
    conflict
    (true true)
    true
    permission_denied
    invalid_request
    |}]
;;

let%expect_test
    "tombstone receipt survives, ID cannot be reused, codec preserves unknown fields"
  =
  let state, _ = created () in
  let initial =
    Agent_store.Organization_document.encode (D.Extension_carrier.of_authored_value state)
    |> ok
  in
  let inject = function
    | `Object envelope ->
      `Object
        (List.map envelope ~f:(fun (field, value) ->
           ( field
           , if String.equal field "payload"
             then (
               match value with
               | `Object payload ->
                 `Object
                   (("future_host", `String "keep")
                    :: List.map payload ~f:(fun (field, value) ->
                      ( field
                      , if String.equal field "projects"
                        then (
                          match value with
                          | `Array [ `Object entry ] ->
                            `Array [ `Object (("future_entry", `String "keep") :: entry) ]
                          | json -> json)
                        else value )))
               | json -> json)
             else value )))
    | json -> json
  in
  let restored =
    D.Document.inspect
      ~limits:Agent_store.Organization_document.limits
      (inject (D.Document.json initial))
    |> ok
    |> Agent_store.Organization_document.restore
    |> ok
  in
  let deletion =
    P.Organization_request.Project.Delete.
      { host_id = host; id; expected_revision = 0L; idempotency_key = key "delete" }
  in
  let deleted, result =
    S.apply
      state
      ~principal:owner
      ~audit:(audit owner "project.delete" deletion.idempotency_key 'd')
      ~now
      ~candidate:None
      (S.Mutation.Delete_project deletion)
    |> ok
  in
  print_endline (status (S.visible_project deleted ~principal:owner id));
  let retry_state, retry =
    S.apply
      deleted
      ~principal:owner
      ~audit:(audit owner "project.delete" deletion.idempotency_key 'd')
      ~now
      ~candidate:None
      (S.Mutation.Delete_project deletion)
    |> ok
  in
  print_s
    [%sexp
      (P.Organization_result.equal result retry : bool)
    , (Int64.equal (S.revision deleted) (S.revision retry_state) : bool)];
  print_endline
    (status
       (S.apply
          deleted
          ~principal:owner
          ~audit:(audit owner "project.create" (key "second-create") 'e')
          ~now
          ~candidate:(Some (S.Candidate.Project id))
          (S.Mutation.Create_project { create with idempotency_key = key "second-create" })));
  let final =
    Agent_store.Organization_document.with_state restored deleted ~now
    |> ok
    |> Agent_store.Organization_document.encode
    |> ok
  in
  let text = D.Document.to_string final in
  print_s
    [%sexp
      (String.is_substring text ~substring:"future_host" : bool)
    , (String.is_substring text ~substring:"future_entry" : bool)];
  let state =
    Agent_store.Organization_document.restore final |> ok |> D.Extension_carrier.value
  in
  print_s
    [%sexp (List.length (S.projects state) : int), (List.length (S.receipts state) : int)];
  [%expect
    {|
    organization_not_found
    (true true)
    conflict
    (true true)
    (1 2)
    |}]
;;

let%expect_test "receipt retirement requires expiry and preserves retained unknown fields"
  =
  let state, _ = created () in
  let later = P.Timestamp.add_ms now 43_200_000 |> ok in
  let rename =
    P.Organization_request.Project.Update.
      { host_id = host
      ; id
      ; expected_revision = 0L
      ; name = name "First"
      ; idempotency_key = key "noop"
      }
  in
  let rename_audit =
    Agent_store.Idempotency_store.Command_audit.
      { key =
          { principal_id = owner.id
          ; session_id = None
          ; method_name = "project.update"
          ; idempotency_key = rename.idempotency_key
          }
      ; request_digest = S.request_digest (S.Mutation.Update_project rename) |> ok
      ; protected_record = false
      }
  in
  let state, _ =
    S.apply
      state
      ~principal:owner
      ~audit:rename_audit
      ~now:later
      ~candidate:None
      (S.Mutation.Update_project rename)
    |> ok
  in
  let original =
    Agent_store.Organization_document.encode (D.Extension_carrier.of_authored_value state)
    |> ok
  in
  let inject =
    match D.Document.json original with
    | `Object envelope ->
      `Object
        (List.map envelope ~f:(fun (field, value) ->
           ( field
           , if String.equal field "payload"
             then (
               match value with
               | `Object payload ->
                 `Object
                   (List.map payload ~f:(fun (field, value) ->
                      ( field
                      , if String.equal field "receipts"
                        then (
                          match value with
                          | `Array entries ->
                            `Array
                              (List.map entries ~f:(function
                                 | `Object entry ->
                                   `Object (("future_receipt", `String "keep") :: entry)
                                 | json -> json))
                          | json -> json)
                        else value )))
               | json -> json)
             else value )))
    | _ -> failwith "document object"
  in
  let restored =
    D.Document.inspect ~limits:Agent_store.Organization_document.limits inject
    |> ok
    |> Agent_store.Organization_document.restore
    |> ok
  in
  let without_receipts =
    S.restore
      ~server_id:host
      ~revision:(S.revision state)
      ~projects:(S.projects state)
      ~collections:[]
      ~receipts:[]
    |> ok
  in
  print_s
    [%sexp
      (Result.is_error
         (Agent_store.Organization_document.with_state
            restored
            without_receipts
            ~now:later)
       : bool)];
  print_s
    [%sexp
      (Result.is_error
         (Agent_store.Organization_document.with_state
            restored
            (S.empty ~server_id:(P.Id.Server.of_string "srv_foreign" |> ok))
            ~now:later)
       : bool)];
  let day = P.Timestamp.add_ms now 86_400_001 |> ok in
  let update =
    { rename with name = name "After expiry"; idempotency_key = key "after-expiry" }
  in
  let audit =
    { rename_audit with
      key = { rename_audit.key with idempotency_key = update.idempotency_key }
    ; request_digest = S.request_digest (S.Mutation.Update_project update) |> ok
    }
  in
  let next, _ =
    S.apply
      state
      ~principal:owner
      ~audit
      ~now:day
      ~candidate:None
      (S.Mutation.Update_project update)
    |> ok
  in
  let final =
    Agent_store.Organization_document.with_state restored next ~now:day
    |> ok
    |> Agent_store.Organization_document.encode
    |> ok
  in
  let roundtrip =
    Agent_store.Organization_document.restore final |> ok |> D.Extension_carrier.value
  in
  print_s
    [%sexp
      (List.length (S.receipts roundtrip) : int)
    , (List.length (S.projects roundtrip) : int)];
  print_s
    [%sexp
      (String.is_substring (D.Document.to_string final) ~substring:"future_receipt"
       : bool)];
  print_s [%sexp (List.length (S.receipts (D.Extension_carrier.value restored)) : int)];
  [%expect
    {|
    true
    true
    (2 1)
    true
    2
    |}]
;;

let%expect_test "retained identity bounds and overflow reject before transition" =
  let entry i =
    S.Project_entry.
      { group =
          P.Organization_group.Project.create
            ~id:(P.Id.Project.of_string (sprintf "prj_capacity_%d" i) |> ok)
            ~creator_principal_id:owner.id
            ~name:(name "Bounded")
            ~revision:0L
            ~created_at:now
            ~updated_at:now
          |> ok
      ; deleted_at = None
      }
  in
  let entries = List.init 4096 ~f:entry in
  let state =
    S.restore ~server_id:host ~revision:0L ~projects:entries ~collections:[] ~receipts:[]
    |> ok
  in
  print_endline
    (status
       (S.apply
          state
          ~principal:owner
          ~audit:(audit owner "project.create" create.idempotency_key 'a')
          ~now
          ~candidate:(Some (S.Candidate.Project id))
          (S.Mutation.Create_project create)));
  print_endline
    (status
       (S.restore
          ~server_id:host
          ~revision:0L
          ~projects:(entry 4096 :: entries)
          ~collections:[]
          ~receipts:[]));
  let overflow =
    S.restore
      ~server_id:host
      ~revision:Int64.max_value
      ~projects:[]
      ~collections:[]
      ~receipts:[]
    |> ok
  in
  print_endline
    (status
       (S.apply
          overflow
          ~principal:owner
          ~audit:(audit owner "project.create" create.idempotency_key 'a')
          ~now
          ~candidate:(Some (S.Candidate.Project id))
          (S.Mutation.Create_project create)));
  let group =
    P.Organization_group.Project.create
      ~id
      ~creator_principal_id:owner.id
      ~name:(name "Impossible")
      ~revision:99L
      ~created_at:now
      ~updated_at:now
    |> ok
  in
  print_endline
    (status
       (S.restore
          ~server_id:host
          ~revision:0L
          ~projects:[ { group; deleted_at = None } ]
          ~collections:[]
          ~receipts:[]));
  [%expect
    {|
    conflict
    invalid_request
    conflict
    invalid_request
    |}]
;;

let%expect_test "dedicated scopes and independent request decoders enforce invariants" =
  print_s
    [%sexp
      (P.Scope.of_string "organization.view" |> ok : P.Scope.t)
    , (P.Scope.of_string "organization.manage" |> ok : P.Scope.t)];
  let update name_json revision =
    `Object
      [ "host_id", P.Id.Server.to_json host
      ; "id", P.Id.Project.to_json id
      ; "expected_revision", revision
      ; "name", name_json
      ; "idempotency_key", P.Idempotency_key.to_json (key "decode")
      ]
  in
  print_s
    [%sexp
      (Result.is_error
         (P.Organization_request.Project.Update.of_json
            (update (`String "") (`Number "0")))
       : bool)
    , (Result.is_error
         (P.Organization_request.Project.Update.of_json
            (update (`String "Name") (`Number "-1")))
       : bool)
    , (Result.is_error
         (P.Organization_request.Project.Update.of_json
            (update (`String "bad\000name") (`Number "0")))
       : bool)];
  let group =
    P.Organization_group.Project.create
      ~id
      ~creator_principal_id:owner.id
      ~name:(name "Stored")
      ~revision:0L
      ~created_at:now
      ~updated_at:now
    |> ok
  in
  let wrong =
    Agent_store.Organization_state.Receipt.
      { key = (audit owner "project.delete" (key "wrong-method") 'a').key
      ; request_digest = String.make 64 'a'
      ; result = P.Organization_result.Project_created group
      ; created_at = now
      ; expires_at = P.Timestamp.add_ms now 86_400_000 |> ok
      }
  in
  print_endline
    (status
       (S.restore
          ~server_id:host
          ~revision:1L
          ~projects:[ { group; deleted_at = None } ]
          ~collections:[]
          ~receipts:[ wrong ]));
  [%expect
    {|
    (View_organization Manage_organization)
    (true true true)
    invalid_request
    |}]
;;

let%expect_test
    "expired same-key create renews lifetime without transferring old extensions"
  =
  let digest = (audit owner "project.create" create.idempotency_key 'a').request_digest in
  (* Independent named-field fixture, including receipt- and result-owned future
    fields. The stable receipt ID hashes the canonical key, not its outcome. *)
  let receipt_id =
    Digestif.SHA256.digest_string
      {|{"idempotency_key":"create","method_name":"project.create","principal_id":"pri_owner"}|}
    |> Digestif.SHA256.to_hex
  in
  let group_json =
    {|{"id":"prj_first","creator_principal_id":"pri_owner","name":"First","revision":0,"created_at":"2026-10-08T12:00:00Z","updated_at":"2026-10-08T12:00:00Z"}|}
  in
  let fixture =
    sprintf
      {|{"format":"ochat.document","schema_version":1,"kind":"host.organization","payload":{"server_id":"srv_organization_test","revision":"1","projects":[{"id":"prj_first","group":%s,"deleted_at":null,"future_group":"retain"}],"collections":[],"receipts":[{"id":"%s","key":{"principal_id":"pri_owner","method_name":"project.create","idempotency_key":"create"},"request_digest":"%s","result":{"kind":"project.created","value":%s,"future_old_result":"retire"},"created_at":"2026-10-08T12:00:00Z","expires_at":"2026-10-09T12:00:00Z","future_old_receipt":"retire"}]}}|}
      group_json
      receipt_id
      digest
      group_json
  in
  let carrier =
    D.Document.decode ~limits:Agent_store.Organization_document.limits fixture
    |> ok
    |> Agent_store.Organization_document.restore
    |> ok
  in
  let state = D.Extension_carrier.value carrier in
  let deleted_at = P.Timestamp.add_ms now 600_000 |> ok in
  let deletion =
    P.Organization_request.Project.Delete.
      { host_id = host; id; expected_revision = 0L; idempotency_key = key "delete" }
  in
  let deleted, _ =
    S.apply
      state
      ~principal:owner
      ~audit:(audit owner "project.delete" deletion.idempotency_key 'd')
      ~now:deleted_at
      ~candidate:None
      (S.Mutation.Delete_project deletion)
    |> ok
  in
  let carrier =
    Agent_store.Organization_document.with_state carrier deleted ~now:deleted_at |> ok
  in
  let document = Agent_store.Organization_document.encode carrier |> ok in
  let carrier = Agent_store.Organization_document.restore document |> ok in
  let after_expiry = P.Timestamp.add_ms now 86_400_001 |> ok in
  let renewal = { create with name = name "Fresh" } in
  let renewal_mutation = S.Mutation.Create_project renewal in
  let renewal_audit =
    { (audit owner "project.create" renewal.idempotency_key 'a') with
      request_digest = S.request_digest renewal_mutation |> ok
    }
  in
  let renewed_id = P.Id.Project.of_string "prj_renewed" |> ok in
  let next, result =
    S.apply
      deleted
      ~principal:owner
      ~audit:renewal_audit
      ~now:after_expiry
      ~candidate:(Some (S.Candidate.Project renewed_id))
      renewal_mutation
    |> ok
  in
  let final =
    Agent_store.Organization_document.with_state carrier next ~now:after_expiry
    |> ok
    |> Agent_store.Organization_document.encode
    |> ok
  in
  let text = D.Document.to_string final in
  print_s
    [%sexp
      (String.is_substring text ~substring:"future_old_receipt" : bool)
    , (String.is_substring text ~substring:"future_old_result" : bool)
    , (String.is_substring text ~substring:"future_group" : bool)];
  let roundtrip =
    Agent_store.Organization_document.restore final |> ok |> D.Extension_carrier.value
  in
  print_s
    [%sexp
      (List.length (S.projects roundtrip) : int)
    , (List.length (S.receipts roundtrip) : int)];
  print_endline (status (S.visible_project roundtrip ~principal:owner id));
  print_s
    [%sexp
      ((match result with
        | P.Organization_result.Project_created group ->
          P.Id.Project.equal group.id renewed_id && Int64.equal group.revision 0L
        | _ -> false)
       : bool)];
  let replay =
    S.lookup_receipt
      roundtrip
      ~principal:owner
      ~now:after_expiry
      ~key:renewal_audit.key
      ~request_digest:renewal_audit.request_digest
    |> ok
    |> Option.value_exn
  in
  print_s [%sexp (P.Organization_result.equal result replay : bool)];
  [%expect
    {|
    (false false true)
    (2 2)
    organization_not_found
    true
    true
    |}]
;;
