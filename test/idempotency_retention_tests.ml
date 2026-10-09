open Core
open Agent_store_test_fixtures
module Store = Agent_store.Idempotency_store
module P = Agent_protocol

let receipt name outcome =
  Store.
    { key =
        { principal_id = P.Id.Principal.of_string "pri_retention" |> protocol_ok
        ; session_id = Some session_id
        ; method_name = "job.cancel"
        ; idempotency_key = P.Idempotency_key.of_string name |> protocol_ok
        }
    ; request_digest = name
    ; accepted_transaction_sequence = None
    ; outcome
    ; created_at = timestamp
    ; expires_at = Some timestamp
    ; retention = Standard
    }
;;

let references store candidates =
  Store.with_retained_references
    store
    ~candidates
    ~max_records:16
    ~max_bytes:65536
    ~f:(fun references -> Ok references)
  |> store_ok
  |> Option.value_exn
;;

let%expect_test
    "cached result retention includes disk-only and memory-only replies after lost \
     acknowledgements"
  =
  with_temp_directory "idempotency-retention" (fun env root ->
    let path = Filename.concat root "responses.sexp" in
    let armed = ref None in
    let wrapped = Job_store_fixtures.fault_env env armed in
    let store = Store.open_or_create ~env:wrapped ~path |> store_ok in
    let first = P.Id.Blob.create ()
    and second = P.Id.Blob.create () in
    let a = receipt "a" (Success (`String (P.Id.Blob.to_string first))) in
    let b = receipt "b" (Success (`String (P.Id.Blob.to_string second))) in
    Store.record store a |> store_ok |> ignore;
    armed := Some true;
    assert (Result.is_error (Store.record store b));
    (match Store.lookup store ~key:b.key ~request_digest:b.request_digest with
     | Missing -> ()
     | _ -> failwith "lost acknowledgement changed the memory view");
    (* Disk JSON may use an equivalent escaped spelling absent from the memory map. *)
    let encoded = Jsonaf.to_string (`String (P.Id.Blob.to_string second)) in
    let id = P.Id.Blob.to_string second in
    let escaped =
      sprintf "\"\\u%04x%s\"" (Char.to_int id.[0]) (String.drop_prefix id 1)
    in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let artifact_name =
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / root)
      |> List.find_exn ~f:(fun name ->
        String.is_prefix name ~prefix:"idempotency-outcome-"
        && String.is_suffix name ~suffix:".json"
        && String.is_substring
             (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / Filename.concat root name))
             ~substring:encoded)
    in
    let artifact = Eio.Path.(Eio.Stdenv.fs env / Filename.concat root artifact_name) in
    let old_bytes = Eio.Path.load artifact in
    let bytes = String.substr_replace_all old_bytes ~pattern:encoded ~with_:escaped in
    let reference bytes =
      Agent_store.Idempotency_outcome.Reference.of_jsonaf
        (`Object
            [ "tag", `String "terminal"
            ; "digest", `String (Agent_store.Document_record.digest bytes)
            ; "encoded_bytes", `String (Int.to_string (String.length bytes))
            ])
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum ([%sexp_of: Document_schema.Error.t] error))
      |> Result.ok_or_failwith
    in
    let old_reference = reference old_bytes
    and new_reference = reference bytes in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(
        Eio.Stdenv.fs env
        / Filename.concat
            root
            (Agent_store.Idempotency_outcome_store.basename new_reference))
      bytes;
    let metadata =
      String.substr_replace_all
        (Eio.Path.load file)
        ~pattern:
          (Jsonaf.to_string
             (Agent_store.Idempotency_outcome.Reference.to_jsonaf old_reference))
        ~with_:
          (Jsonaf.to_string
             (Agent_store.Idempotency_outcome.Reference.to_jsonaf new_reference))
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) file metadata;
    assert (
      List.equal
        P.Id.Blob.equal
        (List.sort [ first; second ] ~compare:P.Id.Blob.compare)
        (references store [ first; second ]));
    armed := Some true;
    assert (Result.is_error (Store.prune_expired store ~now:timestamp));
    (match Store.lookup store ~key:a.key ~request_digest:a.request_digest with
     | Replay _ -> ()
     | _ -> failwith "failed prune lost the memory reply");
    assert (List.equal P.Id.Blob.equal [ first ] (references store [ first; second ]));
    print_endline
      "disk-only escaped JSON reference retained after failed save acknowledgement";
    print_endline
      "memory-only replay reference retained after failed prune acknowledgement");
  [%expect
    {|
    disk-only escaped JSON reference retained after failed save acknowledgement
    memory-only replay reference retained after failed prune acknowledgement
    |}]
;;

let%expect_test
    "pending responses defer collection and the verified callback excludes concurrent \
     cache writes"
  =
  with_temp_directory "idempotency-retention-lock" (fun env root ->
    Eio.Switch.run (fun sw ->
      let active = ref false in
      let wrapped =
        Job_store_fixtures.fault_env env (ref None) ~before_open_out:(fun _ ->
          assert (not !active))
      in
      let store =
        Store.open_or_create ~env:wrapped ~path:(Filename.concat root "responses.sexp")
        |> store_ok
      in
      let id = P.Id.Blob.create () in
      let pending = receipt "pending" Pending in
      Store.record store pending |> store_ok |> ignore;
      let calls = ref 0 in
      let deferred =
        Store.with_retained_references
          store
          ~candidates:[ id ]
          ~max_records:16
          ~max_bytes:65536
          ~f:(fun _ ->
            incr calls;
            Ok ())
        |> store_ok
      in
      assert (Option.is_none deferred);
      [%test_eq: int] 0 !calls;
      Store.complete
        store
        ~key:pending.key
        ~request_digest:pending.request_digest
        ~accepted_transaction_sequence:None
        ~outcome:(Success (`String "complete"))
      |> store_ok
      |> ignore;
      let entered, enter = Eio.Promise.create () in
      let attempted, attempt = Eio.Promise.create () in
      let completed, complete = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.await entered;
        Eio.Promise.resolve attempt ();
        Store.record store (receipt "later" (Success (`String (P.Id.Blob.to_string id))))
        |> store_ok
        |> ignore;
        Eio.Promise.resolve complete ());
      let result =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
          Store.with_retained_references
            store
            ~candidates:[ id ]
            ~max_records:16
            ~max_bytes:65536
            ~f:(fun found ->
              assert (List.is_empty found);
              active := true;
              Eio.Promise.resolve enter ();
              Eio.Promise.await attempted;
              Eio.Fiber.yield ();
              assert (Option.is_none (Eio.Promise.peek completed));
              active := false;
              Ok ()))
        |> store_ok
      in
      assert (Option.is_some result);
      Eio.Promise.await completed;
      assert (List.equal P.Id.Blob.equal [ id ] (references store [ id ]));
      print_endline
        "pending response skipped the callback; completed responses allowed it";
      print_endline "cache writer remained blocked until the verified callback returned"));
  [%expect
    {|
    pending response skipped the callback; completed responses allowed it
    cache writer remained blocked until the verified callback returned
    |}]
;;

let%expect_test
    "corrupt cached replies, duplicates and budget excess never invoke the collection \
     callback"
  =
  with_temp_directory "idempotency-retention-invalid" (fun env root ->
    let path = Filename.concat root "responses.sexp" in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let store = Store.open_or_create ~env ~path |> store_ok in
    Store.record store (receipt "one" (Success (`String "kept"))) |> store_ok |> ignore;
    let original = Eio.Path.load file in
    let calls = ref 0 in
    let check ?(max_records = 16) ?(max_bytes = 65536) () =
      Store.with_retained_references
        store
        ~candidates:[]
        ~max_records
        ~max_bytes
        ~f:(fun _ ->
          incr calls;
          Ok ())
    in
    assert (Result.is_error (check ~max_records:1 ()));
    assert (Result.is_error (check ~max_bytes:1 ()));
    Eio.Path.save ~create:(`Or_truncate 0o600) file "corrupt";
    assert (Result.is_error (check ()));
    let rec duplicate = function
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if String.equal name "records"
             then (
               match value with
               | `Array [ record ] -> name, `Array [ record; record ]
               | _ -> name, value)
             else name, duplicate value))
      | json -> json
    in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      file
      (Jsonaf.of_string original |> duplicate |> Jsonaf.to_string);
    assert (Result.is_error (check ()));
    Eio.Path.unlink file;
    let target = Filename.concat root "foreign" in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / target)
      original;
    Eio.Path.symlink ~link_to:target file;
    assert (Result.is_error (check ()));
    [%test_eq: int] 0 !calls;
    [%test_eq: string] original (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / target));
    print_endline
      "record/byte limits, corrupt file, duplicate key and symlink all refused before \
       callback");
  [%expect
    {| record/byte limits, corrupt file, duplicate key and symlink all refused before callback |}]
;;

let%expect_test
    "legacy cache restores attach history before receipt decoding and preserves evidence"
  =
  with_temp_directory "idempotency-history-conversion" (fun env root ->
    let path = Filename.concat root "responses.json" in
    let record = receipt "legacy-attach" Pending in
    let key = { record.key with method_name = "session.attach" } in
    let key_json =
      `Object
        [ "principal_id", P.Id.Principal.to_json key.principal_id
        ; "session_id", P.Id.Session.to_json session_id
        ; "method_name", `String key.method_name
        ; "idempotency_key", P.Idempotency_key.to_json key.idempotency_key
        ]
    in
    let record_id = Agent_store.Document_record.digest (Jsonaf.to_string key_json) in
    (* Literal old owner shape: independent of the current cache encoder and
       strict History.entry encoder. The embedded result is opaque to the cache
       except for the declared Attach snapshot ownership. *)
    let bytes =
      sprintf
        {|{"format":"ochat.document","schema_version":1,"kind":"store.idempotency_cache","cache_evidence":"outer","payload":{"records":[{"record_id":%s,"key":%s,"request_digest":"legacy-attach","accepted_transaction_sequence":null,"outcome":{"tag":"success","value":{"replay":{"type":"snapshot","snapshot":{"canonical_history":{"entries":[{"id":"old","evidence":"retained"}]},"deferred_entries":[],"effective_history":null}},"opaque":{"entries":[{"id":"unknown"}]}}},"created_at":%s,"expires_at":null,"retention":"protected","record_evidence":"original"}]}}|}
        (Jsonaf.to_string (`String record_id))
        (Jsonaf.to_string key_json)
        (Jsonaf.to_string (P.Timestamp.to_json timestamp))
    in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    Eio.Path.save ~create:(`Or_truncate 0o600) file bytes;
    let store = Store.open_or_create ~env ~path |> store_ok in
    (match Store.lookup store ~key ~request_digest:"legacy-attach" with
     | Replay { outcome = Success json; _ } ->
       let text = Jsonaf.to_string json in
       printf
         "revision-initialized=%b opaque-untouched=%b\n"
         (String.is_substring text ~substring:{|"content_revision":"0"|})
         (String.is_substring text ~substring:{|"opaque":{"entries":[{"id":"unknown"}]}|})
     | _ -> failwith "legacy completed attach receipt unavailable");
    assert (String.equal bytes (Eio.Path.load file));
    Store.record store (receipt "new-pending" Pending) |> store_ok |> ignore;
    let current = Eio.Path.load file in
    printf
      "current-version=%b original-evidence=%b reopened=%b\n"
      (String.is_substring current ~substring:{|"schema_version":3|})
      (let artifacts =
         Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / root)
         |> List.filter ~f:(fun name ->
           String.is_prefix name ~prefix:"idempotency-outcome-"
           && String.is_suffix name ~suffix:".json")
         |> List.map ~f:(fun name ->
           Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / Filename.concat root name))
       in
       let roots = current :: artifacts in
       List.for_all [ "outer"; "retained"; "original" ] ~f:(fun evidence ->
         List.exists roots ~f:(fun text -> String.is_substring text ~substring:evidence)))
      (Result.is_ok (Store.open_or_create ~env ~path));
    [%expect
      {|revision-initialized=true opaque-untouched=true
current-version=true original-evidence=true reopened=true|}])
;;

let%expect_test
    "stopped continuation with no accepted transaction remains honestly pending after \
     interruption"
  =
  with_temp_directory "continue-pending" (fun env root ->
    let path = Filename.concat root "idempotency.json" in
    let pending = receipt "no-op-continue" Pending in
    let pending =
      { pending with
        key = { pending.key with method_name = "session.continue_history" }
      ; retention = Protected
      ; expires_at = None
      ; accepted_transaction_sequence = None
      }
    in
    let owner = Store.open_or_create ~env ~path |> store_ok in
    Store.record owner pending |> store_ok |> ignore;
    (* No journal command admission and no terminal cache completion occurred.
       Reopening must not infer Not_started from the currently stopped session. *)
    let reopened = Store.open_or_create ~env ~path |> store_ok in
    Store.reconcile_accepted reopened [] |> store_ok |> ignore;
    match
      Store.lookup reopened ~key:pending.key ~request_digest:pending.request_digest
    with
    | Replay { outcome = Pending; accepted_transaction_sequence = None; _ } ->
      print_endline "original outcome remains unknown; no terminal continuation inferred"
    | _ -> failwith "interrupted no-op continuation became terminal");
  [%expect {|original outcome remains unknown; no terminal continuation inferred|}]
;;
