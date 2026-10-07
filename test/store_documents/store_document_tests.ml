open! Core
module S = Agent_store
module D = Document_schema

let () = Mirage_crypto_rng_unix.use_default ()

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : S.Store_error.t)]
;;

let doc_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let limits = D.Limits.default

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let frame_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : S.Frame.error)]
;;

let%expect_test "authored frame carries its checksum without weakening stored reads" =
  List.iter [ 0; 1; 0xffff ] ~f:(fun flags ->
    let payload = "authored\000payload\n" in
    let encoded =
      S.Frame.Encoded.create ~max_payload_length:1024 ~flags payload |> frame_ok
    in
    let bytes = S.Frame.Encoded.bytes encoded in
    assert (
      String.equal
        bytes
        (S.Frame.encode ~max_payload_length:1024 ~flags payload |> frame_ok));
    let body = String.prefix bytes (String.length bytes - 32) in
    assert (
      String.equal
        (S.Frame.Encoded.checksum_hex encoded)
        Digestif.SHA256.(digest_string body |> to_hex));
    (match
       S.Frame.decode ~max_payload_length:1024 ~contents:bytes ~offset:0 |> frame_ok
     with
     | Complete { frame; next_offset } ->
       assert (next_offset = String.length bytes);
       assert (S.Frame.flags frame = flags);
       assert (String.equal (S.Frame.payload frame) payload);
       assert (
         String.equal (S.Frame.checksum_hex frame) (S.Frame.Encoded.checksum_hex encoded))
     | Incomplete_tail _ -> assert false);
    let corrupted = Bytes.of_string bytes in
    Bytes.set corrupted 20 'x';
    assert (
      match
        S.Frame.decode
          ~max_payload_length:1024
          ~contents:(Bytes.to_string corrupted)
          ~offset:0
      with
      | Error Checksum_mismatch -> true
      | Error _ | Ok _ -> false);
    assert (
      Result.is_error (S.Frame.decode ~max_payload_length:1 ~contents:bytes ~offset:0)));
  assert (Result.is_error (S.Frame.Encoded.create ~max_payload_length:1 ~flags:0 "large"));
  assert (Result.is_error (S.Frame.Encoded.create ~max_payload_length:1024 ~flags:(-1) ""));
  print_endline "authored checksum agrees; stored corruption and limits reject";
  [%expect {| authored checksum agrees; stored corruption and limits reject |}]
;;

let session_id = Agent_protocol.Id.Session.of_string "ses_store_document" |> protocol_ok
let timestamp = Agent_protocol.Timestamp.of_string "2026-08-15T12:00:00Z" |> protocol_ok
let document kind payload = D.Document.create ~limits ~kind ~version:1 ~payload |> doc_ok

let text document =
  match D.Json.field (D.Document.payload document) ~name:"text" with
  | Value (`String value) -> value
  | Absent | Null | Value _ -> ""
;;

(* Independently authored named fields, rather than a typed encoder fixture. *)
let original_transaction =
  {|{ "kind":"session.transaction", "schema_version":1,"format":"ochat.document","payload":{"session_id":"ses_store_document","generation":"0","transaction_sequence":"1","previous_transaction_hash":null,"session_revision":"1","first_event_sequence":null,"last_event_sequence":null,"accepted_at_ns":"0","command_audit":null,"delta":{"format":"ochat.document","kind":"session.delta","schema_version":1,"payload":{"text":"first","future_delta":null}},"durable_events":[],"future_payload":7},"future_envelope":true }|}
;;

let snapshot_document sequence hash value =
  let counter = `String (Int64.to_string sequence) in
  let state =
    document
      "session.state"
      (`Object
          [ ( "identity"
            , `Object
                [ "session_id", `String "ses_store_document"; "generation", `String "0" ]
            )
          ; ( "counters"
            , `Object
                [ "transaction_sequence", counter
                ; "revision", counter
                ; "event_sequence", `String "0"
                ] )
          ; ( "spec"
            , `Object
                [ "prompt_revision_id", `String "prv_store_document"
                ; "workspace_instance", `Object [ "conflict_domain", `String "workspace" ]
                ] )
          ; "text", `String value
          ; "future_state", `Null
          ])
  in
  S.Snapshot.create
    ~limits
    ~session_id
    ~transaction_sequence:sequence
    ~transaction_hash:hash
    ~event_sequence:0L
    ~created_at:timestamp
    ~prompt_artifact:"prv_store_document"
    ~workspace_identity:"workspace"
    ~payload:state
  |> ok
;;

let with_directory f =
  Eio_main.run (fun env ->
    let directory =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        ("store-document-"
         ^ Agent_protocol.Id.Transaction.to_string
             (Agent_protocol.Id.Transaction.create ()))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / directory) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~f:(fun () -> f env directory)
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path))
;;

let%expect_test "durable structural limits agree from authored children through recovery" =
  let max_payload_length = 4 * 1024 * 1024 in
  let durable_limits = S.Document_fields.limits ~max_bytes:max_payload_length |> doc_ok in
  let many_fields =
    `Object (List.init 100_001 ~f:(fun i -> sprintf "field_%06d" i, `Null))
  in
  let deep_value =
    List.fold (List.init 132 ~f:Fn.id) ~init:`Null ~f:(fun value _ ->
      `Object [ "nested", value ])
  in
  List.iter
    [ "fields", many_fields; "depth", deep_value ]
    ~f:(fun (bound, bulk) ->
      (* These current named documents fit the configured byte budget. The generic
       JSON profile remains deliberately stricter than the durable profile. *)
      assert (
        match D.Json.validate ~limits:D.Limits.default bulk with
        | Error (Limit_exceeded actual) -> String.equal actual bound
        | Error _ | Ok () -> false);
      let child kind payload =
        D.Document.create ~limits:durable_limits ~kind ~version:1 ~payload |> doc_ok
      in
      let delta =
        child "session.delta" (`Object [ "text", `String bound; "bulk", bulk ])
      in
      let transaction =
        S.Transaction.create
          ~limits:durable_limits
          ~session_id
          ~generation:0
          ~transaction_sequence:1L
          ~previous_transaction_hash:None
          ~session_revision:1L
          ~first_event_sequence:None
          ~last_event_sequence:None
          ~accepted_at_ns:0L
          ~command_audit:None
          ~delta
          ~durable_events:[]
        |> ok
      in
      let bytes = S.Transaction.encode transaction in
      let direct = S.Transaction.decode bytes |> ok in
      assert (String.equal (S.Transaction.hash transaction) (S.Transaction.hash direct));
      assert (String.equal bytes (S.Transaction.encode direct));
      let base = snapshot_document 0L None bound in
      let state =
        match D.Document.payload base.payload with
        | `Object fields -> child "session.state" (`Object (fields @ [ "bulk", bulk ]))
        | _ -> assert false
      in
      let snapshot =
        S.Snapshot.with_value
          base
          ~limits:durable_limits
          { (S.Snapshot.value base) with payload = state }
        |> ok
      in
      let assert_bulk document =
        match D.Json.field (D.Document.payload document) ~name:"bulk" with
        | Value restored -> assert (D.Json.equal bulk restored)
        | Absent | Null -> assert false
      in
      with_directory (fun env root ->
        Eio.Switch.run (fun sw ->
          let journal =
            S.Journal.create
              ~env
              ~directory:(Filename.concat root "journal")
              ~max_payload_length
              ~max_segment_bytes:16777216L
              ~max_segment_frames:100
            |> ok
          in
          let writer =
            S.Commit_writer.create
              ~sw
              ~journal
              ~session_id
              ~next_transaction_sequence:1L
              ~previous_transaction_hash:None
              ~queue_capacity:2
            |> ok
          in
          let committed =
            S.Commit_writer.commit writer ~durability:Flush transaction |> ok
          in
          S.Commit_writer.close writer;
          let entry = List.hd_exn (S.Journal.scan journal |> ok).entries in
          assert (String.equal bytes (S.Frame.payload entry.frame));
          let record =
            match
              S.Document_record.of_frame
                entry.frame
                ~limits:durable_limits
                ~expected_digest:(Some committed.transaction_hash)
            with
            | Ok record -> record
            | Error error -> raise_s [%sexp (error : S.Document_record.Error.t)]
          in
          let restored =
            S.Transaction.decode_record record ~limits:durable_limits |> ok
          in
          assert_bulk restored.delta;
          let snapshot_directory = Filename.concat root "snapshot" in
          let installed =
            S.Snapshot.install
              ~env
              ~directory:snapshot_directory
              ~max_payload_length
              snapshot
            |> ok
          in
          let reread =
            S.Snapshot.read_file
              ~env
              ~directory:snapshot_directory
              ~max_payload_length
              ~filename:installed.filename
            |> ok
          in
          assert_bulk reread.snapshot.payload;
          let recovered =
            S.Recovery.load
              ~env
              ~journal
              ~snapshot_directory
              ~max_snapshot_payload_length:max_payload_length
              ~session_id
              ~initial:""
              ~restore_snapshot:(fun snapshot ->
                assert_bulk snapshot.S.Snapshot.payload;
                Ok "checkpoint")
              ~apply:(fun _ transaction ->
                assert_bulk transaction.S.Transaction.delta;
                Ok (text transaction.delta))
              ~validate_transaction:(fun transaction ->
                assert_bulk transaction.S.Transaction.delta;
                Ok ())
              ~validate:(fun _ -> Ok ())
            |> ok
          in
          assert (String.equal recovered.state bound);
          assert (
            Option.equal
              String.equal
              recovered.latest_transaction_hash
              (Some committed.transaction_hash));
          S.Journal.prune_before_transaction journal ~transaction_sequence:1L
          |> ok
          |> ignore;
          S.Snapshot.prune_older
            ~env
            ~directory:snapshot_directory
            ~max_payload_length
            ~keep:1
          |> ok
          |> ignore;
          let names = Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / root) in
          assert (
            Result.is_error
              (S.Snapshot.install
                 ~env
                 ~directory:(Filename.concat root "byte-rejected")
                 ~max_payload_length:128
                 snapshot));
          assert (
            List.equal
              String.equal
              (List.sort names ~compare:String.compare)
              (Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / root)
               |> List.sort ~compare:String.compare))));
      printf
        "%s: writer, original frame, snapshot and recovery agree; byte limit rejects\n"
        bound);
  [%expect
    {|
    fields: writer, original frame, snapshot and recovery agree; byte limit rejects
    depth: writer, original frame, snapshot and recovery agree; byte limit rejects
    |}]
;;

let%expect_test "restored transaction digests are exact original bytes across edits" =
  let restored = S.Transaction.decode original_transaction |> ok in
  let normalized =
    D.Extension_carrier.template (S.Transaction.carrier restored)
    |> Option.value_exn
    |> D.Document.to_string
  in
  print_s
    [%sexp
      { original_bytes =
          (String.equal original_transaction (S.Transaction.encode restored) : bool)
      ; different_normalization =
          (not
             (String.equal
                (S.Transaction.hash restored)
                (S.Document_record.digest normalized))
           : bool)
      ; sha256 = (S.Transaction.hash restored : string)
      }];
  let edited =
    S.Transaction.with_value
      restored
      ~limits
      { (S.Transaction.value restored) with session_revision = 2L }
    |> ok
  in
  print_s
    [%sexp
      { original_still_unchanged =
          (String.equal original_transaction (S.Transaction.encode restored) : bool)
      ; edited_digest_matches_bytes =
          (String.equal
             (S.Transaction.hash edited)
             (S.Document_record.digest (S.Transaction.encode edited))
           : bool)
      ; retained_outer =
          (String.is_substring (S.Transaction.encode edited) ~substring:"future_envelope"
           : bool)
      ; retained_payload =
          (String.is_substring (S.Transaction.encode edited) ~substring:"future_payload"
           : bool)
      ; retained_child =
          (String.is_substring (S.Transaction.encode edited) ~substring:"future_delta"
           : bool)
      }];
  [%expect
    {|
    ((original_bytes true) (different_normalization true)
     (sha256 ff224977c5931bfe70a548360c7d4c143fa98bef9cdac418cf2ef8a3973152bd))
    ((original_still_unchanged true) (edited_digest_matches_bytes true)
     (retained_outer true) (retained_payload true) (retained_child true))
    |}]
;;

let%expect_test "commit writer appends original bytes and checkpoints anchor them" =
  with_directory (fun env root ->
    Eio.Switch.run (fun sw ->
      let journal =
        S.Journal.create
          ~env
          ~directory:(Filename.concat root "journal")
          ~max_payload_length:16384
          ~max_segment_bytes:1048576L
          ~max_segment_frames:100
        |> ok
      in
      let first = S.Transaction.decode original_transaction |> ok in
      let writer =
        S.Commit_writer.create
          ~sw
          ~journal
          ~session_id
          ~next_transaction_sequence:1L
          ~previous_transaction_hash:None
          ~queue_capacity:2
        |> ok
      in
      let committed = S.Commit_writer.commit writer ~durability:Flush first |> ok in
      S.Commit_writer.close writer;
      let snapshot_directory = Filename.concat root "snapshot" in
      S.Snapshot.install
        ~env
        ~directory:snapshot_directory
        ~max_payload_length:16384
        (snapshot_document 1L (Some committed.transaction_hash) "first")
      |> ok
      |> ignore;
      let recovered =
        S.Recovery.load
          ~env
          ~journal
          ~snapshot_directory
          ~max_snapshot_payload_length:16384
          ~session_id
          ~initial:""
          ~restore_snapshot:(fun snapshot -> Ok (text snapshot.S.Snapshot.payload))
          ~apply:(fun _ transaction -> Ok (text transaction.S.Transaction.delta))
          ~validate_transaction:(fun _ -> Ok ())
          ~validate:(fun _ -> Ok ())
        |> ok
      in
      let entries = (S.Journal.scan journal |> ok).entries in
      print_s
        [%sexp
          { appended_exactly =
              (String.equal
                 (S.Frame.payload (List.hd_exn entries).frame)
                 original_transaction
               : bool)
          ; head_is_original =
              (Option.equal
                 String.equal
                 recovered.latest_transaction_hash
                 (Some (S.Document_record.digest original_transaction))
               : bool)
          ; state = (recovered.state : string)
          }]));
  [%expect {| ((appended_exactly true) (head_is_original true) (state first)) |}]
;;

let%expect_test "failed document recovery preserves crash tails and CURRENT pointers" =
  List.iter
    [ "beta"; "malformed"; "newer"; "wrong_session"; "semantics"; "domain" ]
    ~f:(fun failure ->
      with_directory (fun env root ->
        let journal_directory = Filename.concat root "journal" in
        let snapshot_directory = Filename.concat root "snapshot" in
        let journal =
          S.Journal.create
            ~env
            ~directory:journal_directory
            ~max_payload_length:16384
            ~max_segment_bytes:1048576L
            ~max_segment_frames:100
          |> ok
        in
        let installed =
          S.Snapshot.install
            ~env
            ~directory:snapshot_directory
            ~max_payload_length:16384
            (snapshot_document 0L None "initial")
          |> ok
        in
        let payload =
          match failure with
          | "beta" -> "independent unsupported beta payload"
          | "malformed" -> "{\"format\":\"ochat.document\",\"format\":\"duplicate\"}"
          | "newer" ->
            String.substr_replace_first
              original_transaction
              ~pattern:"\"schema_version\":1"
              ~with_:"\"schema_version\":2"
          | "wrong_session" ->
            String.substr_replace_first
              original_transaction
              ~pattern:"ses_store_document"
              ~with_:"ses_foreign"
          | "semantics" ->
            String.substr_replace_first
              original_transaction
              ~pattern:"\"future_envelope\":true"
              ~with_:"\"required_semantics\":[\"future-required\"]"
          | "domain" -> original_transaction
          | _ -> assert false
        in
        S.Journal.append journal ~durability:Flush ~flags:0 ~payload |> ok |> ignore;
        let segment =
          S.Journal_segment.open_existing
            ~env
            ~directory:journal_directory
            ~id:(S.Journal.current_segment journal)
          |> ok
        in
        let partial =
          S.Frame.encode ~max_payload_length:16384 ~flags:0 "physical-tail"
          |> frame_ok
          |> Fn.flip String.drop_suffix 3
        in
        S.Journal_segment.append ~env ~durability:Flush segment ~frame:partial
        |> ok
        |> ignore;
        let path name = Eio.Path.(Eio.Stdenv.fs env / name) in
        let files =
          [ S.Journal_segment.path segment
          ; Filename.concat journal_directory "CURRENT"
          ; Filename.concat snapshot_directory "CURRENT"
          ; Filename.concat snapshot_directory installed.filename
          ]
        in
        let originals = List.map files ~f:(fun file -> file, Eio.Path.load (path file)) in
        let restores = ref 0 in
        let recovered =
          S.Recovery.load
            ~env
            ~journal
            ~snapshot_directory
            ~max_snapshot_payload_length:16384
            ~session_id
            ~initial:""
            ~restore_snapshot:(fun snapshot ->
              incr restores;
              Ok (text snapshot.S.Snapshot.payload))
            ~apply:(fun _ transaction -> Ok (text transaction.S.Transaction.delta))
            ~validate_transaction:(fun _ ->
              if String.equal failure "domain"
              then Error (S.Store_error.Corrupt "independent domain rejection")
              else Ok ())
            ~validate:(fun _ -> Ok ())
        in
        print_s
          [%sexp
            (( failure
             , (Result.is_error recovered : bool)
             , (List.for_all originals ~f:(fun (file, bytes) ->
                  String.equal bytes (Eio.Path.load (path file)))
                : bool)
             , (!restores : int) )
             : string * bool * bool * int)]));
  [%expect
    {|
    (beta true true 0)
    (malformed true true 0)
    (newer true true 0)
    (wrong_session true true 0)
    (semantics true true 0)
    (domain true true 1)
    |}]
;;

let%expect_test "snapshot edits retain outer and embedded unknown fields" =
  with_directory (fun env root ->
    let first_directory = Filename.concat root "first" in
    let first =
      S.Snapshot.install
        ~env
        ~directory:first_directory
        ~max_payload_length:16384
        (snapshot_document
           1L
           (Some (S.Document_record.digest original_transaction))
           "before")
      |> ok
    in
    let file = Eio.Path.(Eio.Stdenv.fs env / first_directory / first.filename) in
    let record =
      S.Document_record.decode_file ~limits ~expected_digest:None (Eio.Path.load file)
    in
    let record =
      match record with
      | Ok record -> record
      | Error error -> raise_s [%sexp (error : S.Document_record.Error.t)]
    in
    let json =
      match D.Document.json (S.Document_record.document record) with
      | `Object fields -> `Object (fields @ [ "future_snapshot", `Null ])
      | _ -> assert false
    in
    let document = D.Document.inspect ~limits json |> doc_ok in
    let framed = S.Document_record.encode document ~limits ~flags:0 in
    let framed =
      match framed with
      | Ok bytes -> bytes
      | Error error -> raise_s [%sexp (error : S.Document_record.Error.t)]
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) file framed;
    let restored =
      S.Snapshot.read_file
        ~env
        ~directory:first_directory
        ~max_payload_length:16384
        ~filename:first.filename
      |> ok
    in
    let payload =
      match D.Document.payload restored.snapshot.payload with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if String.equal name "text" then name, `String "after" else name, value))
      | _ -> assert false
    in
    let state =
      match D.Document.json restored.snapshot.payload with
      | `Object fields ->
        D.Document.inspect
          ~limits
          (`Object
              (List.map fields ~f:(fun (name, value) ->
                 if String.equal name "payload" then name, payload else name, value)))
        |> doc_ok
      | _ -> assert false
    in
    let updated =
      S.Snapshot.with_value
        restored.snapshot
        ~limits
        { (S.Snapshot.value restored.snapshot) with payload = state }
      |> ok
    in
    let installed =
      S.Snapshot.install
        ~env
        ~directory:(Filename.concat root "next")
        ~max_payload_length:16384
        updated
      |> ok
    in
    let bytes =
      Eio.Path.load
        Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "next" / installed.filename)
    in
    print_s
      [%sexp
        { changed = (String.equal (text installed.snapshot.payload) "after" : bool)
        ; outer_retained = (String.is_substring bytes ~substring:"future_snapshot" : bool)
        ; child_retained = (String.is_substring bytes ~substring:"future_state" : bool)
        }]);
  [%expect {| ((changed true) (outer_retained true) (child_retained true)) |}]
;;

let%expect_test "unsupported retained snapshots prevent pruning before any deletion" =
  with_directory (fun env root ->
    let directory = Filename.concat root "snapshot" in
    List.iter [ 1L; 2L; 3L ] ~f:(fun sequence ->
      S.Snapshot.install
        ~env
        ~directory
        ~max_payload_length:16384
        (snapshot_document
           sequence
           (Some (S.Document_record.digest (Int64.to_string sequence)))
           "kept")
      |> ok
      |> ignore);
    let oldest =
      Eio.Path.(Eio.Stdenv.fs env / directory / "snapshot-0000000000000001.bin")
    in
    let before = Eio.Path.load oldest in
    let decoded =
      S.Frame.decode ~max_payload_length:16384 ~contents:before ~offset:0 |> frame_ok
    in
    let payload =
      match decoded with
      | Complete { frame; next_offset = _ } -> S.Frame.payload frame
      | Incomplete_tail _ -> assert false
    in
    let unsupported =
      String.substr_replace_first
        payload
        ~pattern:"\"schema_version\":1"
        ~with_:"\"schema_version\":2"
    in
    let bytes =
      S.Frame.encode ~max_payload_length:16384 ~flags:0 unsupported |> frame_ok
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) oldest bytes;
    let names =
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)
      |> List.sort ~compare:String.compare
    in
    let pointer = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / directory / "CURRENT") in
    let result =
      S.Snapshot.prune_older ~max_payload_length:16384 ~env ~directory ~keep:2
    in
    print_s
      [%sexp
        { rejected = (Result.is_error result : bool)
        ; names_unchanged =
            (List.equal
               String.equal
               names
               (Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)
                |> List.sort ~compare:String.compare)
             : bool)
        ; oldest_unchanged = (String.equal bytes (Eio.Path.load oldest) : bool)
        ; pointer_unchanged =
            (String.equal
               pointer
               (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / directory / "CURRENT"))
             : bool)
        }];
    let semantics =
      String.substr_replace_first
        payload
        ~pattern:"\"payload\":"
        ~with_:"\"required_semantics\":[\"future-required\"],\"payload\":"
    in
    let semantic_bytes =
      S.Frame.encode ~max_payload_length:16384 ~flags:0 semantics |> frame_ok
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) oldest semantic_bytes;
    assert (
      Result.is_error
        (S.Snapshot.prune_older ~max_payload_length:16384 ~env ~directory ~keep:2));
    assert (String.equal semantic_bytes (Eio.Path.load oldest));
    assert (
      String.equal
        pointer
        (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / directory / "CURRENT")));
    assert (
      List.equal
        String.equal
        names
        (Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)
         |> List.sort ~compare:String.compare)));
  [%expect
    {|
    ((rejected true) (names_unchanged true) (oldest_unchanged true)
     (pointer_unchanged true))
    |}]
;;

let%expect_test "aggregate cache completion, reopen and retention share durable limits" =
  with_directory (fun env root ->
    let module I = S.Idempotency_store in
    let key name =
      I.Key.
        { principal_id =
            Agent_protocol.Id.Principal.of_string "pri_store_document" |> protocol_ok
        ; session_id = Some session_id
        ; method_name = "session.send_message"
        ; idempotency_key = Agent_protocol.Idempotency_key.of_string name |> protocol_ok
        }
    in
    let pending name =
      I.
        { key = key name
        ; request_digest = name
        ; accepted_transaction_sequence = None
        ; outcome = Pending
        ; created_at = timestamp
        ; expires_at = None
        ; retention = Standard
        }
    in
    let outcome =
      `Object (List.init 50_001 ~f:(fun i -> sprintf "field_%06d" i, `Null))
    in
    let path = Filename.concat root "cache.json" in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let cache = I.open_or_create ~env ~path |> ok in
    List.iter [ "first"; "second" ] ~f:(fun name ->
      I.record cache (pending name) |> ok |> ignore;
      I.complete
        cache
        ~key:(key name)
        ~request_digest:name
        ~accepted_transaction_sequence:None
        ~outcome:(Success outcome)
      |> ok
      |> ignore);
    let saved = Eio.Path.load file in
    assert (
      match D.Json.decode ~limits:D.Limits.default saved with
      | Error (Limit_exceeded "fields") -> true
      | Error _ | Ok _ -> false);
    let cache_limits = S.Document_fields.limits ~max_bytes:(16 * 1024 * 1024) |> doc_ok in
    let captured = D.Document.decode ~limits:cache_limits saved |> doc_ok in
    let blob = Agent_protocol.Id.Blob.create () in
    let json =
      match D.Document.json captured with
      | `Object fields ->
        `Object
          (fields
           @ [ ( "future_cache"
               , `Object [ "blob", Agent_protocol.Id.Blob.to_json blob; "opaque", `Null ]
               )
             ])
      | _ -> assert false
    in
    let captured = D.Document.inspect ~limits:cache_limits json |> doc_ok in
    Eio.Path.save ~create:(`Or_truncate 0o600) file (D.Document.to_string captured);
    let reopened = I.open_or_create ~env ~path |> ok in
    let accepted =
      I.mark_accepted
        reopened
        ~key:(key "first")
        ~request_digest:"first"
        ~transaction_sequence:17L
      |> ok
    in
    assert (Option.equal Int64.equal accepted.accepted_transaction_sequence (Some 17L));
    let check_outcome cache name =
      match I.lookup cache ~key:(key name) ~request_digest:name with
      | Replay { outcome = Success actual; _ } -> assert (D.Json.equal outcome actual)
      | Missing | Conflict _ | Replay _ -> assert false
    in
    List.iter [ "first"; "second" ] ~f:(check_outcome reopened);
    let reread = I.open_or_create ~env ~path |> ok in
    List.iter [ "first"; "second" ] ~f:(check_outcome reread);
    let references =
      I.with_retained_references
        reread
        ~candidates:[ blob; Agent_protocol.Id.Blob.create () ]
        ~max_records:4
        ~max_bytes:(8 * 1024 * 1024)
        ~f:(fun references -> Ok references)
      |> ok
      |> Option.value_exn
    in
    assert (List.equal Agent_protocol.Id.Blob.equal references [ blob ]);
    I.record reread (pending "byte-rejected") |> ok |> ignore;
    let before = Eio.Path.load file in
    let individual_outcome = `String (String.make (15 * 1024 * 1024) 'x') in
    D.Json.validate ~limits:cache_limits individual_outcome |> doc_ok;
    let rejected =
      I.complete
        reread
        ~key:(key "byte-rejected")
        ~request_digest:"byte-rejected"
        ~accepted_transaction_sequence:None
        ~outcome:(Success individual_outcome)
    in
    assert (
      match rejected with
      | Error (Document (Limit_exceeded "bytes")) -> true
      | Error _ | Ok _ -> false);
    assert (String.equal before (Eio.Path.load file));
    assert (
      match
        I.lookup reread ~key:(key "byte-rejected") ~request_digest:"byte-rejected"
      with
      | Replay { outcome = Pending; _ } -> true
      | Missing | Conflict _ | Replay _ -> false);
    assert (String.is_substring before ~substring:"future_cache");
    print_endline
      "aggregate responses complete and reopen; updates retain future references; byte \
       rejection preserves pending receipt and file");
  [%expect
    {| aggregate responses complete and reopen; updates retain future references; byte rejection preserves pending receipt and file |}]
;;

let%expect_test "idempotency updates and deliberate expiry retain unrelated extensions" =
  with_directory (fun env root ->
    let module I = S.Idempotency_store in
    let key name =
      I.Key.
        { principal_id =
            Agent_protocol.Id.Principal.of_string "pri_store_document" |> protocol_ok
        ; session_id = Some session_id
        ; method_name = "session.stop"
        ; idempotency_key = Agent_protocol.Idempotency_key.of_string name |> protocol_ok
        }
    in
    let path = Filename.concat root "cache.sexp" in
    let cache = I.open_or_create ~env ~path |> ok in
    let receipt name retention =
      I.
        { key = key name
        ; request_digest = name
        ; accepted_transaction_sequence = None
        ; outcome = Success (`String "kept")
        ; created_at = timestamp
        ; expires_at = Some timestamp
        ; retention
        }
    in
    I.record cache (receipt "expire" Standard) |> ok |> ignore;
    I.record cache (receipt "protect" Protected) |> ok |> ignore;
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let rec add = function
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if String.equal name "records"
             then (
               match value with
               | `Array records ->
                 ( name
                 , `Array
                     (List.map records ~f:(function
                        | `Object fields -> `Object (fields @ [ "future_record", `Null ])
                        | value -> value)) )
               | value -> name, value)
             else name, add value))
      | value -> value
    in
    let enriched =
      match Jsonaf.of_string (Eio.Path.load file) |> add with
      | `Object fields -> `Object (fields @ [ "future_cache", `String "kept" ])
      | _ -> assert false
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) file (Jsonaf.to_string enriched);
    let cache = I.open_or_create ~env ~path |> ok in
    I.mark_accepted
      cache
      ~key:(key "protect")
      ~request_digest:"protect"
      ~transaction_sequence:4L
    |> ok
    |> ignore;
    let after_update = Eio.Path.load file in
    let removed = I.prune_expired cache ~now:timestamp |> ok in
    let after_expiry = Eio.Path.load file in
    print_s
      [%sexp
        { unknown_survived_update =
            (String.is_substring after_update ~substring:"future_record" : bool)
        ; removed : int
        ; remaining_unknown_record =
            (String.is_substring after_expiry ~substring:"future_record" : bool)
        ; envelope_unknown =
            (String.is_substring after_expiry ~substring:"future_cache" : bool)
        ; protected_survived =
            ((match I.lookup cache ~key:(key "protect") ~request_digest:"protect" with
              | Replay _ -> true
              | Missing | Conflict _ -> false)
             : bool)
        }]);
  [%expect
    {|
    ((unknown_survived_update true) (removed 1) (remaining_unknown_record true)
     (envelope_unknown true) (protected_survived true))
    |}]
;;

let%expect_test "beta idempotency bytes are rejected and preserved" =
  with_directory (fun env root ->
    let path = Filename.concat root "cache.sexp" in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let beta = "(independently-authored unsupported beta bytes)" in
    Eio.Path.save ~create:(`Exclusive 0o600) file beta;
    let result = S.Idempotency_store.open_or_create ~env ~path in
    print_s
      [%sexp
        { unsupported_beta =
            ((match result with
              | Error (S.Store_error.Document D.Error.Unsupported_beta_format) -> true
              | Ok _ | Error _ -> false)
             : bool)
        ; original_bytes_unchanged = (String.equal beta (Eio.Path.load file) : bool)
        }]);
  [%expect {| ((unsupported_beta true) (original_bytes_unchanged true)) |}]
;;

let%expect_test "restored command audit edits retain named extensions" =
  let module A = S.Idempotency_store.Command_audit in
  let restored =
    D.Document.decode
      ~limits
      {|{"format":"ochat.document","kind":"session.command_audit","schema_version":1,"payload":{"key":{"principal_id":"pri_store_document","session_id":"ses_store_document","method_name":"session.stop","idempotency_key":"audit","future_key":null},"request_digest":"original","protected_record":false,"future_payload":7},"future_envelope":true}|}
    |> doc_ok
    |> A.restore
    |> ok
  in
  let edited =
    D.Extension_carrier.with_value
      restored
      { (D.Extension_carrier.value restored) with request_digest = "edited" }
  in
  let encoded = A.encode_carrier edited |> ok |> D.Document.to_string in
  print_s
    [%sexp
      { key_extension = (String.is_substring encoded ~substring:"future_key" : bool)
      ; payload_extension =
          (String.is_substring encoded ~substring:"future_payload" : bool)
      ; envelope_extension =
          (String.is_substring encoded ~substring:"future_envelope" : bool)
      ; edit_applied = (String.is_substring encoded ~substring:"edited" : bool)
      }];
  [%expect
    {|
    ((key_extension true) (payload_extension true) (envelope_extension true)
     (edit_applied true))
    |}]
;;

let%expect_test "wide snapshot counters sort numerically for fallback and pruning" =
  with_directory (fun env root ->
    let directory = Filename.concat root "snapshot" in
    let hash sequence = Some (S.Document_record.digest (Int64.to_string sequence)) in
    let install sequence =
      S.Snapshot.install
        ~env
        ~directory
        ~max_payload_length:16384
        (snapshot_document sequence (hash sequence) "kept")
      |> ok
    in
    let oldest = install 9_999_999_999_999_999L in
    let newest = install 10_000_000_000_000_000L in
    let dangling = install 10_000_000_000_000_001L in
    Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / directory / dangling.filename);
    let fallback =
      S.Snapshot.load_current ~env ~directory ~max_payload_length:16384
      |> ok
      |> Option.value_exn
    in
    assert (String.equal fallback.filename newest.filename);
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / directory / "CURRENT")
      (newest.filename ^ "\n");
    let pruned =
      S.Snapshot.prune_older ~env ~directory ~max_payload_length:16384 ~keep:1 |> ok
    in
    assert (
      not (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / directory / oldest.filename)));
    assert (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / directory / newest.filename));
    assert (
      Result.is_error
        (S.Snapshot.read_stored_file
           ~env
           ~directory
           ~max_payload_length:Int.max_value
           ~filename:newest.filename));
    print_s [%sexp { newest_selected = (true : bool); pruned : int }]);
  [%expect {| ((newest_selected true) (pruned 1)) |}]
;;

let%expect_test "exhausted durable counters never wrap or write rejected frames" =
  with_directory (fun env root ->
    Eio.Switch.run (fun sw ->
      let directory = Filename.concat root "writer" in
      let journal =
        S.Journal.create
          ~env
          ~directory
          ~max_payload_length:16384
          ~max_segment_bytes:1048576L
          ~max_segment_frames:100
        |> ok
      in
      let previous = Some (S.Document_record.digest "previous") in
      let writer =
        S.Commit_writer.create
          ~sw
          ~journal
          ~session_id
          ~next_transaction_sequence:Int64.max_value
          ~previous_transaction_hash:previous
          ~queue_capacity:2
        |> ok
      in
      let restored = S.Transaction.decode original_transaction |> ok in
      let last =
        S.Transaction.with_value
          restored
          ~limits
          { (S.Transaction.value restored) with
            transaction_sequence = Int64.max_value
          ; previous_transaction_hash = previous
          ; session_revision = Int64.max_value
          }
        |> ok
      in
      let committed = S.Commit_writer.commit writer ~durability:Flush last |> ok in
      let path = Eio.Path.(Eio.Stdenv.fs env / directory / "0000000000000001.log") in
      let before = Eio.Path.load path in
      assert (Int64.equal committed.transaction_sequence Int64.max_value);
      assert (Result.is_error (S.Commit_writer.commit writer ~durability:Flush last));
      assert (String.equal before (Eio.Path.load path));
      S.Commit_writer.close writer;
      let directory = Filename.concat root "segment" in
      ignore
        (S.Journal.create
           ~env
           ~directory
           ~max_payload_length:16384
           ~max_segment_bytes:1048576L
           ~max_segment_frames:1
         |> ok
         : S.Journal.t);
      let max_id = S.Journal_segment.Id.of_int64 Int64.max_value |> ok in
      let maximum =
        Eio.Path.(Eio.Stdenv.fs env / directory / S.Journal_segment.Id.filename max_id)
      in
      Eio.Path.rename
        Eio.Path.(Eio.Stdenv.fs env / directory / "0000000000000001.log")
        maximum;
      let current = Eio.Path.(Eio.Stdenv.fs env / directory / "CURRENT") in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        current
        (S.Journal_segment.Id.filename max_id ^ "\n");
      let journal =
        S.Journal.open_existing
          ~env
          ~directory
          ~max_payload_length:16384
          ~max_segment_bytes:1048576L
          ~max_segment_frames:1
        |> ok
      in
      let before = Eio.Path.load maximum
      and pointer = Eio.Path.load current in
      S.Journal.validate_seal_checkpoint journal |> ok;
      S.Journal.seal_checkpoint journal |> ok;
      assert (Result.is_error (S.Journal.rotate journal ~terminal_payload:"sealed"));
      assert (
        Result.is_error
          (S.Journal.append
             journal
             ~durability:Flush
             ~flags:0
             ~payload:original_transaction));
      assert (String.equal before (Eio.Path.load maximum));
      assert (String.equal pointer (Eio.Path.load current));
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        maximum
        (S.Frame.encode ~max_payload_length:16384 ~flags:0 original_transaction
         |> frame_ok);
      let nonempty =
        S.Journal.open_existing
          ~env
          ~directory
          ~max_payload_length:16384
          ~max_segment_bytes:1048576L
          ~max_segment_frames:1
        |> ok
      in
      let before = Eio.Path.load maximum in
      assert (Result.is_error (S.Journal.validate_seal_checkpoint nonempty));
      assert (Result.is_error (S.Journal.seal_checkpoint nonempty));
      assert (String.equal before (Eio.Path.load maximum));
      assert (String.equal pointer (Eio.Path.load current));
      let directory = Filename.concat root "tiny" in
      let journal =
        S.Journal.create
          ~env
          ~directory
          ~max_payload_length:1
          ~max_segment_bytes:1048576L
          ~max_segment_frames:2
        |> ok
      in
      ignore
        (S.Journal.append journal ~durability:Flush ~flags:0 ~payload:"a" |> ok
         : S.Journal.append_result);
      let path = Eio.Path.(Eio.Stdenv.fs env / directory / "0000000000000001.log") in
      let before = Eio.Path.load path in
      assert (Result.is_error (S.Journal.validate_seal_checkpoint journal));
      assert (Result.is_error (S.Journal.seal_checkpoint journal));
      assert (
        Result.is_error (S.Journal.append journal ~durability:Flush ~flags:0 ~payload:"b"));
      assert (String.equal before (Eio.Path.load path));
      print_endline
        "last transaction acknowledged; exhausted writer and segment reject unchanged"));
  [%expect
    {| last transaction acknowledged; exhausted writer and segment reject unchanged |}]
;;

let%expect_test "every retained fallback tail validates before crash-tail repair" =
  with_directory (fun env root ->
    let journal_directory = Filename.concat root "journal"
    and snapshot_directory = Filename.concat root "snapshot" in
    let journal =
      S.Journal.create
        ~env
        ~directory:journal_directory
        ~max_payload_length:16384
        ~max_segment_bytes:1048576L
        ~max_segment_frames:100
      |> ok
    in
    let first = S.Transaction.decode original_transaction |> ok in
    let second =
      S.Transaction.with_value
        first
        ~limits
        { (S.Transaction.value first) with
          transaction_sequence = 2L
        ; session_revision = 2L
        ; previous_transaction_hash = Some (S.Transaction.hash first)
        ; delta =
            document "session.delta" (`Object [ "text", `String "retire_unknown_entry" ])
        }
      |> ok
    in
    List.iter [ first; second ] ~f:(fun transaction ->
      S.Journal.append
        journal
        ~durability:Flush
        ~flags:0
        ~payload:(S.Transaction.encode transaction)
      |> ok
      |> ignore);
    List.iter
      [ snapshot_document 0L None "empty"
      ; snapshot_document 2L (Some (S.Transaction.hash second)) "empty"
      ]
      ~f:(fun snapshot ->
        S.Snapshot.install
          ~env
          ~directory:snapshot_directory
          ~max_payload_length:16384
          snapshot
        |> ok
        |> ignore);
    let path relative = Eio.Path.(Eio.Stdenv.fs env / root / relative) in
    let journal_path = path "journal/0000000000000001.log" in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      journal_path
      (Eio.Path.load journal_path ^ "\001");
    let before = Eio.Path.load journal_path in
    let snapshot_current = Eio.Path.load (path "snapshot/CURRENT")
    and journal_current = Eio.Path.load (path "journal/CURRENT") in
    (* Valid individual deltas can still retire unknown-bearing state illegally
       when replayed together. The newest checkpoint covers both transitions. *)
    let apply unknown transaction =
      if Int64.equal transaction.S.Transaction.transaction_sequence 1L
      then Ok true
      else if unknown
      then
        Error
          (S.Store_error.Document (D.Error.Extension_conflict [ "entry"; "future_host" ]))
      else Ok false
    in
    let recovered =
      S.Recovery.load
        ~env
        ~journal
        ~snapshot_directory
        ~max_snapshot_payload_length:16384
        ~session_id
        ~initial:false
        ~restore_snapshot:(fun _ -> Ok false)
        ~apply
        ~validate_transaction:(fun _ -> Ok ())
        ~validate:(fun _ -> Ok ())
    in
    assert (Result.is_error recovered);
    assert (String.equal before (Eio.Path.load journal_path));
    assert (String.equal snapshot_current (Eio.Path.load (path "snapshot/CURRENT")));
    assert (String.equal journal_current (Eio.Path.load (path "journal/CURRENT")));
    print_endline
      "covered transitions replayed from fallback; invalid retirement leaves crash tail \
       and pointers intact");
  [%expect
    {| covered transitions replayed from fallback; invalid retirement leaves crash tail and pointers intact |}]
;;

let with_fixture ~unknown_oldest f =
  with_directory (fun env root ->
    let journal_directory = Filename.concat root "journal"
    and snapshot_directory = Filename.concat root "snapshot" in
    let journal =
      S.Journal.create
        ~env
        ~directory:journal_directory
        ~max_payload_length:16384
        ~max_segment_bytes:1048576L
        ~max_segment_frames:100
      |> ok
    in
    let transactions =
      let first = S.Transaction.decode original_transaction |> ok in
      List.fold
        (List.init 5 ~f:(fun i -> Int64.of_int (i + 2)))
        ~init:[ first ]
        ~f:(fun reversed sequence ->
          let previous = List.hd_exn reversed in
          let transaction =
            S.Transaction.with_value
              previous
              ~limits
              { (S.Transaction.value previous) with
                transaction_sequence = sequence
              ; session_revision = sequence
              ; previous_transaction_hash = Some (S.Transaction.hash previous)
              }
            |> ok
          in
          transaction :: reversed)
      |> List.rev
    in
    List.iter transactions ~f:(fun transaction ->
      S.Journal.append
        journal
        ~durability:Flush
        ~flags:0
        ~payload:(S.Transaction.encode transaction)
      |> ok
      |> ignore);
    List.iter [ 0L; 2L; 4L ] ~f:(fun sequence ->
      let hash =
        List.find transactions ~f:(fun transaction ->
          Int64.equal transaction.S.Transaction.transaction_sequence sequence)
        |> Option.map ~f:S.Transaction.hash
      in
      let snapshot = snapshot_document sequence hash "same" in
      let snapshot =
        if unknown_oldest && Int64.equal sequence 0L
        then (
          let payload =
            match D.Document.payload snapshot.payload with
            | `Object fields ->
              document
                "session.state"
                (`Object
                    (List.map fields ~f:(fun (key, value) ->
                       ( key
                       , if String.equal key "future_state"
                         then `String "opaque"
                         else value ))))
            | _ -> assert false
          in
          S.Snapshot.with_value
            snapshot
            ~limits
            { (S.Snapshot.value snapshot) with payload }
          |> ok)
        else snapshot
      in
      S.Snapshot.install
        ~env
        ~directory:snapshot_directory
        ~max_payload_length:16384
        snapshot
      |> ok
      |> ignore);
    let path relative = Eio.Path.(Eio.Stdenv.fs env / root / relative) in
    let journal_path = path "journal/0000000000000001.log" in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      journal_path
      (Eio.Path.load journal_path ^ "\001");
    let before = Eio.Path.load journal_path
    and snapshot_current = Eio.Path.load (path "snapshot/CURRENT")
    and journal_current = Eio.Path.load (path "journal/CURRENT") in
    f env journal snapshot_directory;
    assert (String.equal before (Eio.Path.load journal_path));
    assert (String.equal snapshot_current (Eio.Path.load (path "snapshot/CURRENT")));
    assert (String.equal journal_current (Eio.Path.load (path "journal/CURRENT"))))
;;

let apply_shared_document document transaction =
  let sequence =
    `String (Int64.to_string transaction.S.Transaction.transaction_sequence)
  in
  D.Document.replace_payload_scalars
    document
    ~limits
    ~updates:
      [ [ "counters"; "transaction_sequence" ], sequence
      ; [ "counters"; "revision" ], sequence
      ]
  |> Result.map_error ~f:(fun error -> S.Store_error.Document error)
;;

let equivalent_shared_document left right =
  Ok (String.equal (D.Document.to_string left) (D.Document.to_string right))
;;

let run_shared_preflight
      ~shared
      ~env
      ~journal
      ~snapshot_directory
      ~apply
      ~validate_transaction
      ~validate
  =
  let restore_snapshot snapshot = Ok snapshot.S.Snapshot.payload in
  let initial = (snapshot_document 0L None "same").payload in
  if shared
  then
    S.Recovery.preflight_shared
      ~env
      ~journal
      ~snapshot_directory
      ~max_snapshot_payload_length:16384
      ~session_id
      ~initial
      ~restore_snapshot
      ~apply
      ~equivalent:equivalent_shared_document
      ~validate_transaction
      ~validate
  else
    S.Recovery.preflight
      ~env
      ~journal
      ~snapshot_directory
      ~max_snapshot_payload_length:16384
      ~session_id
      ~initial
      ~restore_snapshot
      ~apply
      ~validate_transaction
      ~validate
;;

let%expect_test "shared preflight reuses only complete matching validated suffixes" =
  with_fixture ~unknown_oldest:false (fun env journal snapshot_directory ->
    List.iter
      [ false, 12; true, 6 ]
      ~f:(fun (shared, expected_applies) ->
        let applies = ref 0
        and transaction_validations = ref 0
        and state_validations = ref 0 in
        run_shared_preflight
          ~shared
          ~env
          ~journal
          ~snapshot_directory
          ~apply:(fun state transaction ->
            incr applies;
            apply_shared_document state transaction)
          ~validate_transaction:(fun _ ->
            incr transaction_validations;
            Ok ())
          ~validate:(fun _ ->
            incr state_validations;
            Ok ())
        |> ok;
        assert (!applies = expected_applies);
        assert (!transaction_validations = 6);
        assert (!state_validations = 6)));
  print_endline
    "three anchors: 12 ordinary applies, 6 shared; all domains and final states validate";
  [%expect
    {| three anchors: 12 ordinary applies, 6 shared; all domains and final states validate |}]
;;

let%expect_test "shared preflight rejects invalid covered prefixes and newer suffixes" =
  List.iter [ 1L; 6L ] ~f:(fun rejected ->
    with_fixture ~unknown_oldest:false (fun env journal snapshot_directory ->
      let seen = ref [] in
      let result =
        run_shared_preflight
          ~shared:true
          ~env
          ~journal
          ~snapshot_directory
          ~apply:(fun state transaction ->
            let sequence = transaction.S.Transaction.transaction_sequence in
            seen := sequence :: !seen;
            if Int64.equal sequence rejected
            then Error (S.Store_error.Corrupt "invalid replay transition")
            else apply_shared_document state transaction)
          ~validate_transaction:(fun _ -> Ok ())
          ~validate:(fun _ -> Ok ())
      in
      assert (Result.is_error result);
      assert (List.mem !seen rejected ~equal:Int64.equal)));
  print_endline "covered invalid prefix and newest invalid suffix refuse without repair";
  [%expect {| covered invalid prefix and newest invalid suffix refuse without repair |}]
;;

let%expect_test "shared preflight unequal unknown carriers continue and reject retirement"
  =
  with_fixture ~unknown_oldest:true (fun env journal snapshot_directory ->
    let applied_unknown_tail = ref false in
    let result =
      run_shared_preflight
        ~shared:true
        ~env
        ~journal
        ~snapshot_directory
        ~apply:(fun state transaction ->
          if
            Int64.equal transaction.S.Transaction.transaction_sequence 3L
            &&
            match D.Json.field (D.Document.payload state) ~name:"future_state" with
            | Value (`String _) -> true
            | Absent | Null | Value _ -> false
          then (
            applied_unknown_tail := true;
            Error (S.Store_error.Document (D.Error.Extension_conflict [ "future_state" ])))
          else apply_shared_document state transaction)
        ~validate_transaction:(fun _ -> Ok ())
        ~validate:(fun _ -> Ok ())
    in
    assert (Result.is_error result);
    assert !applied_unknown_tail);
  print_endline "equal counters with unequal unknown fields cannot skip failing tail";
  [%expect {| equal counters with unequal unknown fields cannot skip failing tail |}]
;;

let retention_preflight
      ~env
      ~journal
      ~snapshot_directory
      ~apply
      ~validate_transaction
      ~validate
  =
  S.Recovery.Retention_preflight.create
    ~env
    ~journal
    ~snapshot_directory
    ~max_snapshot_payload_length:16384
    ~session_id
    ~initial:(snapshot_document 0L None "same").payload
    ~restore_snapshot:(fun snapshot -> Ok snapshot.S.Snapshot.payload)
    ~apply
    ~equivalent:equivalent_shared_document
    ~validate_transaction
    ~validate
;;

let%expect_test "retention scope reuses a prefix but repeats every fresh validator" =
  with_fixture ~unknown_oldest:false (fun env journal snapshot_directory ->
    let applies = ref 0
    and transaction_validations = ref 0
    and state_validations = ref 0 in
    let archive_path =
      Eio.Path.(Eio.Stdenv.fs env / Filename.dirname snapshot_directory / "archive")
    in
    let write_archive contents =
      Eio.Path.save ~create:(`Or_truncate 0o600) archive_path contents
    in
    write_archive "present";
    let at_certified_head state =
      match D.Json.field (D.Document.payload state) ~name:"counters" with
      | Value counters ->
        (match D.Json.field counters ~name:"transaction_sequence" with
         | Value (`String "6") -> true
         | Absent | Null | Value _ -> false)
      | Absent | Null -> false
    in
    let create () =
      retention_preflight
        ~env
        ~journal
        ~snapshot_directory
        ~apply:(fun state transaction ->
          incr applies;
          apply_shared_document state transaction)
        ~validate_transaction:(fun _ ->
          incr transaction_validations;
          Ok ())
        ~validate:(fun state ->
          incr state_validations;
          if
            at_certified_head state
            && not (String.equal (Eio.Path.load archive_path) "present")
          then Error (S.Store_error.Corrupt "archive unavailable")
          else Ok ())
    in
    let check scope expected_applies =
      applies := 0;
      transaction_validations := 0;
      state_validations := 0;
      S.Recovery.Retention_preflight.check scope |> ok;
      assert (!applies = expected_applies);
      assert (!transaction_validations = 6);
      assert (!state_validations = 6)
    in
    let scope = create () in
    check scope 6;
    check scope 0;
    (* A rejected retirement request does not undo successful validation. *)
    assert (
      Result.is_error
        (S.Snapshot.prune_older
           ~env
           ~directory:snapshot_directory
           ~max_payload_length:16384
           ~keep:0));
    check scope 0;
    write_archive "changed";
    applies := 0;
    transaction_validations := 0;
    state_validations := 0;
    assert (Result.is_error (S.Recovery.Retention_preflight.check scope));
    assert (!applies = 0);
    assert (!transaction_validations = 6);
    assert (!state_validations = 4);
    write_archive "present";
    check scope 0;
    (* Proofs belong to one fixed owner, never a global cache. *)
    check (create ()) 6);
  print_endline
    "first check matches shared replay; repeats apply zero, validate all, and never \
     repair";
  [%expect
    {| first check matches shared replay; repeats apply zero, validate all, and never repair |}]
;;

let next_retention_transaction previous sequence =
  S.Transaction.with_value
    previous
    ~limits
    { (S.Transaction.value previous) with
      transaction_sequence = sequence
    ; session_revision = sequence
    ; previous_transaction_hash = Some (S.Transaction.hash previous)
    }
  |> ok
;;

let append_retention_transaction journal transaction =
  S.Journal.append
    journal
    ~durability:Flush
    ~flags:0
    ~payload:(S.Transaction.encode transaction)
  |> ok
  |> ignore
;;

let install_retention_snapshot ~env ~directory transactions sequence =
  let hash =
    List.find transactions ~f:(fun transaction ->
      Int64.equal transaction.S.Transaction.transaction_sequence sequence)
    |> Option.map ~f:S.Transaction.hash
  in
  S.Snapshot.install
    ~env
    ~directory
    ~max_payload_length:16384
    (snapshot_document sequence hash "same")
  |> ok
;;

let with_retention_files f =
  with_directory (fun env root ->
    let directory = Filename.concat root "snapshot" in
    let journal =
      S.Journal.create
        ~env
        ~directory:(Filename.concat root "journal")
        ~max_payload_length:16384
        ~max_segment_bytes:1048576L
        ~max_segment_frames:100
      |> ok
    in
    let first = S.Transaction.decode original_transaction |> ok in
    let transactions =
      List.fold
        (List.init 5 ~f:(fun i -> Int64.of_int (i + 2)))
        ~init:[ first ]
        ~f:(fun reversed sequence ->
          next_retention_transaction (List.hd_exn reversed) sequence :: reversed)
      |> List.rev
    in
    List.iter transactions ~f:(append_retention_transaction journal);
    let snapshots =
      List.map [ 0L; 2L; 4L ] ~f:(install_retention_snapshot ~env ~directory transactions)
    in
    f env root journal directory transactions snapshots)
;;

let retention_path env root relative = Eio.Path.(Eio.Stdenv.fs env / root / relative)

let assert_retention_read_only ~env ~root f =
  let paths =
    List.map
      [ "journal/0000000000000001.log"; "journal/CURRENT"; "snapshot/CURRENT" ]
      ~f:(retention_path env root)
  in
  let original = List.map paths ~f:Eio.Path.load in
  let result = f () in
  List.iter2_exn original paths ~f:(fun bytes path ->
    assert (String.equal bytes (Eio.Path.load path)));
  result
;;

let rewrite_retention_journal ~env ~root transactions =
  let contents =
    List.map transactions ~f:(fun transaction ->
      S.Frame.encode ~max_payload_length:16384 ~flags:0 (S.Transaction.encode transaction)
      |> frame_ok)
    |> String.concat
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (retention_path env root "journal/0000000000000001.log")
    (contents ^ "\001")
;;

let%expect_test "certified effective heads replay an appended suffix only once" =
  with_retention_files (fun env root journal directory transactions _ ->
    let applies = ref 0
    and transaction_validations = ref 0
    and state_validations = ref 0 in
    let scope =
      retention_preflight
        ~env
        ~journal
        ~snapshot_directory:directory
        ~apply:(fun state transaction ->
          incr applies;
          apply_shared_document state transaction)
        ~validate_transaction:(fun _ ->
          incr transaction_validations;
          Ok ())
        ~validate:(fun _ ->
          incr state_validations;
          Ok ())
    in
    S.Recovery.Retention_preflight.check scope |> ok;
    assert (!applies = 6);
    let seventh = next_retention_transaction (List.last_exn transactions) 7L in
    let eighth = next_retention_transaction seventh 8L in
    List.iter [ seventh; eighth ] ~f:(append_retention_transaction journal);
    let transactions = transactions @ [ seventh; eighth ] in
    install_retention_snapshot ~env ~directory transactions 8L |> ignore;
    rewrite_retention_journal ~env ~root transactions;
    applies := 0;
    transaction_validations := 0;
    state_validations := 0;
    assert_retention_read_only ~env ~root (fun () ->
      S.Recovery.Retention_preflight.check scope |> ok);
    assert (!applies = 2);
    assert (!transaction_validations = 8);
    assert (!state_validations = 8);
    applies := 0;
    assert_retention_read_only ~env ~root (fun () ->
      S.Recovery.Retention_preflight.check scope |> ok);
    assert (!applies = 0);
    S.Snapshot.prune_older ~env ~directory ~max_payload_length:16384 ~keep:2
    |> ok
    |> ignore;
    state_validations := 0;
    assert_retention_read_only ~env ~root (fun () ->
      S.Recovery.Retention_preflight.check scope |> ok);
    assert (!applies = 0);
    assert (!state_validations = 4));
  print_endline
    "new anchor and equal certified heads share two appended applies; pruning stays safe";
  [%expect
    {| new anchor and equal certified heads share two appended applies; pruning stays safe |}]
;;

let opaque_state state =
  match D.Json.field (D.Document.payload state) ~name:"future_state" with
  | Value (`String _) -> true
  | Absent | Null | Value _ -> false
;;

let snapshot_with_opaque_state snapshot =
  let payload =
    D.Document.replace_payload_scalars
      snapshot.S.Snapshot.payload
      ~limits
      ~updates:[ [ "future_state" ], `String "opaque" ]
    |> doc_ok
  in
  S.Snapshot.with_value snapshot ~limits { (S.Snapshot.value snapshot) with payload }
  |> ok
;;

let overwrite_retention_snapshot
      ~env
      ~directory
      (installed : S.Snapshot.installed)
      snapshot
  =
  (* Simulate externally replaced, individually valid complete bytes. Production
     installation correctly refuses to replace an existing checkpoint filename. *)
  let record = S.Snapshot.Stored.record (S.Snapshot.stored snapshot) in
  let contents =
    S.Frame.encode
      ~max_payload_length:16384
      ~flags:0
      (S.Document_record.stored_bytes record)
    |> frame_ok
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / directory / installed.S.Snapshot.filename)
    contents
;;

let%expect_test "changed snapshot bytes cannot borrow the previous replay seed" =
  with_retention_files (fun env root journal directory _ snapshots ->
    let applies = ref 0 in
    let scope =
      retention_preflight
        ~env
        ~journal
        ~snapshot_directory:directory
        ~apply:(fun state transaction ->
          incr applies;
          if
            opaque_state state
            && Int64.equal transaction.S.Transaction.transaction_sequence 3L
          then
            Error (S.Store_error.Document (D.Error.Extension_conflict [ "future_state" ]))
          else apply_shared_document state transaction)
        ~validate_transaction:(fun _ -> Ok ())
        ~validate:(fun _ -> Ok ())
    in
    S.Recovery.Retention_preflight.check scope |> ok;
    let oldest = List.hd_exn snapshots in
    let oldest_path =
      Eio.Path.(Eio.Stdenv.fs env / directory / oldest.S.Snapshot.filename)
    in
    let original = Eio.Path.load oldest_path in
    overwrite_retention_snapshot
      ~env
      ~directory
      oldest
      (snapshot_with_opaque_state oldest.snapshot);
    applies := 0;
    assert_retention_read_only ~env ~root (fun () ->
      assert (Result.is_error (S.Recovery.Retention_preflight.check scope)));
    assert (!applies = 3);
    (* An unsuccessful check did not replace the previously certified proof. *)
    Eio.Path.save ~create:(`Or_truncate 0o600) oldest_path original;
    applies := 0;
    assert_retention_read_only ~env ~root (fun () ->
      S.Recovery.Retention_preflight.check scope |> ok);
    assert (!applies = 0));
  print_endline
    "same counters with changed unknown bytes replay and reject; failure keeps prior \
     proof";
  [%expect
    {| same counters with changed unknown bytes replay and reject; failure keeps prior proof |}]
;;

let%expect_test
    "rewritten covered transaction prefix requires replay despite valid rechaining"
  =
  with_retention_files (fun env root journal directory transactions snapshots ->
    let seen = ref [] in
    let scope =
      retention_preflight
        ~env
        ~journal
        ~snapshot_directory:directory
        ~apply:(fun state transaction ->
          seen := transaction.S.Transaction.transaction_sequence :: !seen;
          if String.equal (text transaction.delta) "invalid transition"
          then Error (S.Store_error.Corrupt "invalid covered transition")
          else apply_shared_document state transaction)
        ~validate_transaction:(fun _ -> Ok ())
        ~validate:(fun _ -> Ok ())
    in
    S.Recovery.Retention_preflight.check scope |> ok;
    let first = List.hd_exn transactions in
    let first =
      S.Transaction.with_value
        first
        ~limits
        { (S.Transaction.value first) with
          delta =
            document "session.delta" (`Object [ "text", `String "invalid transition" ])
        }
      |> ok
    in
    let changed =
      List.fold (List.tl_exn transactions) ~init:[ first ] ~f:(fun reversed old ->
        let transaction =
          S.Transaction.with_value
            old
            ~limits
            { (S.Transaction.value old) with
              previous_transaction_hash = Some (S.Transaction.hash (List.hd_exn reversed))
            }
          |> ok
        in
        transaction :: reversed)
      |> List.rev
    in
    rewrite_retention_journal ~env ~root changed;
    List.iter (List.tl_exn snapshots) ~f:(fun installed ->
      let sequence = installed.S.Snapshot.snapshot.transaction_sequence in
      let transaction =
        List.find_exn changed ~f:(fun transaction ->
          Int64.equal transaction.S.Transaction.transaction_sequence sequence)
      in
      overwrite_retention_snapshot
        ~env
        ~directory
        installed
        (snapshot_document sequence (Some (S.Transaction.hash transaction)) "same"));
    seen := [];
    assert_retention_read_only ~env ~root (fun () ->
      assert (Result.is_error (S.Recovery.Retention_preflight.check scope)));
    assert (List.mem !seen 1L ~equal:Int64.equal));
  print_endline
    "valid new checksums, chain and anchors cannot reuse an old covered prefix";
  [%expect
    {| valid new checksums, chain and anchors cannot reuse an old covered prefix |}]
;;

let%expect_test "missing certified head replays and malformed prefix refuses before reuse"
  =
  List.iter [ false; true ] ~f:(fun gap ->
    with_retention_files (fun env root journal directory transactions _ ->
      let applies = ref 0 in
      let scope =
        retention_preflight
          ~env
          ~journal
          ~snapshot_directory:directory
          ~apply:(fun state transaction ->
            incr applies;
            apply_shared_document state transaction)
          ~validate_transaction:(fun _ -> Ok ())
          ~validate:(fun _ -> Ok ())
      in
      S.Recovery.Retention_preflight.check scope |> ok;
      let changed =
        if gap
        then
          List.filter transactions ~f:(fun transaction ->
            not (Int64.equal transaction.S.Transaction.transaction_sequence 3L))
        else List.take transactions 4
      in
      rewrite_retention_journal ~env ~root changed;
      applies := 0;
      let result =
        assert_retention_read_only ~env ~root (fun () ->
          S.Recovery.Retention_preflight.check scope)
      in
      if gap
      then (
        assert (Result.is_error result);
        assert (!applies = 0))
      else (
        result |> ok;
        assert (!applies = 4))));
  print_endline
    "truncated head requires full replay; missing prefix link rejects before any apply";
  [%expect
    {| truncated head requires full replay; missing prefix link rejects before any apply |}]
;;

let%expect_test "certified equal heads retain distinct unknown carriers" =
  with_retention_files (fun env root journal directory transactions snapshots ->
    let oldest = List.hd_exn snapshots in
    overwrite_retention_snapshot
      ~env
      ~directory
      oldest
      (snapshot_with_opaque_state oldest.snapshot);
    let rejected = ref false in
    let scope =
      retention_preflight
        ~env
        ~journal
        ~snapshot_directory:directory
        ~apply:(fun state transaction ->
          if
            opaque_state state
            && Int64.equal transaction.S.Transaction.transaction_sequence 7L
          then (
            rejected := true;
            Error (S.Store_error.Document (D.Error.Extension_conflict [ "future_state" ])))
          else apply_shared_document state transaction)
        ~validate_transaction:(fun _ -> Ok ())
        ~validate:(fun _ -> Ok ())
    in
    S.Recovery.Retention_preflight.check scope |> ok;
    let seventh = next_retention_transaction (List.last_exn transactions) 7L in
    append_retention_transaction journal seventh;
    rewrite_retention_journal ~env ~root (transactions @ [ seventh ]);
    assert_retention_read_only ~env ~root (fun () ->
      assert (Result.is_error (S.Recovery.Retention_preflight.check scope)));
    assert !rejected);
  print_endline "certified counter equality cannot substitute a different unknown carrier";
  [%expect {| certified counter equality cannot substitute a different unknown carrier |}]
;;

let%expect_test
    "immutable recovery read preserves incomplete tails and validates retained fallbacks"
  =
  with_directory (fun env root ->
    let journal_directory = Filename.concat root "journal" in
    let snapshot_directory = Filename.concat root "snapshot" in
    let journal =
      S.Journal.create
        ~env
        ~directory:journal_directory
        ~max_payload_length:16384
        ~max_segment_bytes:1048576L
        ~max_segment_frames:100
      |> ok
    in
    let first = S.Transaction.decode original_transaction |> ok in
    S.Snapshot.install
      ~env
      ~directory:snapshot_directory
      ~max_payload_length:16384
      (snapshot_document 0L None "initial")
    |> ok
    |> ignore;
    S.Journal.append journal ~durability:Flush ~flags:0 ~payload:original_transaction
    |> ok
    |> ignore;
    S.Snapshot.install
      ~env
      ~directory:snapshot_directory
      ~max_payload_length:16384
      (snapshot_document 1L (Some (S.Transaction.hash first)) "head")
    |> ok
    |> ignore;
    let segment =
      S.Journal_segment.open_existing
        ~env
        ~directory:journal_directory
        ~id:(S.Journal.current_segment journal)
      |> ok
    in
    let partial =
      S.Frame.encode ~max_payload_length:16384 ~flags:0 "physical-tail"
      |> frame_ok
      |> Fn.flip String.drop_suffix 3
    in
    S.Journal_segment.append ~env ~durability:Flush segment ~frame:partial |> ok |> ignore;
    let files =
      [ Filename.concat journal_directory "CURRENT"
      ; Filename.concat snapshot_directory "CURRENT"
      ; S.Journal_segment.path segment
      ]
    in
    let bytes () =
      List.map files ~f:(fun path -> Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path))
    in
    let original = bytes () in
    let scan = S.Journal.scan journal |> ok in
    assert (Option.is_some scan.crash_tail);
    let restored = ref 0
    and applied = ref 0
    and transactions = ref 0 in
    let read reject_older =
      S.Recovery.read
        ~env
        ~journal
        ~snapshot_directory
        ~max_snapshot_payload_length:16384
        ~session_id
        ~initial:"none"
        ~restore_snapshot:(fun snapshot ->
          Int.incr restored;
          Ok (text snapshot.S.Snapshot.payload))
        ~apply:(fun previous transaction ->
          Int.incr applied;
          if reject_older && String.equal previous "initial"
          then Error (S.Store_error.Corrupt "older fallback rejected")
          else Ok (text transaction.S.Transaction.delta))
        ~validate_transaction:(fun _ ->
          Int.incr transactions;
          Ok ())
        ~validate:(fun _ -> Ok ())
    in
    let result = read false |> ok in
    assert (String.equal result.state "head");
    assert (not result.repaired_crash_tail);
    [%test_eq: int] 2 !restored;
    [%test_eq: int] 1 !applied;
    [%test_eq: int] 1 !transactions;
    assert (List.equal String.equal original (bytes ()));
    assert (Result.is_error (read true));
    assert (List.equal String.equal original (bytes ()));
    assert (Option.is_some (S.Journal.scan journal |> ok).crash_tail));
  print_endline
    "selected head read; every fallback checked; journal and both CURRENT files unchanged";
  [%expect
    {| selected head read; every fallback checked; journal and both CURRENT files unchanged |}]
;;
