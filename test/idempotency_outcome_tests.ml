open! Core
open Agent_store_test_fixtures
module I = Agent_store.Idempotency_store
module O = Agent_store.Idempotency_outcome
module P = Agent_protocol

let receipt index =
  let name = "outcome-" ^ Int.to_string index in
  I.
    { key =
        { principal_id = P.Id.Principal.of_string "pri_outcome_capacity" |> protocol_ok
        ; session_id = Some session_id
        ; method_name = "session.attach"
        ; idempotency_key = P.Idempotency_key.of_string name |> protocol_ok
        }
    ; request_digest = name
    ; accepted_transaction_sequence = None
    ; outcome = Pending
    ; created_at = timestamp
    ; expires_at = None
    ; retention = Protected
    }
;;

let%expect_test "large aggregate terminal replies reopen and replay without evicting keys"
  =
  with_temp_directory "outcome-capacity" (fun env root ->
    let path = Filename.concat root "responses.sexp" in
    let cache = I.open_or_create ~env ~path |> store_ok in
    let payload index =
      `Object
        [ "index", `Number (Int.to_string index)
        ; "reply", `String (String.make (1024 * 1024) 'x')
        ]
    in
    List.iter (List.init 18 ~f:Fn.id) ~f:(fun index ->
      let request = receipt index in
      I.record cache request |> store_ok |> ignore;
      I.complete
        cache
        ~key:request.key
        ~request_digest:request.request_digest
        ~accepted_transaction_sequence:(Some (Int64.of_int index))
        ~outcome:(Success (payload index))
      |> store_ok
      |> ignore);
    let metadata = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
    let reopened = I.open_or_create ~env ~path |> store_ok in
    List.iter (List.init 18 ~f:Fn.id) ~f:(fun index ->
      let request = receipt index in
      (match
         I.lookup reopened ~key:request.key ~request_digest:request.request_digest
       with
       | Replay
           { outcome = Success value; accepted_transaction_sequence = Some sequence; _ }
         ->
         assert (Jsonaf.exactly_equal value (payload index));
         assert (Int64.equal sequence (Int64.of_int index))
       | Missing | Conflict _ | Replay _ -> failwith "terminal receipt lost after reopen");
      match I.lookup reopened ~key:request.key ~request_digest:"foreign" with
      | Conflict _ -> ()
      | Missing | Replay _ -> failwith "digest conflict lost");
    assert (String.length metadata < 32 * 1024);
    print_endline
      "18 MiB replies; compact metadata; all exact replays and conflicts retained");
  [%expect
    {| 18 MiB replies; compact metadata; all exact replays and conflicts retained |}]
;;

let%expect_test "whole failure custody keeps future fields and numeric lexemes" =
  let error = P.Error.create Conflict ~message:"original failure" ~retryable:false () in
  let raw =
    `Object
      [ "tag", `String "failure"
      ; "error", P.Error.to_json error
      ; "future", `Object [ "counter", `Number "1.00"; "nullable", `Null ]
      ]
  in
  let outcome = O.create raw |> store_ok in
  let decoded = O.decode (O.reference outcome) (O.to_string outcome) |> store_ok in
  assert (Jsonaf.exactly_equal raw (O.jsonaf decoded));
  (match O.value decoded with
   | Failure actual -> assert (P.Protocol_error.equal_code actual.code error.code)
   | Success _ -> failwith "failed outcome became success");
  print_endline "whole failure subtree retained";
  [%expect {| whole failure subtree retained |}]
;;

let%expect_test
    "artifact acknowledgement failure leaves Pending and verified retry completes"
  =
  with_temp_directory "outcome-ack" (fun env root ->
    let path = Filename.concat root "responses.sexp" in
    let armed = ref None in
    let failed_target = ref None in
    let wrapped =
      Job_store_fixtures.fault_env
        env
        armed
        ~on_failure:(fun target -> failed_target := Some target)
        ~matches_rename:(fun path ->
          String.is_prefix (Filename.basename path) ~prefix:"idempotency-outcome-")
    in
    let cache = I.open_or_create ~env:wrapped ~path |> store_ok in
    let request = receipt 42 in
    I.record cache request |> store_ok |> ignore;
    armed := Some true;
    assert (
      match
        I.complete
          cache
          ~key:request.key
          ~request_digest:request.request_digest
          ~accepted_transaction_sequence:None
          ~outcome:(Success (`String "actual reply"))
      with
      | Error (Agent_store.Store_error.Io _) -> true
      | Ok _ | Error _ -> false);
    assert (
      Option.exists !failed_target ~f:(fun target ->
        String.is_prefix (Filename.basename target) ~prefix:"idempotency-outcome-"));
    assert (Option.is_none !armed);
    (match I.lookup cache ~key:request.key ~request_digest:request.request_digest with
     | Replay { outcome = Pending; _ } -> ()
     | Missing | Conflict _ | Replay _ ->
       failwith "artifact failure changed receipt authority");
    let reopened = I.open_or_create ~env ~path |> store_ok in
    (match I.lookup reopened ~key:request.key ~request_digest:request.request_digest with
     | Replay { outcome = Pending; _ } -> ()
     | Missing | Conflict _ | Replay _ ->
       failwith "artifact file became receipt authority");
    I.complete
      cache
      ~key:request.key
      ~request_digest:request.request_digest
      ~accepted_transaction_sequence:None
      ~outcome:(Success (`String "actual reply"))
    |> store_ok
    |> ignore;
    let reopened = I.open_or_create ~env ~path |> store_ok in
    (match I.lookup reopened ~key:request.key ~request_digest:request.request_digest with
     | Replay { outcome = Success (`String "actual reply"); _ } -> ()
     | Missing | Conflict _ | Replay _ ->
       failwith "verified artifact retry did not complete");
    print_endline
      "artifact alone grants no receipt; exact retry survives uncertain artifact ACK");
  [%expect
    {| artifact alone grants no receipt; exact retry survives uncertain artifact ACK |}]
;;

let%expect_test "orphan pruning preserves linked outcomes and rejects a replaced payload" =
  with_temp_directory "outcome-orphans" (fun env root ->
    let path = Filename.concat root "responses.sexp" in
    let cache = I.open_or_create ~env ~path |> store_ok in
    let request = receipt 1 in
    I.record cache { request with outcome = Success (`String "kept") }
    |> store_ok
    |> ignore;
    let orphan =
      O.create (`Object [ "tag", `String "success"; "value", `String "orphan" ])
      |> store_ok
    in
    let directory = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.with_open_dir directory (fun opened ->
      Agent_store.Idempotency_outcome_store.publish orphan ~directory:opened
      |> store_ok
      |> ignore);
    let orphan_path =
      Eio.Path.(
        directory / Agent_store.Idempotency_outcome_store.basename (O.reference orphan))
    in
    let temporary =
      Eio.Path.(
        directory
        / (Agent_store.Idempotency_outcome_store.basename (O.reference orphan)
           ^ ".tmp-123-456"))
    in
    Eio.Path.save ~create:(`Exclusive 0o600) temporary "{partial atomic publication";
    I.prune_expired cache ~now:timestamp |> store_ok |> ignore;
    assert (not (Eio.Path.is_file orphan_path));
    assert (not (Eio.Path.is_file temporary));
    let reopened = I.open_or_create ~env ~path |> store_ok in
    (match I.lookup reopened ~key:request.key ~request_digest:request.request_digest with
     | Replay { outcome = Success (`String "kept"); _ } -> ()
     | Missing | Conflict _ | Replay _ -> failwith "prune lost retained replay");
    Eio.Path.with_open_dir directory (fun opened ->
      Agent_store.Idempotency_outcome_store.publish orphan ~directory:opened
      |> store_ok
      |> ignore);
    let malformed = Eio.Path.(directory / "idempotency-outcome-foreign.json") in
    Eio.Path.save ~create:(`Exclusive 0o600) malformed "unknown owner data";
    assert (
      match I.prune_expired cache ~now:timestamp with
      | Error (Agent_store.Store_error.Document _) -> true
      | Ok _ | Error _ -> false);
    assert (Eio.Path.is_file orphan_path);
    assert (String.equal (Eio.Path.load malformed) "unknown owner data");
    Eio.Path.unlink malformed;
    let temporary =
      Eio.Path.(
        directory
        / (Agent_store.Idempotency_outcome_store.basename (O.reference orphan)
           ^ ".tmp-987-654"))
    in
    Eio.Path.symlink ~link_to:(Filename.basename path) temporary;
    assert (
      match I.prune_expired cache ~now:timestamp with
      | Error (Agent_store.Store_error.Corrupt _) -> true
      | Ok _ | Error _ -> false);
    assert (Eio.Path.is_file orphan_path);
    Eio.Path.unlink temporary;
    I.prune_expired cache ~now:timestamp |> store_ok |> ignore;
    let name =
      Eio.Path.read_dir directory
      |> List.find_exn ~f:(fun name ->
        String.is_prefix name ~prefix:"idempotency-outcome-"
        && String.is_suffix name ~suffix:".json")
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(directory / name) "{}";
    assert (
      match I.open_or_create ~env ~path with
      | Error (Agent_store.Store_error.Corrupt _) -> true
      | Ok _ | Error _ -> false);
    assert (
      match I.prune_expired reopened ~now:timestamp with
      | Error (Agent_store.Store_error.Corrupt _) -> true
      | Ok _ | Error _ -> false);
    print_endline
      "validated orphan removed; retained replay kept; corrupt root fails closed");
  [%expect
    {| validated orphan removed; retained replay kept; corrupt root fails closed |}]
;;

let%expect_test
    "Pending future value and error custody cannot collide with terminal fields"
  =
  with_temp_directory "outcome-collision" (fun env root ->
    let path = Filename.concat root "responses.sexp" in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let cache = I.open_or_create ~env ~path |> store_ok in
    let request = receipt 7 in
    I.record cache request |> store_ok |> ignore;
    let blob = P.Id.Blob.create () in
    let pending =
      `Object
        [ "tag", `String "pending"
        ; "value", P.Id.Blob.to_json blob
        ; "error", `Object [ "unknown", `Number "1.00" ]
        ]
    in
    let rec rewrite = function
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             name, if String.equal name "outcome" then pending else rewrite value))
      | `Array values -> `Array (List.map values ~f:rewrite)
      | (`String _ | `Number _ | `True | `False | `Null) as value -> value
    in
    let json = Jsonaf.of_string (Eio.Path.load file) |> rewrite in
    Eio.Path.save ~create:(`Or_truncate 0o600) file (Jsonaf.to_string json);
    let reopened = I.open_or_create ~env ~path |> store_ok in
    I.complete
      reopened
      ~key:request.key
      ~request_digest:request.request_digest
      ~accepted_transaction_sequence:None
      ~outcome:(Success (`String "actual terminal reply"))
    |> store_ok
    |> ignore;
    let twice = I.open_or_create ~env ~path |> store_ok in
    (match I.lookup twice ~key:request.key ~request_digest:request.request_digest with
     | Replay { outcome = Success (`String "actual terminal reply"); _ } -> ()
     | Missing | Conflict _ | Replay _ ->
       failwith "Pending future value replaced terminal reply");
    let retained =
      I.with_retained_references
        twice
        ~candidates:[ blob ]
        ~max_records:4
        ~max_bytes:65536
        ~f:(fun values -> Ok values)
      |> store_ok
      |> Option.value_exn
    in
    assert (List.equal P.Id.Blob.equal retained [ blob ]);
    let artifact_name =
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / root)
      |> List.find_exn ~f:(fun name ->
        String.is_prefix name ~prefix:"idempotency-outcome-"
        && String.is_suffix name ~suffix:".json")
    in
    let artifact =
      Jsonaf.of_string
        (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / Filename.concat root artifact_name))
    in
    assert (
      match Document_schema.Json.field artifact ~name:"pending_custody" with
      | Value actual -> Jsonaf.exactly_equal pending actual
      | Absent | Null -> false);
    print_endline
      "terminal reply exact; colliding Pending fields and blob custody retained");
  [%expect {| terminal reply exact; colliding Pending fields and blob custody retained |}]
;;

let%expect_test "compound custody has separate original component limits" =
  let component = String.make ((16 * 1024 * 1024) - 256) 'x' in
  let terminal = `Object [ "tag", `String "success"; "value", `String component ] in
  let pending = `Object [ "tag", `String "pending"; "value", `String component ] in
  let outcome = O.create ~pending_custody:pending terminal |> store_ok in
  assert (O.Reference.encoded_bytes (O.reference outcome) > 16 * 1024 * 1024);
  let restored = O.decode (O.reference outcome) (O.to_string outcome) |> store_ok in
  assert (Jsonaf.exactly_equal (O.jsonaf restored) terminal);
  let oversized =
    `Object
      [ "tag", `String "success"; "value", `String (String.make (16 * 1024 * 1024) 'x') ]
  in
  assert (
    match O.create ~pending_custody:(`Object [ "tag", `String "pending" ]) oversized with
    | Error (Agent_store.Store_error.Document (Document_schema.Error.Limit_exceeded _)) ->
      true
    | Ok _ | Error _ -> false);
  assert (
    match O.create ~pending_custody:`Null terminal with
    | Error (Agent_store.Store_error.Document _) -> true
    | Ok _ | Error _ -> false);
  print_endline "near-limit components compose; terminal oversize and null custody reject";
  [%expect {| near-limit components compose; terminal oversize and null custody reject |}]
;;

let%expect_test
    "near-full legacy metadata completes existing receipts but refuses a new key"
  =
  with_temp_directory "outcome-legacy-headroom" (fun env root ->
    let path = Filename.concat root "responses.sexp" in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let cache = I.open_or_create ~env ~path |> store_ok in
    let request index =
      let request = receipt index in
      { request with key = { request.key with session_id = None } }
    in
    List.iter [ 1; 2 ] ~f:(fun index ->
      I.record cache (request index) |> store_ok |> ignore);
    let rec legacy = function
      | `Object fields ->
        `Object
          (List.filter_map fields ~f:(fun (name, value) ->
             if
               List.mem
                 [ "accepted_transaction_sequence"; "expires_at"; "session_id" ]
                 name
                 ~equal:String.equal
             then None
             else
               Some
                 ( name
                 , if String.equal name "schema_version"
                   then `Number "2"
                   else legacy value )))
      | `Array values -> `Array (List.map values ~f:legacy)
      | (`String _ | `Number _ | `True | `False | `Null) as value -> value
    in
    let base =
      match legacy (Jsonaf.of_string (Eio.Path.load file)) with
      | `Object fields -> `Object (fields @ [ "future_padding", `String "" ])
      | _ -> failwith "fixture envelope"
    in
    let padding = (16 * 1024 * 1024) - String.length (Jsonaf.to_string base) in
    let legacy =
      match base with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             ( name
             , if String.equal name "future_padding"
               then `String (String.make padding 'x')
               else value )))
      | _ -> assert false
    in
    let original = Jsonaf.to_string legacy in
    assert (String.length original = 16 * 1024 * 1024);
    Eio.Path.save ~create:(`Or_truncate 0o600) file original;
    let reopened = I.open_or_create ~env ~path |> store_ok in
    assert (String.equal original (Eio.Path.load file));
    let first = request 1
    and second = request 2 in
    I.mark_accepted
      reopened
      ~key:first.key
      ~request_digest:first.request_digest
      ~transaction_sequence:Int64.max_value
    |> store_ok
    |> ignore;
    I.complete
      reopened
      ~key:first.key
      ~request_digest:first.request_digest
      ~accepted_transaction_sequence:None
      ~outcome:(Success (`String "first"))
    |> store_ok
    |> ignore;
    I.complete
      reopened
      ~key:second.key
      ~request_digest:second.request_digest
      ~accepted_transaction_sequence:None
      ~outcome:(Failure (P.Error.create Conflict ~message:"second" ~retryable:false ()))
    |> store_ok
    |> ignore;
    I.mark_accepted
      reopened
      ~key:second.key
      ~request_digest:second.request_digest
      ~transaction_sequence:Int64.max_value
    |> store_ok
    |> ignore;
    let saved = Eio.Path.load file in
    assert (String.length saved > 16 * 1024 * 1024);
    let fresh = request 3 in
    assert (
      match I.record reopened fresh with
      | Error (Admission_capacity (Limit_exceeded _)) -> true
      | Error _ | Ok _ -> false);
    assert (String.equal saved (Eio.Path.load file));
    (match I.lookup reopened ~key:fresh.key ~request_digest:fresh.request_digest with
     | Missing -> ()
     | Replay _ | Conflict _ -> failwith "rejected key became Pending");
    let twice = I.open_or_create ~env ~path |> store_ok in
    List.iter [ first; second ] ~f:(fun request ->
      match I.lookup twice ~key:request.key ~request_digest:request.request_digest with
      | Replay
          { outcome = Success _ | Failure _
          ; accepted_transaction_sequence = Some sequence
          ; _
          } -> assert (Int64.equal sequence Int64.max_value)
      | Missing | Conflict _ | Replay _ -> failwith "old receipt stranded by capacity");
    print_endline
      "old exact16MiB readable; both completion orders settle; new key rejects before \
       Pending");
  [%expect
    {| old exact16MiB readable; both completion orders settle; new key rejects before Pending |}]
;;
