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
    let rec escape = function
      | Sexp.Atom value when String.equal value encoded -> Sexp.Atom escaped
      | List values -> List (List.map values ~f:escape)
      | value -> value
    in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let bytes = Eio.Path.load file |> Sexp.of_string |> escape |> Sexp.to_string_mach in
    Eio.Path.save ~create:(`Or_truncate 0o600) file bytes;
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
    let rec duplicate (sexp : Sexp.t) : Sexp.t =
      match sexp with
      | Sexp.List [ Atom "records"; List [ record ] ] ->
        List [ Atom "records"; List [ record; record ] ]
      | List values -> List (List.map values ~f:duplicate)
      | value -> value
    in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      file
      (Sexp.of_string original |> duplicate |> Sexp.to_string_mach);
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
