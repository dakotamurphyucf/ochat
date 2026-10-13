open! Core
module P = Agent_protocol
module B = P.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store

let () = Mirage_crypto_rng_unix.use_default ()

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let upload_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Store.Error.t)]
;;

let check label condition =
  if not condition then raise_s [%sexp "upload invariant failed", (label : string)]
;;

let server_id = P.Id.Server.of_string "srv_authoring_upload_test" |> protocol_ok
let principal = P.Id.Principal.of_string "pri_authoring_upload_test" |> protocol_ok
let foreign = P.Id.Principal.of_string "pri_authoring_upload_foreign" |> protocol_ok
let start = P.Timestamp.of_string "2026-10-13T00:00:00Z" |> protocol_ok
let later ms = P.Timestamp.add_ms start ms |> protocol_ok
let digest bytes = Chatml_source.Digest.of_contents bytes ~checkpoint:Fn.id

let root =
  Chatml_source.Id.create ~package_namespace:"pkg" ~relative_path:"chatml.project.json"
  |> Or_error.ok_exn
;;

let script =
  Chatml_source.Id.create ~package_namespace:"pkg" ~relative_path:"src/main.chatml"
  |> Or_error.ok_exn
;;

let member id bytes =
  let source =
    P.Authoring.Source.create
      ~id
      ~digest:(digest bytes)
      ~byte_length:(String.length bytes)
      ~editor_revision:None
    |> protocol_ok
  in
  B.Member.create ~source |> protocol_ok
;;

let root_bytes = "root\000\255"
let script_bytes = "hello\r\nworld"

let manifest ?(script_bytes = script_bytes) () =
  let selection =
    P.Authoring.Context.Selection.create ~target:(Entry "main") ~environment:None
    |> protocol_ok
  in
  B.Manifest.create
    ~project_source:root
    ~selection
    ~members:[ member root root_bytes; member script script_bytes ]
  |> protocol_ok
;;

let begin_key = P.Idempotency_key.of_string "begin-upload-test" |> protocol_ok
let commit_key = P.Idempotency_key.of_string "commit-upload-test" |> protocol_ok
let other_commit_key = P.Idempotency_key.of_string "other-commit" |> protocol_ok

let begin_request ?(manifest = manifest ()) () =
  B.Begin.create_request ~idempotency_key:begin_key ~manifest ()
;;

let limits
      ?(max_member_bytes = 128)
      ?(max_chunk_bytes = 64)
      ?(max_work_steps = 10000)
      ?(max_journal_records = 32)
      ()
  =
  Store.Limits.create
    ~max_active_uploads:4
    ~max_active_per_principal:2
    ~max_members:4
    ~max_manifest_bytes:65536
    ~max_member_bytes
    ~max_bundle_bytes:256
    ~max_chunk_bytes
    ~max_reserved_bytes:512
    ~max_reserved_per_principal:256
    ~max_work_steps
    ~max_journal_records
    ~max_journal_bytes:16384
    ~max_idempotency_records:8
    ~max_tombstones:8
    ~max_tombstone_bytes:600000
    ~max_recovery_entries:1000
    ~max_recovery_metadata_bytes:1048576
    ~max_recovery_source_bytes:512
    ~staging_ttl:(Time_ns.Span.of_sec 60.)
    ~receipt_ttl:(Time_ns.Span.of_sec 120.)
    ~expired_tombstone_ttl:(Time_ns.Span.of_sec 180.)
  |> Or_error.ok_exn
;;

let with_root name f =
  Eio_main.run (fun env ->
    let temporary =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        (name ^ "." ^ P.Id.Transaction.(create () |> to_string))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let root =
            Agent_store.Data_root.create ~env ~path:(Filename.concat temporary "data")
            |> store_ok
          in
          Agent_store.Durable_file.replace
            ~env
            ~durability:Flush_file_and_directory
            ~path:(Agent_store.Data_root.server_id_path root)
            (P.Id.Server.to_string server_id ^ "\n")
          |> store_ok;
          let lock =
            Agent_store.Lock.acquire
              ~env
              ~sw
              ~path:(Agent_store.Data_root.daemon_lock_path root)
              ~server_id
              ~process_start_identity:(Some "upload-test")
              ~nonce:"upload-test"
            |> store_ok
          in
          f env sw root lock))
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path))
;;

let open_store env sw root lock =
  Store.open_or_create
    ~env
    ~sw
    ~data_root:root
    ~daemon_lock:lock
    ~host_id:server_id
    ~limits:(limits ())
    ~checkpoint:Fn.id
  |> upload_ok
;;

let upload_directory env root upload_id =
  Eio.Path.(
    Eio.Stdenv.fs env
    / Agent_store.Data_root.path root
    / "authoring-uploads"
    / P.Id.Authoring_upload.to_string upload_id)
;;

let phase_file env root upload_id =
  Eio.Path.(upload_directory env root upload_id / "phase.json")
;;

let send_chunk store upload_id source byte_offset bytes =
  let request =
    B.Chunk.create_request ~upload_id ~source ~byte_offset ~bytes |> protocol_ok
  in
  Store.chunk store ~principal ~request ~now:(fun () -> start) ~checkpoint:Fn.id
;;

module Host = Agent_server.Authoring_upload_host

let make_actor ?(id = principal) current =
  let principal =
    P.Principal.create
      ~id
      ~authentication_kind:"local.upload_test"
      ~scopes:P.Scope.Set.empty
      ~attributes:[]
    |> protocol_ok
  in
  Operator_authorization.guarded ~principal ~is_current:(fun () -> !current)
;;

let host_ok = function
  | Ok value -> value
  | Error error -> raise_s (Host.Error.sexp_of_t error)
;;

let status_request upload_id = B.Status.create_request ~upload_id

let wait_until label condition =
  let rec loop remaining =
    if condition ()
    then ()
    else if remaining = 0
    then raise_s [%sexp "timed out waiting for fiber", (label : string)]
    else (
      Eio.Fiber.yield ();
      loop (remaining - 1))
  in
  loop 100
;;

let make_policy allowed checks =
  Host.Upload_policy.create
    ~authorize_upload:(fun _ ->
      Int.incr checks;
      !allowed)
    ~authorize_parent:(fun _ _ -> false)
;;

let with_host name f =
  with_root name (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    Eio.Switch.run (fun host_sw ->
      let clock = Eio_mock.Clock.make () in
      let allowed = ref true in
      let checks = ref 0 in
      let current = ref true in
      let actor = make_actor current in
      let host =
        Host.create store ~sw:host_sw ~clock ~upload_policy:(make_policy allowed checks)
      in
      f ~env ~sw ~root_handle ~store ~host ~clock ~actor ~current ~allowed ~checks;
      Host.close_and_wait host |> host_ok))
;;

let with_sibling_host env sw root_handle f =
  Eio.Switch.run (fun sibling_sw ->
    let root =
      Agent_store.Data_root.create
        ~env
        ~path:(Agent_store.Data_root.path root_handle ^ "-sibling")
      |> store_ok
    in
    Agent_store.Durable_file.replace
      ~env
      ~durability:Flush_file_and_directory
      ~path:(Agent_store.Data_root.server_id_path root)
      (P.Id.Server.to_string server_id ^ "\n")
    |> store_ok;
    let lock =
      Agent_store.Lock.acquire
        ~env
        ~sw:sibling_sw
        ~path:(Agent_store.Data_root.daemon_lock_path root)
        ~server_id
        ~process_start_identity:(Some "upload-host-sibling-test")
        ~nonce:"upload-host-sibling-test"
      |> store_ok
    in
    let store = open_store env sibling_sw root lock in
    Eio.Switch.run (fun host_sw ->
      let host =
        Host.create
          store
          ~sw:host_sw
          ~clock:(Eio_mock.Clock.make ())
          ~upload_policy:(make_policy (ref true) (ref 0))
      in
      f host;
      Host.close_and_wait host |> host_ok))
;;

let%test_unit "host grant, actor lifetime, reentrancy, and private lookup symmetry" =
  with_host
    "ochat-authoring-upload-host-admission"
    (fun
        ~env
         ~sw
         ~root_handle
         ~store:_
         ~host
         ~clock:_
         ~actor
         ~current
         ~allowed
         ~checks:_
       ->
       let response =
         Host.begin_upload host ~actor ~request:(begin_request ()) ~checkpoint:Fn.id
         |> host_ok
       in
       let parent =
         P.Authoring_context.Context.Expected.of_json
           (`Object
               [ ( "context_id"
                 , P.Id.Authoring_context.to_json (P.Id.Authoring_context.create ()) )
               ; "host", P.Id.Server.to_json server_id
               ; "generation", `Number "0"
               ; ( "basis_fingerprint"
                 , `String (Chatml_source.Digest.to_hex (digest "parent")) )
               ])
         |> protocol_ok
       in
       let parent_request =
         B.Begin.create_request
           ~parent_context:parent
           ~idempotency_key:begin_key
           ~manifest:(manifest ())
           ()
       in
       check
         "optional parent needs current read authority"
         (match
            Host.begin_upload host ~actor ~request:parent_request ~checkpoint:Fn.id
          with
          | Error Host.Error.Unauthorized -> true
          | _ -> false);
       let upload_id = B.Begin.upload_id response in
       let request = status_request upload_id in
       let reentrant = ref None in
       let self_close = ref None in
       let ancestor_reentrant = ref None in
       let ancestor_close = ref None in
       with_sibling_host env sw root_handle (fun sibling ->
         let sibling_upload =
           Host.begin_upload sibling ~actor ~request:(begin_request ()) ~checkpoint:Fn.id
           |> host_ok
           |> B.Begin.upload_id
         in
         let status =
           Host.status host ~actor ~request ~checkpoint:(fun () ->
             if Option.is_none !reentrant
             then (
               reentrant := Some (Host.status host ~actor ~request ~checkpoint:Fn.id);
               self_close := Some (Host.close_and_wait host);
               ignore
                 (Host.status
                    sibling
                    ~actor
                    ~request:(status_request sibling_upload)
                    ~checkpoint:(fun () ->
                      if Option.is_none !ancestor_reentrant
                      then (
                        ancestor_reentrant
                        := Some (Host.status host ~actor ~request ~checkpoint:Fn.id);
                        ancestor_close := Some (Host.close_and_wait host)))
                  |> host_ok)))
           |> host_ok
         in
         check
           "open stage"
           (match status with
            | Store.Status.Open _ -> true
            | _ -> false));
       List.iter
         [ "reentrant request", !reentrant
         ; "ancestor reentrant request", !ancestor_reentrant
         ]
         ~f:(fun (label, outcome) ->
           check
             label
             (match outcome with
              | Some (Error Host.Error.Busy) -> true
              | _ -> false));
       List.iter
         [ "self-close", !self_close; "ancestor close", !ancestor_close ]
         ~f:(fun (label, outcome) ->
           check
             label
             (match outcome with
              | Some (Error Host.Error.Busy) -> true
              | _ -> false));
       let unknown = P.Id.Authoring_upload.create () in
       let foreign_actor = make_actor ~id:foreign (ref true) in
       let missing =
         Host.status host ~actor ~request:(status_request unknown) ~checkpoint:Fn.id
       in
       let foreign = Host.status host ~actor:foreign_actor ~request ~checkpoint:Fn.id in
       check
         "missing and foreign share finite error"
         (match missing, foreign with
          | ( Error (Host.Error.Store Host.Error.Store_code.Missing_or_foreign)
            , Error (Host.Error.Store Host.Error.Store_code.Missing_or_foreign) ) -> true
          | _ -> false);
       allowed := false;
       check
         "grant checked before lookup"
         (match Host.status host ~actor ~request ~checkpoint:Fn.id with
          | Error Host.Error.Unauthorized -> true
          | _ -> false);
       allowed := true;
       current := false;
       check
         "original actor lifetime"
         (match Host.status host ~actor ~request ~checkpoint:Fn.id with
          | Error Host.Error.Expired_actor -> true
          | _ -> false))
;;

let%test_unit "yielding checkpoint revocation leaves both gates usable" =
  with_host
    "ochat-authoring-upload-host-checkpoint"
    (fun
        ~env:_
         ~sw:_
         ~root_handle:_
         ~store:_
         ~host
         ~clock:_
         ~actor
         ~current:_
         ~allowed
         ~checks:_
       ->
       let upload_id =
         Host.begin_upload host ~actor ~request:(begin_request ()) ~checkpoint:Fn.id
         |> host_ok
         |> B.Begin.upload_id
       in
       let request = status_request upload_id in
       check
         "post-callback revocation"
         (match
            Host.status host ~actor ~request ~checkpoint:(fun () ->
              Eio.Fiber.yield ();
              allowed := false)
          with
          | Error Host.Error.Unauthorized -> true
          | _ -> false);
       allowed := true;
       let marker = Failure "host checkpoint marker" in
       check
         "unexpected checkpoint exception retains identity"
         (try
            ignore
              (Host.status host ~actor ~request ~checkpoint:(fun () -> raise marker)
               : (Store.Status.t, Host.Error.t) result);
            false
          with
          | exn -> phys_equal exn marker);
       ignore (Host.status host ~actor ~request ~checkpoint:Fn.id |> host_ok))
;;

let queued_store_gate name ~change ~accept =
  with_root name (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    let epoch = P.Timestamp.of_string "1970-01-01T00:00:00Z" |> protocol_ok in
    let upload_id =
      Store.begin_upload
        store
        ~principal
        ~request:(begin_request ())
        ~now:(fun () -> epoch)
        ~checkpoint:Fn.id
      |> upload_ok
      |> B.Begin.upload_id
    in
    let held, entered = Eio.Promise.create () in
    let release, release_gate = Eio.Promise.create () in
    let direct_done, resolve_direct = Eio.Promise.create () in
    let first_checkpoint = ref true in
    Eio.Fiber.fork ~sw (fun () ->
      let result =
        Store.status
          store
          ~principal
          ~upload_id
          ~now:(fun () -> epoch)
          ~checkpoint:(fun () ->
            if !first_checkpoint
            then (
              first_checkpoint := false;
              Eio.Promise.resolve entered ();
              Eio.Promise.await release))
      in
      Eio.Promise.resolve resolve_direct result);
    Eio.Promise.await held;
    Eio.Switch.run (fun host_sw ->
      let clock = Eio_mock.Clock.make () in
      let allowed = ref true in
      let checks = ref 0 in
      let actor = make_actor (ref true) in
      let host =
        Host.create store ~sw:host_sw ~clock ~upload_policy:(make_policy allowed checks)
      in
      let finished, resolve_finished = Eio.Promise.create () in
      Eio.Fiber.fork ~sw:host_sw (fun () ->
        let result =
          Host.status host ~actor ~request:(status_request upload_id) ~checkpoint:Fn.id
        in
        Eio.Promise.resolve resolve_finished result);
      wait_until "host reached store queue" (fun () -> !checks >= 2);
      change ~allowed ~clock;
      Eio.Promise.resolve release_gate ();
      ignore (Eio.Promise.await direct_done |> upload_ok);
      check "queued operation admission" (accept (Eio.Promise.await finished));
      Host.close_and_wait host |> host_ok))
;;

let%test_unit "revocation after store queue prevents private lookup" =
  queued_store_gate
    "ochat-authoring-upload-host-queued-revocation"
    ~change:(fun ~allowed ~clock:_ -> allowed := false)
    ~accept:(function
      | Error Host.Error.Unauthorized -> true
      | _ -> false)
;;

let%test_unit "host clock sampled after store queue expires staged upload" =
  queued_store_gate
    "ochat-authoring-upload-host-queued-expiry"
    ~change:(fun ~allowed:_ ~clock -> Eio_mock.Clock.set_time clock 61.)
    ~accept:(function
      | Ok Store.Status.Expired -> true
      | _ -> false)
;;

let%test_unit "close cancels and joins an active request without poisoning the store gate"
  =
  with_root "ochat-authoring-upload-host-close" (fun env sw root_handle lock ->
    let store = open_store env sw root_handle lock in
    Eio.Switch.run (fun host_sw ->
      let actor = make_actor (ref true) in
      let host =
        Host.create
          store
          ~sw:host_sw
          ~clock:(Eio_mock.Clock.make ())
          ~upload_policy:(make_policy (ref true) (ref 0))
      in
      let upload_id =
        Host.begin_upload host ~actor ~request:(begin_request ()) ~checkpoint:Fn.id
        |> host_ok
        |> B.Begin.upload_id
      in
      let entered, enter = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let finished, finish = Eio.Promise.create () in
      let callback_cleaned = ref false in
      Eio.Fiber.fork ~sw:host_sw (fun () ->
        let outcome =
          try
            ignore
              (Host.status
                 host
                 ~actor
                 ~request:(status_request upload_id)
                 ~checkpoint:(fun () ->
                   Eio.Promise.resolve enter ();
                   Exn.protect
                     ~f:(fun () -> Eio.Promise.await never)
                     ~finally:(fun () -> callback_cleaned := true))
               : (Store.Status.t, Host.Error.t) result);
            `Returned
          with
          | Eio.Cancel.Cancelled _ -> `Cancelled
        in
        Eio.Cancel.protect (fun () -> Eio.Promise.resolve finish outcome));
      Eio.Promise.await entered;
      Host.close_and_wait host |> host_ok;
      check "callback cleanup joined before close returned" !callback_cleaned;
      check
        "cancelled operation joined before close"
        (match Eio.Promise.await finished with
         | `Cancelled -> true
         | `Returned -> false);
      check
        "closed facade rejects later request"
        (match
           Host.status host ~actor ~request:(status_request upload_id) ~checkpoint:Fn.id
         with
         | Error Host.Error.Closed -> true
         | _ -> false)))
;;
