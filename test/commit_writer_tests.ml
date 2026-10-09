open Core
open Agent_store_test_fixtures
module S = Agent_store
module Writer = S.Commit_writer

let segment_name = S.Journal_segment.Id.filename S.Journal_segment.Id.first

module Fault = struct
  type boundary =
    | Before_write
    | Partial_write
    | After_sync

  type failure =
    | Io
    | Timeout
    | Unexpected

  type t =
    { boundary : boundary
    ; failure : failure
    ; secondary_timeout : bool
    }

  exception Primary_failure

  let raise_failure = function
    | Io -> raise (Core_unix.Unix_error (EIO, "audit publication fault", "segment"))
    | Timeout -> raise Eio.Time.Timeout
    | Unexpected -> raise Primary_failure
  ;;

  let wrap_file
        (Eio.Resource.T (file, handler))
        ~armed
        ~secondary
        ~before_failure
        ~after_sync
    =
    let module Original = (val Eio.Resource.get handler Eio.File.Pi.Write) in
    let reached selected =
      armed := None;
      secondary := selected.secondary_timeout;
      before_failure ();
      raise_failure selected.failure
    in
    let module File = struct
      include Original

      let single_write file buffers =
        match !armed with
        | Some ({ boundary = Before_write; _ } as selected) -> reached selected
        | Some ({ boundary = Partial_write; _ } as selected) ->
          let buffer = Cstruct.concat buffers in
          ignore
            (Original.single_write
               file
               [ Cstruct.sub buffer 0 (Int.min 8 (Cstruct.length buffer)) ]);
          reached selected
        | Some _ | None -> Original.single_write file buffers
      ;;

      let copy file ~src = Eio.Flow.Pi.simple_copy ~single_write file ~src

      let sync file =
        Original.sync file;
        after_sync ();
        match !armed with
        | Some ({ boundary = After_sync; _ } as selected) -> reached selected
        | Some _ | None -> ()
      ;;
    end
    in
    Eio.Resource.T (file, Eio.File.Pi.rw (module File))
  ;;

  let wrap ?(after_sync = fun () -> ()) env ~armed ~secondary ~before_failure =
    let directory, path = Eio.Stdenv.fs env in
    let native_directory = directory in
    let (Eio.Resource.T (native, handler)) = directory in
    let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
    let module Directory = struct
      include Original

      let rename directory source _destination target =
        Original.rename directory source native_directory target
      ;;

      let open_out directory ~sw ~append ~create path =
        let file = Original.open_out directory ~sw ~append ~create path in
        if String.is_suffix path ~suffix:segment_name
        then wrap_file file ~armed ~secondary ~before_failure ~after_sync
        else file
      ;;

      let open_in directory ~sw path =
        if !secondary && String.is_suffix path ~suffix:segment_name
        then (
          secondary := false;
          raise Eio.Time.Timeout);
        Original.open_in directory ~sw path
      ;;
    end
    in
    let fs =
      ( Eio.Resource.T
          (native, Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Directory)) ])
      , path )
    in
    object
      method fs = fs
      method cwd = env#cwd
      method stdin = env#stdin
      method stdout = env#stdout
      method stderr = env#stderr
      method net = env#net
      method domain_mgr = env#domain_mgr
      method process_mgr = env#process_mgr
      method clock = env#clock
      method mono_clock = env#mono_clock
      method secure_random = env#secure_random
      method debug = env#debug
      method backend_id = env#backend_id
    end
  ;;
end

let journal env directory =
  S.Journal.create
    ~env
    ~directory
    ~max_payload_length:16384
    ~max_segment_bytes:1048576L
    ~max_segment_frames:100
  |> store_ok
;;

let reopen_journal env directory =
  S.Journal.open_existing
    ~env
    ~directory
    ~max_payload_length:16384
    ~max_segment_bytes:1048576L
    ~max_segment_frames:100
  |> store_ok
;;

let writer ?(queue_capacity = 8) sw journal sequence previous_hash =
  Writer.create
    ~sw
    ~journal
    ~session_id
    ~next_transaction_sequence:sequence
    ~previous_transaction_hash:previous_hash
    ~queue_capacity
  |> store_ok
;;

let transaction sequence previous_hash =
  S.Transaction.create
    ~limits:Document_schema.Limits.default
    ~session_id
    ~generation:0
    ~transaction_sequence:sequence
    ~previous_transaction_hash:previous_hash
    ~session_revision:sequence
    ~first_event_sequence:None
    ~last_event_sequence:None
    ~accepted_at_ns:sequence
    ~command_audit:None
    ~delta:(named_document "session.delta" (`Object [ "text", `String "current" ]))
    ~durable_events:[]
  |> store_ok
;;

let recover env root journal =
  S.Recovery.load
    ~env
    ~journal
    ~snapshot_directory:(Filename.concat root "snapshots")
    ~max_snapshot_payload_length:16384
    ~session_id
    ~initial:[]
    ~restore_snapshot:(fun _ -> Error (S.Store_error.Corrupt "unexpected test snapshot"))
    ~apply:(fun sequences transaction ->
      Ok (sequences @ [ transaction.S.Transaction.transaction_sequence ]))
    ~validate_transaction:S.Transaction.validate
    ~validate:(fun _ -> Ok ())
  |> store_ok
;;

let%expect_test "commit writer replies to interrupted commits and rejects queued effects" =
  List.iter
    [ "partial-Timeout", Fault.Partial_write, Fault.Timeout, 0
    ; "after-fsync-Timeout", After_sync, Timeout, 1
    ; "after-fsync-exception", After_sync, Unexpected, 1
    ]
    ~f:(fun (name, boundary, failure, count) ->
      with_temp_directory "ochat-writer-exception" (fun native_env root ->
        let reached, reached_resolver = Eio.Promise.create () in
        let release, release_resolver = Eio.Promise.create () in
        let armed = ref None
        and secondary = ref false in
        let env =
          Fault.wrap native_env ~armed ~secondary ~before_failure:(fun () ->
            Eio.Promise.resolve reached_resolver ();
            Eio.Promise.await release)
        in
        let directory = Filename.concat root "journal" in
        let journal = journal env directory in
        let path = Eio.Path.(Eio.Stdenv.fs native_env / directory / segment_name) in
        Eio.Switch.run (fun sw ->
          let writer = writer sw journal 1L None in
          armed := Some Fault.{ boundary; failure; secondary_timeout = false };
          let first =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Writer.commit writer ~durability:Flush (transaction 1L None))
          in
          Eio.Promise.await reached;
          let queued =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Writer.commit writer ~durability:Flush (transaction 1L None))
          in
          (* Controlled scheduler handoff: the worker is blocked on release and
             the second request fits the empty bounded queue. *)
          Eio.Fiber.yield ();
          Eio.Promise.resolve release_resolver ();
          (match Eio.Promise.await first with
           | Error Eio.Time.Timeout ->
             (match failure with
              | Timeout -> ()
              | Io | Unexpected -> assert false)
           | Error Fault.Primary_failure ->
             (match failure with
              | Unexpected -> ()
              | Io | Timeout -> assert false)
           | Ok _ | Error _ -> assert false);
          (match Eio.Promise.await queued with
           | Ok (Error _) -> ()
           | Ok (Ok _) | Error _ -> assert false);
          assert (Option.is_none !armed);
          let original = Eio.Path.load path in
          assert (
            Result.is_error (Writer.commit writer ~durability:Flush (transaction 1L None)));
          [%test_eq: string] original (Eio.Path.load path);
          Writer.close writer;
          Writer.close writer);
        let reopened = reopen_journal native_env directory in
        let restored = recover native_env root reopened in
        [%test_eq: int] count (List.length restored.state);
        [%test_eq: int64] (Int64.of_int count) restored.latest_transaction_sequence;
        assert (Bool.equal restored.repaired_crash_tail (count = 0));
        Eio.Switch.run (fun sw ->
          let next = Int64.(restored.latest_transaction_sequence + 1L) in
          let writer = writer sw reopened next restored.latest_transaction_hash in
          let committed =
            Writer.commit
              writer
              ~durability:Flush
              (transaction next restored.latest_transaction_hash)
            |> store_ok
          in
          [%test_eq: int64] next committed.transaction_sequence;
          Writer.close writer);
        let final = reopen_journal native_env directory |> recover native_env root in
        [%test_eq: int] (count + 1) (List.length final.state);
        print_endline
          (name
           ^ ": original reply; queued requests failed; close/reopen resumes verified \
              head")));
  [%expect
    {|
    partial-Timeout: original reply; queued requests failed; close/reopen resumes verified head
    after-fsync-Timeout: original reply; queued requests failed; close/reopen resumes verified head
    after-fsync-exception: original reply; queued requests failed; close/reopen resumes verified head
  |}]
;;

let%expect_test "commit writer preserves genuine owning-switch cancellation" =
  let exception Owning_switch_failed in
  with_temp_directory "ochat-writer-owning-cancellation" (fun native_env root ->
    Eio.Switch.run (fun callers ->
      let reached, reached_resolver = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let armed = ref None
      and secondary = ref false in
      let env =
        Fault.wrap native_env ~armed ~secondary ~before_failure:(fun () ->
          Eio.Promise.resolve reached_resolver ();
          Eio.Promise.await never)
      in
      let directory = Filename.concat root "journal" in
      let journal = journal env directory in
      let owner = ref None
      and reply = ref None in
      (try
         Eio.Switch.run (fun sw ->
           let selected = writer sw journal 1L None in
           owner := Some selected;
           armed
           := Some
                Fault.
                  { boundary = After_sync
                  ; failure = Unexpected
                  ; secondary_timeout = false
                  };
           reply
           := Some
                (Eio.Fiber.fork_promise ~sw:callers (fun () ->
                   Writer.commit selected ~durability:Flush (transaction 1L None)));
           Eio.Promise.await reached;
           Eio.Switch.fail sw Owning_switch_failed)
       with
       | Owning_switch_failed -> ());
      (match Eio.Promise.await (Option.value_exn !reply) with
       | Error (Eio.Cancel.Cancelled _) -> ()
       | Ok _ | Error _ -> assert false);
      assert (Option.is_none !armed);
      Writer.close (Option.value_exn !owner);
      assert (
        Result.is_error
          (Writer.commit
             (Option.value_exn !owner)
             ~durability:Flush
             (transaction 1L None)));
      let restored = reopen_journal native_env directory |> recover native_env root in
      [%test_eq: int64] 1L restored.latest_transaction_sequence;
      print_endline
        "owning cancellation stops worker; in-flight caller receives cancellation; \
         canonical bytes recover"));
  [%expect
    {|owning cancellation stops worker; in-flight caller receives cancellation; canonical bytes recover|}]
;;

let%expect_test
    "capacity-one writer shutdown releases external queued and blocked callers"
  =
  let exception Owning_switch_failed in
  with_temp_directory "ochat-writer-bounded-shutdown" (fun native_env root ->
    Eio.Switch.run (fun callers ->
      let reached, reached_resolver = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let armed = ref None
      and secondary = ref false in
      let env =
        Fault.wrap native_env ~armed ~secondary ~before_failure:(fun () ->
          Eio.Promise.resolve reached_resolver ();
          Eio.Promise.await never)
      in
      let directory = Filename.concat root "journal" in
      let journal = journal env directory in
      let path = Eio.Path.(Eio.Stdenv.fs native_env / directory / segment_name) in
      let owner = ref None
      and replies = ref None
      and closing = ref None in
      let original_bytes = ref "" in
      (try
         Eio.Switch.run (fun sw ->
           let selected = writer ~queue_capacity:1 sw journal 1L None in
           owner := Some selected;
           armed
           := Some
                Fault.
                  { boundary = After_sync
                  ; failure = Unexpected
                  ; secondary_timeout = false
                  };
           let first =
             Eio.Fiber.fork_promise ~sw:callers (fun () ->
               Writer.commit selected ~durability:Flush (transaction 1L None))
           in
           Eio.Promise.await reached;
           original_bytes := Eio.Path.load path;
           let second_started, second_started_resolver = Eio.Promise.create () in
           let second =
             Eio.Fiber.fork_promise ~sw:callers (fun () ->
               Eio.Promise.resolve second_started_resolver ();
               Writer.commit selected ~durability:Flush (transaction 1L None))
           in
           Eio.Promise.await second_started;
           let third_started, third_started_resolver = Eio.Promise.create () in
           let third =
             Eio.Fiber.fork_promise ~sw:callers (fun () ->
               Eio.Promise.resolve third_started_resolver ();
               Writer.commit selected ~durability:Flush (transaction 1L None))
           in
           Eio.Promise.await third_started;
           let close_started, close_started_resolver = Eio.Promise.create () in
           let close =
             Eio.Fiber.fork_promise ~sw:callers (fun () ->
               Eio.Promise.resolve close_started_resolver ();
               Writer.close selected)
           in
           Eio.Promise.await close_started;
           (* The worker cannot consume: first owns the fsync barrier, second
             fills capacity one, third and close block in bounded Stream.add. *)
           Eio.Fiber.yield ();
           assert (not (Eio.Promise.is_resolved second));
           assert (not (Eio.Promise.is_resolved third));
           assert (not (Eio.Promise.is_resolved close));
           replies := Some (first, second, third);
           closing := Some close;
           Eio.Switch.fail sw Owning_switch_failed)
       with
       | Owning_switch_failed -> ());
      let first, second, third = Option.value_exn !replies in
      (match Eio.Promise.await first with
       | Error (Eio.Cancel.Cancelled _) -> ()
       | Ok _ | Error _ -> assert false);
      List.iter [ second; third ] ~f:(fun reply ->
        match Eio.Promise.await reply with
        | Ok (Error _) -> ()
        | Ok (Ok _) | Error _ -> assert false);
      (match Eio.Promise.await (Option.value_exn !closing) with
       | Ok () -> ()
       | Error _ -> assert false);
      Writer.close (Option.value_exn !owner);
      [%test_eq: string] !original_bytes (Eio.Path.load path);
      let reopened = reopen_journal native_env directory in
      let restored = recover native_env root reopened in
      [%test_eq: int64] 1L restored.latest_transaction_sequence;
      Eio.Switch.run (fun sw ->
        let fresh =
          writer ~queue_capacity:1 sw reopened 2L restored.latest_transaction_hash
        in
        [%test_eq: int64]
          2L
          (Writer.commit
             fresh
             ~durability:Flush
             (transaction 2L restored.latest_transaction_hash)
           |> store_ok)
            .transaction_sequence;
        Writer.close fresh);
      [%test_eq: int64]
        2L
        (reopen_journal native_env directory |> recover native_env root)
          .latest_transaction_sequence;
      print_endline
        "original cancellation wins; queued and blocked callers stop; close completes; \
         no duplicate append"));
  [%expect
    {|original cancellation wins; queued and blocked callers stop; close completes; no duplicate append|}]
;;

let%expect_test "resolved successful commit reply wins simultaneous writer stop" =
  with_temp_directory "ochat-writer-resolved-reply" (fun native_env root ->
    let reached, reached_resolver = Eio.Promise.create () in
    let release, release_resolver = Eio.Promise.create () in
    let pause = ref false in
    let armed = ref None
    and secondary = ref false in
    let after_sync () =
      if !pause
      then (
        pause := false;
        Eio.Promise.resolve reached_resolver ();
        Eio.Promise.await release)
    in
    let env =
      Fault.wrap ~after_sync native_env ~armed ~secondary ~before_failure:(fun () -> ())
    in
    let directory = Filename.concat root "journal" in
    let journal = journal env directory in
    Eio.Switch.run (fun sw ->
      let selected = writer ~queue_capacity:1 sw journal 1L None in
      pause := true;
      let first =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Writer.commit selected ~durability:Flush (transaction 1L None))
      in
      Eio.Promise.await reached;
      let close_started, close_started_resolver = Eio.Promise.create () in
      let close =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Eio.Promise.resolve close_started_resolver ();
          Writer.close selected)
      in
      Eio.Promise.await close_started;
      Eio.Fiber.yield ();
      assert (not (Eio.Promise.is_resolved close));
      (* Releasing the successful fsync lets the worker resolve Commit then
         consume the buffered Close and signal stopped without another I/O. *)
      Eio.Promise.resolve release_resolver ();
      (match Eio.Promise.await first with
       | Ok (Ok committed) -> [%test_eq: int64] 1L committed.transaction_sequence
       | Ok (Error _) | Error _ -> assert false);
      (match Eio.Promise.await close with
       | Ok () -> ()
       | Error _ -> assert false);
      Writer.close selected);
    [%test_eq: int64]
      1L
      (reopen_journal native_env directory |> recover native_env root)
        .latest_transaction_sequence;
    print_endline
      "resolved normal commit retained when closed and reply signals are both ready");
  [%expect
    {|resolved normal commit retained when closed and reply signals are both ready|}]
;;
