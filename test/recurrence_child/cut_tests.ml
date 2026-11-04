open! Core
open Support
module Store = Agent_store
module Reservation = A.Recurrence_child_reservation

let rec associated_reference = function
  | A.Session_delta.Recurrence_catalog_changed catalog ->
    Option.bind (Catalog.find catalog recurrence_id) ~f:(fun entry ->
      Entry.frontier entry
      |> protocol_ok
      |> Frontier.child_associations
      |> List.hd
      |> Option.map ~f:A.Recurrence_child_association.reference)
  | Batch deltas -> List.find_map deltas ~f:associated_reference
  | _ -> None
;;

let observed_parent_link env path ~parent_id ~child_id =
  (* Observe the completed physical write through the unwrapped filesystem.
     Decoded bytes are evidence of the cut, never fresh execution authority. *)
  let bytes = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
  let scan =
    Store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024) bytes
    |> store_ok
  in
  assert (not scan.crash_tail);
  match List.last scan.entries with
  | Some entry when Store.Frame.flags entry.frame = 0 ->
    let transaction =
      Store.Transaction.decode (Store.Frame.payload entry.frame) |> store_ok
    in
    if P.Id.Session.equal parent_id transaction.session_id
    then (
      let delta =
        A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
        |> document_ok
        |> A.Session_delta_document.value
      in
      match associated_reference delta with
      | Some reference when P.Id.Session.equal child_id reference.child_session_id ->
        assert (String.is_substring bytes ~substring:"recurrence.child-association.v1");
        Some reference
      | Some _ | None -> None)
    else None
  | Some _ | None -> None
;;

let record daemon row =
  Ledger.find
    (S.delegations (Daemon.store daemon))
    (Reservation.delegation_key row |> protocol_ok)
  |> store_ok
  |> Option.value_exn
;;

let%expect_test "post-sync parent association repairs from a fresh authorized host" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      (fun () ->
         let prompt = Filename.concat root "parent.chatmd" in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(Eio.Stdenv.fs env / prompt)
           {|<developer>Captured child ACK cut.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
         let configuration = T.config root root prompt in
         let provider_calls = ref 0 in
         let expected = ref None in
         let durable_reference = ref None in
         let observed = ref false in
         let armed = ref false in
         let fault = ref None in
         let wrapped =
           Crash_fault_io.wrap
             env
             ~boundary:After_sync
             ~matches:(fun path ->
               String.is_substring path ~substring:"/journal/"
               && String.is_suffix path ~suffix:".log")
             ~reached:(fun path ->
               match !armed, !expected with
               | true, Some (parent_id, row) ->
                 (match
                    observed_parent_link
                      env
                      path
                      ~parent_id
                      ~child_id:(Reservation.child_session_id row)
                  with
                  | None -> ()
                  | Some reference ->
                    armed := false;
                    observed := true;
                    durable_reference := Some reference;
                    let exn =
                      Core_unix.Unix_error
                        (EIO, "injected recurrence parent association sync", path)
                    in
                    fault := Some (path, Exn.to_string exn);
                    raise exn)
               | false, _ | true, None -> ())
         in
         with_daemon wrapped root configuration ~provider_calls (fun sw daemon ->
           let connection = T.connection daemon principal in
           protect_cleanup
             ~finally:(fun () -> Agent_client.Connection.close connection)
             (fun () ->
                T.initialize connection;
                let created, _ = T.create_session ~start_immediately:false connection in
                let parent = loaded daemon created.id in
                start_parent_settled wrapped sw connection parent;
                with_planner wrapped sw daemon parent (fun () ->
                  let permit =
                    reserve parent (target wrapped daemon parent ~root_file:prompt)
                  in
                  let row = Actor.Recurrence_creation.reservation permit in
                  expected := Some (created.id, row);
                  armed := true;
                  let before = state parent in
                  let () =
                    match
                      Factory.create_recurrence_child
                        (Daemon.factory daemon)
                        ~parent
                        ~permit
                    with
                    | Error error ->
                      let path, exception_message =
                        match !fault with
                        | Some fault -> fault
                        | None ->
                          raise_s
                            [%sexp
                              "parent association post-sync cut was not reached"
                            , (error.code : P.Error.code)
                            , (String.prefix error.message 512 : string)
                            , (error.retryable : bool)]
                      in
                      [%test_eq: P.Error.code] Persistence_error error.code;
                      [%test_eq: bool] true error.retryable;
                      (* Session_persistence exposes the typed store sexp here,
                         rather than Store_error.to_protocol_error's prose. *)
                      (match
                         Store.Store_error.t_of_sexp (Sexp.of_string error.message)
                       with
                       | Io failure ->
                         [%test_eq: string] "append journal segment" failure.operation;
                         [%test_eq: string] path failure.path;
                         [%test_eq: string] exception_message failure.message
                       | _ -> failwith "post-sync failure lost its typed IO cause");
                      ()
                    | Ok _ -> failwith "post-sync injection was acknowledged"
                  in
                  assert !observed;
                  assert (Ledger.equal_stage Child_installed (record daemon row).stage);
                  assert (
                    Option.is_none
                      (Registry.find
                         (Daemon.registry daemon)
                         (Reservation.child_session_id row)));
                  assert (
                    List.is_empty
                      (entry parent
                       |> Entry.frontier
                       |> protocol_ok
                       |> Frontier.child_associations));
                  (* The shared pending projection rejects a stale equal-revision
                     rewrite before the already-unavailable writer is reached.
                     No creation-only poison flag or stale control install. *)
                  (match
                     command_result parent "after-uncertain-link" (Pause recurrence_id)
                   with
                   | Error error ->
                     [%test_eq: P.Error.code] Persistence_error error.code;
                     [%test_eq: bool] false error.retryable;
                     (match
                        Store.Store_error.t_of_sexp (Sexp.of_string error.message)
                      with
                      | Corrupt reason ->
                        [%test_eq: string]
                          "canonical projection target cannot regress or alter \
                           equal-revision hints"
                          reason
                      | _ -> failwith "stale control lost its canonical projection fence")
                   | Ok _ -> failwith "uncertain writer accepted a stale control commit");
                  let unchanged = state parent in
                  [%test_eq: int64] before.counters.revision unchanged.counters.revision;
                  [%test_eq: int64]
                    before.counters.transaction_sequence
                    unchanged.counters.transaction_sequence;
                  assert (Entry.equal_control (Entry.control (entry parent)) Enabled))));
         (* This is exception/cleanup recovery after a post-sync write, not an
            externally killed process. Fresh host and current proof are real. *)
         with_daemon env root configuration ~provider_calls (fun sw daemon ->
           let parent_id, original_row = Option.value_exn !expected in
           let parent = Registry.load (Daemon.registry daemon) parent_id |> protocol_ok in
           let connection = T.connection daemon principal in
           protect_cleanup
             ~finally:(fun () -> Agent_client.Connection.close connection)
             (fun () ->
                T.initialize connection;
                start_parent_settled env sw connection parent;
                with_planner env sw daemon parent (fun () ->
                  let replayed =
                    entry parent
                    |> Entry.frontier
                    |> protocol_ok
                    |> Frontier.child_associations
                    |> List.hd_exn
                    |> A.Recurrence_child_association.reference
                  in
                  assert (
                    Ledger.Reference.equal replayed (Option.value_exn !durable_reference));
                  let permit =
                    reserve_occurrence
                      ~actor:(Operator_authorization.trusted_local principal)
                      parent
                      (Reservation.occurrence original_row)
                  in
                  let row = Actor.Recurrence_creation.reservation permit in
                  assert (Reservation.equal original_row row);
                  let before = record daemon row in
                  assert (Ledger.equal_stage Child_installed before.stage);
                  let result =
                    Factory.create_recurrence_child
                      (Daemon.factory daemon)
                      ~parent
                      ~permit
                    |> protocol_ok
                  in
                  let child_id =
                    P.Session_ref.session_id (Factory.Recurrence_child.session result)
                  in
                  assert (
                    P.Id.Session.equal
                      child_id
                      (Reservation.child_session_id original_row));
                  let _, final = assert_dormant daemon child_id in
                  assert (Ledger.equal_stage Linked final.stage);
                  List.iter
                    [ Option.value_exn !durable_reference
                    ; Ledger.reference before
                    ; Ledger.reference final
                    ]
                    ~f:(fun reference ->
                      assert (
                        Ledger.Reference.equal
                          reference
                          (Factory.Recurrence_child.reference result)));
                  [%test_eq: int] 0 !provider_calls;
                  print_endline
                    "post-sync link durable; stale writer fenced; fresh permit repairs \
                     same child")))));
  [%expect
    {| post-sync link durable; stale writer fenced; fresh permit repairs same child |}]
;;
