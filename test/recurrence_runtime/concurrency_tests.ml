open! Core
open Support

type change =
  | Pause
  | Edit
  | Source
  | Stop
  | Epoch
[@@deriving sexp_of]

let archive fixture actor =
  let module Archive = Agent_store.Session_archive_record in
  let observation = S.read_lifecycle fixture.store fixture.handle |> store_ok in
  let current = S.Lifecycle.Observation.value observation in
  let summary = A.Session_state.summary (state actor) in
  let anchor =
    Archive.Anchor.create
      ~generation:summary.generation
      ~session_revision:summary.revision
      ~latest_event_sequence:summary.latest_event_sequence
    |> protocol_ok
  in
  let key : Agent_store.Idempotency_store.Key.t =
    { principal_id
    ; session_id = Some session_id
    ; method_name = "session.delete"
    ; idempotency_key =
        P.Idempotency_key.of_string "archive-during-zone-read" |> protocol_ok
    }
  in
  let transition =
    Archive.prepare
      current
      ~expected:(Archive.revision current)
      ~anchor
      ~action:Archive
      ~key
      ~request_digest:(String.make 64 'a')
      ~now:!(fixture.wall)
    |> protocol_ok
  in
  let current_entry =
    Agent_store.Session_index.find_checked (S.session_index fixture.store) session_id
    |> store_ok
    |> Option.value_exn
  in
  let prepared =
    S.prepare_lifecycle
      fixture.store
      fixture.handle
      observation
      ~current_entry
      ~transition
      ~now:!(fixture.wall)
    |> store_ok
  in
  S.publish_lifecycle fixture.store fixture.handle prepared |> store_ok |> ignore
;;

let%expect_test
    "real bounded zone IO leaves mailbox responsive and rejects stale \
     controls/source/lifecycle"
  =
  List.iter [ Pause; Edit; Source; Stop; Epoch ] ~f:(fun change ->
    with_store (fun fixture ->
      let actor = create_actor fixture ~sw:fixture.sw () in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown actor)
        ~f:(fun () ->
          let directory =
            Filename.concat
              fixture.initial.spec.workspace_instance.canonical_root.native_path
              "zones"
          in
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs fixture.env / directory);
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs fixture.env / directory / "UTC")
            utc_bytes;
          let zone_directory =
            Configuration.Zone_directory.create
              ~directory
              ~source:"pinned-2025b"
              ~from:(P.Timestamp.to_time_ns (wall_at_ms (-86_400_000)))
              ~until:(P.Timestamp.to_time_ns (wall_at_ms 172_800_000))
            |> protocol_ok
          in
          let entered, enter = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          let armed = ref false in
          let checkpoint () =
            Eio.Fiber.check ();
            if !armed
            then (
              armed := false;
              Eio.Promise.resolve enter ();
              Eio.Promise.await released)
          in
          let host = open_host fixture ~zone_directory ~checkpoint () in
          Exn.protect
            ~finally:(fun () ->
              if Option.is_none (Eio.Promise.peek released)
              then Eio.Promise.resolve release ();
              Host.close_and_wait host)
            ~f:(fun () ->
              assert (Host.Zone_status.equal (Host.zone_status host) Available);
              install actor host;
              let rule =
                Recurrence_rules.Rule.Calendar.create
                  ~pattern:Daily
                  ~ofday:(Time_ns.Ofday.of_string "12:00:01")
                  ~zone:"UTC"
                  ~start_date:(Date.of_string "2026-08-15")
                  ()
                |> Result.map_error ~f:(fun e ->
                  Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t e))
                |> Result.ok_or_failwith
              in
              create actor "calendar" (Calendar rule) |> ignore;
              armed := true;
              let completed, complete = Eio.Promise.create () in
              Eio.Fiber.fork ~sw:fixture.sw (fun () ->
                Eio.Promise.resolve complete (A.Session_actor.sweep_recurrences actor));
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 5. (fun () ->
                Eio.Promise.await entered;
                (match change with
                 | Pause ->
                   mutate actor "paused-while-loading" (Pause (id "calendar")) |> ignore
                 | Edit ->
                   let current = E.current (entry actor "calendar") in
                   mutate
                     actor
                     "edited-while-loading"
                     (Edit
                        ( id "calendar"
                        , { schedule = A.Recurrence_definition.schedule current
                          ; target = A.Recurrence_definition.target current
                          ; policy = A.Recurrence_definition.policy current
                          } ))
                   |> ignore
                 | Source ->
                   A.Session_actor.change_moderator actor None |> protocol_ok |> ignore
                 | Stop ->
                   let attachment, _ =
                     A.Session_actor.attach actor ~mode:Read_write ~subscribe:false
                     |> protocol_ok
                   in
                   A.Session_actor.stop actor ~attachment_id:attachment.id ~mode:Cancel
                   |> protocol_ok
                   |> ignore
                 | Epoch -> archive fixture actor);
                let after_control = (state actor).counters.transaction_sequence in
                Eio.Promise.resolve release ();
                assert (Result.is_error (Eio.Promise.await completed));
                assert (
                  Int64.equal after_control (state actor).counters.transaction_sequence));
              assert (Int64.equal (consumed actor "calendar") 0L);
              assert (Option.is_none (H.alarm (frontier actor "calendar")));
              assert (List.is_empty (H.ready (frontier actor "calendar")));
              print_s [%sexp (change : change)]))));
  [%expect
    {|
    Pause
    Edit
    Source
    Stop
    Epoch
    |}]
;;
