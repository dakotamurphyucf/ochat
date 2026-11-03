open! Core
open Public_durable_admission_test
module Public = P.Public

let%expect_test
    "ordered instruction status agrees with replacements and preserves absent extras"
  =
  let status revision = instruction_status ~revision ~selected:false in
  let fields = Public.Snapshot.fields snapshot in
  let session = { fields.session with revision = 0L; latest_event_sequence = 0L } in
  let snapshot_fields =
    { fields with session; revision = 0L; latest_event_sequence = 0L }
  in
  let snapshot = Public.Snapshot.create snapshot_fields |> ok in
  let observed t =
    Agent_client.Projection.snapshot t
    |> Public.Snapshot.fields
    |> fun (fields : Public.Snapshot.Fields.t) -> fields.instruction_status
  in
  let admit sequence ?instruction_status ?replacement_snapshot () =
    let session =
      { session with revision = sequence; latest_event_sequence = sequence }
    in
    let payload = P.Event.Durable.Payload.Session_updated session in
    let internal =
      P.Event.Durable.of_payload
        ~session_id
        ~sequence
        ~revision:sequence
        ~timestamp
        payload
    in
    Public.Durable.of_internal_envelope
      internal
      ~body:(Full (Shared (Public.Durable.Shared_payload.of_internal payload |> ok)))
      ~instruction_status
      ~extension_status:None
      ~replacement_snapshot
  in
  let event sequence ?instruction_status ?replacement_snapshot () =
    admit sequence ?instruction_status ?replacement_snapshot () |> ok
  in
  let replacement sequence instruction_status =
    Public.Snapshot.create
      { snapshot_fields with
        session = { session with revision = sequence; latest_event_sequence = sequence }
      ; revision = sequence
      ; latest_event_sequence = sequence
      ; instruction_status
      }
    |> ok
  in
  let apply t event = Agent_client.Projection.apply_event t event |> ok in
  let initial = Agent_client.Projection.install_snapshot snapshot in
  let first_event = event 1L ~instruction_status:(status 1L) () in
  let first = apply initial first_event in
  assert (Option.equal P.Instruction_status.equal (observed first) (Some (status 1L)));
  let second = apply first (event 2L ()) in
  assert (Option.equal P.Instruction_status.equal (observed second) (observed first));
  let history = P.Event.Durable.Payload.History_appended [] in
  let internal =
    P.Event.Durable.of_payload ~session_id ~sequence:3L ~revision:3L ~timestamp history
  in
  let unrelated =
    Public.Durable.of_internal_envelope
      internal
      ~body:(Full (History_appended []))
      ~instruction_status:None
      ~extension_status:None
      ~replacement_snapshot:None
    |> ok
  in
  let third = apply second unrelated in
  assert (Option.equal P.Instruction_status.equal (observed third) (observed first));
  let fourth = apply third (event 4L ~instruction_status:(status 2L) ()) in
  assert (Option.equal P.Instruction_status.equal (observed fourth) (Some (status 2L)));
  assert (Result.is_error (Agent_client.Projection.apply_event fourth first_event));
  let fifth = apply fourth (event 5L ~replacement_snapshot:(replacement 5L None) ()) in
  assert (Option.is_none (observed fifth));
  let sixth = apply fifth (event 6L ~instruction_status:(status 0L) ()) in
  assert (Option.equal P.Instruction_status.equal (observed sixth) (Some (status 0L)));
  let coherent =
    event
      7L
      ~instruction_status:(status 3L)
      ~replacement_snapshot:(replacement 7L (Some (status 3L)))
      ()
  in
  let seventh = apply sixth (D.of_json (D.to_json coherent) |> ok) in
  assert (Option.equal P.Instruction_status.equal (observed seventh) (Some (status 3L)));
  List.iter
    [ None; Some (status 3L) ]
    ~f:(fun replacement_status ->
      let snapshot = replacement 8L replacement_status in
      assert (
        Result.is_error
          (admit 8L ~instruction_status:(status 4L) ~replacement_snapshot:snapshot ()));
      let original = event 8L ~instruction_status:(status 4L) () in
      let wire =
        D.to_json original
        |> fun wire ->
        match wire with
        | `Object fields ->
          let payload = List.Assoc.find_exn fields "payload" ~equal:String.equal in
          replace_field
            wire
            "payload"
            (replace_field
               payload
               "replacement_snapshot"
               (Public.Snapshot.to_json snapshot))
        | _ -> assert false
      in
      let projected =
        Result.bind (D.of_json wire) ~f:(Agent_client.Projection.apply_event seventh)
      in
      assert (Result.is_error projected);
      assert (
        Option.equal P.Instruction_status.equal (observed seventh) (Some (status 3L))));
  print_endline
    "ordered updates preserve absent extras; replacements and explicit zero differ; \
     coherent dual status applies; contradictory wire cannot enter client";
  [%expect
    {| ordered updates preserve absent extras; replacements and explicit zero differ; coherent dual status applies; contradictory wire cannot enter client |}]
;;

let%expect_test "actual daemon dispatcher get and attach expose supported empty status" =
  let module F = Agent_server_test_support in
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = F.temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:false Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let prompt_file = Filename.concat root "prompt.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Offline instruction projection fixture.</developer>";
        Eio.Switch.run (fun sw ->
          let options =
            { Agent_server.Daemon.default_options with
              inference_policy =
                F.inference_policy
                  ~default_model:"fixture-model"
                  ~post_stream:(fun ~sw:_ ~inputs:_ ->
                    failwith "unexpected model dispatch")
            }
          in
          let daemon =
            Agent_server.Daemon.start
              ~options
              ~sw
              ~env
              ~config:(F.config root root prompt_file)
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ()
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> ok)
            ~f:(fun () ->
              let connection = F.connection daemon (F.principal ()) in
              Exn.protect
                ~finally:(fun () -> Agent_client.Connection.close connection)
                ~f:(fun () ->
                  F.initialize connection;
                  let session, _ = F.create_session connection in
                  let get =
                    match
                      Agent_client.Connection.request
                        connection
                        (Session_get { session_id = session.id; history = None })
                      |> ok
                    with
                    | Public.Result.Session_get snapshot -> snapshot
                    | _ -> failwith "expected actual session.get snapshot"
                  in
                  let attached =
                    Agent_client.Connection.request
                      connection
                      (Session_attach
                         { session_id = session.id
                         ; requested_mode = Read_only
                         ; subscribe = false
                         ; after_sequence = None
                         ; reclaim_token = None
                         ; idempotency_key =
                             P.Idempotency_key.of_string "instruction-projection-attach"
                             |> ok
                         })
                    |> ok
                  in
                  let attached =
                    match attached with
                    | Public.Result.Session_attach { replay = Snapshot snapshot; _ } ->
                      snapshot
                    | _ -> failwith "expected actual attachment snapshot"
                  in
                  let expected = Some (instruction_status ~revision:0L ~selected:false) in
                  List.iter [ get; attached ] ~f:(fun snapshot ->
                    assert (
                      Option.equal
                        P.Instruction_status.equal
                        expected
                        (Public.Snapshot.fields snapshot).instruction_status));
                  print_endline
                    "real daemon dispatcher and in-memory client get/attach return \
                     supported zero selection without model execution")))));
  [%expect
    {| real daemon dispatcher and in-memory client get/attach return supported zero selection without model execution |}]
;;
