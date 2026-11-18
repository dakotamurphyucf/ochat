open! Core
open Agent_server_test_support
open Retry_support
module Client = Agent_client
module Handle = Client.Session_handle
module Recurrences = Client.Recurrence_controls
module Query = P.Recurrence_query
module Control = P.Recurrence_control
module Support = Goal_recurrence_host_support

let key value =
  P.Idempotency_key.of_string ("recurrence-operator:" ^ value) |> protocol_ok
;;

let id = P.Id.Recurrence.of_string "rec_operator_command" |> protocol_ok

let view = function
  | Query.Result.Available view -> view
  | Unavailable _ -> failwith "recurrence controller is unavailable"
;;

let require_error code result =
  let (error : P.Error.t) = rejected result in
  [%test_eq: P.Error.code] code error.code;
  error
;;

let intent (basis : Query.Basis.t) ~key action =
  Control.Intent.create
    ~generation:basis.generation
    ~expected_catalog_revision:basis.catalog_revision
    ~key
    action
  |> protocol_ok
;;

let source_basis (basis : Query.Basis.t) : Control.Source_basis.t =
  { source = Option.value_exn basis.source
  ; expected_lifecycle_revision = basis.lifecycle_revision
  }
;;

let capture schedule session text =
  Control.Capture.create
    ~schedule
    ~policy:P.Recurrence_policy.default
    ~selector:
      (Existing { session; input = { kind = Plain_text; text; attachments = [] } })
    ()
  |> protocol_ok
;;

let lose_create_reply daemon f =
  let physical = Support.operator_connection daemon principal in
  let dropped = ref None in
  let request command =
    let reply = Client.Connection.request physical command in
    match reply with
    | Ok (P.Public.Result.Non_history result) ->
      (match P.Public.Result.Non_history.value result with
       | Recurrence_control receipt when Control.Verb.equal receipt.verb Create ->
         dropped := Some receipt;
         Client.Connection.close physical;
         Error
           (P.Error.create
              Interrupted
              ~message:"test dropped the Create reply"
              ~retryable:true
              ())
       | _ -> reply)
    | _ -> reply
  in
  let client =
    Client.Transport.create
      ~request
      ~next_notification:(fun () -> Client.Connection.next_notification physical)
      ~close:(fun () -> Client.Connection.close physical)
    |> Client.Connection.create
  in
  protect_cleanup
    ~finally:(fun () -> Client.Connection.close client)
    (fun () ->
       initialize client;
       let result = f client in
       result, Option.value_exn !dropped)
;;

let with_target env sw daemon f =
  Source.with_loaded_entry daemon (fun ~entry ~runtime:_ ~revision:_ ->
    let session_id = (state entry).identity.session_id in
    Support.with_operator daemon (fun connection ->
      Support.with_writer env sw connection ~session_id (fun handle ->
        f connection handle entry)))
;;

let remove_target env connection handle target =
  let stopped = Handle.stop handle ~mode:Cancel |> protocol_ok in
  assert (P.Session.equal_desired_state stopped.desired_state Stopped);
  let revision =
    Support.await ~clock:(Eio.Stdenv.clock env) ~phase:"joining target Stop" (fun () ->
      let current = state target in
      match current.lifecycle.observed with
      | Stopped -> Some current.counters.revision
      | _ -> None)
  in
  let id = Handle.session_id handle in
  Handle.delete
    handle
    ~expected_revision:revision
    ~policy:Remove
    ~confirmation:(P.Id.Session.to_string id)
  |> protocol_ok
  |> ignore;
  require_error Session_not_found (Client.Admin.get_session connection id) |> ignore
;;

let with_reader env sw daemon session f =
  let scopes =
    Set.inter principal.scopes (P.Scope.Set.of_list [ View_session_transcript ])
  in
  Support.with_operator daemon ~principal:{ principal with scopes } (fun connection ->
    Support.with_attachment
      env
      sw
      connection
      ~session_id:(P.Session_ref.session_id session)
      ~mode:Read_only
      (fun handle ->
         let attachment () = (Handle.attachment handle).id in
         f (Recurrences.create connection ~session ~attachment ())))
;;

let definition entry =
  let catalog = Option.value_exn (state entry).recurrence_catalog in
  Catalog.find catalog id |> Option.value_exn |> Entry.current
;;

let assert_frozen_target definition ~session ~(target_state : A.Session_state.t) ~input =
  let target = A.Recurrence_definition.target definition in
  match A.Recurrence_target.view target with
  | Existing_session value ->
    assert (P.Session_ref.equal session value.session);
    let _, source = A.Goal_catalog_source.capture target_state |> protocol_ok in
    assert (P.Run_source.equal source value.source);
    [%test_eq: int64]
      target_state.spec.configuration_revision
      value.configuration_revision;
    [%test_eq: string] input value.input.text;
    assert (List.is_empty value.input.attachments)
  | Fresh_child _ -> failwith "controller did not retain the real selected target"
;;

let%expect_test
    "recurrence operator writes use the leased dispatcher and replay frozen inputs"
  =
  with_fixture ~max_root_agents:2 (fun env sw daemon parent _runtime _revision counters ->
    with_planner env sw daemon parent (fun () ->
      with_target env sw daemon (fun target_connection target_handle target ->
        let server_id = Agent_store.Session_store.server_id (Daemon.store daemon) in
        let session_id = (state parent).identity.session_id in
        let session = P.Session_ref.create ~server_id ~session_id in
        let target_session =
          P.Session_ref.create ~server_id ~session_id:(Handle.session_id target_handle)
        in
        let first_due = Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_sec 3600.) in
        let period =
          Recurrence_rules.Rule.Period.create_ms 600_000L
          |> Result.map_error ~f:(fun error ->
            Sexp.to_string_hum [%sexp (error : Recurrence_rules.Timing_error.t)])
          |> Result.ok_or_failwith
        in
        let schedule =
          P.Recurrence_schedule.Interval
            { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
            ; first_due
            }
        in
        let read = Recurrences.create target_connection ~session () in
        let initial = Recurrences.list read |> protocol_ok |> view in
        assert (List.is_empty initial.value);
        let create ~key text =
          intent
            initial.basis
            ~key
            (Create
               { id
               ; basis = source_basis initial.basis
               ; capture = capture schedule target_session text
               })
        in
        let create_key = key "create" in
        let original = create ~key:create_key "frozen recurrence input" in
        let previous_attachment, dropped =
          lose_create_reply daemon (fun connection ->
            Support.with_writer env sw connection ~session_id (fun handle ->
              let controls = Handle.recurrences handle |> protocol_ok in
              let error =
                Recurrences.command controls original |> require_error Interrupted
              in
              assert error.retryable;
              (Handle.attachment handle).id))
        in
        let frozen = definition parent in
        assert_frozen_target
          frozen
          ~session:target_session
          ~target_state:(state target)
          ~input:"frozen recurrence input";
        let frozen_digest = A.Recurrence_definition.digest frozen in
        assert (Control.Verb.equal dropped.verb Create);
        assert (P.Id.Principal.equal dropped.principal_id principal.id);
        assert (
          String.equal
            dropped.intent_sha256
            (Control.Intent.digest original ~session |> protocol_ok));
        remove_target env target_connection target_handle target;
        Support.with_operator daemon (fun connection ->
          Support.with_writer env sw connection ~session_id (fun handle ->
            assert (
              not
                (P.Id.Attachment.equal previous_attachment (Handle.attachment handle).id));
            let controls = Handle.recurrences handle |> protocol_ok in
            let read () =
              (* The host can publish its first planning revision during either
                 half of the shared read. A rejected snapshot is safe to read
                 again. Preserve every other failure and surface contention
                 if the bounded observer cannot obtain a stable pair. *)
              let rec get remaining =
                match Recurrences.get controls id with
                | Error { code = Conflict; retryable = true; _ } when remaining > 0 ->
                  Eio.Fiber.yield ();
                  get (remaining - 1)
                | result -> result |> protocol_ok |> view
              in
              get 3
            in
            let stable = state parent in
            let frames = Existing.frames env parent in
            let providers = !(counters.providers) in
            let assert_unchanged () =
              Existing.assert_same stable parent;
              [%test_eq: string list] frames (Existing.frames env parent);
              [%test_eq: int] providers !(counters.providers)
            in
            let replayed = Recurrences.command controls original |> protocol_ok in
            assert (
              Jsonaf.exactly_equal
                (Control.Result.payload dropped)
                (Control.Result.payload replayed));
            [%test_eq: string]
              frozen_digest
              (A.Recurrence_definition.digest (definition parent));
            assert_unchanged ();
            let conflict =
              Recurrences.command
                controls
                (create ~key:create_key "changed recurrence input")
              |> require_error Conflict
            in
            assert (not conflict.retryable);
            [%test_eq: string]
              "Recurrence command conflicts with its retained key or the current \
               selection"
              conflict.message;
            assert_unchanged ();
            let current = read () in
            assert (
              match current.value.control with
              | Paused Operator_pause -> true
              | _ -> false);
            let edit basis target =
              Control.Action.Edit
                { id
                ; expected_control_revision = current.value.control_revision
                ; basis
                ; capture = capture schedule target "edited recurrence input"
                }
            in
            let basis = source_basis current.basis in
            Recurrences.command
              controls
              (intent initial.basis ~key:(key "stale-catalog") (edit basis session))
            |> require_error Conflict
            |> ignore;
            assert_unchanged ();
            let stale_source =
              P.Run_source.create
                ~observer:basis.source.observer
                ~generation:basis.source.generation
                ~installation_epoch:(Int64.succ basis.source.installation_epoch)
              |> protocol_ok
            in
            let stale_lifecycle =
              P.Session_lifecycle.Revision.succ basis.expected_lifecycle_revision
              |> protocol_ok
            in
            (* The physical target is absent. A lookup before CAS would reject
               as Session_not_found instead of the stale-controller conflict. *)
            List.iter
              [ "stale-source", { basis with source = stale_source }
              ; ( "stale-lifecycle"
                , { basis with expected_lifecycle_revision = stale_lifecycle } )
              ]
              ~f:(fun (label, changed_basis) ->
                Recurrences.command
                  controls
                  (intent
                     current.basis
                     ~key:(key label)
                     (edit changed_basis target_session))
                |> require_error Conflict
                |> ignore;
                assert_unchanged ());
            let changed =
              Recurrences.command
                controls
                (intent current.basis ~key:(key "edit") (edit basis session))
              |> protocol_ok
            in
            assert (Control.Verb.equal changed.verb Edit);
            assert_frozen_target
              (definition parent)
              ~session
              ~target_state:(state parent)
              ~input:"edited recurrence input";
            let current = read () in
            let enable =
              Control.Action.Enable
                { id
                ; expected_control_revision = current.value.control_revision
                ; basis = source_basis current.basis
                }
            in
            Recurrences.command controls (intent current.basis ~key:(key "enable") enable)
            |> protocol_ok
            |> ignore;
            let enabled = read () in
            assert (
              match enabled.value.control with
              | Enabled -> true
              | _ -> false);
            let pause ~key expected_control_revision =
              intent enabled.basis ~key (Pause { id; expected_control_revision })
            in
            let reject_unchanged code request =
              let before = state parent in
              let frames = Existing.frames env parent in
              request () |> require_error code |> ignore;
              Existing.assert_same before parent;
              [%test_eq: string list] frames (Existing.frames env parent)
            in
            reject_unchanged Conflict (fun () ->
              Recurrences.command
                controls
                (pause ~key:(key "stale-control") current.value.control_revision));
            with_reader env sw daemon session (fun reader ->
              reject_unchanged Permission_denied (fun () ->
                Recurrences.command
                  reader
                  (pause ~key:(key "denied-pause") enabled.value.control_revision)));
            (* Planning can advance the catalog without changing the enabled
               definition. Refresh that specific stale fence once, as a client
               would after a conflict. Any other rejection remains a failure. *)
            let pause_current (view : Query.Definition.t Query.Result.view) key =
              Recurrences.command
                controls
                (intent
                   view.basis
                   ~key
                   (Pause { id; expected_control_revision = view.value.control_revision }))
            in
            let observed = read () in
            (match pause_current observed (key "pause") with
             | Ok _ -> ()
             | Error { code = Conflict; _ } ->
               let newer = read () in
               assert (
                 Int64.(newer.basis.catalog_revision > observed.basis.catalog_revision));
               [%test_eq: int64]
                 observed.value.control_revision
                 newer.value.control_revision;
               pause_current newer (key "pause-current") |> protocol_ok |> ignore
             | Error error -> Error error |> protocol_ok);
            let paused = read () in
            assert (
              match paused.value.control with
              | Paused Operator_pause -> true
              | _ -> false);
            let retire =
              Control.Action.Retire
                { id; expected_control_revision = paused.value.control_revision }
            in
            Recurrences.command controls (intent paused.basis ~key:(key "retire") retire)
            |> protocol_ok
            |> ignore;
            let retired = read () in
            assert (
              match retired.value.control with
              | Retired Operator_retire -> true
              | _ -> false);
            let detached_attachment = (Handle.attachment handle).id in
            Handle.detach handle |> protocol_ok;
            let unbound = Recurrences.create connection ~session () in
            let observed = Recurrences.get unbound id |> protocol_ok |> view in
            [%test_eq: int64]
              retired.basis.catalog_revision
              observed.basis.catalog_revision;
            let detached =
              Recurrences.create
                connection
                ~session
                ~attachment:(fun () -> detached_attachment)
                ()
            in
            reject_unchanged Permission_denied (fun () ->
              Recurrences.command detached original);
            Support.with_operator daemon (fun reconnected ->
              Support.with_writer env sw reconnected ~session_id (fun renewed ->
                assert (
                  not
                    (P.Id.Attachment.equal
                       detached_attachment
                       (Handle.attachment renewed).id));
                let writer = Handle.recurrences renewed |> protocol_ok in
                let before_replay = state parent in
                let replay_frames = Existing.frames env parent in
                let replayed = Recurrences.command writer original |> protocol_ok in
                assert (
                  Jsonaf.exactly_equal
                    (Control.Result.payload dropped)
                    (Control.Result.payload replayed));
                Existing.assert_same before_replay parent;
                [%test_eq: string list] replay_frames (Existing.frames env parent)));
            [%test_eq: int] 0 !(counters.providers))))));
  print_endline "lost Create reply replays its original receipt after target removal";
  print_endline "detached writer cannot recover the receipt; a new writer can";
  print_endline
    "only the current writer can change the recurrence and stale intents leave no frames";
  [%expect
    {|
    lost Create reply replays its original receipt after target removal
    detached writer cannot recover the receipt; a new writer can
    only the current writer can change the recurrence and stale intents leave no frames
    |}]
;;
