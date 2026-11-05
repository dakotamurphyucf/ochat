open! Core
open Agent_server_test_support
module A = Agent_session
module P = Agent_protocol
module Factory = Agent_server.Session_factory
module Daemon = Agent_server.Daemon
module Owner = Agent_server.Runtime_owner
module Source = A.Runtime_builder.Source
module Existing = Source_preparation_tests
module Actor = A.Session_actor
module Attempt = Factory.Deferred_run_attempt

let attempted = function
  | Attempt.Outcome.Attempted attempt -> attempt
  | Retained receipt ->
    raise_s
      [%sexp "fresh source request returned retained work", (receipt : P.Run_receipt.t)]
;;

let request entry key =
  let current = Existing.state entry in
  A.Run_admission_request.Headless.create
    ~session_id:current.identity.session_id
    ~generation:current.identity.generation
    ~expected_revision:current.counters.revision
    ~mode:Single_turn
    ~input:Authored_start
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let require_no_effects attempt =
  let observation = Attempt.no_effects attempt |> Option.value_exn in
  Actor.No_effects_observation.check_current observation |> protocol_ok;
  Actor.No_effects_observation.check_basis observation |> protocol_ok;
  observation
;;

let%expect_test
    "Factory deferred source absence requires its actual closed resource owner"
  =
  let constructions = ref 0 in
  let providers = ref 0 in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      failwith "deferred source verification dispatched inference")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr constructions;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~qualify_chatml_extensions:true
    ~prompt_text:Deferred_source_support.prompt_text
    ~policy
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         let factory = Daemon.factory daemon in
         Deferred_source_support.with_loaded_entry
           daemon
           (fun ~entry ~runtime:actual_runtime ~revision ->
              let before = Existing.state entry in
              let frames = Existing.frames env entry in
              let counts = !constructions, !providers in
              let unchanged () =
                Existing.assert_same before entry;
                [%test_eq: string list] frames (Existing.frames env entry);
                [%test_eq: int * int] counts (!constructions, !providers);
                [%test_eq: int] 0 !providers;
                assert (Owner.is_loaded entry.runtime)
              in
              let original = Operator_authorization.trusted_local (principal ()) in
              let substituted_owner =
                Owner.create ~actor:entry.actor ~initial:None ~build:(fun () ->
                  failwith "substituted Owner must not construct resources")
              in
              let substituted_entry = { entry with runtime = substituted_owner } in
              let substituted_callbacks = ref 0 in
              (match
                 Factory.with_effect_deferred_run_source
                   factory
                   substituted_entry
                   ~actor:original
                   ~authorize:(fun _ -> Ok ())
                   ~request:(request entry "deferred-substituted-owner")
                   (fun _ _ ->
                      Int.incr substituted_callbacks;
                      Ok ())
               with
               | Error error -> [%test_eq: P.Error.code] Permission_denied error.code
               | Ok _ -> failwith "same Actor bypassed the actual Factory Owner lease");
              [%test_eq: int] 0 !substituted_callbacks;
              (* This empty value acquired nothing. Closing it would retire the real
              shared Actor, so it is intentionally not used as a resource owner. *)
              unchanged ();
              print_endline "same-Actor substituted Owner is denied before issuance";
              Eio.Switch.run (fun sw ->
                let entered, entered_u = Eio.Promise.create () in
                let release, release_u = Eio.Promise.create () in
                let release_borrow () =
                  if Option.is_none (Eio.Promise.peek release)
                  then Eio.Promise.resolve release_u ()
                in
                let borrowed =
                  Eio.Fiber.fork_promise ~sw (fun () ->
                    Owner.with_loaded_runtime entry.runtime (fun runtime ->
                      assert (phys_equal runtime actual_runtime);
                      Eio.Promise.resolve entered_u ();
                      Eio.Promise.await release;
                      Ok ()))
                in
                Fun.protect ~finally:release_borrow (fun () ->
                  Eio.Fiber.first
                    (fun () -> Eio.Promise.await entered)
                    (fun () ->
                       Eio.Promise.await_exn borrowed |> protocol_ok;
                       failwith "loaded callback ended before the barrier");
                  let callbacks = ref 0 in
                  (match
                     Factory.with_effect_deferred_run_source
                       factory
                       entry
                       ~actor:original
                       ~authorize:(fun _ -> Ok ())
                       ~request:(request entry "deferred-source-during-live-lease")
                       (fun _ _ ->
                          Int.incr callbacks;
                          Ok ())
                   with
                   | Error error -> [%test_eq: P.Error.code] Conflict error.code
                   | Ok _ -> failwith "source issuance bypassed the live Owner lease");
                  [%test_eq: int] 0 !callbacks;
                  assert (Option.is_none (Eio.Promise.peek borrowed));
                  unchanged ();
                  release_borrow ();
                  Eio.Promise.await_exn borrowed |> protocol_ok));
              unchanged ();
              print_endline "actual live Owner callback excludes deferred issuance";
              let stage key f =
                Factory.with_effect_deferred_run_source
                  factory
                  entry
                  ~actor:original
                  ~authorize:(fun _ -> Ok ())
                  ~request:(request entry key)
                  f
                |> protocol_ok
                |> attempted
              in
              let captured_file =
                Eio.Path.(
                  A.Prompt_revision.materialized_tree revision
                  / A.Prompt_revision.root_relative_path revision)
              in
              let bytes = Eio.Path.load captured_file in
              let restore () =
                Eio.Path.unlink captured_file;
                Eio.Path.save ~create:(`Exclusive 0o400) captured_file bytes
              in
              let callbacks = ref 0 in
              Fun.protect ~finally:restore (fun () ->
                Eio.Path.unlink captured_file;
                Eio.Path.save
                  ~create:(`Exclusive 0o600)
                  captured_file
                  (bytes ^ "\nchanged");
                let original_failure =
                  Existing.with_preparation
                    entry
                    "ordinary-verified-source-rejection"
                    (fun preparation ->
                       match
                         Factory.with_prepared_run_source
                           factory
                           entry
                           ~preparation
                           (fun _ ->
                              Int.incr callbacks;
                              Ok ())
                       with
                       | Error error -> error
                       | Ok () ->
                         failwith "ordinary source verifier accepted changed bytes")
                in
                let result =
                  stage "deferred-verified-source-rejection" (fun _ _ ->
                    Int.incr callbacks;
                    Ok ())
                in
                (match Attempt.result result with
                 | Error error ->
                   [%test_eq: P.Error.code] Prompt_unavailable error.code;
                   assert (
                     Jsonaf.exactly_equal
                       (P.Error.to_json original_failure)
                       (P.Error.to_json error))
                 | Ok () -> failwith "deferred source accepted changed captured bytes");
                [%test_eq: int] 0 !callbacks;
                ignore (require_no_effects result : Actor.No_effects_observation.t);
                unchanged ());
              print_endline
                "verified source rejection retains Error and genuine no-effects \
                 observation";
              let ordinary_callbacks = ref 0 in
              let ordinary_error = ref None in
              let escaped = ref None in
              let blocked =
                stage "deferred-source-blocks-ordinary-borrow" (fun deferred prepared ->
                  assert (
                    Option.is_some
                      (A.Run_preparation.original_actor
                         (A.Run_preparation.Deferred.preparation deferred)));
                  Owner.Prepared.check_basis prepared |> protocol_ok;
                  escaped := Some prepared;
                  let result =
                    Owner.with_loaded_runtime entry.runtime (fun _ ->
                      Int.incr ordinary_callbacks;
                      Ok ())
                  in
                  (match result with
                   | Error error ->
                     [%test_eq: P.Error.code] Conflict error.code;
                     ordinary_error := Some error
                   | Ok () ->
                     failwith "ordinary runtime escaped the exclusive source lane");
                  result)
              in
              [%test_eq: int] 0 !ordinary_callbacks;
              (match Attempt.result blocked with
               | Error error ->
                 assert (
                   Jsonaf.exactly_equal
                     (P.Error.to_json (Option.value_exn !ordinary_error))
                     (P.Error.to_json error))
               | Ok () -> failwith "ordinary blocked borrow lost its original error");
              (* Ordinary entry's negative marker cannot be laundered into a safe
             retry by the fact that the exclusive lane prevented its callback. *)
              assert (Option.is_none (Attempt.no_effects blocked));
              assert (
                Result.is_error (Owner.Prepared.check_basis (Option.value_exn !escaped)));
              unchanged ();
              print_endline
                "blocked ordinary callback cannot fabricate completed source provenance";
              let closed_prepared = ref None in
              let clean =
                stage "deferred-source-closed-before-real-borrow" (fun _ prepared ->
                  Owner.Prepared.check_basis prepared |> protocol_ok;
                  (match Source.selection (Existing.source prepared) with
                   | Authored selected -> assert (phys_equal selected revision)
                   | Captured _ | Authored_child _ | Generated _ ->
                     failwith "ordinary source changed family");
                  closed_prepared := Some prepared;
                  Ok ())
              in
              Attempt.result clean |> protocol_ok;
              let observation = require_no_effects clean in
              assert (
                Result.is_error
                  (Owner.Prepared.check_basis (Option.value_exn !closed_prepared)));
              unchanged ();
              let actual_borrows = ref 0 in
              Owner.with_loaded_runtime entry.runtime (fun runtime ->
                assert (phys_equal runtime actual_runtime);
                Int.incr actual_borrows;
                Ok ())
              |> protocol_ok;
              [%test_eq: int] 1 !actual_borrows;
              assert (
                Result.is_error (Actor.No_effects_observation.check_basis observation));
              assert (
                Result.is_error (Actor.No_effects_observation.check_current observation));
              unchanged ();
              print_endline
                "same loaded resource escape invalidates closed absence despite \
                 unchanged durable state")));
  [%expect
    {|
    same-Actor substituted Owner is denied before issuance
    actual live Owner callback excludes deferred issuance
    verified source rejection retains Error and genuine no-effects observation
    blocked ordinary callback cannot fabricate completed source provenance
    same loaded resource escape invalidates closed absence despite unchanged durable state |}]
;;
