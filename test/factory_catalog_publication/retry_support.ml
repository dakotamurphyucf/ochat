open! Core
open Agent_server_test_support
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module Actor = A.Session_actor
module Factory = Agent_server.Session_factory
module Daemon = Agent_server.Daemon
module Owner = Agent_server.Runtime_owner
module Registry = Agent_server.Session_registry
module Existing = Source_preparation_tests
module Source = Deferred_source_support
module Entry = A.Recurrence_entry
module Catalog = A.Recurrence_catalog
module Frontier = A.Recurrence_frontier
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Execution = A.Recurrence_execution
module Res = Openai.Responses

let state = Existing.state
let protect_cleanup = Creation_tests.protect_cleanup
let principal = principal ()
let original = Operator_authorization.trusted_local principal

let authorize current =
  if
    Agent_server.Authorization.session_visible_to
      principal
      (A.Session_state.summary current)
  then Ok ()
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"retry fixture is hidden"
         ~retryable:false
         ())
;;

let authorize_execution current _ = authorize current

let rejected = function
  | Error error -> error
  | Ok _ -> failwith "pending retry entered execution"
;;

let assert_error expected actual =
  assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json actual))
;;

let document_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let completed_response text =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id = "539-retry-result"
    ; status = "completed"
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; phase = None
    ; _type = "message"
    }
  in
  let item = Res.Response_stream.Item.Output_message message in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Output_text_delta
      { item_id = message.id
      ; output_index = 0
      ; content_index = 0
      ; delta = text
      ; type_ = "response.output_text.delta"
      }
  ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

type counters =
  { inference_ports : int ref
  ; providers : int ref
  ; transcripts : string list ref
  }

let with_recoverable_fixture
      ?(wrap_env = Fn.id)
      ?(max_root_agents = 1)
      ?(post_stream = fun _ -> completed_response "539 retry complete")
      f
  =
  let counters = { inference_ports = ref 0; providers = ref 0; transcripts = ref [] } in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      Int.incr counters.providers;
      counters.transcripts
      := !(counters.transcripts)
         @ [ `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string ];
      post_stream inputs)
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr counters.inference_ports;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~prompt_text:Source.prompt_text
    ~qualify_chatml_extensions:true
    ~wrap_env
    ~max_root_agents
    ~policy
    (fun env sw root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Source.with_loaded_entry daemon (fun ~entry ~runtime ~revision ->
           f env sw root daemon policy entry runtime revision counters)))
;;

let with_fixture ?wrap_env ?max_root_agents ?post_stream f =
  with_recoverable_fixture
    ?wrap_env
    ?max_root_agents
    ?post_stream
    (fun env sw _root daemon _policy entry runtime revision counters ->
       f env sw daemon entry runtime revision counters)
;;

let recurrence_id = P.Id.Recurrence.of_string "rec_539_factory_retry" |> protocol_ok

let recurrence entry =
  Catalog.find (Option.value_exn (state entry).recurrence_catalog) recurrence_id
  |> Option.value_exn
;;

let frontier entry = Entry.frontier (recurrence entry) |> protocol_ok

let execution entry intent =
  Frontier.find_execution (frontier entry) (Intent.occurrence intent) |> Option.value_exn
;;

let with_planner env sw daemon controller f =
  let module Configuration = Agent_server.Recurrence_automation_configuration in
  let module Host = Agent_server.Recurrence_automation_host in
  let configuration =
    Configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_539_planner" |> protocol_ok)
      ~controllers:[ (state controller).identity.session_id ]
      ~expires_at:None
      ~zone_directory:None
    |> protocol_ok
  in
  let host =
    Host.open_owned
      configuration
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
      ~sw
      ~fs:(Eio.Stdenv.fs env)
      ~now:P.Timestamp.now
      ~checkpoint:Eio.Fiber.check
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Host.close_and_wait host)
    (fun () ->
       Actor.set_recurrence_planning_host
         controller.Registry.actor
         (Host.planning_host host)
       |> protocol_ok;
       f ())
;;

let command controller key mutation =
  let current = state controller in
  let catalog = Option.value current.recurrence_catalog ~default:Catalog.empty in
  let _, source = A.Goal_catalog_source.capture current |> protocol_ok in
  let needs_source = A.Recurrence_command.Mutation.needs_source mutation in
  let request =
    A.Recurrence_command.create
      ~session_id:current.identity.session_id
      ~generation:current.identity.generation
      ~source:(Option.some_if needs_source source)
      ~expected_lifecycle_revision:
        (Option.some_if needs_source P.Session_lifecycle.Revision.zero)
      ~expected_catalog_revision:(Catalog.revision catalog)
      ~expected_control_revision:
        (Option.map (Catalog.find catalog recurrence_id) ~f:Entry.control_revision)
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
      ~mutation
    |> protocol_ok
  in
  Actor.recurrence_command controller.actor ~actor:original ~authorize ~request
  |> protocol_ok
  |> ignore
;;

type occurrence =
  { session : P.Session_ref.t
  ; intent : Intent.t
  ; previous : Attempt.t
  }

let create_occurrence daemon controller target_entry =
  let current = state target_entry in
  let session =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
      ~session_id:current.identity.session_id
  in
  let source = A.Recurrence_source.installed current |> protocol_ok in
  let target =
    A.Recurrence_target.create_existing
      ~session
      ~source
      ~configuration_revision:current.spec.configuration_revision
      ~input:{ kind = Plain_text; text = "539 actual retry input"; attachments = [] }
    |> protocol_ok
  in
  let period =
    Recurrence_rules.Rule.Period.create_ms 600_000L
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum [%sexp (error : Recurrence_rules.Timing_error.t)])
    |> Result.ok_or_failwith
  in
  let schedule =
    A.Recurrence_definition.Schedule.Interval
      { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
      ; first_due = P.Timestamp.to_time_ns (P.Timestamp.now ())
      }
  in
  command
    controller
    "539-create"
    (Create (recurrence_id, { schedule; target; policy = A.Recurrence_policy.default }));
  command controller "539-enable" (Enable recurrence_id);
  Actor.sweep_recurrences controller.actor |> protocol_ok;
  let recurrence = recurrence controller in
  let frontier = Entry.frontier recurrence |> protocol_ok in
  let nominal = Frontier.ready frontier |> List.hd_exn in
  let nominal_json =
    Frontier.to_document frontier
    |> document_ok
    |> D.Document.payload
    |> fun payload ->
    match D.Json.field payload ~name:"ready" with
    | Value (`Array rows) ->
      List.find_exn rows ~f:(fun row ->
        let retained = A.Recurrence_nominal.of_json row |> protocol_ok in
        A.Recurrence_identity.Occurrence.equal
          (A.Recurrence_nominal.key nominal)
          (A.Recurrence_nominal.key retained))
    | Absent | Null | Value _ -> failwith "actual planner has no complete Ready row"
  in
  let intent =
    Intent.create
      ~definition:(Entry.current recurrence)
      ~control_revision:(Entry.control_revision recurrence)
      ~nominal_json
      ~target:session
      ~source
      ~child:None
    |> protocol_ok
  in
  let previous =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id:principal.id
      ~expected_revision:(state target_entry).counters.revision
      ~key:(P.Idempotency_key.of_string "539-initial-attempt" |> protocol_ok)
    |> protocol_ok
  in
  { session; intent; previous }
;;

let attempted = function
  | Factory.Deferred_run_attempt.Outcome.Attempted attempt -> attempt
  | Retained _ -> failwith "fresh retry fixture returned retained DATA"
;;

let initial_absence ?controller env daemon entry occurrence =
  let controller = Option.value controller ~default:entry in
  let failure =
    P.Error.create
      Invalid_request
      ~message:"539 selected initial source failure"
      ~retryable:false
      ~data:(`Object [ "phase", `String "after-controller-own-ack" ])
      ()
  in
  let attempt =
    Factory.with_effect_deferred_run_source
      (Daemon.factory daemon)
      entry
      ~actor:original
      ~authorize
      ~request:
        (Attempt.request occurrence.previous ~intent:occurrence.intent |> protocol_ok)
      (fun deferred prepared ->
         let preparation = A.Run_preparation.Deferred.preparation deferred in
         let before = state controller in
         let frames = Existing.frames env controller in
         let target_before = state entry in
         let target_frames = Existing.frames env entry in
         let row =
           if phys_equal controller.Registry.actor entry.Registry.actor
           then
             Actor.record_prepared_recurrence_execution
               controller.actor
               ~binding:(Owner.Prepared.binding prepared)
               ~actor:original
               ~authorize:authorize_execution
               ~intent:occurrence.intent
               ~attempt:occurrence.previous
           else
             Actor.record_recurrence_execution
               controller.actor
               ~actor:original
               ~authorize:authorize_execution
               ~intent:occurrence.intent
               ~attempt:occurrence.previous
         in
         let row = protocol_ok row in
         assert (Intent.equal occurrence.intent (Execution.intent row));
         assert (
           Attempt.equal
             occurrence.previous
             (Execution.Attempt.request (Execution.current_attempt row)));
         [%test_eq: int64]
           (Int64.succ before.counters.revision)
           (state controller).counters.revision;
         [%test_eq: int]
           (List.length frames + 1)
           (List.length (Existing.frames env controller));
         if not (phys_equal controller.actor entry.actor)
         then (
           Existing.assert_same target_before entry;
           [%test_eq: string list] target_frames (Existing.frames env entry));
         [%test_eq: int64]
           (Attempt.expected_revision occurrence.previous)
           (A.Run_preparation.request preparation).expected_revision;
         [%test_eq: string]
           (Attempt.request_sha256 occurrence.previous)
           (A.Run_preparation.request_sha256 preparation);
         Owner.Prepared.check_basis prepared |> protocol_ok;
         Error failure)
    |> protocol_ok
    |> attempted
  in
  assert_error failure (Factory.Deferred_run_attempt.result attempt |> rejected);
  assert (Option.is_none (Factory.Deferred_run_attempt.retry attempt));
  let observation = Factory.Deferred_run_attempt.no_effects attempt |> Option.value_exn in
  Actor.No_effects_observation.check_current observation |> protocol_ok;
  Actor.No_effects_observation.check_basis observation |> protocol_ok;
  observation
;;

let next_request entry occurrence =
  Intent.request
    occurrence.intent
    ~expected_revision:(state entry).counters.revision
    ~key:(P.Idempotency_key.of_string "539-next-attempt" |> protocol_ok)
  |> protocol_ok
;;

let consume
      daemon
      controller
      target
      ~runtime:expected_runtime
      prepared
      held
      intent
      attempt
  =
  let observe () =
    Actor.observe_recurrence_run
      target.Registry.actor
      ~actor:original
      ~authorize
      ~intent
      ~attempt
    |> protocol_ok
    |> Option.value_exn
  in
  (if phys_equal controller.Registry.actor target.Registry.actor
   then
     Actor.import_held_recurrence_execution
       controller.actor
       ~held
       ~actor:original
       ~authorize:authorize_execution
       ~automatic_policy:(fun _ _ -> Ok true)
       ~observation:(observe ())
   else
     Actor.import_recurrence_execution
       controller.actor
       ~actor:original
       ~authorize:authorize_execution
       ~automatic_policy:(fun _ _ -> Ok true)
       ~observation:(observe ()))
  |> protocol_ok
  |> ignore;
  let permit =
    Actor.acquire_recurrence_activation
      controller.actor
      ~actor:original
      ~authorize:authorize_execution
      ~observation:(observe ())
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () ->
      Actor.release_recurrence_activation controller.actor permit |> protocol_ok)
    (fun () ->
       let checked =
         Actor.check_recurrence_activation
           controller.actor
           permit
           ~observation:(observe ())
         |> protocol_ok
       in
       Owner.with_held_input_runtime
         target.runtime
         ~prepared
         ~held
         ~checked
         (fun runtime ->
            Owner.Held_input_runtime.submit runtime ~prepare:(fun ~runtime ~scope ->
              assert (phys_equal expected_runtime runtime);
              Factory.prepare_held_input
                (Daemon.factory daemon)
                ~entry:target
                ~runtime
                ~scope)))
;;

(* Verify the actual retained tree; restoring original bytes does not renew any
   preparation. The caller still needs the exact Actor/Factory retry handoff. *)
let with_changed_source revision f =
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
  protect_cleanup ~finally:restore (fun () ->
    Eio.Path.unlink captured_file;
    Eio.Path.save ~create:(`Exclusive 0o600) captured_file (bytes ^ "\n539 changed bytes");
    f ())
;;

let ordinary_source_failure daemon entry revision =
  with_changed_source revision (fun () ->
    Existing.with_preparation entry "539-ordinary-source-error" (fun preparation ->
      Factory.with_prepared_run_source
        (Daemon.factory daemon)
        entry
        ~preparation
        (fun _ -> failwith "changed source entered ordinary callback")
      |> rejected))
;;
