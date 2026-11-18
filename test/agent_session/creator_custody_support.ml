open! Core
open Fixtures
module S = Agent_session
module A = S.Session_actor
module P = Agent_protocol
module M = Chat_response.Moderator_manager
module EC = Chat_response.Extension_compiler
module N = S.Native_tool_invocation
module Q = S.Moderator_queue
module Snapshot = Session.Moderator_state.Identity_snapshot

module Original = struct
  type t =
    | Authenticated of Operator_authorization.t
    | No_principal
end

module Native_original = struct
  type t =
    | Principal_b
    | No_principal
end

type native_observation =
  | Principal of
      { principal : P.Id.Principal.t
      ; current : bool
      ; proof : Operator_authorization.t
      }
  | No_principal

type t =
  { actor : A.t
  ; backend : S.Memory_backend.t
  ; manager : M.t
  ; definition : EC.definition
  ; tools : S.Script_tool_calls.t
  ; writer : P.Id.Attachment.t
  ; b : Operator_authorization.t
  ; b_live : bool Atomic.t
  ; a : Operator_authorization.t
  ; effects : int ref
  ; native : native_observation list ref
  ; missing : int ref
  ; denials : P.Error.t list ref
  ; reject_link : bool ref
  ; rejected_links : int ref
  ; reject_creation : bool ref
  ; rejected_creation : (S.Session_state.t * S.Session_state.t) option ref
  ; reject_delivery : P.Id.Schedule.t option ref
  ; rejected_deliveries : int ref
  ; action : (S.Operation_worker.Input.t -> S.Operation_worker.Capabilities.t -> unit) ref
  ; worker_failure : (exn * Stdlib.Printexc.raw_backtrace) option ref
  }

let state t = A.state t.actor |> protocol_ok
let snapshot t = M.identity_snapshot t.manager |> Result.ok_or_failwith

let history t =
  (state t).conversation.canonical_history
  |> S.History_codec.all_of_protocol
  |> protocol_ok
;;

let source t = M.invocation_observer t.manager |> Option.value_exn

let queue t =
  S.Moderator_checkpoint.queue (state t).moderator |> protocol_ok |> Option.value_exn
;;

let requests : P.Invocation.follow_up =
  { request_turn = false; request_compaction = false; end_session = None }
;;

(* These are the normal public Actor service ports, with the same receiver and
   selected receipt ownership as Factory.extension_{schedules,subscriptions,ingress}. *)
let services actor tools =
  let schedules =
    S.Script_schedule_service.create
      ~host:
        { create =
            (fun owner source ~delay_ms ~payload ~misfire ->
              A.create_script_schedule actor ~owner ~source ~delay_ms ~payload ~misfire)
        ; stage =
            (fun owner source ~previous ~next ->
              A.stage_schedule_mutation actor ~owner ~source ~previous ~next)
        ; get = (fun owner source id -> A.read_script_schedule actor ~owner ~source ~id)
        ; select =
            (fun owner source receipts ->
              A.select_schedule_mutations actor ~owner ~source ~receipts)
        ; abort =
            (fun owner receipt ->
              A.abort_schedule_mutation actor ~owner ~receipt |> protocol_ok)
        }
  in
  let subscriptions =
    S.Script_subscription_service.create
      ~limits:S.Staged_subscriptions.default_limits
      ~host:
        { create =
            (fun owner source ~kind ~lifetime_ms ~wake ~completion_schema ->
              A.create_script_subscription
                actor
                ~owner
                ~source
                ~kind
                ~lifetime_ms
                ~wake
                ~completion_schema)
        ; stage =
            (fun owner source ~previous ~next ->
              A.stage_subscription_mutation actor ~owner ~source ~previous ~next)
        ; get =
            (fun owner source id -> A.read_script_subscription actor ~owner ~source ~id)
        ; finish =
            (fun owner source id ~expected_epoch completion ->
              A.finish_script_subscription
                actor
                ~owner
                ~source
                ~id
                ~expected_epoch
                completion)
        ; select =
            (fun owner source receipts ->
              A.select_subscription_mutations actor ~owner ~source ~receipts)
        ; abort =
            (fun owner receipt ->
              A.abort_subscription_mutation actor ~owner ~receipt |> protocol_ok)
        ; get_job = (fun owner id -> A.read_script_job actor ~owner ~id)
        }
  in
  let ingress =
    S.Script_ingress_service.create
      ~host:
        { register =
            (fun owner source ~subscription_id ~expected_epoch ~namespace ~schema ->
              A.create_script_ingress
                actor
                ~owner
                ~source
                ~subscription_id
                ~expected_epoch
                ~namespace
                ~schema)
        ; get = (fun owner source id -> A.read_script_ingress actor ~owner ~source ~id)
        ; revoke =
            (fun owner source id ~reason ->
              A.revoke_script_ingress actor ~owner ~source ~id ~reason)
        ; select =
            (fun owner source receipts ->
              A.select_ingress_mutations actor ~owner ~source ~receipts)
        ; abort =
            (fun owner receipt ->
              A.abort_ingress_mutation actor ~owner ~receipt |> protocol_ok)
        }
  in
  tools
  |> fun tools ->
  S.Script_tool_calls.with_schedule_service tools schedules
  |> fun tools ->
  S.Script_tool_calls.with_subscription_service tools subscriptions
  |> fun tools -> S.Script_tool_calls.with_ingress_service tools ingress
;;

let definition env registry =
  let dir = Eio.Stdenv.cwd env in
  let source =
    {|<script id="creator-custody" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  (match p.input with
  | `String("same") ->
    let* timer = Schedule.after_ms(0, `String("same")) in
    let* sub = Subscription.create("same", `Some(60000), `No_wake) in
    let* armed = Subscription.arm(sub, 0, `Some(timer), `None) in
    let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(sub), `Null)) in
    Task.pure(state + 1)
  | `String("later") ->
    let* timer = Schedule.after_ms(0, `String("later")) in
    let* sub = Subscription.create("later", `Some(60000), `No_wake) in
    let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(sub), `Null)) in
    Task.pure(state + 1)
  | `String("spare") ->
    let* timer = Schedule.after_ms(0, `String("spare")) in
    let* () = Invocation.resolve(p.context.invocation_id, `Complete(`Null)) in
    Task.pure(state + 1)
  | `String("ingress") ->
    let* sub = Subscription.create("ingress", `Some(60000), `No_wake) in
    let* registration = Ingress.register(sub, 0, "external.creator", `Bool(true)) in
    let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(sub), `Null)) in
    Task.pure(state + 1)
  | _ -> Task.fail("unknown creator fixture request"))
| `Internal_event(payload) ->
  let* result = Tool.call("read_file", `Object([])) in
  (match result with
  | `Ok(value) -> Task.pure(state + 1)
  | `Error(code) -> Task.fail(code))
| _ -> Task.pure(state)
</script>
<tool name="creator" type="moderator" moderator="creator-custody"
 input_schema="any.json" output_schema="any.json" completion_schema="any.json"/>|}
  in
  let loader =
    Source_loader.captured_filesystem ~root:dir ~sources:[ "any.json", "true" ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs ~dir ~source_loader:loader source
  in
  EC.prepare_definition_in_domain ~env ~capabilities:registry elements
  |> Result.map_error ~f:(fun errors ->
    String.concat ~sep:"; " (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
  |> Result.ok_or_failwith
;;

let manager env definition ?snapshot () =
  let _, artifact =
    M.Registry.of_definition M.Registry.empty definition |> Result.ok_or_failwith
  in
  let allocator =
    History_entry.Allocator.create ~namespace:"creator-custody" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  M.create_entries
    ~env
    ?snapshot
    ~artifact:(Option.value_exn artifact)
    ~capabilities:Chat_response.Moderation.Capabilities.default
    ~allocator
    ()
  |> Result.ok_or_failwith
;;

let authentication_a () =
  let principal =
    P.Principal.create
      ~id:second_principal_id
      ~authentication_kind:"local"
      ~scopes:(P.Scope.Set.of_list [ Send_messages ])
      ~attributes:[]
    |> protocol_ok
  in
  Operator_authorization.nonexpiring_static principal
;;

let with_fixture ?environment ?restored ?(native_original = Native_original.Principal_b) f
  =
  let reject_link = ref false
  and rejected_links = ref 0
  and reject_creation = ref false
  and rejected_creation = ref None
  and reject_delivery = ref None
  and rejected_deliveries = ref 0
  and backend_for_rejection = ref None in
  Job_fixtures.with_actor
    ?environment
    ~start:(Option.is_none restored)
    ~prepare_state:(fun initial ->
      match restored with
      | Some state -> state
      | None ->
        { initial with
          identity = { initial.identity with creating_principal = Some principal_id }
        })
    ~reject_save:(fun transition ->
      let candidate = transition.S.Session_transition.state in
      if !reject_creation && not (List.is_empty candidate.schedules)
      then (
        reject_creation := false;
        let before = S.Memory_backend.state (Option.value_exn !backend_for_rejection) in
        rejected_creation := Some (before, candidate);
        true)
      else if
        Option.exists !reject_delivery ~f:(fun id ->
          List.exists candidate.schedules ~f:(fun timer ->
            P.Id.Schedule.equal id timer.P.Schedule.id
            &&
            match timer.status with
            | Delivered -> true
            | Scheduled | Delivering | Cancelled | Failed _ -> false))
      then (
        Int.incr rejected_deliveries;
        true)
      else if
        !reject_link
        && List.exists transition.S.Session_transition.state.schedules ~f:(fun timer ->
          Option.exists timer.P.Schedule.ownership ~f:(fun ownership ->
            Option.is_some ownership.subscription)
          && Jsonaf.exactly_equal timer.payload (`String "later"))
      then (
        Int.incr rejected_links;
        true)
      else false)
    (fun env _sw actor writer backend ->
       backend_for_rejection := Some backend;
       let b_live = Atomic.make true in
       let b = Agent_authority_tests.authentication b_live in
       let a = authentication_a () in
       let effects = ref 0
       and native = ref []
       and missing = ref 0
       and denials = ref [] in
       let registry =
         native_registry effects ~raises:false ~on_call:(fun () ->
           let borrowed = N.borrow () |> protocol_ok in
           let observation =
             match N.borrowed_agent_actor borrowed |> protocol_ok with
             | Some proof ->
               Principal
                 { principal = (Operator_authorization.principal proof).id
                 ; current = Operator_authorization.is_current proof
                 ; proof
                 }
             | None -> No_principal
           in
           native := observation :: !native)
       in
       let definition = definition env registry in
       let restored_snapshot =
         Option.bind restored ~f:(fun state ->
           S.Moderator_checkpoint.decode state.S.Session_state.moderator |> protocol_ok)
       in
       let manager = manager env definition ?snapshot:restored_snapshot () in
       let tools =
         S.Script_tool_calls.create
           ~registry:(fun () -> registry)
           ~moderator_names:(String.Set.singleton "creator")
           ~now:(fun () -> timestamp)
           ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
           ~requires_active_moderator:(fun _ -> false)
           ~authorize:(fun _ _ ->
             let open Result.Let_syntax in
             let%bind borrowed = N.borrow () in
             match native_original, N.borrowed_agent_actor borrowed with
             | Native_original.No_principal, Ok None -> Ok ()
             | (Principal_b | No_principal), Ok (Some proof) ->
               let principal = (Operator_authorization.principal proof).id in
               let current = Operator_authorization.is_current proof in
               if
                 match native_original with
                 | Principal_b -> P.Id.Principal.equal principal principal_id && current
                 | No_principal -> false
               then Ok ()
               else (
                 let failure =
                   P.Error.create
                     Permission_denied
                     ~message:"creator original is not current"
                     ~retryable:false
                     ()
                 in
                 denials := failure :: !denials;
                 Error failure)
             | Principal_b, Ok None ->
               Int.incr missing;
               let failure =
                 P.Error.create
                   Permission_denied
                   ~message:"creator original is unavailable"
                   ~retryable:false
                   ()
               in
               denials := failure :: !denials;
               Error failure
             | (Principal_b | No_principal), Error failure ->
               Int.incr missing;
               denials := failure :: !denials;
               Error failure)
           ~prepare_output:(fun _ -> Ok (`String "actual creator native result"))
           ~defer_observation:(fun _ -> Ok ())
         |> services actor
       in
       let action =
         ref
           (fun
               (_ : S.Operation_worker.Input.t) (_ : S.Operation_worker.Capabilities.t) ->
              ())
       in
       let worker_failure = ref None in
       let worker =
         S.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
           try
             !action input caps;
             let current = A.state actor |> protocol_ok in
             S.Operation_worker.Completed
               { final_history =
                   S.History_codec.all_of_protocol current.conversation.canonical_history
                   |> protocol_ok
               ; moderator_snapshot = current.moderator
               ; runtime_requests = []
               }
           with
           | exn ->
             let trace = Stdlib.Printexc.get_raw_backtrace () in
             worker_failure := Some (exn, trace);
             Stdlib.Printexc.raise_with_backtrace exn trace)
       in
       (match restored with
        | None ->
          A.change_moderator
            actor
            (Some
               (S.Moderator_checkpoint.encode
                  (M.identity_snapshot manager |> Result.ok_or_failwith)))
          |> protocol_ok
          |> ignore
        | Some _ -> ());
       A.set_runtime_worker
         ~moderator_manager:manager
         actor
         ~worker:(Some worker)
         ~inference:
           (Some
              (Inference_ports.create ~config:Chat_response.Config.default ()
               |> Inference_ports.execution))
       |> protocol_ok;
       f
         env
         { actor
         ; backend
         ; manager
         ; definition
         ; tools
         ; writer = writer.id
         ; b
         ; b_live
         ; a
         ; effects
         ; native
         ; missing
         ; denials
         ; reject_link
         ; rejected_links
         ; reject_creation
         ; rejected_creation
         ; reject_delivery
         ; rejected_deliveries
         ; action
         ; worker_failure
         })
;;

let run_with_original t ~original f =
  t.action := f;
  let reservation = A.reserve_history_block t.actor ~count:1 |> protocol_ok in
  let entry =
    Pending_input_tests.occurrence
      (Int64.to_int_exn reservation.first_sequence)
      "actual creator-custody Operation"
  in
  (match original with
   | Original.Authenticated original ->
     A.submit_message_authenticated t.actor ~actor:original ~attachment_id:t.writer entry
   | No_principal ->
     A.submit_message
       t.actor
       ~submitting_principal:principal_id
       ~attachment_id:t.writer
       entry)
  |> protocol_ok
  |> ignore;
  let rec await () =
    Option.iter !(t.worker_failure) ~f:(fun (exn, trace) ->
      Stdlib.Printexc.raise_with_backtrace exn trace);
    match (state t).active_operation, (state t).lifecycle.observed with
    | None, Idle -> ()
    | _ ->
      Eio.Fiber.yield ();
      await ()
  in
  await ()
;;

let run t ~original f = run_with_original t ~original:(Authenticated original) f

let invoke_with_original ?(reject_creation = false) t ~original text =
  let parent_proof = ref None in
  let entered = ref false in
  let before = state t in
  let before_snapshot = snapshot t in
  run_with_original t ~original (fun input caps ->
    let proof = caps.original_agent_actor () |> protocol_ok in
    (match original, proof with
     | Original.Authenticated _, Some proof ->
       assert (Operator_authorization.is_current proof)
     | No_principal, None -> ()
     | Authenticated _, None | No_principal, Some _ ->
       failwith "actual creator submission changed its original");
    parent_proof := proof;
    entered := true;
    let call_id = "creator-" ^ P.Id.Invocation.to_string (P.Id.Invocation.create ()) in
    let id = History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith in
    let payload = Jsonaf.to_string (`String text) in
    let call =
      Openai.Responses_history.create_with_id_exn
        ~id
        (Chat_response.Tool_call.call_item
           ~kind:Function
           ~name:"creator"
           ~payload
           ~call_id
           ~id:None)
    in
    let dispatch =
      S.Moderator_tool_dispatch.create
        ~script_tools:t.tools
        ~definition:t.definition
        ~manager:t.manager
        ~input
        ~capabilities:caps
        ~available_tools:[]
        ~session_meta:`Null
        ~now:(fun () -> timestamp)
        ~validate_work:(fun _ -> Error "only actual selected subscription is allowed")
        ~admit:(fun _ -> Ok ())
        ~prepare_outcome:(fun _ -> Ok ())
        ()
    in
    let request =
      Chat_response.In_memory_stream.Tool_dispatch.
        { kind = Function
        ; original_name = "creator"
        ; original_payload = payload
        ; name = "creator"
        ; payload
        ; rejection = None
        ; call
        ; history = input.history @ [ call ]
        ; source = None
        ; parent_call_id = None
        }
    in
    let result =
      Exn.protect
        ~finally:(fun () -> t.reject_creation := false)
        ~f:(fun () ->
          t.reject_creation := reject_creation;
          dispatch.run request ~authorize:ignore |> Option.value_exn)
    in
    let output_id =
      History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
    in
    let output =
      Openai.Responses_history.create_with_id_exn
        ~id:output_id
        (Chat_response.Tool_call.output_item
           ~kind:Function
           ~call_id
           ~output:result.output)
    in
    (Option.value_exn result.commit_output) output);
  assert !entered;
  Option.iter !parent_proof ~f:(fun proof ->
    assert (not (Operator_authorization.is_current proof)));
  assert (Operator_authorization.is_current t.b);
  let after = state t in
  let created =
    List.filter after.invocations ~f:(fun invocation ->
      String.equal invocation.P.Invocation.context.tool_name "creator"
      && not
           (List.exists before.invocations ~f:(fun old ->
              P.Id.Invocation.equal old.context.id invocation.context.id)))
    |> function
    | [ created ] -> created
    | [] | _ :: _ :: _ ->
      failwith "actual creator did not produce one distinct invocation"
  in
  if reject_creation
  then (
    let installed, candidate = Option.value_exn !(t.rejected_creation) in
    assert (not (List.is_empty candidate.schedules));
    assert (not (List.is_empty candidate.subscriptions));
    assert (
      List.equal
        (fun a b -> Jsonaf.exactly_equal (P.Schedule.to_json a) (P.Schedule.to_json b))
        installed.schedules
        after.schedules);
    assert (
      List.equal
        (fun a b ->
           Jsonaf.exactly_equal (P.Subscription.to_json a) (P.Subscription.to_json b))
        installed.subscriptions
        after.subscriptions);
    assert (
      List.equal
        S.External_ingress.equal
        installed.ingress_registrations
        after.ingress_registrations);
    assert (
      List.equal
        (fun a b -> Jsonaf.exactly_equal (P.Job.to_json a) (P.Job.to_json b))
        installed.jobs
        after.jobs);
    (* The truthful failed Invocation publication initializes the first empty
       queue lifetime. It must preserve every other checkpoint byte and cannot
       retain the rejected creator's state or append queued work. *)
    assert (
      Option.is_none (S.Moderator_checkpoint.queue installed.moderator |> protocol_ok));
    let _, installed_source =
      S.Moderator_source_installation.capture installed |> protocol_ok
    in
    let empty_queue = Q.bootstrap ~source:installed_source ~queue:[] |> protocol_ok in
    let expected_checkpoint =
      S.Moderator_checkpoint.update_with_queue
        installed.moderator
        ~snapshot:before_snapshot
        ~queue:empty_queue
      |> protocol_ok
    in
    assert (Option.equal Jsonaf.exactly_equal (Some expected_checkpoint) after.moderator);
    [%test_eq: string]
      (S.Moderator_checkpoint.digest before_snapshot)
      (S.Moderator_checkpoint.digest (snapshot t));
    assert_same_session_snapshot after (S.Memory_backend.state t.backend);
    match created.status with
    | Published (Fail _) -> ()
    | Admitted | Dispatching | Resolved _
    | Published (Complete _ | Pending _ | Cancelled _) ->
      failwith "rejected Fresh creation lost its truthful failed callback");
  created
;;

let invoke t text = invoke_with_original t ~original:(Authenticated t.b) text |> ignore

let drain t =
  let result = ref None in
  run t ~original:t.a (fun _ caps ->
    result
    := Some
         (S.Moderator_event.run_queued_idle
            ~claim:caps.with_queued_moderator_event
            ~script_tools:t.tools
            ~manager:t.manager
            ~history:(fun () -> history t)
            ~available_tools:[]
            ~session_meta:`Null
            ~now:(fun () -> timestamp)
            ()));
  Option.value_exn !result
;;

let assert_native_b t count =
  [%test_eq: int] count !(t.effects);
  [%test_eq: int] count (List.length !(t.native));
  List.iter !(t.native) ~f:(function
    | Principal observation ->
      assert (P.Id.Principal.equal observation.principal principal_id);
      assert observation.current;
      assert (not (Operator_authorization.is_current observation.proof))
    | No_principal -> failwith "B creator unexpectedly became no-principal");
  assert (Operator_authorization.is_current t.b);
  assert (Operator_authorization.is_current t.a)
;;

let assert_backend t =
  assert_same_session_snapshot (state t) (S.Memory_backend.state t.backend)
;;

let schedule t label =
  List.find_exn (state t).schedules ~f:(fun schedule ->
    Jsonaf.exactly_equal schedule.P.Schedule.payload (`String label))
;;

let subscription t kind =
  List.find_exn (state t).subscriptions ~f:(fun subscription ->
    String.equal subscription.P.Subscription.context.kind kind)
;;

let assert_creator t (timer : P.Schedule.t) =
  let ownership = Option.value_exn timer.ownership in
  let id =
    match ownership.creator with
    | Invocation id -> id
    | Moderator_event _ -> failwith "compiled Tool_invoked did not create timer"
  in
  assert (
    List.exists (state t).invocations ~f:(fun invocation ->
      P.Id.Invocation.equal invocation.context.id id
      &&
      match invocation.status with
      | Published (Pending _ | Complete _) -> true
      | Admitted | Dispatching | Resolved _ | Published (Fail _ | Cancelled _) -> false));
  let provenance = Option.value_exn timer.inference_provenance in
  assert (
    Option.equal
      P.Inference_origin.equal
      (Some Unbound)
      (P.Schedule_provenance.origin provenance));
  assert (
    List.is_empty
      (S.Run_state.runs (Option.value (state t).run_state ~default:S.Run_state.empty)));
  id
;;

let deliver_timer ?(reject_ack = false) t label =
  let timer = schedule t label in
  let claimed =
    A.claim_schedule t.actor ~schedule_id:timer.id ~generation:timer.generation
    |> protocol_ok
    |> Option.value_exn
  in
  let before = snapshot t in
  let frame = Chat_response.Schedule_delivery.capture claimed |> Result.ok_or_failwith in
  let installed_before = state t in
  let enqueue () =
    M.enqueue_internal_event_entries
      t.manager
      ~event:frame
      ~prepare:(fun ~before:actual ~snapshot ->
        assert (
          String.equal
            (S.Moderator_checkpoint.digest actual)
            (S.Moderator_checkpoint.digest before));
        A.complete_schedule
          ~expected:actual
          ~expected_schedule:claimed
          ~moderator_manager:t.manager
          t.actor
          ~schedule_id:timer.id
          ~generation:timer.generation
          ~moderator_snapshot:(Some (S.Moderator_checkpoint.encode snapshot))
        |> Result.map ~f:ignore
        |> Result.map_error ~f:(fun error -> error.P.Error.message))
  in
  if reject_ack
  then (
    let rejected =
      Exn.protect
        ~finally:(fun () -> t.reject_delivery := None)
        ~f:(fun () ->
          t.reject_delivery := Some timer.id;
          enqueue ())
    in
    (match rejected with
     | Error message -> [%test_eq: string] "injected job transition failure" message
     | Ok _ -> failwith "actual delivery ACK fault did not reject");
    [%test_eq: int] 1 !(t.rejected_deliveries);
    assert_same_session_snapshot installed_before (state t);
    assert_same_session_snapshot installed_before (S.Memory_backend.state t.backend);
    [%test_eq: string]
      (S.Moderator_checkpoint.digest before)
      (S.Moderator_checkpoint.digest (snapshot t)));
  enqueue () |> Result.ok_or_failwith |> ignore;
  let delivered = schedule t label in
  [%test_eq: int] 1 delivered.delivery_count;
  (match delivered.status with
   | Delivered -> ()
   | Scheduled | Delivering | Cancelled | Failed _ ->
     failwith "actual timer delivery was not ACKed");
  let entry = List.last_exn (Q.entries (queue t)) in
  (match Q.Entry.producer entry with
   | Some (Schedule_delivery { id; generation; delivery_count }) ->
     assert (P.Id.Schedule.equal timer.id id);
     [%test_eq: int] timer.generation generation;
     [%test_eq: int] 1 delivery_count
   | None
   | Some
       ( Event _
       | Invocation _
       | Invocation_at_source _
       | Job_completion _
       | Source_setup _
       | Ingress_delivery _ ) -> failwith "timer entry lost exact producer");
  let provenance = Option.value_exn timer.inference_provenance in
  assert (
    P.Run_source.equal (Q.source (queue t)) (P.Schedule_provenance.source provenance));
  assert (
    Option.equal
      P.Inference_origin.equal
      (Q.Entry.origin entry)
      (P.Schedule_provenance.origin provenance))
;;

let successful_drain t =
  match drain t with
  | Ok (Some _) -> ()
  | Ok None -> failwith "actual contextual queue head was not executed"
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

(* Compare at the rejected commit boundary, AFTER the genuine A claim. A later
   failure receipt is a separate honest transition, not a checkpoint ACK. *)
let link_later t ~reject =
  let result = ref None
  and receipt = ref None
  and rejection_verified = ref false in
  run t ~original:t.a (fun _ caps ->
    let proof = caps.original_agent_actor () |> protocol_ok |> Option.value_exn in
    assert (
      P.Id.Principal.equal (Operator_authorization.principal proof).id second_principal_id);
    result
    := Some
         (caps.with_moderator_event
            ~snapshot:(fun () -> Ok (snapshot t))
            ~event:Turn_start
            (fun ~executing
              ~policy_caller:_
              ~model_jobs:_
              ~allocate_inference:_
              ~retirement_reason
              ~event:_
              ~execute:_
              ~commit ->
               assert (Option.is_none retirement_reason);
               receipt := Some executing.context.id;
               let owner = P.Job.Moderator_event executing.context.id in
               let before = state t in
               let before_snapshot = snapshot t in
               let timer = schedule t "later" in
               let previous = subscription t "later" in
               let next =
                 P.Subscription.arm
                   previous
                   ~expected_epoch:previous.epoch
                   ~timer_id:(Some timer.id)
                   ~job_id:None
                 |> protocol_ok
               in
               let sub_receipt =
                 A.stage_subscription_mutation
                   t.actor
                   ~owner
                   ~source:(source t)
                   ~previous:(Some previous)
                   ~next
                 |> protocol_ok
               in
               let ownership = Option.value_exn timer.ownership in
               let linked =
                 { timer with
                   ownership =
                     Some
                       { ownership with
                         subscription = Some (next.context.id, next.epoch)
                       }
                 }
               in
               let timer_receipt =
                 A.stage_schedule_mutation
                   t.actor
                   ~owner
                   ~source:(source t)
                   ~previous:(Some timer)
                   ~next:linked
                 |> protocol_ok
               in
               A.select_subscription_mutations
                 t.actor
                 ~owner
                 ~source:(source t)
                 ~receipts:[ sub_receipt ]
               |> protocol_ok;
               A.select_schedule_mutations
                 t.actor
                 ~owner
                 ~source:(source t)
                 ~receipts:[ timer_receipt ]
               |> protocol_ok;
               let committed =
                 Exn.protect
                   ~finally:(fun () -> t.reject_link := false)
                   ~f:(fun () ->
                     t.reject_link := reject;
                     commit ~snapshot:before_snapshot ~requests)
               in
               if reject
               then (
                 (match committed with
                  | Error failure -> [%test_eq: P.Error.code] Internal_error failure.code
                  | Ok () -> failwith "later link fault did not reject actual ACK");
                 assert_same_session_snapshot before (state t);
                 assert_same_session_snapshot before (S.Memory_backend.state t.backend);
                 [%test_eq: string]
                   (S.Moderator_checkpoint.digest before_snapshot)
                   (S.Moderator_checkpoint.digest (snapshot t));
                 rejection_verified := true);
               committed));
    if reject
    then assert (Result.is_error (Option.value_exn !result))
    else assert (Option.value_exn !result |> protocol_ok));
  if reject then assert !rejection_verified;
  let id = Option.value_exn !receipt in
  let event =
    List.find_exn (state t).moderator_executions ~f:(fun event ->
      P.Id.Moderator_execution.equal event.context.id id)
  in
  (match reject, event.status with
   | true, Failed _ | false, Completed _ -> ()
   | true, (Running | Completed _ | Interrupted _)
   | false, (Running | Failed _ | Interrupted _) ->
     failwith "later callback terminal disposition is untruthful");
  id
;;

let registration t = List.hd_exn (state t).ingress_registrations
let ingress_payload = `Object [ "actual", `String "creator ingress" ]

let prepare_ingress t key =
  Ingress_delivery_tests.prepare t.actor (registration t) key ingress_payload
;;

let accept_ingress t key =
  let plan = prepare_ingress t key |> protocol_ok |> Ingress_delivery_tests.proposal in
  let frame = S.Ingress_submission.frame plan |> protocol_ok in
  let accepted = ref None in
  M.enqueue_internal_event_entries
    t.manager
    ~event:(Chat_response.Ingress_delivery.capture frame)
    ~prepare:(fun ~before ~snapshot ->
      A.commit_ingress_submission t.actor plan ~before ~snapshot
      |> Result.map ~f:(fun receipt -> accepted := Some receipt)
      |> Result.map_error ~f:(fun error -> error.P.Error.message))
  |> Result.ok_or_failwith
  |> ignore;
  let receipt = Option.value_exn !accepted in
  let entry = List.last_exn (Q.entries (queue t)) in
  (match Q.Entry.producer entry with
   | Some (Ingress_delivery { registration_id; event_id }) ->
     assert (P.Id.Capability.equal registration_id (registration t).context.id);
     assert (P.Id.Ingress_event.equal event_id receipt.id)
   | None
   | Some
       ( Event _
       | Invocation _
       | Invocation_at_source _
       | Job_completion _
       | Schedule_delivery _
       | Source_setup _ ) -> failwith "ingress lost exact retained receipt producer");
  let provenance = Option.value_exn (registration t).inference_provenance in
  assert (
    P.Run_source.equal
      (Q.source (queue t))
      (S.External_ingress.Provenance.source provenance));
  assert (Option.equal P.Inference_origin.equal (Some Unbound) (Q.Entry.origin entry));
  receipt
;;

let denied_drain t =
  let before = state t in
  let before_effects = !(t.effects) in
  let before_denials = List.length !(t.denials) in
  let result = drain t in
  [%test_eq: int] before_effects !(t.effects);
  assert (Operator_authorization.is_current t.a);
  let new_native_denial () =
    assert (List.length !(t.denials) > before_denials);
    assert (
      List.exists (state t).invocations ~f:(fun invocation ->
        (not
           (List.exists before.invocations ~f:(fun previous ->
              P.Id.Invocation.equal previous.context.id invocation.context.id)))
        && String.equal invocation.context.tool_name "read_file"
        &&
        match invocation.status with
        | Resolved (Fail failure) | Published (Fail failure) ->
          String.equal failure.code "invocation.permission_denied"
        | Admitted | Dispatching
        | Resolved (Complete _ | Pending _ | Cancelled _)
        | Published (Complete _ | Pending _ | Cancelled _) -> false))
  in
  match result with
  | Error failure when P.Error.equal_code failure.code Permission_denied -> ()
  | Error _ | Ok (Some _) -> new_native_denial ()
  | Ok None -> failwith "nonempty denied contextual head disappeared"
;;

let revoke_ingress t =
  run t ~original:t.a (fun _ caps ->
    caps.with_moderator_event
      ~snapshot:(fun () -> Ok (snapshot t))
      ~event:Turn_start
      (fun ~executing
        ~policy_caller:_
        ~model_jobs:_
        ~allocate_inference:_
        ~retirement_reason:_
        ~event:_
        ~execute:_
        ~commit ->
         let owner = P.Job.Moderator_event executing.context.id in
         let receipt, _ =
           A.revoke_script_ingress
             t.actor
             ~owner
             ~source:(source t)
             ~id:(registration t).context.id
             ~reason:"actual current A revokes ingress"
           |> protocol_ok
         in
         A.select_ingress_mutations
           t.actor
           ~owner
           ~source:(source t)
           ~receipts:[ receipt ]
         |> protocol_ok;
         commit ~snapshot:(snapshot t) ~requests)
    |> protocol_ok
    |> fun committed -> assert committed)
;;

let assert_native_none t count =
  [%test_eq: int] count !(t.effects);
  [%test_eq: int] count (List.length !(t.native));
  List.iter !(t.native) ~f:(function
    | No_principal -> ()
    | Principal _ -> failwith "no-principal creator borrowed ambient authentication");
  assert (Operator_authorization.is_current t.a)
;;

module Observation_expectation = struct
  type t =
    | Admitted_none
    | Unavailable
end

(* Native None alone is compatible with historical execution. This public strict
   continuation borrow requires the actual retained admission after native ACK. *)
let observe_original t expectation =
  let native =
    List.filter (state t).invocations ~f:(fun invocation ->
      String.equal invocation.P.Invocation.context.tool_name "read_file")
    |> function
    | [ native ] -> native
    | [] | _ :: _ :: _ -> failwith "expected one actual native result to observe"
  in
  let parent = Option.value_exn native.parent_event in
  let observed = ref None in
  let result =
    S.Moderator_observation.drain_idle_with_tools
      ~max_observations:1
      ~script_tools:t.tools
      ~definition:t.definition
      ~claim:(fun handle ->
        A.with_idle_moderator_observation_tools
          t.actor
          ~observer:(source t)
          (fun ~observing ~capabilities ~execute ~commit ->
             observed
             := Some (observing, capabilities.borrow_agent_actor ~invocation:observing);
             handle ~observing ~capabilities ~execute ~commit))
      ~manager:t.manager
      ~history:(fun () -> history t)
      ~available_tools:[]
      ~session_meta:`Null
      ~now:(fun () -> timestamp)
      ()
    |> protocol_ok
  in
  [%test_eq: int] 1 (List.length result.outcomes);
  let observing, borrowed = Option.value_exn !observed in
  assert (P.Id.Invocation.equal observing.context.id native.context.id);
  assert (Option.equal P.Id.Moderator_execution.equal (Some parent) observing.parent_event);
  let observation = Option.value_exn observing.observation in
  assert (P.Invocation.equal_observer observation.observer (source t));
  (match expectation, borrowed with
   | Observation_expectation.Admitted_none, Ok None -> ()
   | Unavailable, Error failure -> [%test_eq: P.Error.code] Permission_denied failure.code
   | Admitted_none, (Ok (Some _) | Error _) | Unavailable, Ok _ ->
     failwith "actual observation changed creator admission availability");
  let completed =
    List.find_exn (state t).invocations ~f:(fun invocation ->
      P.Id.Invocation.equal invocation.context.id native.context.id)
  in
  (match completed.observation with
   | Some { status = Observed; _ } -> ()
   | None | Some { status = Awaiting | Observing | Observation_failed _; _ } ->
     failwith "actual observation did not finish");
  assert_backend t
;;
