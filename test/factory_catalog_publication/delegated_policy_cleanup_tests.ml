open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Direct = Descendant_turn_adapter_tests
module Tree = Descendant_ancestry_support
module Goal = Goal_service_tests
module Model = Delegated_policy_model_tests
module Failure = Delegated_policy_caller_support.Failure
module G = P.Goal
module E = P.Moderator_execution
module L = A.Inference_ledger
module PC = A.Policy_canonical_usage
module Cause = A.Policy_cause_data
module Book = A.Goal_policy_contributions
module D = Agent_store.Delegation_store
module Origin = Agent_store.Delegation_goal_origin

let model_marker = "597_TWO_ROOT_POLICY_MODEL"
let recipe = "597-two-root-model.chatmd"

(* The actual forwarded provider is held across the two bounded correction
   sweeps. Keep that controlled hold inside both nested policy budgets. *)
let root_prompt =
  {|<developer>597 actual upper policy owner Q.</developer>
<tool name="policy_recipe" agent="597-two-root-model.chatmd" local/>
<script id="upper-policy" language="chatml" kind="moderator" api="extensibility-v1" wall_time="60s">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(p) ->
  if (if p.name == "read_file" then state == 0 else false) then
    let* result = Model.call("agent_prompt_v1", `Object([
      {key = "prompt"; value = `String("597-two-root-model.chatmd")},
      {key = "input"; value = `String("597_TWO_ROOT_POLICY_MODEL")},
      {key = "is_local"; value = `Bool(true)}
    ])) in
    (match result with
    | `Ok(_) -> Task.pure(state + 1)
    | `Refused(message) -> Task.fail(message)
    | `Error(message) -> Task.fail(message))
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let intermediate_prompt =
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>597 actual intermediate P creates C under its own Goal.</developer>
<tool name="agent_create" type="inherited"/>
<tool name="read_file" type="inherited"/>
<script id="middle-policy" language="chatml" kind="moderator" api="extensibility-v1" wall_time="60s">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(p) ->
  if (if p.name == "read_file" then state == 0 else false) then
    let* result = Tool.call("read_file", `Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String("597-policy.txt")}
    ])) in
    (match result with
    | `Ok(_) -> Task.pure(state + 1)
    | `Error(code) -> Task.fail(code))
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

module Scenario = struct
  type t =
    | Cleanup
    | Completed_vector
end

module Warm_vector = struct
  let original = "593-original-private.txt"
  let middle = "593-middle-approved.txt"
  let final = "593-root-approved.txt"
  let rejected = "593-rejected-private.txt"
  let private_reason = "593 private upper policy rejection"
  let files = [ original; middle; final; rejected ]
  let args file = `Object [ "root", `String "data"; "file", `String file ]

  (* The controlled nested provider wait and real Goal/store acknowledgements
     share these script deadlines. Per-stage fixture waits remain bounded. *)
  let upper_prompt =
    {|<developer>593 actual upper vector policy R.</developer>
<tool name="policy_recipe" agent="597-two-root-model.chatmd" local/>
<script id="upper-policy" language="chatml" kind="moderator" api="extensibility-v1" wall_time="60s">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(p) ->
  if p.name == "read_file" then
    if state == 0 then
      let* result = Model.call("agent_prompt_v1", `Object([
        {key = "prompt"; value = `String("597-two-root-model.chatmd")},
        {key = "input"; value = `String("597_TWO_ROOT_POLICY_MODEL")},
        {key = "is_local"; value = `Bool(true)}
      ])) in
      (match result with
      | `Ok(_) -> Task.pure(state + 1)
      | `Refused(message) -> Task.fail(message)
      | `Error(message) -> Task.fail(message))
    else
      let* ignored = (if state == 1 then
        Tool.rewrite_args(`Object([
          {key = "root"; value = `String("data")},
          {key = "file"; value = `String("593-root-approved.txt")}
        ]))
        else Tool.reject("593 private upper policy rejection")) in
      Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
  ;;

  let middle_prompt =
    {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>593 actual middle vector policy M creates C under its Goal.</developer>
<tool name="agent_create" type="inherited"/>
<tool name="read_file" type="inherited"/>
<script id="middle-policy" language="chatml" kind="moderator" api="extensibility-v1" wall_time="60s">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(p) ->
  if p.name == "read_file" then
    let* checked = (if state == 0 then
      let* result = Tool.call("read_file", `Object([
        {key = "root"; value = `String("data")},
        {key = "file"; value = `String("597-policy.txt")}
      ])) in
      (match result with
      | `Ok(_) -> Task.pure(())
      | `Error(code) -> Task.fail(code))
      else Task.pure(())) in
    let* ignored = Tool.rewrite_args(`Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String("593-middle-approved.txt")}
    ])) in
    Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
  ;;

  let response ~id file =
    Delegated_policy_caller_support.Provider.function_response
      { name = "read_file"; file; id }
  ;;

  (* Expected event DATA use the public event codec; these values issue no call. *)
  let event ~id file =
    let call : Chat_response.Moderation.Tool_call.t =
      { id
      ; name = "read_file"
      ; args = args file
      ; kind = Function
      ; payload_text = Jsonaf.to_string (args file)
      ; meta = `Null
      }
    in
    let captured =
      Chat_response.Moderation.Event.to_value (Pre_tool_call call)
      |> Session.Snapshot.of_value
      |> Result.ok_or_failwith
    in
    `Object [ "snapshot", Session.Snapshot.to_jsonaf captured ]
  ;;
end

let within env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f with
  | Eio.Time.Timeout -> failwith ("597 two-root cleanup timed out: " ^ phase)
;;

(* Capture the committed-event pulse before the authoritative state read. A
   wakeup only asks us to recheck the same predicate; it is not completion proof.
   Operation terminal events and receipt status updates both publish this pulse. *)
let await_leaf_quiet (entry : Registry.entry) =
  let rec loop () =
    let changed = A.Durable_event_log.changed entry.durable_events in
    let current = state entry in
    if
      Option.is_none current.active_operation
      && List.for_all current.moderator_executions ~f:(fun execution ->
        match execution.P.Moderator_execution.status with
        | Completed _ | Failed _ | Interrupted _ -> true
        | Running -> false)
    then current
    else (
      Eio.Promise.await changed;
      Eio.Fiber.yield ();
      loop ())
  in
  loop ()
;;

let session daemon entry =
  P.Session_ref.create
    ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
    ~session_id:(state entry).identity.session_id
;;

let activate daemon entry ~key ~text =
  let owner, before = Goal.create_goal daemon entry in
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Workflow
      ~input:(User_submission { kind = Plain_text; text; attachments = [] })
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
    |> fun request ->
    A.Goal_activation_request.create
      ~goal_id:Goal.goal_id
      ~expected_goal_revision:(G.revision (Goal.goal before))
      ~expected_catalog_revision:
        (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
      ~request
    |> protocol_ok
  in
  Agent_server.Goal_run_service.create
    ~factory:(Daemon.factory daemon)
    ~authorize:(fun _ -> authorize)
  |> fun service ->
  Agent_server.Goal_run_service.start_episode
    service
    ~entry
    ~actor:original
    ~session:owner
    ~request
  |> protocol_ok
  |> Goal.activated
;;

let book_json current =
  Option.value_exn current.A.Session_state.goal_policy_contributions
  |> Book.to_document
  |> protocol_ok
  |> Document_schema.Document.json
;;

let ledger_json current =
  L.to_document current.A.Session_state.inference_ledger
  |> Result.map_error ~f:(fun failure -> Sexp.to_string_hum (L.Error.sexp_of_t failure))
  |> Result.ok_or_failwith
  |> Document_schema.Document.json
;;

let assert_same_head before after =
  let before = Model.pending_projection before in
  let after = Model.pending_projection after in
  assert (PC.Stream.equal (PC.Projection.stream before) (PC.Projection.stream after));
  [%test_eq: int64] 1L (PC.Projection.admitted_attempts before);
  [%test_eq: int64] 1L (PC.Projection.admitted_attempts after);
  assert (
    Jsonaf.exactly_equal
      (Jsonaf.member_exn "head" (PC.Projection.to_json before))
      (Jsonaf.member_exn "head" (PC.Projection.to_json after)))
;;

let assert_input current expected =
  let subtotal = Model.subtotal current in
  [%test_eq: int64] 1L (A.Goal_usage.attempts (Book.Subtotal.usage subtotal));
  let actual =
    A.Goal_usage.component (Book.Subtotal.usage subtotal) Input
    |> A.Goal_usage.Metric.actual
  in
  assert (P.Inference_query.Metric.equal_sum (Tokens expected) actual)
;;

module Root_read = struct
  type t =
    { mutable selected : (string * (unit -> unit)) option
    ; mutable hits : int
    ; failure : Failure.t
    }

  let create () = { selected = None; hits = 0; failure = Failure.create () }

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_stat
      ~matches:(fun path ->
        Option.exists t.selected ~f:(fun (selected, _) -> String.equal selected path))
      ~reached:(fun _ ->
        let _, run = Option.value_exn t.selected in
        (* Clear before the real metadata RPC, whose own Store reads must run. *)
        t.selected <- None;
        t.hits <- Int.succ t.hits;
        Failure.guard t.failure run)
  ;;
end

module Provider_phase = struct
  type t =
    | Upper_finished
    | Create_leaf
    | Middle_finished
    | Leaf
end

type tree =
  { upper : Registry.entry
  ; middle : Registry.entry
  ; leaf : Registry.entry
  ; counters : counters
  ; released : unit Eio.Promise.t
  ; provider_exited : unit Eio.Promise.t
  ; model_calls : int ref
  ; before_upper : A.Session_state.t
  ; release : unit -> unit
  ; join_forwarding_callback : unit -> unit
  ; await_next_candidate : unit -> A.Session_state.t
  ; continue_candidate : unit -> unit
  ; file_reads : int String.Table.t
  }

let with_tree ?(scenario = Scenario.Cleanup) f =
  let read = Root_read.create () in
  let failure = Failure.create () in
  let phase = ref Provider_phase.Upper_finished in
  let leaf_emitted = ref false in
  let model_calls = ref 0 in
  let candidate = ref 0 in
  let candidate_leaf = ref None in
  let file_reads = String.Table.create () in
  let next_candidate, enter_candidate = Eio.Promise.create () in
  let candidate_released, release_candidate = Eio.Promise.create () in
  let continue_candidate () =
    if Option.is_none (Eio.Promise.peek candidate_released)
    then Eio.Promise.resolve release_candidate ()
  in
  let entered, enter = Eio.Promise.create () in
  let released, release_u = Eio.Promise.create () in
  let provider_exited, exit_provider = Eio.Promise.create () in
  let release () =
    if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release_u ()
  in
  let selected_root, selected_middle, extra_sources =
    match scenario with
    | Cleanup -> root_prompt, intermediate_prompt, []
    | Completed_vector ->
      ( Warm_vector.upper_prompt
      , Warm_vector.middle_prompt
      , [ Warm_vector.original, "593 original must never be disclosed"
        ; Warm_vector.middle, "593 intermediate must never be disclosed"
        ; Warm_vector.final, "593 final root-approved sentinel"
        ; Warm_vector.rejected, "593 rejected must never be disclosed"
        ] )
  in
  let provider inputs =
    Failure.guard failure (fun () ->
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      if String.is_substring transcript ~substring:model_marker
      then (
        Int.incr model_calls;
        if !model_calls <> 1 then failwith "two-root cleanup replayed the policy Model";
        Eio.Promise.resolve enter ();
        Exn.protect
          ~finally:(fun () -> Eio.Promise.resolve exit_provider ())
          ~f:(fun () ->
            Eio.Promise.await released;
            completed_response "597 one actual two-root policy Model completed"))
      else (
        match !phase with
        | Upper_finished ->
          phase := Create_leaf;
          completed_response "597 actual upper Goal finished middle creation"
        | Create_leaf ->
          phase := Middle_finished;
          Birth.create_response
            ~key:"597-two-root-leaf"
            ~call_id:"597-two-root-leaf"
            ~tools:[ "read_file" ]
            [ "child.chatmd", Model.child_prompt ]
        | Middle_finished ->
          phase := Leaf;
          completed_response "597 actual middle Goal finished leaf creation"
        | Leaf
          when match scenario with
               | Cleanup -> false
               | Completed_vector -> true ->
          (match !candidate with
           | 0 ->
             Int.incr candidate;
             Warm_vector.response ~id:"593-vector-first" Warm_vector.original
           | 1 ->
             Int.incr candidate;
             (* Capture the leaf before the second provider returns its response.
                The previous invocation must already be Published. *)
             let leaf = Option.value_exn !candidate_leaf in
             Eio.Promise.resolve enter_candidate (state leaf);
             Eio.Promise.await candidate_released;
             Warm_vector.response ~id:"593-vector-second" Warm_vector.rejected
           | _ -> completed_response "593 actual vector leaf completed both candidates")
        | Leaf ->
          if !leaf_emitted
          then completed_response "597 actual leaf finished its inherited read"
          else (
            leaf_emitted := true;
            Direct.function_response
              "read_file"
              (`Object [ "root", `String "data"; "file", `String "597-effect.txt" ]))))
  in
  Birth.with_native_goal_child
  (* The enclosing watchdog covers native birth, the controlled correction
       hold and settlement. Their individual deadlines identify a stalled phase. *)
    ~timeout:180.
    ~entry_cleanup:
      (match scenario with
       | Cleanup -> Source.direct_cleanup ()
       | Completed_vector ->
         (* The warm assertions finish with the runtime tree live. Let the Daemon
            join its schedulers and dispose the registered entries together. *)
         Source.Daemon_shutdown)
    ~wrap_env:(fun env ->
      let env = Root_read.wrap read env in
      match scenario with
      | Cleanup -> env
      | Completed_vector ->
        Crash_fault_io.wrap
          env
          ~boundary:Before_open_in
          ~matches:(fun path ->
            List.exists Warm_vector.files ~f:(fun file ->
              String.is_suffix path ~suffix:("/" ^ file)))
          ~reached:(fun path ->
            let file =
              List.find_exn Warm_vector.files ~f:(fun file ->
                String.is_suffix path ~suffix:("/" ^ file))
            in
            Hashtbl.update file_reads file ~f:(fun count ->
              Int.succ (Option.value count ~default:0))))
    ~parent_prompt_text:selected_root
    ~sources:
      ([ recipe, "<developer>597 actual two-root policy recipe.</developer>"
       ; "597-policy.txt", "597 actual middle policy read"
       ; "597-effect.txt", "597 actual leaf native read"
       ]
       @ extra_sources)
    ~inherited_tools:
      [ "agent_create", ""; "read_file", Descendant_goal_resume_tests.parent_reader ]
    ~child_sources:[ "child.chatmd", selected_middle ]
    ~post_creation_stream:provider
    (fun env _sw daemon upper middle _scope _reference counters ->
       protect_cleanup
         ~finally:(fun () ->
           read.selected <- None;
           release ();
           continue_candidate ())
         (fun () ->
            Failure.restore failure (fun () ->
              within env "install and start actual middle source" (fun () ->
                Direct.start_child daemon middle);
              within env "actual middle Goal births leaf" (fun () ->
                ignore
                  (activate
                     daemon
                     middle
                     ~key:"597-two-root-middle-goal"
                     ~text:"Create the genuine leaf under this Goal."
                   : P.Run_receipt.t));
              let value =
                within env "join actual leaf publication and middle operation" (fun () ->
                  let rec await () =
                    Failure.raise_if_present failure;
                    match Tree.published middle "597-two-root-leaf" with
                    | Some value when Option.is_none (state middle).active_operation ->
                      value
                    | Some _ | None ->
                      Eio.Fiber.yield ();
                      await ()
                  in
                  await ())
              in
              let id =
                Jsonaf.member_exn "session_id" value
                |> Jsonaf.string_exn
                |> P.Id.Session.of_string
                |> protocol_ok
              in
              let leaf = Registry.find (Daemon.registry daemon) id |> Option.value_exn in
              candidate_leaf := Some leaf;
              let born = state leaf in
              let reference = Option.value_exn born.spec.delegation in
              let actual =
                D.resolve
                  (Agent_store.Session_store.delegations (Daemon.store daemon))
                  reference
                |> Existing.store_ok
              in
              assert (
                P.Id.Session.equal
                  actual.key.parent_session_id
                  (state middle).identity.session_id);
              let origin = Option.value_exn actual.admission.goal_origin in
              let scopes = Origin.scopes origin in
              [%test_eq: int] 2 (List.length scopes);
              List.iter [ upper; middle ] ~f:(fun root ->
                assert (
                  List.exists scopes ~f:(fun scope ->
                    P.Session_ref.equal (session daemon root) (Origin.Scope.owner scope))));
              within env "install and start actual leaf source" (fun () ->
                Direct.start_child daemon leaf);
              let before_upper = Model.checkpoint upper in
              let before_middle = Model.checkpoint middle in
              let join_forwarding_callback () =
                within env "join actual forwarding callback after nested Model" (fun () ->
                  Eio.Promise.await provider_exited;
                  Model.checkpoint middle |> ignore)
              in
              let rec await_next_candidate () =
                let changed = A.Durable_event_log.changed leaf.durable_events in
                Failure.raise_if_present failure;
                let current = state leaf in
                match Eio.Promise.peek next_candidate with
                | Some at_provider -> at_provider
                | None when Option.is_none current.active_operation ->
                  raise_s
                    [%sexp
                      "actual leaf Run ended before the second provider candidate"
                    , (current.run_state : A.Run_state.t option)]
                | None ->
                  Eio.Fiber.any
                    [ (fun () -> ignore (Eio.Promise.await next_candidate))
                    ; (fun () -> Eio.Promise.await changed)
                    ; (fun () -> Failure.await failure)
                    ];
                  await_next_candidate ()
              in
              protect_cleanup
                ~finally:(fun () ->
                  read.selected <- None;
                  release ();
                  continue_candidate ();
                  if Eio.Promise.is_resolved entered then join_forwarding_callback ();
                  within env "join actual leaf after policy provider release" (fun () ->
                    await_leaf_quiet leaf |> ignore);
                  within env "join actual middle policy checkpoint" (fun () ->
                    Model.checkpoint middle |> ignore);
                  within env "join actual upper policy checkpoint" (fun () ->
                    Model.checkpoint upper |> ignore))
                (fun () ->
                   ignore
                     (within env "activate actual leaf Goal" (fun () ->
                        activate
                          daemon
                          leaf
                          ~key:"597-two-root-leaf-goal"
                          ~text:"597 actual inherited two-root read")
                      : P.Run_receipt.t);
                   within env "actual forwarded Model provider entry" (fun () ->
                     let roots = [ upper, before_upper; middle, before_middle ] in
                     let rec await () =
                       (* Receipt changes publish Session_updated. Subscribe before
                          inspecting each Actor, so publication during a state read
                          can wake this waiter too. *)
                       let changed =
                         List.map roots ~f:(fun (entry, _) ->
                           A.Durable_event_log.changed entry.Registry.durable_events)
                       in
                       Failure.raise_if_present failure;
                       match Eio.Promise.peek entered with
                       | Some () -> ()
                       | None ->
                         List.iter roots ~f:(fun (entry, (before : A.Session_state.t)) ->
                           List.iter (state entry).moderator_executions ~f:(fun receipt ->
                             let is_new =
                               not
                                 (List.exists before.moderator_executions ~f:(fun old ->
                                    P.Id.Moderator_execution.equal
                                      old.E.context.id
                                      receipt.E.context.id))
                             in
                             if is_new
                             then (
                               match receipt.status with
                               | Failed _ | Interrupted _ ->
                                 raise_s
                                   [%sexp
                                     "actual forwarding callback ended before provider"
                                   , (receipt.context.id : P.Id.Moderator_execution.t)
                                   , (receipt.status : E.status)]
                               | Running | Completed _ -> ())));
                         Eio.Fiber.any
                           ((fun () -> Failure.await failure)
                            :: List.map (entered :: changed) ~f:(fun changed () ->
                              Eio.Promise.await changed));
                         await ()
                     in
                     await ());
                   let tree =
                     { upper
                     ; middle
                     ; leaf
                     ; counters
                     ; released
                     ; provider_exited
                     ; model_calls
                     ; before_upper
                     ; release
                     ; join_forwarding_callback
                     ; await_next_candidate
                     ; continue_candidate
                     ; file_reads
                     }
                   in
                   f env daemon tree read;
                   Failure.raise_if_present read.failure))))
;;

let%expect_test "two real policy roots progress independently after the first root fails" =
  with_tree (fun env daemon tree read ->
    let upper = state tree.upper in
    let row = Model.new_rows tree.before_upper upper |> Model.only "forwarded policy H" in
    let handle = L.Row.handle row in
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Running -> ()
     | Prepared | Terminal _ | Interrupted _ ->
       failwith "two-root provider preceded actual Running H");
    let accounting =
      Result.of_option
        upper.policy_canonical_usage
        ~error:"forwarded provider preceded actual causal canonical ACK"
      |> Result.ok_or_failwith
    in
    let binding =
      PC.bindings accounting
      |> List.filter ~f:(fun binding ->
        Int64.equal (L.Handle.ordinal handle) (PC.Binding.ordinal binding))
      |> Model.only "same actual forwarded H binding"
    in
    let cause = Model.cause_for accounting binding in
    assert (P.Session_ref.equal (session daemon tree.middle) (Cause.caller cause));
    assert (P.Session_ref.equal (session daemon tree.upper) (Cause.callee cause));
    let forwarding = Option.value_exn (Cause.forwarding cause) in
    assert (
      List.exists (Cause.Forwarding.frames forwarding) ~f:(fun frame ->
        let seed = Cause.Forwarding.Frame.seed frame in
        P.Session_ref.equal (session daemon tree.leaf) (Cause.Seed.caller seed)
        && P.Session_ref.equal (session daemon tree.middle) (Cause.Seed.callee seed)));
    assert (
      List.equal
        D.Reference.equal
        [ Option.value_exn (state tree.middle).spec.delegation ]
        (Cause.path cause));
    (* Observe the public canonical target order used by Set.freeze. These DATA
       identify the fault leg only; no test constructor creates a route/receipt. *)
    let remote =
      PC.Binding.choices binding
      |> List.filter_map ~f:(fun choice ->
        match PC.Choice.coverage choice with
        | Covered_local | Covered_physical -> None
        | Causal_overlay ->
          let target = PC.Choice.target choice in
          if
            P.Session_ref.equal
              (Cause.Goal_target.owner target)
              (session daemon tree.upper)
          then None
          else Some target)
      |> List.sort ~compare:(fun a b ->
        String.compare (Cause.Goal_target.root a) (Cause.Goal_target.root b))
    in
    let first, second =
      match remote with
      | [ first; second ] -> first, second
      | _ -> failwith "actual forwarding did not create two remote root requests"
    in
    let entry target =
      List.filter [ tree.middle; tree.leaf ] ~f:(fun entry ->
        P.Session_ref.equal (session daemon entry) (Cause.Goal_target.owner target))
      |> Model.only "actual remote target Actor"
    in
    let first = entry first in
    let second = entry second in
    assert (not (phys_equal first.actor second.actor));
    let before_first = state first in
    let before_second = state second in
    List.iter [ before_first; before_second ] ~f:(fun current ->
      let projected = Model.pending_projection current in
      [%test_eq: int64] 1L (PC.Projection.admitted_attempts projected));
    (* Actual provider row correction; actual registered graph owns propagation. *)
    let revision =
      Descendant_closed_goal_correction_tests.next_usage_revision tree.upper handle
    in
    Actor.observe_inference
      tree.upper.actor
      ~handle
      (Child_usage_fault_tests.correction handle revision 7L)
    |> protocol_ok;
    let corrected_source = state tree.upper in
    let providers = !(tree.counters.providers) in
    [%test_eq: int] 1 !(tree.model_calls);
    let writer, _ =
      Actor.attach first.actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Actor.detach first.actor writer.id |> protocol_ok)
      (fun () ->
         let metadata_revision = (state first).identity.metadata_revision in
         let handle = Option.value_exn first.store_handle in
         read.selected
         <- Some
              ( Filename.concat
                  (Agent_store.Session_store.Handle.directory handle)
                  "ARCHIVED"
              , fun () ->
                  let patch =
                    P.Session_metadata.Patch.create
                      ~name:(Set "597 first-root capture changed during real Store read")
                      ~set_labels:[]
                      ~remove_labels:[]
                    |> protocol_ok
                  in
                  within env "actual first-root metadata ACK during Store read" (fun () ->
                    Actor.update_metadata
                      first.actor
                      ~attachment_id:writer.id
                      ~expected_metadata_revision:metadata_revision
                      ~patch
                      ()
                    |> protocol_ok
                    |> ignore) );
         let rejected =
           match
             within env "independent two-root correction sweep" (fun () ->
               Actor.drain_inference_usage tree.upper.actor)
           with
           | Error failure -> failure
           | Ok () -> failwith "changed first root capture was reported as settled"
         in
         Failure.raise_if_present read.failure;
         [%test_eq: int] 1 read.hits;
         assert (Option.is_none read.selected);
         [%test_eq: P.Error.code] Conflict rejected.code;
         [%test_eq: string]
           "causal factual Actor/controller/State observation changed"
           rejected.message;
         let failed_first = state first in
         let progressed_second = state second in
         [%test_eq: int64]
           Int64.(metadata_revision + 1L)
           failed_first.identity.metadata_revision;
         assert (Jsonaf.exactly_equal (book_json before_first) (book_json failed_first));
         assert (
           not
             (Jsonaf.exactly_equal
                (book_json before_second)
                (book_json progressed_second)));
         assert_input progressed_second 7L;
         assert_same_head before_second progressed_second;
         assert (Actor.inference_usage_cleanup_pending tree.upper.actor);
         List.iter
           [ first, before_first; second, before_second; tree.upper, corrected_source ]
           ~f:(fun (entry, before) ->
             assert (Jsonaf.exactly_equal (ledger_json before) (ledger_json (state entry))));
         [%test_eq: int] providers !(tree.counters.providers);
         [%test_eq: int] 1 !(tree.model_calls);
         assert (Option.is_none (Eio.Promise.peek tree.released));
         assert (not (Eio.Promise.is_resolved tree.provider_exited));
         (* No re-admission/re-observation authority is constructed: the SAME
           registered duty retries its exact retained source observation/vector. *)
         within env "retry same retained two-root correction vector" (fun () ->
           Actor.drain_inference_usage tree.upper.actor |> protocol_ok);
         let recovered_first = state first in
         let repeated_second = state second in
         assert_input recovered_first 7L;
         assert_same_head before_first recovered_first;
         assert (
           Jsonaf.exactly_equal (book_json progressed_second) (book_json repeated_second));
         assert (not (Actor.inference_usage_cleanup_pending tree.upper.actor));
         [%test_eq: int] providers !(tree.counters.providers);
         [%test_eq: int] 1 !(tree.model_calls);
         assert (not (Eio.Promise.is_resolved tree.provider_exited));
         List.iter
           [ first, before_first; second, before_second; tree.upper, corrected_source ]
           ~f:(fun (entry, before) ->
             assert (Jsonaf.exactly_equal (ledger_json before) (ledger_json (state entry))))));
  print_endline
    "first real root Conflict retained; second corrected before retry; same H and \
     provider";
  [%expect
    {| first real root Conflict retained; second corrected before retry; same H and provider |}]
;;

let%expect_test "warm Owned vector commits two rewrites before a later root rejection" =
  with_tree ~scenario:Completed_vector (fun env daemon tree _read ->
    let invocation (current : A.Session_state.t) provider_id =
      List.filter current.invocations ~f:(fun invocation ->
        Option.equal
          String.equal
          invocation.P.Invocation.context.provider_call_id
          (Some provider_id))
      |> Model.only "actual leaf provider invocation"
    in
    let receipt (current : A.Session_state.t) ~child ~invocation ~reference =
      List.filter current.moderator_executions ~f:(fun receipt ->
        E.equal_phase receipt.E.context.phase Pre_tool_call
        && Option.exists receipt.delegation ~f:(fun delegated ->
          P.Id.Session.equal delegated.child_session_id child
          && P.Id.Invocation.equal delegated.child_invocation_id invocation))
      |> Model.only "exact vector member/candidate receipt"
      |> fun (receipt : E.t) ->
      let delegated = Option.value_exn receipt.delegation in
      [%test_eq: string] reference.D.Reference.admission_sha256 delegated.admission_sha256;
      (match receipt.status with
       | Completed _ -> ()
       | Running | Failed _ | Interrupted _ ->
         raise_s [%sexp "vector callback did not commit", (receipt : E.t)]);
      receipt
    in
    let checkpoint (before : A.Session_state.t) (after : A.Session_state.t) count =
      let decode current =
        A.Moderator_checkpoint.decode current.A.Session_state.moderator
        |> protocol_ok
        |> Option.value_exn
      in
      let before = decode before in
      let after = decode after in
      [%test_eq: string] before.script_id after.script_id;
      [%test_eq: string] before.script_source_hash after.script_source_hash;
      assert (not after.halted);
      match after.current_state with
      | Int value -> [%test_eq: int] count value
      | _ -> failwith "warm vector policy checkpoint lost its integer state"
    in
    let unchanged_receipt (current : A.Session_state.t) (saved : E.t) =
      let actual =
        List.find_exn current.moderator_executions ~f:(fun receipt ->
          P.Id.Moderator_execution.equal saved.context.id receipt.E.context.id)
      in
      assert (E.equal saved actual)
    in
    let installed (current : A.Session_state.t) =
      A.Run_state.installation (Option.value_exn current.run_state)
      |> A.Run_source_installation.captured ~generation:current.identity.generation
      |> protocol_ok
    in
    let cause_for_event (current : A.Session_state.t) (receipt : E.t) =
      Option.value_exn current.policy_canonical_usage
      |> PC.to_document
      |> protocol_ok
      |> Document_schema.Document.payload
      |> Jsonaf.member_exn "causes"
      |> Jsonaf.list_exn
      |> List.map ~f:(fun raw -> Cause.of_json raw |> protocol_ok)
      |> List.filter ~f:(fun cause ->
        P.Id.Moderator_execution.equal receipt.context.id (Cause.parent_event cause))
      |> Model.only "actual registered vector callback Cause"
    in
    let check_direct_cause current ~entry ~caller ~invocation ~path receipt =
      let cause = cause_for_event current receipt in
      assert (P.Session_ref.equal (session daemon entry) (Cause.callee cause));
      assert (P.Session_ref.equal (session daemon tree.leaf) (Cause.caller cause));
      assert (P.Id.Invocation.equal invocation (Cause.child_invocation cause));
      assert (List.equal D.Reference.equal path (Cause.path cause));
      [%test_eq: string]
        (List.hd_exn path).D.Reference.admission_sha256
        (Cause.admission_sha256 cause);
      assert (P.Run_source.equal (installed current) (Cause.callee_source cause));
      assert (
        Cause.Caller_source.matches_installed
          (Cause.caller_source cause)
          (installed caller));
      assert (Cause.matches_event cause receipt);
      cause
    in
    let file_reads file = Hashtbl.find tree.file_reads file |> Option.value ~default:0 in
    let assert_no_disclosure () =
      List.iter
        [ Warm_vector.original; Warm_vector.middle; Warm_vector.rejected ]
        ~f:(fun file -> [%test_eq: int] 0 (file_reads file))
    in
    (* The actual nested Model is still waiting in its provider. Its admitted H
       and copied Goal DATA predate either direct root callback for C. *)
    let upper_at_model = state tree.upper in
    let middle_at_model = state tree.middle in
    let leaf_at_model = state tree.leaf in
    (* Model-owned preparation uses the actual reserved canonical call before
       Stream_invocation commits C's durable invocation. The admitted M receipt
       already records that exact candidate; join it to C's eventual Published
       invocation below instead of assuming C's DTO exists during this wait. *)
    (* Host policy events name the canonical invocation ID, whereas the later
       durable invocation separately retains its provider_call_id. Correlate
       both actual identities, rather than substituting the provider string. *)
    let middle_running =
      List.filter middle_at_model.moderator_executions ~f:(fun receipt ->
        E.equal_phase receipt.E.context.phase Pre_tool_call
        && (match receipt.status with
            | Running -> true
            | Completed _ | Failed _ | Interrupted _ -> false)
        && Option.exists receipt.delegation ~f:(fun delegated ->
          P.Id.Session.equal delegated.child_session_id leaf_at_model.identity.session_id
          && Int.equal delegated.child_generation leaf_at_model.identity.generation
          && Jsonaf.exactly_equal
               (Warm_vector.event
                  ~id:(P.Id.Invocation.to_string delegated.child_invocation_id)
                  Warm_vector.original)
               receipt.context.event))
      |> Model.only "actual Running middle callback for reserved leaf candidate"
    in
    let first_id = (Option.value_exn middle_running.delegation).child_invocation_id in
    let row =
      Model.new_rows tree.before_upper upper_at_model |> Model.only "nested Model H"
    in
    let handle = L.Row.handle row in
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Running -> ()
     | Prepared | Terminal _ | Interrupted _ -> failwith "nested Model H is not Running");
    let accounting = Option.value_exn upper_at_model.policy_canonical_usage in
    let binding =
      PC.bindings accounting
      |> List.filter ~f:(fun binding ->
        Int64.equal (L.Handle.ordinal handle) (PC.Binding.ordinal binding))
      |> Model.only "same actual nested Model canonical binding"
    in
    let cause = Model.cause_for accounting binding in
    assert (P.Session_ref.equal (session daemon tree.middle) (Cause.caller cause));
    assert (P.Session_ref.equal (session daemon tree.upper) (Cause.callee cause));
    let leaf_reference = Option.value_exn leaf_at_model.spec.delegation in
    let middle_reference = Option.value_exn middle_at_model.spec.delegation in
    assert (List.equal D.Reference.equal [ middle_reference ] (Cause.path cause));
    let forwarding = Option.value_exn (Cause.forwarding cause) in
    assert (
      List.exists (Cause.Forwarding.frames forwarding) ~f:(fun frame ->
        let seed = Cause.Forwarding.Frame.seed frame in
        P.Session_ref.equal (session daemon tree.leaf) (Cause.Seed.caller seed)
        && P.Session_ref.equal (session daemon tree.middle) (Cause.Seed.callee seed)));
    List.iter [ tree.middle; tree.leaf ] ~f:(fun entry ->
      assert (
        List.exists (PC.Binding.choices binding) ~f:(fun choice ->
          match PC.Choice.coverage choice with
          | Covered_local | Covered_physical -> false
          | Causal_overlay ->
            P.Session_ref.equal
              (session daemon entry)
              (Cause.Goal_target.owner (PC.Choice.target choice)))));
    [%test_eq: int] 0 (file_reads Warm_vector.final);
    tree.release ();
    tree.join_forwarding_callback ();
    let leaf_first =
      within
        env
        "first candidate published before the second provider candidate"
        tree.await_next_candidate
    in
    let middle_first = Model.checkpoint tree.middle in
    let upper_first = Model.checkpoint tree.upper in
    let first = invocation leaf_first "593-vector-first" in
    assert (P.Id.Invocation.equal first_id first.context.id);
    (match first.status with
     | Published (Complete (`String text)) ->
       assert (String.is_substring text ~substring:"593 final root-approved sentinel");
       assert (not (String.is_substring text ~substring:"must never be disclosed"))
     | status ->
       let policy_facts (current : A.Session_state.t) =
         List.filter current.moderator_executions ~f:(fun receipt ->
           E.equal_phase receipt.E.context.phase Pre_tool_call)
         |> List.map ~f:(fun receipt ->
           receipt.E.context.id, receipt.status, receipt.decision, receipt.delegation)
       in
       let model_facts (current : A.Session_state.t) =
         List.filter current.jobs ~f:(fun job ->
           P.Job.equal_kind job.P.Job.kind Model_call)
         |> List.map ~f:(fun job -> job.P.Job.id, job.status)
       in
       raise_s
         [%sexp
           "first vector output was not published"
         , (status : P.Invocation.status)
         , { middle_pre_tool =
               (policy_facts middle_first
                : (P.Id.Moderator_execution.t
                  * E.status
                  * E.Decision.t option
                  * E.delegation option)
                    list)
           ; upper_pre_tool =
               (policy_facts upper_first
                : (P.Id.Moderator_execution.t
                  * E.status
                  * E.Decision.t option
                  * E.delegation option)
                    list)
           ; middle_model = (model_facts middle_first : (P.Id.Job.t * P.Job.status) list)
           ; upper_model = (model_facts upper_first : (P.Id.Job.t * P.Job.status) list)
           }]);
    let first_reads = file_reads Warm_vector.final in
    assert (first_reads > 0);
    assert_no_disclosure ();
    let middle_direct =
      receipt
        middle_first
        ~child:leaf_first.identity.session_id
        ~invocation:first.context.id
        ~reference:leaf_reference
    in
    assert (E.equal_context middle_running.context middle_direct.context);
    assert (
      Option.equal E.equal_delegation middle_running.delegation middle_direct.delegation);
    let upper_direct =
      receipt
        upper_first
        ~child:leaf_first.identity.session_id
        ~invocation:first.context.id
        ~reference:leaf_reference
    in
    assert (
      Jsonaf.exactly_equal
        (Warm_vector.event
           ~id:(P.Id.Invocation.to_string first.context.id)
           Warm_vector.original)
        middle_direct.context.event);
    assert (
      Jsonaf.exactly_equal
        (Warm_vector.event
           ~id:(P.Id.Invocation.to_string first.context.id)
           Warm_vector.middle)
        upper_direct.context.event);
    assert (
      Option.equal
        E.Decision.equal
        (Some (Rewrite_args (Warm_vector.args Warm_vector.middle)))
        middle_direct.decision);
    assert (
      Option.equal
        E.Decision.equal
        (Some (Rewrite_args (Warm_vector.args Warm_vector.final)))
        upper_direct.decision);
    let middle_cause =
      check_direct_cause
        middle_first
        ~entry:tree.middle
        ~caller:leaf_first
        ~invocation:first.context.id
        ~path:[ leaf_reference ]
        middle_direct
    in
    let upper_cause =
      check_direct_cause
        upper_first
        ~entry:tree.upper
        ~caller:leaf_first
        ~invocation:first.context.id
        ~path:[ leaf_reference; middle_reference ]
        upper_direct
    in
    (* This R callback belongs to M's real nested Tool, not C's direct vector. *)
    let nested =
      List.filter middle_first.invocations ~f:(fun nested ->
        Option.equal
          P.Id.Moderator_execution.equal
          nested.P.Invocation.parent_event
          (Some middle_direct.context.id))
      |> Model.only "actual middle callback Tool invocation"
    in
    assert (P.Id.Invocation.equal nested.context.id (Cause.child_invocation cause));
    let upper_nested =
      receipt
        upper_first
        ~child:middle_first.identity.session_id
        ~invocation:nested.context.id
        ~reference:middle_reference
    in
    assert (
      P.Id.Moderator_execution.equal upper_nested.context.id (Cause.parent_event cause));
    assert (Option.equal E.Decision.equal (Some Approve) upper_nested.decision);
    assert (
      not (P.Id.Moderator_execution.equal upper_nested.context.id upper_direct.context.id));
    checkpoint middle_at_model middle_first 1;
    checkpoint tree.before_upper upper_first 2;
    let completed_row =
      L.find upper_first.inference_ledger ~ordinal:(L.Handle.ordinal handle)
      |> Option.value_exn
    in
    assert (L.Handle.equal handle (L.Row.handle completed_row));
    (match Inference.Observation.Attempt_record.state (L.Row.record completed_row) with
     | Terminal terminal ->
       (match Inference.Event.Terminal.outcome terminal with
        | Completed -> ()
        | Refused | Incomplete _ | Failed _ -> failwith "nested Model did not complete")
     | Prepared | Running | Interrupted _ -> failwith "nested Model H is not terminal");
    let model_job =
      List.filter upper_first.jobs ~f:(fun job ->
        P.Job.equal_kind Model_call job.P.Job.kind
        && Option.exists job.launch ~f:(fun launch ->
          match P.Job.Birth_parent.callback_owner launch.owner with
          | Some (Moderator_event event) ->
            P.Id.Moderator_execution.equal upper_nested.context.id event
          | Some (Invocation _) | None -> false))
      |> Model.only "actual nested Model callback Job"
    in
    (match model_job.status with
     | Succeeded -> ()
     | Queued
     | Running
     | Waiting_permission _
     | Waiting_completion _
     | Failed _
     | Cancelled
     | Interrupted _ -> failwith "nested Model Job did not succeed");
    List.iter [ middle_first; leaf_first ] ~f:(fun current ->
      let subtotal = Model.subtotal current in
      [%test_eq: int64] 1L (A.Goal_usage.attempts (Book.Subtotal.usage subtotal));
      assert (List.is_empty (Book.Subtotal.pending subtotal)));
    [%test_eq: int] 1 !(tree.model_calls);
    tree.continue_candidate ();
    let leaf_final =
      within
        env
        "second candidate terminal publication and leaf operation completion"
        (fun () -> await_leaf_quiet tree.leaf)
    in
    let middle_final = Model.checkpoint tree.middle in
    let upper_final = Model.checkpoint tree.upper in
    let denied = invocation leaf_final "593-vector-second" in
    (match denied.status with
     | Published (Fail error) ->
       [%test_eq: string] "invocation.pre_tool_rejected" error.code;
       assert (
         not (String.is_substring error.message ~substring:Warm_vector.private_reason))
     | status ->
       raise_s
         [%sexp "later root rejection was not published", (status : P.Invocation.status)]);
    let middle_second =
      receipt
        middle_final
        ~child:leaf_final.identity.session_id
        ~invocation:denied.context.id
        ~reference:leaf_reference
    in
    let upper_second =
      receipt
        upper_final
        ~child:leaf_final.identity.session_id
        ~invocation:denied.context.id
        ~reference:leaf_reference
    in
    assert (
      Jsonaf.exactly_equal
        (Warm_vector.event
           ~id:(P.Id.Invocation.to_string denied.context.id)
           Warm_vector.rejected)
        middle_second.context.event);
    assert (
      Jsonaf.exactly_equal
        (Warm_vector.event
           ~id:(P.Id.Invocation.to_string denied.context.id)
           Warm_vector.middle)
        upper_second.context.event);
    assert (
      Option.equal
        E.Decision.equal
        (Some (Rewrite_args (Warm_vector.args Warm_vector.middle)))
        middle_second.decision);
    assert (
      Option.equal
        E.Decision.equal
        (Some (Reject Warm_vector.private_reason))
        upper_second.decision);
    ignore
      (check_direct_cause
         upper_final
         ~entry:tree.upper
         ~caller:leaf_final
         ~invocation:denied.context.id
         ~path:[ leaf_reference; middle_reference ]
         upper_second
       : Cause.t);
    assert (Cause.equal middle_cause (cause_for_event middle_final middle_direct));
    assert (Cause.equal upper_cause (cause_for_event upper_final upper_direct));
    checkpoint middle_first middle_final 2;
    checkpoint upper_first upper_final 3;
    List.iter [ middle_direct ] ~f:(unchanged_receipt middle_final);
    List.iter [ upper_direct; upper_nested ] ~f:(unchanged_receipt upper_final);
    assert (P.Invocation.equal first (invocation leaf_final "593-vector-first"));
    let same_job =
      List.find_exn upper_final.jobs ~f:(fun job ->
        P.Id.Job.equal model_job.id job.P.Job.id)
    in
    assert (P.Job.equal_kind model_job.kind same_job.kind);
    assert (Option.equal P.Job.equal_launch model_job.launch same_job.launch);
    assert (Jsonaf.exactly_equal model_job.payload same_job.payload);
    assert (Option.equal Jsonaf.exactly_equal model_job.result same_job.result);
    (match same_job.status with
     | Succeeded -> ()
     | Queued
     | Running
     | Waiting_permission _
     | Waiting_completion _
     | Failed _
     | Cancelled
     | Interrupted _ -> failwith "later rejection changed the completed Model Job");
    let final_accounting = Option.value_exn upper_final.policy_canonical_usage in
    let final_binding =
      PC.bindings final_accounting
      |> List.filter ~f:(fun binding ->
        Int64.equal (L.Handle.ordinal handle) (PC.Binding.ordinal binding))
      |> Model.only "same completed Model canonical binding after rejection"
    in
    assert (Cause.equal cause (Model.cause_for final_accounting final_binding));
    List.iter
      [ middle_first, middle_final; leaf_first, leaf_final ]
      ~f:(fun (before, after) ->
        assert (Jsonaf.exactly_equal (book_json before) (book_json after)));
    [%test_eq: int] first_reads (file_reads Warm_vector.final);
    assert_no_disclosure ();
    [%test_eq: int] 1 !(tree.model_calls));
  print_endline
    "warm Owned M then R rewrites committed; later R rejected without file effect; \
     nested Goal Model preserved";
  [%expect
    {| warm Owned M then R rewrites committed; later R rejected without file effect; nested Goal Model preserved |}]
;;

module Goal_independent = struct
  module Descendants = A.Goal_descendants
  module Occurrence = A.Goal_descendant_occurrence
  module Included = A.Goal_descendant_included
  module Pending = A.Goal_descendant_pending

  type provider_phase =
    | Finish_upper
    | Create_leaf
    | Finish_middle
    | Idle
    | Read of string
    | Finish_read

  let provider_key = "593-goal-independent-birth"

  let rewrite_policy ~script ~file ~finish_creator =
    (* A tool follow-up can emit another Turn_end in the same operation. The
       committed Finish intent owns settlement and forbids another Run action. *)
    let turn_end =
      if finish_creator
      then
        {|
| `Turn_end ->
  if state.finish_requested then Task.pure(state)
  else Task.bind(Run.finish(`Object([
    {key = "kind"; value = `String("finish")},
    {key = "terminal"; value = `Object([{key = "kind"; value = `String("completed")}])},
    {key = "relinquish"; value = `Array([])}
  ])), fun ignored -> Task.pure({ state with finish_requested = true }))
|}
      else ""
    in
    sprintf
      {|<script id=%S language="chatml" kind="moderator" api="extensibility-v1" wall_time="60s">
let initial_state = { rewrites = 0; finish_requested = false }
let on_event ctx state event = match event with
| `Pre_tool_call(call) ->
  if call.name == "read_file" then
    Task.bind(Tool.rewrite_args(`Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String(%S)}
    ])), fun ignored -> Task.pure({ state with rewrites = state.rewrites + 1 }))
  else Task.pure(state)
%s| _ -> Task.pure(state)
</script>|}
      script
      file
      turn_end
  ;;

  let upper_prompt =
    "<developer>593 Goal root and outer policy.</developer>"
    ^ rewrite_policy ~script:"upper-policy" ~file:Warm_vector.final ~finish_creator:false
  ;;

  let middle_prompt =
    {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>593 actual creator closes its Run after native child birth.</developer>
<tool type="inherited" name="agent_create"/>
<tool type="inherited" name="read_file"/>
|}
    ^ rewrite_policy ~script:"middle-policy" ~file:Warm_vector.middle ~finish_creator:true
  ;;

  let checkpoint_value (state : A.Session_state.t) =
    let checkpoint = A.Moderator_checkpoint.decode state.moderator |> protocol_ok in
    match (Option.value_exn checkpoint).current_state with
    | Record fields ->
      (match List.Assoc.find fields "rewrites" ~equal:String.equal with
       | Some (Int value) -> value
       | _ -> failwith "Independent policy lost its rewrite count")
    | _ -> failwith "Independent policy lost its checkpoint record"
  ;;

  let assert_creator_finish (current : A.Session_state.t) index creator =
    let intent : A.Run_intent.t =
      A.Run_state.intents index
      |> List.filter ~f:(fun intent ->
        P.Id.Run.equal intent.A.Run_intent.receipt.run_id creator.P.Run_receipt.run_id)
      |> Model.only "one retained creator Run action"
    in
    let action : P.Run_action.t = Finish { terminal = Completed None; relinquish = [] } in
    assert (P.Run_action.equal action intent.action);
    assert (P.Run_receipt.Kind.equal Action intent.receipt.kind);
    let callback : E.t =
      List.find_exn current.moderator_executions ~f:(fun receipt ->
        P.Id.Moderator_execution.equal receipt.E.context.id intent.execution_id)
    in
    assert (E.equal_phase callback.context.phase Turn_end);
    match callback.status, callback.context.operation_id, intent.disposition with
    | Completed _, Some operation_id, Consumed (Some settled) ->
      assert (P.Id.Operation.equal operation_id settled)
    | _ -> failwith "creator Finish did not settle through its committed Turn_end"
  ;;

  let assert_policy (current : A.Session_state.t) ~leaf ~reference ~invocation ~arg ~file =
    let receipt : E.t =
      List.filter current.moderator_executions ~f:(fun (receipt : E.t) ->
        E.equal_phase receipt.context.phase Pre_tool_call
        && Option.exists receipt.delegation ~f:(fun actual ->
          P.Id.Session.equal actual.child_session_id leaf
          && P.Id.Invocation.equal actual.child_invocation_id invocation))
      |> Model.only "actual fresh Independent policy claim"
    in
    let delegated = Option.value_exn receipt.delegation in
    [%test_eq: string] reference.D.Reference.admission_sha256 delegated.admission_sha256;
    (match receipt.status with
     | Completed _ -> ()
     | Running | Failed _ | Interrupted _ ->
       raise_s [%sexp "Independent policy did not commit", (receipt : E.t)]);
    assert (
      Jsonaf.exactly_equal
        (Warm_vector.event ~id:(P.Id.Invocation.to_string invocation) arg)
        receipt.context.event);
    assert (
      Option.equal
        E.Decision.equal
        (Some (Rewrite_args (Warm_vector.args file)))
        receipt.decision);
    receipt
  ;;

  let confirm_attempt leaf roots =
    let current = state leaf in
    let operation = Option.value_exn current.active_operation in
    let local = Descendants.local_included (Option.value_exn current.goal_descendants) in
    assert (List.length local = 2);
    let receipts =
      List.map roots ~f:(fun (owner, scope, reference) ->
        let branch = Direct.branch owner scope reference in
        assert (Option.is_none (Descendants.Branch.pending branch));
        let included =
          match Descendants.Branch.last branch with
          | Some (Included included) -> included
          | None | Some (No_writer_closed _) ->
            failwith "Independent provider preceded actual root confirmation"
        in
        assert (List.mem local included ~equal:Included.equal);
        Included.pending included |> Pending.occurrence)
    in
    match receipts with
    | [ direct; inherited ] ->
      assert (Occurrence.equal direct inherited);
      (match Occurrence.kind direct with
       | Attempt { ordinal; operation_id; invocation_id } ->
         assert (Option.equal P.Id.Operation.equal (Some operation.id) operation_id);
         assert (Option.is_none invocation_id);
         let row = L.find current.inference_ledger ~ordinal |> Option.value_exn in
         (match Inference.Observation.Attempt_record.state (L.Row.record row) with
          | Running -> ()
          | Prepared | Terminal _ | Interrupted _ ->
            failwith "Independent provider preceded actual canonical Running")
       | Host_turn _ -> failwith "Independent provider has no confirmed Attempt")
    | _ -> failwith "Independent provider did not have exactly two Goal roots"
  ;;

  let assert_root_delta ~before ~after ~turns ~attempts =
    let before = Tree.root_marks before in
    let after = Tree.root_marks after in
    [%test_eq: int64] turns Int64.(after.turn - before.turn);
    [%test_eq: int64] attempts Int64.(after.attempt - before.attempt)
  ;;

  let await_denied_turn_close env ~before_leaf ~before_branch ~read_branch =
    let module Branch = Descendants.Branch in
    let accounting = Option.value_exn before_leaf.A.Session_state.goal_descendants in
    let assert_rejected_turn pending =
      assert (Origin.Scope.equal (Branch.scope before_branch) (Pending.scope pending));
      assert (
        D.Reference.equal
          (Branch.reference before_branch)
          (Pending.branch_reference pending));
      [%test_eq: int64]
        (Int64.succ (Branch.last_nonce before_branch))
        (Pending.nonce pending);
      [%test_eq: int64]
        (Int64.succ (Branch.confirmed_prefix before_branch))
        (Pending.next_prefix pending);
      let occurrence = Pending.occurrence pending in
      let frame = Occurrence.frame occurrence in
      assert (
        P.Session_ref.equal
          (Descendants.session accounting)
          (Occurrence.Frame.session frame));
      [%test_eq: int] before_leaf.identity.generation (Occurrence.Frame.generation frame);
      match Occurrence.kind occurrence with
      | Host_turn _ -> ()
      | Attempt _ ->
        failwith "Goal Hold reserved an Attempt instead of the denied Host_turn"
    in
    let rec await observed =
      let branch = read_branch () in
      [%test_eq: int64]
        (Branch.confirmed_prefix before_branch)
        (Branch.confirmed_prefix branch);
      [%test_eq: int64]
        (Int64.succ (Branch.last_nonce before_branch))
        (Branch.last_nonce branch);
      match Branch.pending branch, Branch.last branch with
      | Some pending, last ->
        assert (Option.equal Branch.Last.equal (Branch.last before_branch) last);
        assert_rejected_turn pending;
        assert (Option.for_all observed ~f:(Pending.equal pending));
        Eio.Fiber.yield ();
        await (Some pending)
      | None, Some (No_writer_closed pending) ->
        assert_rejected_turn pending;
        assert (Option.for_all observed ~f:(Pending.equal pending))
      | None, (None | Some (Included _)) ->
        raise_s
          [%sexp "denied Host_turn has no factual close receipt", (branch : Branch.t)]
    in
    within env "original denied Host_turn factual close" (fun () -> await None)
  ;;

  let prepare_with_creator_reservations daemon ~(creator : Registry.entry) leaf =
    let registry = Daemon.registry daemon in
    let handle = Option.value_exn creator.store_handle in
    let id = Agent_store.Session_store.Handle.session_id handle in
    let prepare () = Factory.prepare_session_start (Daemon.factory daemon) leaf in
    let join_reservation () =
      Registry.with_loaded_read registry id (fun current ->
        assert (phys_equal current creator);
        Ok ())
      |> protocol_ok
    in
    join_reservation ();
    Registry.with_lifecycle registry id (fun reservation ->
      (match Registry.Lifecycle_reservation.target reservation with
       | Loaded current -> assert (phys_equal current creator)
       | Indexed _ | Absent -> failwith "creator lost its retained Actor");
      assert (Registry.blocks_native_metadata_read registry creator);
      (match prepare () with
       | Error error ->
         [%test_eq: P.Error.code] Conflict error.code;
         assert error.retryable
       | Ok () -> failwith "Independent Start preflight crossed exclusive lifecycle");
      Ok ())
    |> protocol_ok;
    let retained, signal_retained = Eio.Promise.create () in
    let released, signal_released = Eio.Promise.create () in
    let hold () =
      (* Join any ambient snapshot reservation before taking the same-tuple
         retention phase on the creator whose Run has already settled. Keep
         the mailbox free for the concurrent preflight's committed-state read. *)
      join_reservation ();
      Registry.with_loaded_retention
        registry
        ~actor:creator.actor
        ~runtime:creator.runtime
        ~handle
        (fun () ->
           Eio.Promise.resolve signal_retained ();
           Eio.Promise.await released)
      |> protocol_ok
      |> Option.value_exn
    in
    let preflight () =
      Eio.Promise.await retained;
      Exn.protect
        ~f:(fun () ->
          assert (Registry.lifecycle_reserved registry id);
          assert (Registry.blocks_owned_execution registry creator);
          assert (not (Registry.blocks_native_metadata_read registry creator));
          prepare () |> protocol_ok;
          assert (not (Owner.is_loaded leaf.Registry.runtime)))
        ~finally:(fun () -> Eio.Promise.resolve signal_released ())
    in
    Eio.Fiber.both hold preflight
  ;;

  let run () =
    let failure = Failure.create () in
    let phase = ref Finish_upper in
    let at_provider = ref (fun () -> failwith "Independent provider was not armed") in
    let provider _ =
      Failure.guard failure (fun () ->
        match !phase with
        | Finish_upper ->
          phase := Create_leaf;
          completed_response "593 upper completed middle creation"
        | Create_leaf ->
          phase := Finish_middle;
          Birth.create_response
            ~key:provider_key
            ~call_id:provider_key
            ~lifetime:Independent
            ~tools:[ "read_file" ]
            [ "child.chatmd", Model.child_prompt ]
        | Finish_middle ->
          phase := Idle;
          completed_response "593 creator completed Independent child creation"
        | Read call_id ->
          !at_provider ();
          phase := Finish_read;
          Warm_vector.response ~id:call_id Warm_vector.original
        | Finish_read ->
          !at_provider ();
          phase := Idle;
          completed_response "593 Independent child completed its later input"
        | Idle -> failwith "Independent fixture dispatched an unrequested provider")
    in
    let reads = String.Table.create () in
    let file_reads file = Hashtbl.find reads file |> Option.value ~default:0 in
    Birth.with_native_goal_child
      ~timeout:180.
      ~independent_lifetime_policy:"593-goal-independent"
      ~parent_prompt_text:upper_prompt
      ~sources:
        [ Warm_vector.original, "593 private original"
        ; Warm_vector.middle, "593 private intermediate"
        ; Warm_vector.final, "593 actual Independent selected file"
        ]
      ~wrap_env:(fun env ->
        Crash_fault_io.wrap
          env
          ~boundary:Before_open_in
          ~matches:(fun path ->
            List.exists Warm_vector.files ~f:(fun file ->
              String.equal (Filename.basename path) file))
          ~reached:(fun path ->
            Hashtbl.update reads (Filename.basename path) ~f:(fun count ->
              Int.succ (Option.value count ~default:0))))
      ~inherited_tools:
        [ "agent_create", ""; "read_file", Descendant_goal_resume_tests.parent_reader ]
      ~child_sources:[ "child.chatmd", middle_prompt ]
      ~post_creation_stream:provider
      (fun env _sw daemon upper middle upper_scope middle_reference counters ->
         within env "install Independent creator" (fun () ->
           Direct.start_child daemon middle);
         let creator =
           activate daemon middle ~key:provider_key ~text:"Create the Independent child."
         in
         let rec await_creator () =
           Failure.raise_if_present failure;
           let current = Model.checkpoint middle in
           let index = Option.value_exn current.run_state in
           let run = A.Run_state.find index creator.run_id |> Option.value_exn in
           match run.lifecycle, Tree.published middle provider_key with
           | Terminal (Completed _), Some value
             when Option.is_none current.active_operation ->
             assert (G.State.equal (Open Enabled) (G.state (Goal.goal current)));
             assert_creator_finish current index creator;
             current, value, Goal_finish_tests.terminal_receipt index creator
           | Terminal (Failed _ | Cancelled | Limited | Interrupted), _ ->
             raise_s [%sexp "Independent creator Run ended adversely", (run : P.Run.t)]
           | (Admitted | Active | Waiting _ | Terminal (Completed _)), _ ->
             Eio.Fiber.yield ();
             await_creator ()
         in
         let middle_before, value, creator_terminal =
           within env "Independent publication and terminal creator" await_creator
         in
         let leaf_id =
           Jsonaf.member_exn "session_id" value
           |> Jsonaf.string_exn
           |> P.Id.Session.of_string
           |> protocol_ok
         in
         let leaf = Registry.find (Daemon.registry daemon) leaf_id |> Option.value_exn in
         let born = state leaf in
         Tree.assert_no_local_goal born;
         assert (not (Owner.is_loaded leaf.runtime));
         let reference = Option.value_exn born.spec.delegation in
         let delegations = Agent_store.Session_store.delegations (Daemon.store daemon) in
         let resolve reference = D.resolve delegations reference |> Existing.store_ok in
         let record = resolve reference in
         assert (D.Reference.equal reference (D.reference record));
         assert (D.equal_stage record.stage Linked);
         assert (Option.is_none record.revocation);
         assert (
           P.Id.Session.equal
             record.key.parent_session_id
             middle_before.identity.session_id);
         assert (P.Id.Session.equal record.admission.child_session_id leaf_id);
         (match record.admission.lifetime with
          | Independent _ -> ()
          | Owned | Invocation_owned _ ->
            failwith "native birth did not retain Independent");
         let origin = Option.value_exn record.admission.goal_origin in
         let scopes = Origin.scopes origin in
         let middle_scope =
           List.filter scopes ~f:(fun scope ->
             P.Session_ref.equal (session daemon middle) (Origin.Scope.owner scope))
           |> Model.only "one actual immediate Goal scope"
         in
         assert (
           List.length scopes = 2 && List.mem scopes upper_scope ~equal:Origin.Scope.equal);
         assert (
           P.Run_receipt.equal
             creator
             (P.Goal_episode.admission (Origin.Scope.creator_episode middle_scope)));
         assert (
           Option.exists
             (Descendants.origin (Option.value_exn born.goal_descendants))
             ~f:(Origin.equal origin));
         let roots =
           [ middle, middle_scope, reference; upper, upper_scope, middle_reference ]
         in
         within
           env
           "prepare Independent child under creator lifecycle reservations"
           (fun () -> prepare_with_creator_reservations daemon ~creator:middle leaf);
         within env "install Independent child" (fun () ->
           Direct.start_prepared_child leaf);
         Tree.assert_source
           env
           daemon
           { entry = leaf; reference; record; scope = middle_scope }
           ~text:Model.child_prompt
           ~records:[ record; resolve middle_reference ];
         (* Settle the creator's terminal own usage before measuring either
            later Independent input in the inherited R branch. *)
         Factory.import_child_usage
           (Daemon.factory daemon)
           ~parent:upper
           ~child:middle
           ~actor:original
           ~authorize_parent:authorize
           ~authorize_child:authorize
         |> protocol_ok
         |> ignore;
         let upper_before = Model.checkpoint upper in
         (at_provider := fun () -> confirm_attempt leaf roots);
         Tree.with_writer leaf (fun writer ->
           let submit text =
             Actor.submit_message_authenticated
               leaf.actor
               ~actor:original
               ~attachment_id:writer.id
               (Tree.message leaf text)
           in
           let providers_before = !(counters.providers) in
           let check_later_input index =
             let provider_call_id = "593-independent-later-" ^ Int.to_string index in
             phase := Read provider_call_id;
             let submitted = submit provider_call_id |> protocol_ok in
             assert (Option.is_some submitted.operation_id);
             let current =
               within env "later Independent input" (fun () -> await_leaf_quiet leaf)
             in
             Failure.raise_if_present failure;
             let invocation : P.Invocation.t =
               List.filter current.invocations ~f:(fun invocation ->
                 Option.equal
                   String.equal
                   (Some provider_call_id)
                   invocation.P.Invocation.context.provider_call_id)
               |> Model.only "later Independent native read"
             in
             (match invocation.status with
              | Published (Complete (`String output)) ->
                assert (
                  String.is_substring
                    output
                    ~substring:"593 actual Independent selected file")
              | status ->
                raise_s
                  [%sexp
                    "Independent read did not complete", (status : P.Invocation.status)]);
             let middle_now = Model.checkpoint middle in
             let upper_now = Model.checkpoint upper in
             let middle_receipt =
               assert_policy
                 middle_now
                 ~leaf:leaf_id
                 ~reference
                 ~invocation:invocation.context.id
                 ~arg:Warm_vector.original
                 ~file:Warm_vector.middle
             in
             let upper_receipt =
               assert_policy
                 upper_now
                 ~leaf:leaf_id
                 ~reference
                 ~invocation:invocation.context.id
                 ~arg:Warm_vector.middle
                 ~file:Warm_vector.final
             in
             let creator_index = Option.value_exn middle_now.run_state in
             assert (
               P.Run_receipt.equal
                 creator_terminal
                 (Goal_finish_tests.terminal_receipt creator_index creator));
             assert (G.State.equal (Open Enabled) (G.state (Goal.goal middle_now)));
             [%test_eq: int]
               (checkpoint_value middle_before + index)
               (checkpoint_value middle_now);
             [%test_eq: int]
               (checkpoint_value upper_before + index)
               (checkpoint_value upper_now);
             let turns = Int64.of_int index in
             let attempts = Int64.of_int (2 * index) in
             List.iter
               [ middle_before, middle_now; upper_before, upper_now ]
               ~f:(fun (before, after) ->
                 assert_root_delta ~before ~after ~turns ~attempts);
             let marks = Direct.marks leaf in
             [%test_eq: int64] turns marks.turn;
             [%test_eq: int64] attempts marks.attempt;
             [%test_eq: int64] 0L marks.untracked_turns;
             [%test_eq: int] (providers_before + (2 * index)) !(counters.providers);
             middle_receipt, upper_receipt
           in
           let first_middle, first_upper = check_later_input 1 in
           let creator_before_unload = Model.checkpoint middle in
           let upper_before_unload = Model.checkpoint upper in
           let middle_record = resolve middle_reference in
           let profile =
             Factory.Developer_catalog.find_profile
               (Factory.developer_catalog (Daemon.factory daemon))
               creator_before_unload.spec.permission_profile
             |> Option.value_exn
           in
           [%test_eq: string]
             creator_before_unload.spec.permission_profile_digest
             profile.revision_digest;
           let handle = Option.value_exn middle.Registry.store_handle in
           let observe_middle_source expected =
             Owner.observe_native_source middle.runtime ~handle ~profile ~expected
             |> protocol_ok
           in
           let original_source =
             observe_middle_source creator_before_unload |> Option.value_exn
           in
           let providers_before_reload = !(counters.providers) in
           within env "join child and creator native resource retirement" (fun () ->
             (* The child owns a real parent pin. Release it before testing the
                creator's physical unload. Both Goals remain Enabled. *)
             Owner.unload_and_wait leaf.runtime |> protocol_ok;
             Owner.unload_and_wait middle.runtime |> protocol_ok);
           assert (not (Owner.is_loaded leaf.runtime));
           assert (not (Owner.is_loaded middle.runtime));
           let cold_creator = Model.checkpoint middle in
           assert (Option.is_none (observe_middle_source cold_creator));
           let assert_unchanged_creator current =
             assert (G.equal (Goal.goal creator_before_unload) (Goal.goal current));
             Native_resource_integration_tests.no_lifecycle_replay
               creator_before_unload
               current
           in
           assert_unchanged_creator cold_creator;
           within env "reload only the authenticated Independent child" (fun () ->
             Factory.prepare_session_start (Daemon.factory daemon) leaf |> protocol_ok;
             Owner.ensure_loaded_authenticated leaf.runtime ~actor:original |> protocol_ok);
           assert (Owner.is_loaded leaf.runtime);
           assert (not (Owner.is_loaded middle.runtime));
           let native_creator = Model.checkpoint middle in
           assert_unchanged_creator native_creator;
           let new_source = observe_middle_source native_creator |> Option.value_exn in
           assert (
             not
               (phys_equal
                  (Owner.Native_source.source original_source)
                  (Owner.Native_source.source new_source)));
           assert (D.equal_record record (resolve reference));
           assert (D.equal_record middle_record (resolve middle_reference));
           assert (
             G.equal (Goal.goal upper_before_unload) (Goal.goal (Model.checkpoint upper)));
           assert (
             Option.exists
               (Descendants.origin (Option.value_exn (state leaf).goal_descendants))
               ~f:(Origin.equal origin));
           [%test_eq: int] providers_before_reload !(counters.providers);
           let second_middle, second_upper = check_later_input 2 in
           assert (not (Owner.is_loaded middle.runtime));
           let restored_source =
             observe_middle_source (Model.checkpoint middle) |> Option.value_exn
           in
           assert (
             phys_equal
               (Owner.Native_source.source new_source)
               (Owner.Native_source.source restored_source));
           List.iter
             [ first_middle, second_middle; first_upper, second_upper ]
             ~f:(fun (first, second) ->
               assert (
                 not
                   (P.Id.Moderator_execution.equal first.E.context.id second.E.context.id)));
           let selected_reads = file_reads Warm_vector.final in
           assert (selected_reads > 0);
           [%test_eq: int] 0 (file_reads Warm_vector.original);
           [%test_eq: int] 0 (file_reads Warm_vector.middle);
           Tree.with_writer middle (fun writer ->
             Actor.stop middle.actor ~attachment_id:writer.id ~mode:Cancel
             |> protocol_ok
             |> ignore);
           let held = state middle in
           (match G.state (Goal.goal held) with
            | Open (Held _) -> ()
            | Open Enabled | Closed _ -> failwith "parent Stop did not Hold its Goal");
           assert (P.Session.equal_desired_state held.lifecycle.desired Stopped);
           (match held.lifecycle.observed with
            | Stopped -> ()
            | _ -> failwith "parent Stop did not settle");
           assert (P.Session.equal_desired_state (state leaf).lifecycle.desired Running);
           let before_denial = state leaf in
           let before_leaf_marks = Direct.marks leaf in
           let before_roots =
             List.sort roots ~compare:(fun (_, left, _) (_, right, _) ->
               Origin.Scope.compare_root left right)
             |> List.map ~f:(fun ((owner, scope, reference) as root) ->
               let branch = Direct.branch owner scope reference in
               assert (Option.is_none (Descendants.Branch.pending branch));
               root, branch, Tree.root_marks (state owner))
           in
           let providers = !(counters.providers) in
           let reads_before_denial = List.map Warm_vector.files ~f:file_reads in
           (match submit "593 parent Goal is held" with
            | Error error ->
              [%test_eq: P.Error.code] Permission_denied error.code;
              [%test_eq: string] "descendant root Goal is not enabled" error.message
            | Ok _ -> failwith "parent Goal Hold admitted a fresh Independent input");
           (* Preparation visits the canonical root order. Every preceding root
              reserved the denied Turn and must close that exact occurrence. *)
           List.iter
             before_roots
             ~f:(fun ((owner, scope, reference), before_branch, before_marks) ->
               let read_branch () = Direct.branch owner scope reference in
               if Origin.Scope.compare_root scope middle_scope < 0
               then
                 await_denied_turn_close
                   env
                   ~before_leaf:before_denial
                   ~before_branch
                   ~read_branch
               else assert (Descendants.Branch.equal before_branch (read_branch ()));
               assert (
                 A.Canonical_usage_fold.Marks.equal
                   before_marks
                   (Tree.root_marks (state owner))));
           let denied = state leaf in
           assert (Option.is_none denied.active_operation);
           assert (Jsonaf.exactly_equal (ledger_json before_denial) (ledger_json denied));
           assert (
             List.equal P.Invocation.equal before_denial.invocations denied.invocations);
           assert (
             List.equal
               P.History.equal_entry
               before_denial.conversation.canonical_history
               denied.conversation.canonical_history);
           let local current =
             Descendants.local_included
               (Option.value_exn current.A.Session_state.goal_descendants)
           in
           assert (List.equal Included.equal (local before_denial) (local denied));
           assert (
             A.Canonical_usage_fold.Marks.equal before_leaf_marks (Direct.marks leaf));
           [%test_eq: int] providers !(counters.providers);
           [%test_eq: int list]
             reads_before_denial
             (List.map Warm_vector.files ~f:file_reads);
           Failure.raise_if_present failure))
  ;;
end

let%expect_test
    "Goal-born moderated Independent child uses fresh policy after creator Run finishes"
  =
  Goal_independent.run ();
  print_endline
    "Later inputs honored both policies and Goal roots after creator completion. Parent \
     Stop held its Goal.";
  [%expect
    {| Later inputs honored both policies and Goal roots after creator completion. Parent Stop held its Goal. |}]
;;
