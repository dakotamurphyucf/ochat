open! Core
open Agent_server_test_support
open Retry_support
module B = A.Runtime_builder
module E = P.Moderator_execution
module I = P.Invocation
module M = Chat_response.Moderator_manager
module C = Chat_response.Tool_capability
module Warm = Delegated_warm_resource_tests
module Polling = Ordinary_warm_resource_tests.Polling

let parent_prompt =
  {|<developer>592 genuine parent policy.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="nested-read" language="chatml" kind="tool">
let run ctx input = let* result = Tool.call("read_file", input) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(code) -> Task.fail(code)
</script>
<tool name="nested_read" type="chatml" script="nested-read" entrypoint="run"
 input_schema="schema.json" output_schema="schema.json"><uses tool="read_file"/></tool>
<script id="parent-policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(call) ->
  let* result = Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("policy.txt")}
  ])) in
  (match result with
  | `Ok(value) -> Task.pure(state + 1)
  | `Error(code) -> Task.fail(code))
| `Tool_observed(observation) -> Task.pure(state + 1)
| _ -> Task.pure(state)
</script>|}
;;

let child_prompt =
  {|<config model="fixture-model"/><developer>592 genuine generated child.</developer>
<tool type="inherited" name="read_file"/>
<tool type="inherited" name="nested_read"/>
<script id="child-event" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Turn_end ->
  if state == 0 then
    let* result = Tool.call("read_file", `Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String("event.txt")}
    ])) in
    (match result with
    | `Ok(value) -> Task.pure(state + 1)
    | `Error(code) -> Task.fail(code))
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

module Failure = struct
  type t = (exn * Stdlib.Printexc.raw_backtrace) option ref

  let create () = ref None

  let raise_if_present t =
    Option.iter !t ~f:(fun (exn, backtrace) ->
      Exn.raise_with_original_backtrace exn backtrace)
  ;;

  let guard t f =
    match f () with
    | value -> value
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      Exn.raise_with_original_backtrace exn backtrace
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      if Option.is_none !t then t := Some (exn, backtrace);
      Exn.raise_with_original_backtrace exn backtrace
  ;;

  let restore t f =
    match f () with
    | value ->
      raise_if_present t;
      value
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      raise_if_present t;
      Exn.raise_with_original_backtrace exn backtrace
  ;;
end

module Provider = struct
  type call =
    { name : string
    ; file : string
    ; id : string
    }

  type ambient =
    { entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; exited : unit Eio.Promise.t
    ; exit : unit Eio.Promise.u
    ; release : unit Eio.Promise.t
    ; release_u : unit Eio.Promise.u
    }

  type t =
    { mutable call : call option
    ; mutable ambient : ambient option
    ; mutable child_calls : int
    ; mutable ambient_calls : int
    ; failure : Failure.t
    }

  let create () =
    { call = None
    ; ambient = None
    ; child_calls = 0
    ; ambient_calls = 0
    ; failure = Failure.create ()
    }
  ;;

  let begin_ambient t =
    let entered, enter = Eio.Promise.create () in
    let exited, exit = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let actual = { entered; enter; exited; exit; release; release_u } in
    assert (Option.is_none t.ambient);
    t.ambient <- Some actual;
    actual
  ;;

  let release ambient =
    if Option.is_none (Eio.Promise.peek ambient.release)
    then Eio.Promise.resolve ambient.release_u ()
  ;;

  let function_response call =
    let arguments =
      `Object [ "root", `String "data"; "file", `String call.file ] |> Jsonaf.to_string
    in
    let item arguments status =
      Res.Response_stream.Item.Function_call
        { name = call.name
        ; arguments
        ; call_id = call.id
        ; _type = "function_call"
        ; id = Some call.id
        ; status
        }
    in
    let open Res.Response_stream in
    [ Output_item_added
        { item = item "" None; output_index = 0; type_ = "response.output_item.added" }
    ; Function_call_arguments_done
        { arguments
        ; item_id = call.id
        ; output_index = 0
        ; type_ = "response.function_call_arguments.done"
        }
    ; Output_item_done
        { item = item arguments (Some "completed")
        ; output_index = 0
        ; type_ = "response.output_item.done"
        }
    ]
    |> Stdlib.List.to_seq
  ;;

  let post_stream t inputs =
    Failure.guard t.failure (fun () ->
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      if String.is_substring transcript ~substring:"592_AMBIENT_PARENT"
      then (
        let ambient = Option.value_exn t.ambient in
        assert (Option.is_none (Eio.Promise.peek ambient.entered));
        t.ambient_calls <- t.ambient_calls + 1;
        Eio.Promise.resolve ambient.enter ();
        Exn.protect
          ~finally:(fun () -> Eio.Promise.resolve ambient.exit ())
          ~f:(fun () ->
            Eio.Promise.await ambient.release;
            failwith "ambient parent provider returned instead of actual cancellation"))
      else (
        t.child_calls <- t.child_calls + 1;
        match t.call with
        | Some call ->
          t.call <- None;
          function_response call
        | None -> completed_response "592 child native result completed"))
  ;;
end

(* Genuine trusted-host Caller construction shared by warm/stopped and physical
   cold policy qualification. It installs no Factory relation or Independent
   authority; the native caller still supplies its actual owner. *)
let with_standalone_caller ~env ~daemon ~(source_state : A.Session_state.t) ~original f =
  Eio.Switch.run (fun sw ->
    let selected =
      A.Prompt_catalog.entries (Daemon.prompts daemon)
      |> List.find_exn ~f:(fun entry ->
        Option.exists
          source_state.spec.prompt_definition_id
          ~f:(P.Id.Prompt_definition.equal entry.A.Prompt_catalog.definition.id))
    in
    let revision =
      match selected.availability with
      | Ready revision -> revision
      | Disabled | Unavailable _ -> failwith "actual standalone caller source unavailable"
    in
    let permission =
      A.Permission_policy.create
        ~id:selected.definition.permission_profile
        ~tool_default:Allow
        ~approval_timeout_ms:None
        ~fallback:Fallback_deny
        ~manifest_authorization:Require_grant
        ~evaluator:None
        ~evaluator_revision:None
        ~reviewer:None
      |> protocol_ok
    in
    Legacy_idle_completion_tests.Physical.with_actor
      env
      sw
      daemon
      ~revision
      ~definition:selected.definition
      ~permission
      ~inference_target:source_state.spec.inference_target
      (fun physical ->
         let actor = physical.actor in
         let attachment, _ =
           Actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
         in
         protect_cleanup
           ~finally:(fun () -> Actor.detach actor attachment.id |> protocol_ok)
           (fun () ->
              Actor.start ~actor:original actor ~attachment_id:attachment.id
              |> protocol_ok
              |> ignore;
              f actor attachment.id)))
;;

type t =
  { env : Eio_unix.Stdenv.base
  ; daemon : Daemon.t
  ; parent : Registry.entry
  ; child : Registry.entry
  ; parent_writer : P.Id.Attachment.t
  ; child_writer : P.Id.Attachment.t
  ; parent_runtime : B.t
  ; child_runtime : B.t
  ; parent_initial : A.Session_state.t
  ; child_initial : A.Session_state.t
  ; a : Operator_authorization.t
  ; b : Operator_authorization.t
  ; b_live : bool Atomic.t
  ; provider : Provider.t
  ; counters : counters
  ; phase : string ref
  }

let state entry = Actor.state entry.Registry.actor |> protocol_ok

let within t phase f =
  t.phase := phase;
  Eio.Time.with_timeout_exn t.env#clock 30. f
;;

let await_idle t entry =
  let rec loop () =
    Failure.raise_if_present t.provider.failure;
    let current = Warm.settled entry in
    match current.active_operation, current.lifecycle.observed, current.failure with
    | None, Idle, None -> current
    | _, _, Some failure -> raise_s [%sexp (failure : P.Error.t)]
    | (None | Some _), _, None ->
      Eio.Fiber.yield ();
      loop ()
  in
  loop ()
;;

let submit entry writer ~actor text =
  let message =
    Owner.parse_user_content
      entry.Registry.runtime
      ~id:(A.History_id_source.allocate entry.history_ids |> protocol_ok)
      { kind = Plain_text; text; attachments = [] }
    |> protocol_ok
    |> A.History_codec.to_protocol
  in
  let result =
    match actor with
    | Some actor ->
      Actor.submit_message_authenticated entry.actor ~actor ~attachment_id:writer message
    | None ->
      Actor.submit_message
        entry.actor
        ~submitting_principal:principal.id
        ~attachment_id:writer
        message
  in
  let submission = result |> protocol_ok in
  message, Option.value_exn submission.operation_id
;;

let with_ambient t f =
  let ambient = Provider.begin_ambient t.provider in
  let _, operation =
    submit t.parent t.parent_writer ~actor:(Some t.a) "592_AMBIENT_PARENT"
  in
  let cancel_and_join () =
    protect_cleanup
      ~finally:(fun () -> Provider.release ambient)
      (fun () ->
         within t "cancel actual unrelated parent operation" (fun () ->
           Actor.cancel_operation
             t.parent.actor
             ~attachment_id:t.parent_writer
             ~operation_id:operation
           |> protocol_ok
           |> ignore);
         within t "join actual unrelated parent provider" (fun () ->
           Eio.Promise.await ambient.exited);
         within t "join actual parent terminal checkpoint" (fun () ->
           ignore (await_idle t t.parent));
         t.provider.ambient <- None)
  in
  protect_cleanup ~finally:cancel_and_join (fun () ->
    within t "actual unrelated parent A provider entered" (fun () ->
      Eio.Promise.await ambient.entered);
    let actual = state t.parent in
    assert (
      Option.exists actual.active_operation ~f:(fun active ->
        P.Id.Operation.equal active.id operation));
    assert (Operator_authorization.is_current t.a);
    f actual)
;;

let run_child t ~actor ~name ~file ~id =
  assert (Option.is_none t.provider.call);
  t.provider.call <- Some { Provider.name; file; id };
  let message, operation = submit t.child t.child_writer ~actor ("592 child " ^ id) in
  let current =
    within t ("complete actual child " ^ id) (fun () -> await_idle t t.child)
  in
  assert (Option.is_none t.provider.call);
  (match
     List.filter current.conversation.canonical_history ~f:(fun actual ->
       P.History.Id.equal actual.P.History.id message.id)
   with
   | [ actual ] -> assert (P.History.equal_entry message actual)
   | _ -> failwith "actual submitted child history entry was lost or duplicated");
  assert (
    List.for_all t.child_initial.conversation.canonical_history ~f:(fun original ->
      List.exists
        current.conversation.canonical_history
        ~f:(P.History.equal_entry original)));
  let invocation =
    match
      List.filter current.invocations ~f:(fun invocation ->
        Option.equal String.equal invocation.I.context.provider_call_id (Some id))
    with
    | [ invocation ] -> invocation
    | _ -> failwith "actual canonical child call lost its unique invocation"
  in
  assert (String.equal name invocation.context.tool_name);
  let entry_id = Option.value_exn invocation.context.call_entry_id in
  assert (
    List.exists current.conversation.canonical_history ~f:(fun entry ->
      P.History.Id.equal entry.P.History.id entry_id));
  (match invocation.status with
   | Published (Complete (`String value)) ->
     assert (String.is_substring value ~substring:("592 " ^ file))
   | Admitted | Dispatching | Resolved _
   | Published (Pending _ | Fail _ | Cancelled _)
   | Published (Complete _) ->
     raise_s
       [%sexp
         "real child native call did not complete"
       , (name : string)
       , (invocation.status : I.status)]);
  current, operation, invocation
;;

let policy_receipts current =
  List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
    Option.is_some receipt.E.delegation)
;;

let assert_policy_correspondence t (invocation : I.t) =
  let parent = state t.parent in
  let receipt =
    match
      List.filter (policy_receipts parent) ~f:(fun receipt ->
        let delegation = Option.value_exn receipt.E.delegation in
        P.Id.Invocation.equal delegation.child_invocation_id invocation.context.id)
    with
    | [ receipt ] -> receipt
    | _ -> failwith "real child invocation did not have exactly one parent policy receipt"
  in
  let delegation = Option.value_exn receipt.delegation in
  assert (
    P.Id.Session.equal delegation.child_session_id t.child_initial.identity.session_id);
  [%test_eq: int] t.child_initial.identity.generation delegation.child_generation;
  assert (E.equal_phase Pre_tool_call receipt.context.phase);
  assert (
    Option.is_none receipt.context.operation_id && Option.is_none receipt.context.job);
  (match receipt.status, receipt.decision with
   | Completed _, Some Approve -> ()
   | (Running | Failed _ | Interrupted _), _
   | Completed _, (None | Some (Reject _ | Rewrite_args _ | Redirect _)) ->
     failwith "real parent policy claim did not commit its approving decision");
  let native =
    match
      List.filter parent.invocations ~f:(fun actual ->
        Option.exists
          actual.I.parent_event
          ~f:(P.Id.Moderator_execution.equal receipt.context.id))
    with
    | [ native ] -> native
    | _ -> failwith "actual parent policy did not own exactly one real native result"
  in
  assert (String.equal native.context.tool_name "read_file");
  (match native.status with
   | Resolved (Complete (`String value)) ->
     assert (String.is_substring value ~substring:"592 parent policy real native sentinel")
   | Admitted | Dispatching
   | Resolved (Pending _ | Fail _ | Cancelled _)
   | Resolved (Complete _)
   | Published _ -> failwith "actual parent policy native read did not complete");
  let observation = Option.value_exn native.observation in
  assert (I.equal_observer observation.observer receipt.context.source);
  assert (I.equal_observation_status Awaiting observation.status);
  receipt, native
;;

let observe_original t ~expected ~count =
  let failure = Failure.create () in
  let confirmed = ref [] in
  let runtime = Warm.loaded t.parent in
  let manager = Option.value_exn runtime.B.moderator_manager in
  let definition = M.extension_definition manager |> Option.value_exn in
  let observer = M.invocation_observer manager |> Option.value_exn in
  let entries = ref [] in
  let result =
    Failure.restore failure (fun () ->
      A.Moderator_observation.drain_idle_with_tools
        ~model_recipes:runtime.model_recipes
        ~max_observations:count
        ~script_tools:(Option.value_exn runtime.moderator_script_tools)
        ~definition
        ~claim:(fun handle ->
          Actor.with_idle_moderator_observation_tools
            t.parent.actor
            ~observer
            (fun ~observing ~capabilities ~execute ~commit ->
               Failure.guard failure (fun () ->
                 assert (Option.is_some observing.parent_event);
                 (match
                    expected, capabilities.borrow_agent_actor ~invocation:observing
                  with
                  | Some original, Ok (Some actual) ->
                    assert (Operator_authorization.is_current actual);
                    assert (
                      P.Id.Principal.equal
                        (Operator_authorization.principal actual).id
                        (Operator_authorization.principal original).id);
                    assert (
                      not
                        (P.Id.Principal.equal
                           (Operator_authorization.principal actual).id
                           (Operator_authorization.principal t.a).id))
                  | None, Ok None -> ()
                  | (None | Some _), (Error _ | Ok _) ->
                    failwith "parent observation did not retain the exact child original");
                 entries
                 := (state t.parent).conversation.canonical_history
                    |> A.History_codec.all_of_protocol
                    |> protocol_ok;
                 let open Result.Let_syntax in
                 let%map () = handle ~observing ~capabilities ~execute ~commit in
                 confirmed := observing.context.id :: !confirmed)))
        ~manager
        ~history:(fun () -> !entries)
        ~available_tools:runtime.moderator_tools
        ~session_meta:`Null
        ~now:runtime.now
        ()
      |> protocol_ok)
  in
  [%test_eq: int] count (List.length !confirmed);
  [%test_eq: int] count (List.length result.outcomes);
  !confirmed
;;

let create_child env root daemon parent runtime ~created_at =
  let native = Option.value_exn runtime.B.native_runtime in
  let capabilities =
    Lazy.force native.capabilities
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
  in
  let bundle =
    Chatmd_source_bundle.create
      ~root_file:"child.chatmd"
      ~sources:[ "child.chatmd", child_prompt ]
      ()
    |> Result.ok_or_failwith
  in
  let definition =
    A.Generated_definition.prepare
      ~env
      ~dir:Eio.Path.(env#fs / root)
      ~revision_id:(P.Id.Prompt_revision.create ())
      ~created_at
      ~current_capabilities:(fun () -> capabilities)
      ~references:(C.references capabilities)
      bundle
    |> Result.map_error ~f:(fun errors ->
      List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> String.concat ~sep:"\n")
    |> Result.ok_or_failwith
  in
  Factory.create_generated_session
    ~start_immediately:true
    (Daemon.factory daemon)
    ~parent_session_id:(state parent).identity.session_id
    ~idempotency_key:
      (P.Idempotency_key.of_string "592-genuine-caller-child" |> protocol_ok)
    ~display_name:None
    definition
  |> protocol_ok
;;

let with_fixture
      ?test_root
      ?(wrap_env = Fn.id)
      ?(prompt_text = parent_prompt)
      ?(max_root_agents = 1)
      ?(on_b_current = ignore)
      f
  =
  let polling = Polling.create () in
  let provider = Provider.create () in
  let phase = ref "actual public parent/child construction" in
  with_recoverable_fixture
    ?test_root
    ~timeout:90.
    ~prompt_text
    ~max_root_agents
    ~sources:
      [ "schema.json", "true"
      ; "direct.txt", "592 direct.txt real native sentinel"
      ; "nested.txt", "592 nested.txt real native sentinel"
      ; "event.txt", "592 event.txt real native sentinel"
      ; "none.txt", "592 none.txt real native sentinel"
      ; "policy.txt", "592 parent policy real native sentinel"
      ]
    ~wrap_env:(fun env -> wrap_env (Polling.wrap polling env))
    ~post_stream:(Provider.post_stream provider)
    (fun env _sw root daemon _policy parent parent_runtime _revision counters ->
       protect_cleanup
         ~finally:(fun () ->
           Option.iter provider.ambient ~f:Provider.release;
           polling.resume ())
         (fun () ->
            try
              let created_at =
                polling.wall_now ()
                |> Time_ns.Span.of_sec
                |> Time_ns.of_span_since_epoch
                |> P.Timestamp.of_time_ns
              in
              let child =
                create_child env root daemon parent parent_runtime ~created_at
              in
              let child_session_id =
                Option.value_exn child.store_handle
                |> Agent_store.Session_store.Handle.session_id
              in
              let close_child () =
                protect_cleanup
                  ~finally:(fun () -> Agent_server.Session_registry.close_entry child)
                  (fun () ->
                     let actual =
                       Registry.remove (Daemon.registry daemon) child_session_id
                       |> Option.value_exn
                     in
                     assert (phys_equal actual.actor child.actor);
                     assert (phys_equal actual.runtime child.runtime))
              in
              protect_cleanup ~finally:close_child (fun () ->
                Eio.Time.with_timeout_exn env#clock 30. (fun () ->
                  let rec startup () =
                    let current = Warm.settled child in
                    match
                      ( current.active_operation
                      , List.filter current.moderator_executions ~f:(fun receipt ->
                          E.equal_phase receipt.context.phase Session_start) )
                    with
                    | None, [ { E.status = Completed _; _ } ] -> ()
                    | (None | Some _), _ ->
                      Eio.Fiber.yield ();
                      startup ()
                  in
                  startup ());
                phase := "join actual scheduler passes after startup";
                Eio.Time.with_timeout_exn env#clock 30. (fun () ->
                  Polling.park polling daemon);
                let parent_writer, _ =
                  Actor.attach parent.actor ~mode:Read_write ~subscribe:false
                  |> protocol_ok
                in
                protect_cleanup
                  ~finally:(fun () ->
                    Actor.detach parent.actor parent_writer.id |> protocol_ok)
                  (fun () ->
                     let child_writer, _ =
                       Actor.attach child.actor ~mode:Read_write ~subscribe:false
                       |> protocol_ok
                     in
                     protect_cleanup
                       ~finally:(fun () ->
                         Actor.detach child.actor child_writer.id |> protocol_ok)
                       (fun () ->
                          let b_live = Atomic.make true in
                          let b =
                            Operator_authorization.guarded
                              ~principal:(principal_with_id "pri_592_B")
                              ~is_current:(fun () ->
                                let current = Atomic.get b_live in
                                (* Test-only observation; callers keep this pure and
                                   nonyielding. It grants no authority. *)
                                on_b_current current;
                                current)
                          in
                          let a =
                            Operator_authorization.nonexpiring_static
                              (principal_with_id "pri_592_A")
                          in
                          let fixture =
                            { env :> Eio_unix.Stdenv.base
                            ; daemon
                            ; parent
                            ; child
                            ; parent_writer = parent_writer.id
                            ; child_writer = child_writer.id
                            ; parent_runtime
                            ; child_runtime = Warm.loaded child
                            ; parent_initial = Warm.settled parent
                            ; child_initial = Warm.settled child
                            ; a
                            ; b
                            ; b_live
                            ; provider
                            ; counters
                            ; phase
                            }
                          in
                          f fixture)))
            with
            | exn ->
              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
              (try Eio.traceln "592 Caller fixture phase: %s" !phase with
               | _ -> ());
              Exn.raise_with_original_backtrace exn backtrace))
;;
