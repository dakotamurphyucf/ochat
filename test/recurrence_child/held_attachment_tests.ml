open! Core
open Support
module Cold = Activation_tests
module Owner = Agent_server.Runtime_owner
module Handle = Agent_client.Session_handle
module Held = Actor.Held_run
module Res = Openai.Responses

module Cut = struct
  type t =
    | Original_revoked
    | Attachment_before_sync
    | Owner_control
  [@@deriving sexp_of]
end

module Attachment_fault = struct
  type t =
    { mutable session : P.Id.Session.t option
    ; mutable candidate : (Agent_store.Transaction.t * P.Session.Attachment.t) option
    }

  let create () = { session = None; candidate = None }

  let reached t env path =
    match t.session with
    | None -> ()
    | Some session ->
      (* Inspect only bytes through the original environment. This callback is
         inside the real writer and must not call the Actor or runtime owner. *)
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> store_ok
      in
      (match List.last scan.entries with
       | Some entry when Int.equal (Agent_store.Frame.flags entry.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
           |> store_ok
         in
         if P.Id.Session.equal session transaction.session_id
         then (
           let delta =
             A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           List.iter (Authored_fault_tests.atoms delta) ~f:(function
             | Attachment_added attachment
               when P.Session.equal_attachment_mode attachment.mode Read_write ->
               assert (P.Id.Session.equal attachment.session_id session);
               assert (Option.is_none attachment.owner_lease);
               t.session <- None;
               t.candidate <- Some (transaction, attachment);
               raise (Core_unix.Unix_error (EIO, "held attachment before sync", path))
             | _ -> ()))
       | Some _ | None -> ())
  ;;
end

let state_json current = T.state_document current |> store_ok |> D.Document.json

let permission_in child (expected : P.Permission.t) =
  List.find_exn (state child).permissions ~f:(fun actual ->
    P.Id.Permission.equal actual.id expected.id)
;;

let assert_pending child permission =
  let actual = permission_in child permission in
  [%test_eq: P.Permission.state] Pending actual.state;
  assert (
    Jsonaf.exactly_equal (P.Permission.to_json permission) (P.Permission.to_json actual));
  assert (List.is_empty (state child).grants)
;;

let run_in (current : A.Session_state.t) held =
  A.Run_state.find (Option.value_exn current.run_state) (Held.admission held).run_id
  |> Option.value_exn
;;

let assert_prefix child held (counters : Cold.counters) =
  let current = state child in
  let run = run_in current held in
  [%test_eq: int] 1 !(counters.providers);
  assert (Option.is_none current.active_operation);
  assert (List.is_empty current.grants);
  let job = List.hd_exn current.jobs in
  [%test_eq: int] 1 (List.length current.jobs);
  assert (
    Option.equal
      Jsonaf.exactly_equal
      (Option.bind job.result ~f:(Jsonaf.member "final_text"))
      (Some (`String "520-constructor-model-result")));
  (match job.status with
   | Succeeded -> ()
   | _ -> raise_s [%sexp "first Model did not complete", (job : P.Job.t)]);
  assert (Option.equal P.Inference_origin.equal job.inference_origin (Some (Run run.id)));
  let work = A.Run_job_occurrence.work job |> protocol_ok in
  assert (List.mem run.owned_work work ~equal:P.Run_work.equal);
  assert (
    List.exists run.terminal_work ~f:(fun terminal ->
      P.Run_work.equal terminal.work work
      && P.Run_work.Terminal.equal_outcome terminal.outcome Succeeded));
  (match run.lifecycle with
   | Admitted | Active | Waiting _ -> ()
   | Terminal _ -> raise_s [%sexp "startup terminated at first Ask", (run : P.Run.t)]);
  [%test_eq: int] 1 (List.length current.invocations);
  let native = List.hd_exn current.invocations in
  [%test_eq: string] "read_file" native.context.tool_name;
  (match native.status with
   | Dispatching -> ()
   | _ -> raise_s [%sexp (native : P.Invocation.t)]);
  assert (
    Option.equal P.Inference_origin.equal native.inference_origin (Some (Run run.id)));
  let startup =
    List.find_exn current.moderator_executions ~f:(fun receipt ->
      Option.equal
        P.Id.Moderator_execution.equal
        native.parent_event
        (Some receipt.context.id))
  in
  assert (P.Moderator_execution.equal_phase startup.context.phase Session_start);
  (match startup.status with
   | Running -> ()
   | _ -> raise_s [%sexp (startup : P.Moderator_execution.t)]);
  current
;;

let assert_same_execution before after held =
  assert (P.Run.equal (run_in before held) (run_in after held));
  assert (
    List.equal P.Invocation.equal before.A.Session_state.invocations after.invocations);
  assert (
    List.equal
      P.Moderator_execution.equal
      before.moderator_executions
      after.moderator_executions);
  assert (
    List.equal
      (fun left right -> Jsonaf.exactly_equal (P.Job.to_json left) (P.Job.to_json right))
      before.jobs
      after.jobs);
  assert (Option.equal Jsonaf.exactly_equal before.moderator after.moderator);
  [%test_eq: int64]
    before.counters.owner_lease_generation
    after.counters.owner_lease_generation
;;

let respond handle permission choice =
  Handle.respond_permission
    handle
    ~permission_id:permission.P.Permission.id
    ~permission_generation:permission.generation
    ~choice
    ~reason:None
;;

let deny_and_join child permission handle starting ~counters =
  let completed_jobs = (state child).jobs in
  let denied = respond handle permission Deny |> protocol_ok in
  [%test_eq: P.Permission.state] Denied denied.state;
  (match denied.resolution with
   | Some { choice = Deny; principal_id = Some id; _ } ->
     assert (P.Id.Principal.equal id principal.id)
   | resolution -> raise_s [%sexp (resolution : P.Permission.resolution option)]);
  (match Eio.Promise.await_exn starting with
   | `Returned (Error _) -> ()
   | `Returned (Ok receipt) ->
     raise_s [%sexp "denied startup returned Admission", (receipt : P.Run_receipt.t)]
   | `Cancelled -> failwith "denied startup required external cancellation");
  let actual = permission_in child permission in
  assert (Jsonaf.exactly_equal (P.Permission.to_json denied) (P.Permission.to_json actual));
  let current = state child in
  [%test_eq: int] 1 !(counters.Cold.providers);
  assert (
    List.equal
      (fun left right -> Jsonaf.exactly_equal (P.Job.to_json left) (P.Job.to_json right))
      completed_jobs
      current.jobs);
  [%test_eq: int] 1 (List.length current.jobs);
  (match (List.hd_exn current.jobs).status with
   | Succeeded -> ()
   | _ -> raise_s [%sexp (current.jobs : P.Job.t list)]);
  [%test_eq: int] 1 (List.length current.invocations);
  (match (List.hd_exn current.invocations).status with
   | Resolved (Fail error) | Published (Fail error) ->
     [%test_eq: string] "invocation.permission_denied" error.code
   | status ->
     raise_s [%sexp "denied native did not settle", (status : P.Invocation.status)]);
  assert (List.is_empty current.grants)
;;

let with_pending_authored_start cut f =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
      Mirage_crypto_rng_unix.use_default ();
      let root = T.temporary_root env in
      protect_cleanup
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           let prompt = Filename.concat root "parent.chatmd" in
           let child_prompt = Filename.concat root "authored-child.chatmd" in
           List.iter
             [ ( prompt
               , {|<developer>Actual recurrence controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
               )
             ; child_prompt, Cold.Authored_case.prompt V1_children
             ; ( Filename.concat root "activation-recipe.chatmd"
               , "<developer>520_CONSTRUCTOR_RECIPE. Return the offline fixture \
                  response.</developer>" )
             ; ( Filename.concat root "activation-native.txt"
               , "520 actual allowed native result" )
             ]
             ~f:(fun (path, contents) ->
               Eio.Path.save
                 ~create:(`Exclusive 0o600)
                 Eio.Path.(Eio.Stdenv.fs env / path)
                 contents);
           let counters : Cold.counters = { constructions = ref 0; providers = ref 0 } in
           let policy =
             T.inference_policy
               ~default_model:"fixture-model"
               ~post_stream:(fun ~sw:_ ~inputs ->
                 Int.incr counters.providers;
                 let transcript =
                   `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
                 in
                 Cold.completed_response
                   (if String.is_substring transcript ~substring:"536_NATIVE_CHILD"
                    then "536-native-child-result"
                    else "520-constructor-model-result"))
           in
           let profile =
             { T.permission_profile with
               tool_default = Ask
             ; approval_timeout_ms = Some 30000
             ; approval_fallback = Deny
             }
           in
           let configuration = T.config ~profile root root prompt in
           let configured = List.hd_exn configuration.prompts in
           let configuration =
             { configuration with
               prompts =
                 configuration.prompts
                 @ [ { configured with
                       id = "cold-activation-target"
                     ; path = child_prompt
                     }
                   ]
             }
           in
           let fault = Attachment_fault.create () in
           let daemon_env =
             match cut with
             | Cut.Original_revoked | Owner_control -> env
             | Attachment_before_sync ->
               Crash_fault_io.wrap
                 env
                 ~boundary:Before_sync
                 ~matches:(fun path ->
                   String.is_substring path ~substring:"/journal/"
                   && String.is_suffix path ~suffix:".log")
                 ~reached:(Attachment_fault.reached fault env)
           in
           with_daemon
             ~policy
             ~on_runtime_construction:(fun () -> Int.incr counters.constructions)
             daemon_env
             root
             configuration
             ~provider_calls:counters.providers
             (fun sw daemon ->
                let connection = T.connection daemon principal in
                let handles = ref [] in
                protect_cleanup
                  ~finally:(fun () ->
                    List.iter !handles ~f:Handle.close;
                    Agent_client.Connection.close connection)
                  (fun () ->
                     T.initialize connection;
                     let created, _ =
                       T.create_session ~start_immediately:true connection
                     in
                     let parent = loaded daemon created.id in
                     start_parent_settled env sw connection parent;
                     let original_live = ref true in
                     let original =
                       Operator_authorization.guarded ~principal ~is_current:(fun () ->
                         !original_live)
                     in
                     let try_attach child mode =
                       let result =
                         Handle.attach
                           ~sw
                           ~clock:(Eio.Stdenv.clock env)
                           ~connection
                           ~session_id:(state child).identity.session_id
                           ~mode
                           ~subscribe:false
                           ()
                       in
                       Result.map result ~f:(fun handle ->
                         handles := handle :: !handles;
                         handle)
                     in
                     let attach child mode = try_attach child mode |> protocol_ok in
                     with_planner env sw daemon parent (fun () ->
                       Cold.with_cold_held
                         ~catalog_target:true
                         ~before_prepare:(fun child ->
                           ignore (attach child Read_write : Handle.t))
                         env
                         daemon
                         parent
                         ~root_file:child_prompt
                         ~counters
                         ~original
                         (fun child prepared held checked _intent _attempt ->
                            Eio.Switch.run (fun startup_sw ->
                              let cancellation, cancel_u = Eio.Promise.create () in
                              let starting =
                                Eio.Fiber.fork_promise ~sw:startup_sw (fun () ->
                                  try
                                    Eio.Cancel.sub (fun context ->
                                      Eio.Promise.resolve cancel_u context;
                                      `Returned
                                        (Owner.start_held_authored_runtime
                                           child.runtime
                                           ~prepared
                                           ~held
                                           ~checked:
                                             (Agent_session.Session_actor
                                              .Recurrence_activation
                                              .Checked
                                              .admitted
                                                checked)))
                                  with
                                  | Eio.Cancel.Cancelled _ -> `Cancelled)
                              in
                              let cancel_start () =
                                if Option.is_none (Eio.Promise.peek starting)
                                then
                                  Eio.Cancel.cancel (Eio.Promise.await cancellation) Exit;
                                Eio.Cancel.protect (fun () ->
                                  ignore (Eio.Promise.await_exn starting))
                              in
                              protect_cleanup ~finally:cancel_start (fun () ->
                                let rec await_pending () =
                                  let current = state child in
                                  match
                                    List.filter current.permissions ~f:(fun permission ->
                                      P.Permission.equal_state permission.state Pending)
                                  with
                                  | [ permission ] -> permission
                                  | [] ->
                                    (match Eio.Promise.peek starting with
                                     | None ->
                                       Eio.Fiber.yield ();
                                       await_pending ()
                                     | Some _ ->
                                       (match Eio.Promise.await_exn starting with
                                        | `Returned result ->
                                          ignore
                                            (Cold.authored_start_result
                                               child
                                               ~case:V1_children
                                               result
                                             : P.Run_receipt.t)
                                        | `Cancelled ->
                                          failwith "startup cancelled before Pending");
                                       failwith "startup completed without real Pending")
                                  | pending ->
                                    raise_s [%sexp (pending : P.Permission.t list)]
                                in
                                let permission = await_pending () in
                                [%test_eq: string] "read_file" permission.tool_name;
                                assert_pending child permission;
                                let before = assert_prefix child held counters in
                                f
                                  ~child
                                  ~held
                                  ~permission
                                  ~before
                                  ~counters
                                  ~original
                                  ~original_live
                                  ~attach
                                  ~try_attach
                                  ~fault
                                  ~starting
                                  ~cancel_start;
                                Ok ())))))))))
;;

exception Attachment_sync_failed of P.Error.t

let run cut =
  try
    with_pending_authored_start
      cut
      (fun
          ~child
           ~held
           ~permission
           ~before
           ~counters
           ~original
           ~original_live
           ~attach
           ~try_attach
           ~fault
           ~starting
           ~cancel_start
         ->
         match cut with
         | Cut.Original_revoked | Owner_control ->
           let mode, expected_error =
             match cut with
             | Original_revoked ->
               original_live := false;
               P.Session.Read_write, P.Error.Permission_denied
             | Owner_control -> P.Session.Owner_read_write, P.Error.Conflict
             | Attachment_before_sync -> assert false
           in
           let handle = attach child mode in
           let attachment = Handle.attachment handle in
           assert (Option.is_none attachment.owner_lease);
           assert (
             not
               (List.exists before.attachments ~f:(fun previous ->
                  P.Id.Attachment.equal previous.id attachment.id)));
           [%test_eq: bool]
             (match cut with
              | Original_revoked -> false
              | Owner_control -> true
              | Attachment_before_sync -> assert false)
             (Operator_authorization.is_current original);
           assert_pending child permission;
           assert_same_execution before (state child) held;
           (match respond handle permission Approve_once with
            | Error error ->
              [%test_eq: P.Error.code] expected_error error.code;
              [%test_eq: bool] false error.retryable
            | Ok approved ->
              raise_s [%sexp "invalidated startup approved", (approved : P.Permission.t)]);
           assert_pending child permission;
           assert_same_execution before (state child) held;
           deny_and_join child permission handle starting ~counters
         | Attachment_before_sync ->
           fault.session <- Some before.identity.session_id;
           (* Use the public attach directly here: a failed ACK must return no handle. *)
           let counts = !(counters.constructions), !(counters.providers) in
           let failure =
             match try_attach child Read_write with
             | Error failure ->
               [%test_eq: P.Error.code] Persistence_error failure.code;
               failure
             | Ok _ -> failwith "unconfirmed attachment returned a handle"
           in
           let transaction, attachment = Option.value_exn fault.candidate in
           assert (P.Id.Session.equal transaction.session_id before.identity.session_id);
           [%test_eq: int64]
             (Int64.succ before.counters.revision)
             transaction.session_revision;
           assert (Option.is_none fault.session);
           assert (Operator_authorization.is_current original);
           let assert_confirmed () =
             let confirmed = state child in
             assert (Jsonaf.exactly_equal (state_json before) (state_json confirmed));
             assert (
               not
                 (List.exists confirmed.attachments ~f:(fun actual ->
                    P.Id.Attachment.equal actual.id attachment.id)));
             assert_pending child permission;
             [%test_eq: int * int]
               counts
               (!(counters.constructions), !(counters.providers))
           in
           assert_confirmed ();
           (* Before_sync poisoned the actual writer. Cancel/join the real waiter,
           without pretending a later Deny or Run interruption could ACK. *)
           cancel_start ();
           assert_confirmed ();
           (* Keep the actual attach error primary through the complete lexical
            source cleanup. Returning Ok would instead promote its poisoned-
            writer retirement error over this already observed failure. *)
           raise (Attachment_sync_failed failure))
  with
  | Attachment_sync_failed failure ->
    (match cut with
     | Cut.Attachment_before_sync ->
       [%test_eq: P.Error.code] Persistence_error failure.code
     | Original_revoked | Owner_control ->
       raise_s
         [%sexp
           "unexpected attachment sync failure", (cut : Cut.t), (failure : P.Error.t)])
;;

let%expect_test "held startup attachment failures preserve authority and confirmed facts" =
  List.iter
    Cut.[ Original_revoked; Attachment_before_sync; Owner_control ]
    ~f:(fun cut ->
      try
        run cut;
        print_s [%sexp (cut : Cut.t)]
      with
      | exn -> Exn.reraise exn (Sexp.to_string [%sexp (cut : Cut.t)]));
  [%expect
    {|
    Original_revoked
    +native Invocation cleanup failed: (Corrupt
    + "canonical projection target cannot regress or alter equal-revision hints")
    +queued moderator cleanup failed: (Corrupt
    + "canonical projection target cannot regress or alter equal-revision hints")
    Attachment_before_sync
    Owner_control
    |}]
;;
