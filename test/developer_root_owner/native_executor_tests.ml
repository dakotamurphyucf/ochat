open! Core
module P = Agent_protocol
module H = P.Host_invocation
module I = P.Invocation
module R = Agent_store.Developer_run_store
module O = Agent_server.Developer_root_owner
module X = Agent_server.Developer_native_executor
module C = Chat_response.Tool_capability
module T = Root_owner_tests

let ok = T.ok

let registry ~custom calls =
  let module Definition = struct
    type input = string

    let name = "read_file"
    let description = Some "selected native fixture"
    let type_ = if custom then "custom" else "function"
    let parameters = `Object [ "type", `String (if custom then "string" else "object") ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun input ->
         Int.incr calls;
         assert (String.equal input "{}");
         Openai.Responses.Tool_output.Output.Text "private result")
  in
  C.create
    ~owner:"selected-native-test"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "selected-resource")
    [ Chatmd_shell_spec.Source_ref.digest "selected-native-v1", implementation ]
  |> ok
;;

let pair_registry ~on_parent parent_calls child_calls =
  let module Parent = struct
    type input = string

    let name = "parent_tool"
    let description = Some "nested parent fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let module Child = struct
    type input = string

    let name = "child_tool"
    let description = Some "nested child fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let parent =
    Ochat_function.create_function
      (module Parent)
      (fun _ ->
         Int.incr parent_calls;
         on_parent ();
         Openai.Responses.Tool_output.Output.Text "parent")
  in
  let child =
    Ochat_function.create_function
      (module Child)
      (fun _ ->
         Int.incr child_calls;
         Openai.Responses.Tool_output.Output.Text "child")
  in
  C.create
    ~owner:"nested-selected-native-test"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "nested-resource")
    [ Chatmd_shell_spec.Source_ref.digest "parent-native-v1", parent
    ; Chatmd_shell_spec.Source_ref.digest "child-native-v1", child
    ]
  |> ok
;;

let invocation owner selected reference ~custom =
  H.create_developer
    { H.id = P.Id.Invocation.create ()
    ; owner = P.Execution_owner.Developer owner
    ; origin = Script
    ; parent_invocation = None
    ; parent_job = None
    ; tool_name = reference.C.name
    ; implementation_revision = reference.implementation_revision
    ; capability_fingerprint = C.fingerprint selected
    ; input = (if custom then `String "{}" else `Object [])
    ; created_at = T.now ()
    ; deadline = None
    }
  |> ok
;;

let with_executor
      ?(checkpoint = ignore)
      ?(on_current = ignore)
      ?(wrap_env = Fn.id)
      ~run_name
      ~make_selected
      f
  =
  T.with_store ~checkpoint ~wrap_env (fun env sw store ->
    let captured = T.capture () in
    let owner = T.owner run_name 0 in
    let handle, _ =
      R.admit_epoch
        store
        ~owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
        ~previous:None
      |> ok
    in
    let selected = make_selected env in
    let limits =
      O.Limits.create
        ~max_active_runs:2
        ~max_staged_jobs:2
        ~max_native_occupancy:2
        ~max_active_workers:1
        ~max_quarantined_workers:2
      |> Or_error.ok_exn
    in
    let owners =
      O.Registry.create ~sw ~store ~capacity:(T.capacity ()) ~now:T.now ~limits
    in
    let root =
      O.Registry.admit
        owners
        ~actor:
          (Operator_authorization.guarded ~principal:T.principal ~is_current:(fun () ->
             on_current ();
             true))
        ~capture:captured
        ~registry:selected
        ~selected
        ~job_policy:Chat_response.One_off_request.default_policy
        ~handle
        ~workspace_conflict_domain:"selected-native-test"
      |> ok
    in
    let executor = X.create ~root |> ok in
    let result = f ~env ~root ~executor ~handle ~owner ~selected in
    O.close_and_wait root |> ok;
    O.Registry.close_and_wait owners |> ok;
    result)
;;

let managed_registry env =
  let module Managed = Chat_response.Managed_tool_registry in
  let base =
    C.create
      ~owner:"selected-managed-test"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "managed-resource")
      []
    |> ok
  in
  let dir = Eio.Stdenv.cwd env in
  let source_loader =
    Source_loader.captured_filesystem
      ~root:dir
      ~sources:
        [ "input.json", {|{"type":"object"}|}; "output.json", {|{"type":"object"}|} ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir
      ~source_loader
      {|<script id="owned" language="chatml" kind="tool">
let run ctx input = Task.pure(`Complete(input))
</script>
<tool name="summary" type="chatml" script="owned" entrypoint="run" input_schema="input.json" output_schema="output.json"/>|}
  in
  let managed =
    Managed.prepare ~env ~owner:"selected-managed-test" ~capabilities:base elements
    |> Result.map_error ~f:(fun diagnostics ->
      String.concat
        ~sep:"; "
        (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string))
    |> Result.ok_or_failwith
  in
  managed, Managed.capabilities managed
;;

let%test_unit "selected native Function and Custom share durable dispatch policy" =
  List.iter [ false; true ] ~f:(fun custom ->
    let calls = ref 0 in
    with_executor
      ~run_name:(if custom then "dvr_selected_custom" else "dvr_selected_function")
      ~make_selected:(fun _env -> registry ~custom calls)
      (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
         let reference = List.hd_exn (C.references selected) in
         let admitted = invocation owner selected reference ~custom in
         let terminal =
           X.run_root
             executor
             ~managed:None
             ~reference
             ~invocation:admitted
             ~authorize:(fun _ _ -> Ok ())
             ~prepare_output:(fun _ -> Ok (`String "disclosed"))
           |> ok
         in
         assert (Int.equal !calls 1);
         assert (
           I.equal_status (H.status terminal) (Resolved (Complete (`String "disclosed"))));
         let observed = R.observe handle |> ok in
         let prior =
           R.find_invocation observed (H.context admitted).id
           |> R.Invocation_lookup.prior
           |> Option.value_exn
         in
         assert (
           H.equal_context
             (H.context terminal)
             (H.context (R.Invocation_observation.value prior)));
         assert (
           I.equal_status
             (H.status terminal)
             (H.status (R.Invocation_observation.value prior)));
         let after_wait = invocation owner selected reference ~custom in
         let revoked =
           X.run_root
             executor
             ~managed:None
             ~reference
             ~invocation:after_wait
             ~authorize:(fun _ _ -> O.narrow_selection root ~names:[])
             ~prepare_output:(fun _ -> Ok (`String "disclosed"))
           |> ok
         in
         (match H.status revoked with
          | Resolved (Fail _) -> ()
          | _ -> failwith "replaced selected capability reached a native effect");
         assert (Int.equal !calls 1)))
;;

let%test_unit "managed selected IDs survive Complete and staging expires with callback" =
  let managed_definition = ref None in
  with_executor
    ~run_name:"dvr_selected_managed"
    ~make_selected:(fun env ->
      let managed, selected = managed_registry env in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root:_ ~executor ~handle ~owner ~selected ->
       let module M = Chat_response.Managed_tool_registry in
       let managed = Option.value_exn !managed_definition in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let revision = M.execution_revision managed binding |> ok in
       let admitted =
         H.create_developer
           { (H.context (invocation owner selected reference ~custom:false)) with
             implementation_revision = revision
           }
         |> ok
       in
       let prepared =
         Chat_response.One_off_script.prepare_in_domain
           ~env
           ~capabilities:selected
           ~tools:[]
           ~source:"let main input = Task.pure(input)"
           ()
         |> ok
       in
       let request =
         Chat_response.Background_request.script
           ~prepared
           ~input:`Null
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let stage_after = ref None in
       let selected_ids = ref [] in
       let fail_after_seal = ref false in
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun _execution _borrowed ~effect_entry ~stage_background continuation ->
               stage_after := Some stage_background;
               match effect_entry () with
               | Error _ ->
                 Error
                   (I.Fail
                      { code = "expired"
                      ; message = ""
                      ; retryable = false
                      ; details = `Null
                      })
               | Ok () ->
                 let id = stage_background ~request ~checkpoint:ignore |> ok in
                 selected_ids := id :: !selected_ids;
                 let returned =
                   continuation
                     ~validate_work:(fun _ -> Ok ())
                     ~outcome:(I.Complete (`Object []))
                     ~selected_jobs:[ id ]
                 in
                 if !fail_after_seal
                 then
                   Error
                     (I.Fail
                        { code = "late managed cleanup"
                        ; message = "the sealed result remains canonical"
                        ; retryable = false
                        ; details = `Null
                        })
                 else returned)
         }
       in
       let run admitted =
         X.run_root
           executor
           ~managed:(Some service)
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(function
             | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
             | _ -> Error (P.Error.invalid_request "unexpected managed result"))
         |> ok
       in
       let terminal = run admitted in
       assert (I.equal_status (H.status terminal) (Resolved (Complete (`Object []))));
       let id = List.hd_exn !selected_ids in
       assert (Option.is_some (R.find_job (R.observe handle |> ok) id));
       assert (
         Result.is_error ((Option.value_exn !stage_after) ~request ~checkpoint:ignore));
       fail_after_seal := true;
       let second =
         H.create_developer { (H.context admitted) with id = P.Id.Invocation.create () }
         |> ok
       in
       let retained = run second in
       assert (I.equal_status (H.status retained) (Resolved (Complete (`Object []))));
       assert (List.length !selected_ids = 2);
       assert (
         Option.is_some (R.find_job (R.observe handle |> ok) (List.hd_exn !selected_ids))))
;;

let%test_unit "early selected read failure never enters handler and releases scope" =
  let armed = ref false in
  let fail_next_checkpoint = ref false in
  let failure = Failure "selected read failed before dispatch" in
  let checkpoint () =
    if !fail_next_checkpoint
    then (
      fail_next_checkpoint := false;
      raise failure)
  in
  let on_current () =
    if !armed
    then (
      armed := false;
      fail_next_checkpoint := true)
  in
  let calls = ref 0 in
  with_executor
    ~checkpoint
    ~on_current
    ~run_name:"dvr_selected_early_read"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let run () =
         X.run_root
           executor
           ~managed:None
           ~reference
           ~invocation:admitted
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(fun _ -> Ok (`String "disclosed"))
       in
       armed := true;
       (match run () with
        | exception actual -> assert (phys_equal actual failure)
        | Ok _ | Error _ -> failwith "early Store exception identity was lost");
       assert (Int.equal !calls 0);
       assert (
         Option.is_none
           (R.find_invocation (R.observe handle |> ok) (H.context admitted).id
            |> R.Invocation_lookup.prior));
       ignore (O.current root |> ok : O.Current.t);
       ignore (run () |> ok : H.t);
       assert (Int.equal !calls 1))
;;

let%test_unit "uncertain selected dispatch retains exact failure until owner recovery" =
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  let fail_before_rename = ref false in
  let fail_recovery_checkpoint = ref false in
  let original = Failure "original selected dispatch failure" in
  let later = Failure "later selected recovery failure" in
  let calls = ref 0 in
  with_executor
    ~checkpoint:(fun () ->
      if !fail_recovery_checkpoint
      then (
        fail_recovery_checkpoint := false;
        raise later))
    ~run_name:"dvr_selected_uncertain"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    ~wrap_env:(fun env ->
      Developer_run_store_test_suite.Developer_run_store_tests
      .fault_after_authority_rename_env
        ~before_rename:(fun () ->
          if !fail_before_rename
          then (
            fail_before_rename := false;
            fail_recovery_checkpoint := true;
            raise original))
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked)
    (fun ~env:_ ~root ~executor ~handle ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       rename_armed := true;
       assert (
         Result.is_error
           (X.run_root
              executor
              ~managed:None
              ~reference
              ~invocation:admitted
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "disclosed"))));
       assert (not !rename_armed);
       assert (Int.equal !calls 0);
       sync_blocked := false;
       let id = (H.context admitted).id in
       let prior () =
         R.find_invocation (R.observe handle |> ok) id
         |> R.Invocation_lookup.prior
         |> Option.value_exn
         |> R.Invocation_observation.value
       in
       assert (I.equal_status (H.status (prior ())) Dispatching);
       assert (Result.is_error (O.current root));
       O.reconcile_pending root |> ok;
       (match H.status (prior ()) with
        | Resolved (Fail _) -> ()
        | _ -> failwith "retained native failure was not durably resolved");
       assert (Int.equal !calls 0);
       ignore (O.current root |> ok : O.Current.t);
       let second =
         H.create_developer { (H.context admitted) with id = P.Id.Invocation.create () }
         |> ok
       in
       fail_before_rename := true;
       (match
          X.run_root
            executor
            ~managed:None
            ~reference
            ~invocation:second
            ~authorize:(fun _ _ -> Ok ())
            ~prepare_output:(fun _ -> Ok (`String "disclosed"))
        with
        | exception actual -> assert (phys_equal actual original)
        | Ok _ | Error _ -> failwith "recovery replaced the original dispatch exception");
       assert (Int.equal !calls 0);
       O.reconcile_pending root |> ok;
       assert (
         Option.is_none
           (R.find_invocation (R.observe handle |> ok) (H.context second).id
            |> R.Invocation_lookup.prior));
       ignore (O.current root |> ok : O.Current.t))
;;

let%test_unit "absent native recovery cannot release another scope's dispatch writer" =
  let pause_first_recovery = ref false in
  let reached, reached_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let checkpoint () =
    if !pause_first_recovery
    then (
      pause_first_recovery := false;
      Eio.Promise.resolve reached_u ();
      Eio.Promise.await release)
  in
  let calls = ref 0 in
  with_executor
    ~checkpoint
    ~run_name:"dvr_selected_distinct_dispatch_owner"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env:_ ~root ~executor:_ ~handle:_ ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let first = invocation owner selected reference ~custom:false in
       let second = invocation owner selected reference ~custom:false in
       let first_scope =
         O.with_native_registration root ~invocation:first (fun ~scope -> Ok scope) |> ok
       in
       let dispatched = H.dispatch second |> ok in
       Eio.Switch.run (fun sw ->
         let ready, ready_u = Eio.Promise.create () in
         let go, go_u = Eio.Promise.create () in
         let finished, finished_u = Eio.Promise.create () in
         Eio.Fiber.fork ~sw (fun () ->
           let result =
             O.with_native_registration root ~invocation:second (fun ~scope ->
               Eio.Promise.resolve ready_u ();
               Eio.Promise.await go;
               let revision =
                 O.Native_terminal.save_dispatch scope ~expected_revision:None ~dispatched
                 |> ok
               in
               let selected, terminal =
                 O.with_native_callback scope ~dispatched (fun ~sw:_ ->
                   let selected =
                     O.Native_terminal.seal scope ~outcome:(I.Complete `Null) ~selected:[]
                     |> ok
                   in
                   let terminal = H.resolve dispatched (I.Complete `Null) |> ok in
                   Ok (selected, terminal))
                 |> ok
               in
               ignore
                 (O.Native_terminal.submit scope ~terminal ~selected:(Some selected) |> ok
                  : int);
               Ok revision)
           in
           Eio.Promise.resolve finished_u result);
         Eio.Promise.await ready;
         let witness = O.current root |> ok in
         let first_finished, first_finished_u = Eio.Promise.create () in
         pause_first_recovery := true;
         Eio.Fiber.fork ~sw (fun () ->
           Eio.Promise.resolve
             first_finished_u
             (O.Native_terminal.reconcile_dispatch first_scope));
         Eio.Promise.await reached;
         Eio.Promise.resolve go_u ();
         Eio.Fiber.yield ();
         assert (Result.is_error (O.check_effect_now root witness));
         Eio.Promise.resolve release_u ();
         (match Eio.Promise.await first_finished |> ok with
          | O.Native_terminal.Absent -> ()
          | Installed _ | Uncertain -> failwith "untouched scope acquired dispatch");
         ignore (Eio.Promise.await finished |> ok : int));
       assert (Int.equal !calls 0);
       ignore (O.current root |> ok : O.Current.t))
;;

let%test_unit "nested selected child uses live direct-parent proof and joins" =
  let parent_calls = ref 0 in
  let child_calls = ref 0 in
  let on_parent = ref (fun () -> failwith "parent executor was not installed") in
  with_executor
    ~run_name:"dvr_selected_nested"
    ~make_selected:(fun _env ->
      pair_registry ~on_parent:(fun () -> !on_parent ()) parent_calls child_calls)
    (fun ~env:_ ~root:_ ~executor ~handle ~owner ~selected ->
       let reference name = C.find selected ~name |> ok |> C.reference in
       let root = invocation owner selected (reference "parent_tool") ~custom:false in
       let child =
         H.create_developer
           { (H.context
                (invocation owner selected (reference "child_tool") ~custom:false))
             with
             parent_invocation = Some (H.context root).id
           }
         |> ok
       in
       (on_parent
        := fun () ->
             let terminal =
               X.run_child
                 executor
                 ~managed:None
                 ~reference:(reference "child_tool")
                 ~invocation:child
                 ~authorize:(fun _ _ -> Ok ())
                 ~prepare_output:(fun _ -> Ok (`String "child disclosed"))
               |> ok
             in
             assert (
               I.equal_status
                 (H.status terminal)
                 (Resolved (Complete (`String "child disclosed")))));
       let parent =
         X.run_root
           executor
           ~managed:None
           ~reference:(reference "parent_tool")
           ~invocation:root
           ~authorize:(fun _ _ -> Ok ())
           ~prepare_output:(fun _ -> Ok (`String "parent disclosed"))
         |> ok
       in
       assert (
         I.equal_status
           (H.status parent)
           (Resolved (Complete (`String "parent disclosed"))));
       assert (Int.equal !parent_calls 1 && Int.equal !child_calls 1);
       assert (
         Result.is_error
           (X.run_child
              executor
              ~managed:None
              ~reference:(reference "child_tool")
              ~invocation:
                (H.create_developer
                   { (H.context child) with id = P.Id.Invocation.create () }
                 |> ok)
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "child disclosed"))));
       let observed = R.observe handle |> ok in
       List.iter [ root; child ] ~f:(fun admitted ->
         let durable =
           R.find_invocation observed (H.context admitted).id
           |> R.Invocation_lookup.prior
           |> Option.value_exn
           |> R.Invocation_observation.value
         in
         match H.status durable with
         | Resolved (Complete _) -> ()
         | _ -> failwith "nested selected invocation did not finish durably"))
;;
