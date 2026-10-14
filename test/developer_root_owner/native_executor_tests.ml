open! Core
module P = Agent_protocol
module H = P.Host_invocation
module I = P.Invocation
module R = Agent_store.Developer_run_store
module O = Agent_server.Developer_root_owner
module X = Agent_server.Developer_native_executor
module D = Agent_session.Developer_invocation_owner.Selected
module C = Chat_response.Tool_capability
module T = Root_owner_tests
module Scope = Agent_session.Developer_job_scope

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

let stage_root_job env root selected =
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
  O.with_root_action root ~phase:R.Checkpoint.Active (fun ~action ~sw:_ ->
    let id = O.Action.stage_background action ~request ~checkpoint:ignore |> ok in
    let selected = O.Action.seal action ~selected:[ id ] |> ok in
    Ok (id, selected))
  |> ok
;;

let succeeded_claim claim =
  let state = P.Developer_job.state (R.Claim.job claim) in
  { state with
    status = P.Job.Succeeded
  ; completed_at = Some (T.now ())
  ; result = Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
  }
;;

let with_executor
      ?(checkpoint = ignore)
      ?(on_current = ignore)
      ?(wrap_env = Fn.id)
      ?(now = T.now)
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
    let owners = O.Registry.create ~sw ~store ~capacity:(T.capacity ()) ~now ~limits in
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

let%test_unit "claimed worker alone admits job child and inherited nested ancestry" =
  let parent_calls = ref 0 in
  let child_calls = ref 0 in
  let invoke_nested = ref (fun () -> ()) in
  let retained_active = ref None in
  let nested_id = ref None in
  with_executor
    ~run_name:"dvr_claimed_native_parent"
    ~make_selected:(fun _env ->
      pair_registry ~on_parent:(fun () -> !invoke_nested ()) parent_calls child_calls)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let job_id = stage_root_job env root selected in
       let parent_binding = C.find selected ~name:"parent_tool" |> ok in
       let parent_reference = C.reference parent_binding in
       let child_binding = C.find selected ~name:"child_tool" |> ok in
       let child_reference = C.reference child_binding in
       let parent_invocation =
         let admitted = invocation owner selected parent_reference ~custom:false in
         H.create_developer { (H.context admitted) with parent_job = Some job_id } |> ok
       in
       (invoke_nested
        := fun () ->
             let admitted = invocation owner selected child_reference ~custom:false in
             let child =
               H.create_developer
                 { (H.context admitted) with
                   parent_invocation = Some (H.context parent_invocation).id
                 ; parent_job = Some job_id
                 }
               |> ok
             in
             nested_id := Some (H.context child).id;
             ignore
               (X.run_child
                  executor
                  ~managed:None
                  ~reference:child_reference
                  ~invocation:child
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok (`String "child"))
                : (H.t, P.Error.t) result));
       let expected = R.observe handle |> ok in
       let completed =
         O.with_claimed_job
           root
           ~expected
           ~lookup:(R.find_job_exact expected job_id)
           (fun ~active ~sw:_ ->
              retained_active := Some active;
              let ancestor = O.job_parent root active |> ok in
              assert (P.Id.Job.equal (Scope.Parent.job_id ancestor) job_id);
              assert (Int.equal (Scope.Parent.attempt ancestor) 1);
              assert (Int.equal (Scope.Parent.nested_depth ancestor) 0);
              assert (
                Result.is_error
                  (Scope.Parent.check_now
                     ancestor
                     ~owner:(T.owner "dvr_foreign_claim" 0)
                     ~capture_fingerprint:
                       (Chatml_developer_capture.fingerprint (O.capture root))));
              assert (
                Result.is_error
                  (Scope.Parent.check_now
                     ancestor
                     ~owner
                     ~capture_fingerprint:
                       (Chatml_source.Digest.of_contents
                          "foreign capture"
                          ~checkpoint:ignore)));
              let bad_context =
                H.create_developer
                  { (H.context parent_invocation) with
                    id = P.Id.Invocation.create ()
                  ; parent_job = Some (P.Id.Job.create ())
                  }
                |> ok
              in
              assert (
                Result.is_error
                  (X.run_job_child
                     executor
                     ~active
                     ~managed:None
                     ~reference:parent_reference
                     ~invocation:bad_context
                     ~authorize:(fun _ _ -> Ok ())
                     ~prepare_output:(fun _ -> Ok (`String "parent"))));
              assert (Int.equal !parent_calls 0);
              let terminal =
                X.run_job_child
                  executor
                  ~active
                  ~managed:None
                  ~reference:parent_reference
                  ~invocation:parent_invocation
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok (`String "parent"))
                |> ok
              in
              assert (
                I.equal_status
                  (H.status terminal)
                  (Resolved (Complete (`String "parent"))));
              Ok (succeeded_claim (Scope.claim active)))
         |> ok
         |> Option.value_exn
       in
       assert (Int.equal !parent_calls 1);
       assert (Int.equal !child_calls 1);
       (match (P.Developer_job.state completed).status with
        | P.Job.Succeeded -> ()
        | _ -> failwith "claimed worker did not durably complete");
       let observed = R.observe handle |> ok in
       let child_id = Option.value_exn !nested_id in
       assert (
         Option.is_some (R.Invocation_lookup.prior (R.find_invocation observed child_id)));
       let expired = Option.value_exn !retained_active in
       let retry =
         H.create_developer
           { (H.context parent_invocation) with id = P.Id.Invocation.create () }
         |> ok
       in
       assert (
         Result.is_error
           (X.run_job_child
              executor
              ~active:expired
              ~managed:None
              ~reference:parent_reference
              ~invocation:retry
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok (`String "parent"))));
       assert (Int.equal !parent_calls 1))
;;

let%test_unit "job child permission wait rechecks selection and real deadline" =
  let expired_at = P.Timestamp.of_string "2026-10-13T00:00:02Z" |> ok in
  let deadline = P.Timestamp.of_string "2026-10-13T00:00:01Z" |> ok in
  List.iter [ `Revoke; `Expire ] ~f:(fun mode ->
    let calls = ref 0 in
    let clock = ref (T.now ()) in
    with_executor
      ~now:(fun () -> !clock)
      ~run_name:
        (match mode with
         | `Revoke -> "dvr_job_revoke"
         | `Expire -> "dvr_job_expire")
      ~make_selected:(fun _env -> registry ~custom:false calls)
      (fun ~env ~root ~executor ~handle ~owner ~selected ->
         let job_id = stage_root_job env root selected in
         let reference = List.hd_exn (C.references selected) in
         let admitted = invocation owner selected reference ~custom:false in
         let admitted =
           H.create_developer
             { (H.context admitted) with
               parent_job = Some job_id
             ; deadline = Some deadline
             }
           |> ok
         in
         let expected = R.observe handle |> ok in
         ignore
           (O.with_claimed_job
              root
              ~expected
              ~lookup:(R.find_job_exact expected job_id)
              (fun ~active ~sw:_ ->
                 let terminal =
                   X.run_job_child
                     executor
                     ~active
                     ~managed:None
                     ~reference
                     ~invocation:admitted
                     ~authorize:(fun _ _ ->
                       Eio.Fiber.yield ();
                       match mode with
                       | `Revoke -> O.narrow_selection root ~names:[]
                       | `Expire ->
                         clock := expired_at;
                         Ok ())
                     ~prepare_output:(fun _ -> Ok (`String "private"))
                   |> ok
                 in
                 (match H.status terminal with
                  | Resolved (Fail _) -> ()
                  | _ -> failwith "expired or revoked job parent reached native effect");
                 Ok (succeeded_claim (Scope.claim active)))
            |> ok
            : P.Developer_job.t option);
         assert (Int.equal !calls 0)))
;;

let%test_unit "cancelled claimed parent cannot start an effect while child joins" =
  let calls = ref 0 in
  let original = Failure "claimed parent cancelled before child joined" in
  let ready, ready_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let child_denied = ref false in
  with_executor
    ~run_name:"dvr_cancelled_job_parent"
    ~make_selected:(fun _env -> registry ~custom:false calls)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let job_id = stage_root_job env root selected in
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer { (H.context admitted) with parent_job = Some job_id } |> ok
       in
       let expected = R.observe handle |> ok in
       (match
          Eio.Cancel.sub (fun cancellation ->
            O.with_claimed_job
              root
              ~expected
              ~lookup:(R.find_job_exact expected job_id)
              (fun ~active ~sw ->
                 Eio.Fiber.fork ~sw (fun () ->
                   Eio.Cancel.protect (fun () ->
                     Eio.Promise.resolve ready_u ();
                     Eio.Promise.await release;
                     child_denied
                     := Result.is_error
                          (X.run_job_child
                             executor
                             ~active
                             ~managed:None
                             ~reference
                             ~invocation:admitted
                             ~authorize:(fun _ _ -> Ok ())
                             ~prepare_output:(fun _ -> Ok (`String "private")))));
                 Eio.Promise.await ready;
                 Eio.Cancel.protect (fun () ->
                   Eio.Cancel.cancel cancellation original;
                   Eio.Promise.resolve release_u ());
                 Ok (succeeded_claim (Scope.claim active))))
        with
        | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual original)
        | Ok _ | Error _ -> failwith "claimed worker cancellation identity was lost");
       assert !child_denied;
       assert (Int.equal !calls 0))
;;

let%test_unit "cancelled native parent switch rejects protected nested child" =
  let original = Failure "native parent switch failed before child join" in
  let ready, ready_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let child_denied = ref false in
  let effect_calls = ref 0 in
  with_executor
    ~run_name:"dvr_native_parent_switch"
    ~make_selected:(fun _env -> registry ~custom:false effect_calls)
    (fun ~env:_ ~root ~executor:_ ~handle:_ ~owner ~selected ->
       let reference = List.hd_exn (C.references selected) in
       let admitted = invocation owner selected reference ~custom:false in
       let child =
         let initial = invocation owner selected reference ~custom:false in
         H.create_developer
           { (H.context initial) with parent_invocation = Some (H.context admitted).id }
         |> ok
       in
       let issuer =
         D.create
           ~owner
           ~capture_fingerprint:(Chatml_developer_capture.fingerprint (O.capture root))
           ~ceiling:(O.native_ceiling root)
           ~selected:(fun () -> O.native_selected root)
           ~current:(fun ~owner:_ ~capture_fingerprint:_ ->
             Result.map (O.current root) ~f:ignore)
           ~now:(fun () -> O.native_now root)
         |> ok
       in
       let scope, completion =
         O.with_native_registration root ~invocation:admitted (fun ~scope ->
           let dispatch : D.dispatch_storage =
             { read = O.Native_terminal.read_dispatch scope
             ; save_dispatch = O.Native_terminal.save_dispatch scope
             }
           in
           let completion =
             D.with_invocation
               issuer
               ~parent:D.Root
               ~invocation:admitted
               ~dispatch
               ~parent_live_now:(fun () -> O.Native_terminal.effect_parent_live_now scope)
               ~frozen:(fun () -> None)
               (fun ~active ~dispatched ->
                  O.with_native_callback scope ~dispatched (fun ~sw ->
                    Eio.Fiber.fork ~sw (fun () ->
                      Eio.Cancel.protect (fun () ->
                        Eio.Promise.resolve ready_u ();
                        Eio.Promise.await release;
                        child_denied
                        := Result.is_error
                             (D.check_parent_now issuer ~parent:(D.Child active) child);
                        if not !child_denied then Int.incr effect_calls));
                    Eio.Promise.await ready;
                    Eio.Cancel.protect (fun () ->
                      Eio.Switch.fail sw original;
                      Eio.Promise.resolve release_u ());
                    Ok (I.Complete `Null, None)))
             |> ok
           in
           O.Native_terminal.remember_failure
             scope
             ~terminal:(D.Completion.terminal completion)
           |> ok;
           Ok (scope, completion))
         |> ok
       in
       assert !child_denied;
       assert (Int.equal !effect_calls 0);
       (match D.Completion.dispatch completion with
        | Installed _ -> ()
        | Awaiting_reconciliation -> failwith "failed native parent lost durable dispatch");
       let terminal_revision = O.Native_terminal.resolve_remembered scope |> ok in
       match
         D.Completion.finish completion ~settlement:(Terminal_saved terminal_revision)
       with
       | exception actual -> assert (phys_equal actual original)
       | Ok _ | Error _ -> failwith "failed native parent lost exception identity")
;;

let%test_unit "job expiry during managed preparation cannot reserve child capacity" =
  let module M = Chat_response.Managed_tool_registry in
  let managed_definition = ref None in
  let clock = ref (T.now ()) in
  let expired_at = P.Timestamp.of_string "2026-10-13T00:00:02Z" |> ok in
  let deadline = P.Timestamp.of_string "2026-10-13T00:00:01Z" |> ok in
  let denied = ref 0 in
  with_executor
    ~now:(fun () -> !clock)
    ~run_name:"dvr_job_prepare_expiry"
    ~make_selected:(fun env ->
      let managed, selected = managed_registry env in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let job_id = stage_root_job env root selected in
       let managed = Option.value_exn !managed_definition in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer
           { (H.context admitted) with
             implementation_revision = M.execution_revision managed binding |> ok
           ; parent_job = Some job_id
           ; deadline = Some deadline
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
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun _execution _borrowed ~effect_entry ~stage_background _continuation ->
               match effect_entry () with
               | Error _ -> failwith "managed job child expired before preparation"
               | Ok () ->
                 (match
                    stage_background ~request ~checkpoint:(fun () ->
                      if P.Timestamp.compare !clock deadline < 0
                      then (
                        Eio.Fiber.yield ();
                        clock := expired_at))
                  with
                  | Error _ ->
                    Int.incr denied;
                    Error
                      (I.Fail
                         { code = "expired"
                         ; message = ""
                         ; retryable = false
                         ; details = `Null
                         })
                  | Ok _ -> failwith "expired claimed parent reserved child work"))
         }
       in
       let expected = R.observe handle |> ok in
       ignore
         (O.with_claimed_job
            root
            ~expected
            ~lookup:(R.find_job_exact expected job_id)
            (fun ~active ~sw:_ ->
               let terminal =
                 X.run_job_child
                   executor
                   ~active
                   ~managed:(Some service)
                   ~reference
                   ~invocation:admitted
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(function
                     | Openai.Responses.Tool_output.Output.Text value ->
                       Ok (`String value)
                     | _ -> Error (P.Error.invalid_request "unexpected managed result"))
                 |> ok
               in
               (match H.status terminal with
                | Resolved (Fail _) -> ()
                | _ -> failwith "managed preparation ignored claimed-parent expiry");
               Ok (succeeded_claim (Scope.claim active)))
          |> ok
          : P.Developer_job.t option);
       assert (Int.equal !denied 1);
       ignore (stage_root_job env root selected : P.Id.Job.t))
;;

let%test_unit "managed job child stages exact claimed attempt and nesting depth" =
  let module M = Chat_response.Managed_tool_registry in
  let managed_definition = ref None in
  let selected_child = ref None in
  with_executor
    ~run_name:"dvr_job_managed_ancestry"
    ~make_selected:(fun env ->
      let managed, selected = managed_registry env in
      managed_definition := Some managed;
      selected)
    (fun ~env ~root ~executor ~handle ~owner ~selected ->
       let parent_id = stage_root_job env root selected in
       let managed = Option.value_exn !managed_definition in
       let binding = C.find selected ~name:"summary" |> ok in
       let reference = C.reference binding in
       let admitted = invocation owner selected reference ~custom:false in
       let admitted =
         H.create_developer
           { (H.context admitted) with
             implementation_revision = M.execution_revision managed binding |> ok
           ; parent_job = Some parent_id
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
       let service : Agent_session.Native_tool_invocation.selected_managed_dispatch =
         { definition = managed
         ; current = (fun () -> selected)
         ; run =
             (fun _execution _borrowed ~effect_entry ~stage_background continuation ->
               effect_entry () |> ok;
               let id = stage_background ~request ~checkpoint:ignore |> ok in
               selected_child := Some id;
               continuation
                 ~validate_work:(fun _ -> Ok ())
                 ~outcome:(I.Complete (`Object []))
                 ~selected_jobs:[ id ])
         }
       in
       let expected = R.observe handle |> ok in
       ignore
         (O.with_claimed_job
            root
            ~expected
            ~lookup:(R.find_job_exact expected parent_id)
            (fun ~active ~sw:_ ->
               let terminal =
                 X.run_job_child
                   executor
                   ~active
                   ~managed:(Some service)
                   ~reference
                   ~invocation:admitted
                   ~authorize:(fun _ _ -> Ok ())
                   ~prepare_output:(function
                     | Openai.Responses.Tool_output.Output.Text value ->
                       Ok (`String value)
                     | _ -> Error (P.Error.invalid_request "unexpected managed result"))
                 |> ok
               in
               if
                 not
                   (I.equal_status (H.status terminal) (Resolved (Complete (`Object []))))
               then failwith (Sexp.to_string_hum (I.sexp_of_status (H.status terminal)));
               Ok (succeeded_claim (Scope.claim active)))
          |> ok
          : P.Developer_job.t option);
       let child_id = Option.value_exn !selected_child in
       let child = R.find_job (R.observe handle |> ok) child_id |> Option.value_exn in
       let launch = P.Developer_job.launch child in
       (match P.Developer_job.Launch.parent launch with
        | Invocation id when P.Id.Invocation.equal id (H.context admitted).id -> ()
        | Run_root | Invocation _ -> failwith "child launch lost invocation parent");
       (match P.Developer_job.Launch.parent_job launch with
        | Some (id, attempt) when P.Id.Job.equal id parent_id && Int.equal attempt 1 -> ()
        | None | Some _ -> failwith "child launch lost claimed attempt");
       assert (Int.equal (P.Developer_job.Launch.nested_depth launch) 1))
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
